#' Build the ontology service shared by the REST and MCP servers
#'
#' Normalises the bundle once with `ontologySpecR::as_bundle()` and builds the
#' three runtime contexts every handler needs:
#'
#' * an `objectSetsR` `objectSetsR::ontology_context()` for queries and links,
#' * a `conceptR` `conceptR::concept_context()` (which also creates the audit
#'   tables) for concepts,
#' * an `actionTypesR` `actionTypesR::action_context()` for actions.
#'
#' Both [ontology_router()] / [serve_ontology()] and [mcp_server()] /
#' [serve_mcp()] take the returned object, so the two protocols always answer
#' from the same handlers.
#'
#' To run R action handlers, register them on the action context before
#' serving, e.g.
#' `svc$action_ctx <- actionTypesR::register_handler(svc$action_ctx, "MyAction", fn)`.
#'
#' @param bundle An `ontologySpecR` bundle, or anything
#'   `ontologySpecR::as_bundle()` accepts (a list or a path to a JSON file).
#' @param connection A DBI connection holding the object tables. `conceptR` and
#'   `actionTypesR` create their own tables in it if they are missing.
#' @param actor Character scalar naming who is acting (recorded by `conceptR`,
#'   passed to `auth`). Defaults to the OS user.
#' @param auth Optional authorisation function
#'   `function(actor, operation, resource)` returning `TRUE` to allow. It is
#'   called before every handler with `operation` one of `"read"`,
#'   `"evaluate_concept"`, `"submit_action"` and `resource` the object type,
#'   concept or action type id (or `NULL`). Returning anything but `TRUE`
#'   raises an `ontapi_forbidden` error (HTTP 403 / JSON-RPC -32001). `NULL`
#'   allows everything.
#' @return An `OntologyService` object.
#' @export
ontology_service <- function(bundle, connection, actor = NULL, auth = NULL) {
  if (!is.null(auth) && !is.function(auth)) {
    rlang::abort("`auth` must be NULL or a function(actor, operation, resource).")
  }
  actor <- actor %||% Sys.info()[["user"]]
  if (!is.character(actor) || length(actor) != 1L || !nzchar(actor)) {
    rlang::abort("`actor` must be a non-empty character scalar.")
  }

  spec <- ontologySpecR::as_bundle(bundle)

  structure(
    list(
      bundle      = spec,
      connection  = connection,
      actor       = actor,
      auth        = auth,
      object_ctx  = objectSetsR::ontology_context(spec, connection),
      concept_ctx = conceptR::concept_context(spec, connection, actor = actor),
      action_ctx  = actionTypesR::action_context(spec, connection)
    ),
    class = "OntologyService"
  )
}

#' @export
print.OntologyService <- function(x, ...) {
  b <- x$bundle
  cat(sprintf(
    "<OntologyService> %s %s: %d object types, %d link types, %d action types (actor: %s)\n",
    ontologySpecR::bundle_id(b),
    ontologySpecR::bundle_version(b),
    length(ontologySpecR::bundle_object_types(b)),
    length(ontologySpecR::bundle_link_types(b)),
    length(ontologySpecR::bundle_action_types(b)),
    x$actor
  ))
  invisible(x)
}

is_ontology_service <- function(x) inherits(x, "OntologyService")

assert_service <- function(svc, call = rlang::caller_env()) {
  if (!is_ontology_service(svc)) {
    rlang::abort(
      "Expected an `OntologyService` from `ontology_service()`.",
      call = call
    )
  }
  invisible(svc)
}

# Run the service's auth hook, if any.
authorize <- function(svc, operation, resource = NULL) {
  if (is.null(svc$auth)) return(invisible(TRUE))
  ok <- svc$auth(svc$actor, operation, resource)
  if (!isTRUE(ok)) {
    what <- if (is.null(resource)) operation else paste0(operation, " on '", resource, "'")
    ontapi_forbidden(paste0("Actor '", svc$actor, "' is not allowed to ", what, "."))
  }
  invisible(TRUE)
}
