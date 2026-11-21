#' Build the REST API router for an ontology service
#'
#' Mounts `/v1/health`, `/v1/bundle`, `/v1/objects`, `/v1/concepts` and
#' `/v1/actions`. Every route parses its request and calls the same shared
#' handler the MCP tools use, so both protocols return the same data.
#'
#' Errors map to HTTP statuses: bad request 400, not found 404, forbidden 403,
#' anything else 500, all in the `{error, meta}` envelope. If an API key is
#' set, every request needs `Authorization: Bearer <key>` (else 401).
#'
#' @param svc An `OntologyService` from [ontology_service()].
#' @param api_key Character API key for Bearer-token auth, or `NULL` to read
#'   `ONTOLOGY_API_KEY` from the environment. If neither is set the server runs
#'   unauthenticated (development mode) and warns.
#' @return A `plumber::Plumber` router. Pass it to [serve_ontology()] or call
#'   `plumber::pr_run()` on it.
#' @export
ontology_router <- function(svc, api_key = NULL) {
  assert_service(svc)
  require_pkg("plumber", "the REST server")

  if (is.null(api_key)) {
    api_key <- Sys.getenv("ONTOLOGY_API_KEY", unset = "")
    if (!nzchar(api_key)) {
      api_key <- NULL
      cli::cli_warn(c(
        "!" = "No {.envvar ONTOLOGY_API_KEY} environment variable set.",
        "i" = "Server is running in {.strong development mode} with no authentication."
      ))
    }
  }

  pr <- rest_sub_router()
  pr <- plumber::pr_filter(pr, "auth", make_auth_filter(api_key))

  # Collection roots live on the top-level router: a mounted "/" route would
  # only match with a trailing slash.
  pr <- plumber::pr_get(pr, "/v1/health", function(req, res) h_health(svc))
  pr <- plumber::pr_get(pr, "/v1/bundle", function(req, res) rest_ok(h_bundle(svc)))
  pr <- plumber::pr_get(pr, "/v1/concepts", function(req, res) rest_ok(h_concepts(svc)))

  pr <- plumber::pr_mount(pr, "/v1/bundle",   bundle_router(svc))
  pr <- plumber::pr_mount(pr, "/v1/objects",  objects_router(svc))
  pr <- plumber::pr_mount(pr, "/v1/concepts", concepts_router(svc))
  pr <- plumber::pr_mount(pr, "/v1/actions",  actions_router(svc))
  pr
}

#' Start the ontology REST API server
#'
#' Builds [ontology_router()] and runs it with `plumber::pr_run()`.
#'
#' @inheritParams ontology_router
#' @param host Host address. Default `"127.0.0.1"`.
#' @param port Port. Default `8080L`.
#' @param docs Serve Swagger UI at `/__docs__/`. Default `TRUE`.
#' @export
serve_ontology <- function(svc, host = "127.0.0.1", port = 8080L, docs = TRUE,
                           api_key = NULL) {
  pr <- ontology_router(svc, api_key = api_key)
  pr <- plumber::pr_set_docs(pr, if (isTRUE(docs)) "swagger" else FALSE)
  plumber::pr_run(pr, host = host, port = port)
}

# Every router, including mounted ones, needs the shared JSON serialiser and
# error handler: plumber does not inherit them into mounts.
rest_sub_router <- function() {
  pr <- plumber::pr()
  pr <- plumber::pr_set_serializer(pr, do.call(plumber::serializer_json, JSON_OPTIONS))
  plumber::pr_set_error(pr, make_error_handler())
}

require_pkg <- function(pkg, what, call = rlang::caller_env()) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    rlang::abort(
      paste0("Package '", pkg, "' is required for ", what,
             ". Install it with install.packages('", pkg, "')."),
      call = call
    )
  }
  invisible(TRUE)
}
