#' Create and configure the ontology API plumber router
#'
#' Assembles the full REST API by mounting sub-routers for bundle, objects,
#' actions, health, and optionally concepts. Registers a global Bearer-token
#' auth filter and a global JSON error handler.
#'
#' @param bundle      An `ontologySpecR` bundle object.
#' @param connection  A DBI connection to the backend database.
#' @param action_ctx  An `actionTypesR` ActionContext, or `NULL` to disable
#'   action endpoints.
#' @param concept_ctx A `conceptR` ConceptContext, or `NULL` to disable
#'   concept endpoints.
#' @param api_key     Character API key, or `NULL` to read
#'   `ONTOLOGY_API_KEY` from the environment. If the env var is also unset
#'   the server starts in unauthenticated development mode.
#' @return A configured `plumber::Plumber` router. Call `$run()` or pass it
#'   to [serve_ontology()] to start the HTTP server.
#' @export
ontology_router <- function(bundle,
                             connection,
                             action_ctx  = NULL,
                             concept_ctx = NULL,
                             api_key     = NULL) {
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

  object_ctx <- objectSetsR::ontology_context(bundle, connection)

  ctx <- list(
    bundle      = bundle,
    connection  = connection,
    object_ctx  = object_ctx,
    action_ctx  = action_ctx,
    concept_ctx = concept_ctx,
    api_key     = api_key
  )

  pr <- plumber::pr()
  pr <- plumber::pr_set_serializer(pr, plumber::serializer_json(auto_unbox = FALSE))
  pr <- plumber::pr_filter(pr, "auth", make_auth_filter(api_key))
  pr <- plumber::pr_set_error(pr, make_error_handler())

  pr <- plumber::pr_mount(pr, "/v1/health",  health_router(ctx))
  pr <- plumber::pr_mount(pr, "/v1/bundle",  bundle_router(ctx))
  pr <- plumber::pr_mount(pr, "/v1/objects", objects_router(ctx))
  pr <- plumber::pr_mount(pr, "/v1/actions", actions_router(ctx))

  if (!is.null(concept_ctx)) {
    pr <- plumber::pr_mount(pr, "/v1/concepts", concepts_router(ctx))
  }

  pr
}

#' Start the ontology API HTTP server
#'
#' Convenience wrapper around [ontology_router()] that immediately calls
#' `plumber::pr_run()`.
#'
#' @param ...  Arguments forwarded to [ontology_router()].
#' @param host Character host address. Default `"127.0.0.1"`.
#' @param port Integer port. Default `8080L`.
#' @param docs Logical. Serve Swagger UI at `/docs`. Default `TRUE`.
#' @export
serve_ontology <- function(...,
                            host = "127.0.0.1",
                            port = 8080L,
                            docs = TRUE) {
  pr <- ontology_router(...)

  if (isTRUE(docs)) {
    pr <- plumber::pr_set_docs(pr, "swagger")
  }

  plumber::pr_run(pr, host = host, port = port)
}
