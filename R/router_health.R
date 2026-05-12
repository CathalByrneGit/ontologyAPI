#' Build the /v1/health plumber sub-router
#'
#' @param ctx Server context list created by `ontology_router()`.
#' @return A configured `plumber::Plumber` object.
health_router <- function(ctx) {
  pr <- plumber::pr()

  plumber::pr_get(pr, "/", function(req, res) {
    bundle_id <- tryCatch(
      as.character(ontologySpecR::bundle_id(ctx$bundle)),
      error = function(e) "unknown"
    )
    bundle_version <- tryCatch(
      as.character(ontologySpecR::bundle_version(ctx$bundle)),
      error = function(e) "unknown"
    )
    list(
      status         = jsonlite::unbox("ok"),
      bundle_id      = jsonlite::unbox(bundle_id),
      bundle_version = jsonlite::unbox(bundle_version)
    )
  })
}
