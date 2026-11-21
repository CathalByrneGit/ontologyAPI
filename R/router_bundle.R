#' Build the /v1/bundle plumber sub-router
#'
#' @param svc An `OntologyService`.
#' @return A `plumber::Plumber` object.
#' @noRd
bundle_router <- function(svc) {
  pr <- rest_sub_router()
  pr <- plumber::pr_get(pr, "/objects", function(req, res) rest_ok(h_object_types(svc)))
  pr <- plumber::pr_get(pr, "/links", function(req, res) rest_ok(h_link_types(svc)))
  pr <- plumber::pr_get(pr, "/actions", function(req, res) rest_ok(h_action_types(svc)))
  pr <- plumber::pr_get(pr, "/summary", function(req, res) rest_ok(h_summary(svc)))
  pr
}
