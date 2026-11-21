#' Build the /v1/concepts plumber sub-router
#'
#' @param svc An `OntologyService`.
#' @return A `plumber::Plumber` object.
#' @noRd
concepts_router <- function(svc) {
  pr <- rest_sub_router()

  # GET /v1/concepts/{concept_id}
  pr <- plumber::pr_get(pr, "/<concept_id>", function(concept_id, req, res) {
    rest_ok(h_concept(svc, concept_id))
  })

  # GET /v1/concepts/{concept_id}/evaluate?scope=&version=&stage=&limit=
  pr <- plumber::pr_get(pr, "/<concept_id>/evaluate",
    function(concept_id, req, res) {
      q <- request_query(req)
      rest_ok(h_evaluate_concept(svc, concept_id,
        scope = q$scope, version = q$version, stage = q$stage, limit = q$limit
      ))
    }
  )

  pr
}
