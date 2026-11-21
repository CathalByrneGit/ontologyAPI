#' Build the /v1/actions plumber sub-router
#'
#' @param svc An `OntologyService`.
#' @return A `plumber::Plumber` object.
#' @noRd
actions_router <- function(svc) {
  pr <- rest_sub_router()

  # POST /v1/actions/{action_type_id}/submit
  #   body: {params: {...}, target_ids: [...], dry_run: false}
  pr <- plumber::pr_post(pr, "/<action_type_id>/submit",
    function(action_type_id, req, res) {
      body <- request_body(req)
      rest_ok(h_submit_action(svc, action_type_id,
        params     = body$params,
        target_ids = body$target_ids,
        dry_run    = isTRUE(body$dry_run)
      ))
    }
  )

  # GET /v1/actions/log?n=100&action_type_id=
  pr <- plumber::pr_get(pr, "/log",
    function(req, res) {
      q <- request_query(req)
      rest_ok(h_action_log(svc, n = q$n, action_type_id = q$action_type_id))
    }
  )

  pr
}
