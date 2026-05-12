#' Build the /v1/actions plumber sub-router
#'
#' @param ctx Server context list created by `ontology_router()`.
#' @return A configured `plumber::Plumber` object.
actions_router <- function(ctx) {
  pr <- plumber::pr()

  # POST /v1/actions/{action_type_id}/submit
  plumber::pr_post(pr, "/<action_type_id>/submit",
    function(action_type_id, req, res) {
      start   <- proc.time()["elapsed"]
      body    <- req$body %||% list()
      params  <- body$params     %||% list()
      targets <- body$target_ids %||% character(0L)
      dry_run <- isTRUE(body$dry_run)

      if (is.null(ctx$action_ctx)) {
        http_abort("Action context not configured on this server", status = 422L)
      }

      if (dry_run) {
        action_def <- tryCatch(
          actionTypesR::get_action_type(ctx$action_ctx, action_type_id),
          error = function(e) http_abort(conditionMessage(e), status = 404L)
        )
        impl_kind <- tryCatch(
          actionTypesR::action_impl_kind(action_def),
          error = function(e) "r"
        )
        plan <- list(
          action_type_id = jsonlite::unbox(action_type_id),
          params         = params,
          target_ids     = targets,
          impl_kind      = jsonlite::unbox(as.character(impl_kind))
        )
        return(ok_response(list(plan = plan), elapsed_ms(start)))
      }

      tryCatch(
        actionTypesR::submit_action(
          ctx$action_ctx,
          action_type_id = action_type_id,
          params         = params,
          target_ids     = targets
        ),
        error = function(e) http_abort(conditionMessage(e), status = 422L)
      )

      ok_response(
        list(
          submission_id  = jsonlite::unbox(new_uuid()),
          status         = jsonlite::unbox("success"),
          action_type_id = jsonlite::unbox(action_type_id)
        ),
        elapsed_ms(start)
      )
    }
  )

  # GET /v1/actions/log
  plumber::pr_get(pr, "/log",
    function(req, res, n = "100", action_type_id = NULL) {
      start <- proc.time()["elapsed"]

      if (is.null(ctx$action_ctx)) {
        http_abort("Action context not configured on this server", status = 422L)
      }

      log_entries <- tryCatch(
        actionTypesR::get_action_log(
          ctx$action_ctx,
          n              = as.integer(n),
          action_type_id = action_type_id
        ),
        error = function(e) http_abort(conditionMessage(e), status = 500L)
      )

      rows <- if (is.data.frame(log_entries)) df_to_list(log_entries) else log_entries
      ok_response(rows, elapsed_ms(start))
    }
  )

  pr
}

# Minimal UUID-like string using base R (no external dependency).
new_uuid <- function() {
  hex <- function(n) paste(format(as.hexmode(sample.int(.Machine$integer.max, n)), width = 8L), collapse = "")
  paste0(
    substr(hex(1L), 1L, 8L), "-",
    substr(hex(1L), 1L, 4L), "-4",
    substr(hex(1L), 2L, 4L), "-",
    substr(hex(1L), 1L, 4L), "-",
    substr(hex(2L), 1L, 12L)
  )
}
