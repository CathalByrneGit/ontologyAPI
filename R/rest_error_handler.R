#' Build the plumber error handler
#'
#' Maps the shared error classes to HTTP statuses with [rest_status_for()]:
#' `ontapi_bad_request` 400, `ontapi_not_found` 404, `ontapi_forbidden` 403,
#' anything else 500.
#'
#' @return A function with signature `function(req, res, err)`.
#' @noRd
make_error_handler <- function() {
  function(req, res, err) {
    status <- rest_status_for(err)
    res$status <- status
    kind <- ontapi_error_kind(err) %||% "internal"
    if (status == 500L) {
      cli::cli_inform(c("x" = "Internal error: {conditionMessage(err)}"),
                      class = "ontapi_log")
    }
    error_response(conditionMessage(err), kind = kind)
  }
}
