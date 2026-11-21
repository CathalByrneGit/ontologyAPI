# Errors are defined once, here, and raised by the shared handlers. Each
# protocol maps the classes to its own codes:
#
#   class               REST   MCP (JSON-RPC)
#   ontapi_bad_request  400    -32602 (invalid params)
#   ontapi_not_found    404    -32602 (invalid params)
#   ontapi_forbidden    403    -32001 (server-defined)
#   anything else       500    -32603 (internal error)
#
# The REST mapping lives in rest_status_for(), the MCP one in
# mcp_code_for() (R/mcp_errors.R).

ONTAPI_ERROR_KINDS <- c("bad_request", "not_found", "forbidden")

#' Signal a classed ontologyAPI error
#'
#' @param kind One of `"bad_request"`, `"not_found"`, `"forbidden"`.
#' @param message Error message.
#' @param ... Extra fields stored on the condition.
#' @param call Calling environment.
#' @noRd
ontapi_abort <- function(kind, message, ..., call = rlang::caller_env()) {
  kind <- match.arg(kind, ONTAPI_ERROR_KINDS)
  rlang::abort(
    message,
    class = c(paste0("ontapi_", kind), "ontapi_error"),
    ...,
    call = call
  )
}

ontapi_bad_request <- function(message, ..., call = rlang::caller_env()) {
  ontapi_abort("bad_request", message, ..., call = call)
}

ontapi_not_found <- function(message, ..., call = rlang::caller_env()) {
  ontapi_abort("not_found", message, ..., call = call)
}

ontapi_forbidden <- function(message, ..., call = rlang::caller_env()) {
  ontapi_abort("forbidden", message, ..., call = call)
}

# The error kind of a condition ("bad_request", "not_found", "forbidden"), or
# NULL for any other error.
ontapi_error_kind <- function(err) {
  for (kind in ONTAPI_ERROR_KINDS) {
    if (inherits(err, paste0("ontapi_", kind))) return(kind)
  }
  NULL
}

# HTTP status for a condition.
rest_status_for <- function(err) {
  switch(ontapi_error_kind(err) %||% "internal",
    bad_request = 400L,
    not_found   = 404L,
    forbidden   = 403L,
    500L
  )
}
