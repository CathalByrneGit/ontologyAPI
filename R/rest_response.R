#' Standard JSON envelope for successful REST responses
#'
#' @param data The payload to return under the `data` key.
#' @param took_ms Integer milliseconds elapsed.
#' @return Named list for plumber's JSON serialiser.
#' @noRd
ok_response <- function(data, took_ms = 0L) {
  list(
    data = data,
    meta = list(status = "ok", took_ms = as.integer(took_ms))
  )
}

#' Standard JSON envelope for REST error responses
#'
#' @param message Error message string.
#' @param took_ms Integer milliseconds elapsed.
#' @param kind Optional error kind (`"bad_request"`, `"not_found"`,
#'   `"forbidden"`, `"unauthorized"`, `"internal"`).
#' @noRd
error_response <- function(message, took_ms = 0L, kind = NULL) {
  meta <- list(status = "error", took_ms = as.integer(took_ms))
  if (!is.null(kind)) meta$kind <- kind
  list(error = as.character(message), meta = meta)
}

elapsed_ms <- function(start_time) {
  as.integer(round((proc.time()[["elapsed"]] - start_time) * 1000))
}

# Run one handler call for a route and wrap it in the envelope. Errors
# propagate to the router's error handler (make_error_handler()).
rest_ok <- function(expr) {
  start <- proc.time()[["elapsed"]]
  data <- force(expr)
  ok_response(data, elapsed_ms(start))
}
