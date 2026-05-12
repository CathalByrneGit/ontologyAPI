#' Build a plumber filter that enforces Bearer-token authentication
#'
#' Returns a closure suitable for `plumber::pr_filter()`.
#' When `api_key` is NULL or empty the filter is a no-op (dev mode).
#'
#' @param api_key Character scalar API key, or NULL / "" for no auth.
#' @return A function with signature `function(req, res)`.
make_auth_filter <- function(api_key) {
  function(req, res) {
    if (is.null(api_key) || !nzchar(api_key)) {
      plumber::forward()
      return(invisible(NULL))
    }

    auth_header <- req$HTTP_AUTHORIZATION
    if (is.null(auth_header) || !startsWith(auth_header, "Bearer ")) {
      res$status <- 401L
      return(error_response("Unauthorized: missing or malformed Authorization header"))
    }

    provided_key <- substring(auth_header, 8L)
    if (!identical(provided_key, api_key)) {
      res$status <- 401L
      return(error_response("Unauthorized: invalid API key"))
    }

    plumber::forward()
  }
}
