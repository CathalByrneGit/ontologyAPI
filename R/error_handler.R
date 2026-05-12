#' Build a plumber error handler that wraps errors in the standard envelope
#'
#' HTTP status is inferred from the condition class:
#'   `http_400` → 400, `http_404` → 404, `http_422` → 422, default → 500.
#'
#' @return A function with signature `function(req, res, err)`.
make_error_handler <- function() {
  function(req, res, err) {
    status <- 500L
    if (inherits(err, "http_400")) status <- 400L
    else if (inherits(err, "http_404")) status <- 404L
    else if (inherits(err, "http_422")) status <- 422L

    res$status <- status
    error_response(conditionMessage(err))
  }
}

#' Abort with an HTTP-status-tagged condition
#'
#' @param message Human-readable error message.
#' @param status  HTTP status code integer (400, 404, or 422).
#' @param call    Calling environment for the condition.
http_abort <- function(message, status = 400L, call = rlang::caller_env()) {
  cls <- switch(as.character(status),
    "400" = "http_400",
    "404" = "http_404",
    "422" = "http_422",
    "http_error"
  )
  rlang::abort(message, class = c(cls, "error"), call = call)
}
