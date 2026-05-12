#' Standard JSON envelope for successful responses
#'
#' @param data The payload to return under the `data` key.
#' @param took_ms Integer milliseconds elapsed.
#' @return Named list suitable for plumber JSON serialisation.
ok_response <- function(data, took_ms = 0L) {
  list(
    data = data,
    meta = list(
      status  = jsonlite::unbox("ok"),
      took_ms = jsonlite::unbox(as.integer(took_ms))
    )
  )
}

#' Standard JSON envelope for error responses
#'
#' @param message Error message string.
#' @param took_ms Integer milliseconds elapsed.
#' @return Named list suitable for plumber JSON serialisation.
error_response <- function(message, took_ms = 0L) {
  list(
    error = jsonlite::unbox(as.character(message)),
    meta  = list(
      status  = jsonlite::unbox("error"),
      took_ms = jsonlite::unbox(as.integer(took_ms))
    )
  )
}

#' Milliseconds elapsed since a `proc.time()` snapshot
#'
#' @param start_time Named numeric from `proc.time()`.
#' @return Integer.
elapsed_ms <- function(start_time) {
  as.integer(round((proc.time()["elapsed"] - start_time) * 1000))
}

#' Convert a data frame to a list of row records
#'
#' Scalar values within each row are unboxed so they serialise as JSON
#' scalars rather than single-element arrays.
#'
#' @param df A data frame.
#' @return A list of named lists, one per row.
df_to_list <- function(df) {
  if (!is.data.frame(df) || nrow(df) == 0L) return(list())
  lapply(seq_len(nrow(df)), function(i) {
    row <- as.list(df[i, , drop = FALSE])
    lapply(row, function(v) {
      if (length(v) == 1L && !is.list(v)) jsonlite::unbox(v) else v
    })
  })
}
