# Security-critical: all filter values reach the database only through rlang
# quosures injected into objectSetsR::os_filter(), never via string interpolation.

VALID_OPS <- c(
  "eq", "neq", "gt", "gte", "lt", "lte",
  "contains", "starts_with", "in", "not_in",
  "is_null", "is_not_null"
)

#' Build a dbplyr-compatible rlang expression for one filter operator
#'
#' @param col  An rlang symbol (column name).
#' @param op   Filter operator string; must be one of `VALID_OPS`.
#' @param value R value to compare against (already parsed from JSON).
#' @return An unevaluated rlang expression.
build_filter_expr <- function(col, op, value) {
  switch(op,
    eq          = rlang::expr(!!col == !!value),
    neq         = rlang::expr(!!col != !!value),
    gt          = rlang::expr(!!col > !!value),
    gte         = rlang::expr(!!col >= !!value),
    lt          = rlang::expr(!!col < !!value),
    lte         = rlang::expr(!!col <= !!value),
    contains    = rlang::expr(grepl(!!value, !!col, fixed = TRUE)),
    starts_with = rlang::expr(startsWith(!!col, !!value)),
    `in`        = rlang::expr(!!col %in% !!value),
    not_in      = rlang::expr(!(!!col %in% !!value)),
    is_null     = rlang::expr(is.na(!!col)),
    is_not_null = rlang::expr(!is.na(!!col))
  )
}

#' Apply one filter spec to an ObjectSet
#'
#' @param os          An objectSetsR ObjectSet.
#' @param filter_spec Named list with keys `property`, `op`, `value`.
#' @return Updated ObjectSet.
#' @details Aborts with HTTP 400 if `op` is unknown.
translate_filter <- function(os, filter_spec) {
  property <- filter_spec[["property"]]
  op       <- filter_spec[["op"]]
  value    <- filter_spec[["value"]]

  if (!op %in% VALID_OPS) {
    http_abort(
      paste0(
        "Unknown filter op '", op, "'. Valid ops: ",
        paste(VALID_OPS, collapse = ", ")
      ),
      status = 400L
    )
  }

  col  <- rlang::sym(property)
  expr <- build_filter_expr(col, op, value)

  rlang::inject(objectSetsR::os_filter(os, !!expr))
}

#' Apply a list of filter specs sequentially to an ObjectSet
#'
#' @param os      An objectSetsR ObjectSet.
#' @param filters List of filter spec lists, or NULL / empty list.
#' @return Updated ObjectSet (unchanged if `filters` is empty).
apply_filters <- function(os, filters) {
  if (is.null(filters) || length(filters) == 0L) return(os)
  for (f in filters) os <- translate_filter(os, f)
  os
}
