# One JSON encoding for both protocols, so a handler result serialises the
# same way whether plumber (REST) or the MCP transports write it.
#
# * auto_unbox: length-1 vectors become scalars. Handlers return arrays as
#   unnamed lists, which always stay arrays.
# * NULL and NA become JSON null.
# * digits = NA keeps full precision.

JSON_OPTIONS <- list(auto_unbox = TRUE, null = "null", na = "null", digits = NA)

to_json <- function(x, pretty = FALSE) {
  do.call(jsonlite::toJSON, c(list(x), JSON_OPTIONS, list(pretty = pretty)))
}

# A named list that serialises as {} even when empty.
json_object <- function(x) {
  if (length(x) == 0L) return(structure(list(), names = character(0)))
  x
}
