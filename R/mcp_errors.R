# JSON-RPC error codes (from ontologyMCP's errors.R).
MCP_PARSE_ERROR      <- -32700L
MCP_INVALID_REQUEST  <- -32600L
MCP_METHOD_NOT_FOUND <- -32601L
MCP_INVALID_PARAMS   <- -32602L
MCP_INTERNAL_ERROR   <- -32603L
# Server-defined range (-32000 to -32099).
MCP_FORBIDDEN        <- -32001L

#' Signal an MCP protocol error
#'
#' Throws a condition caught by handle_message() and turned into a JSON-RPC
#' error response. Handler errors use the shared ontapi_* classes instead and
#' are mapped by mcp_code_for().
#'
#' @param code Integer error code (use MCP_* constants).
#' @param message Human-readable error description.
#' @param data Optional additional data.
#' @noRd
mcp_error <- function(code, message, data = NULL) {
  stop(structure(
    class = c("mcp_error", "error", "condition"),
    list(message = message, code = code, data = data, call = NULL)
  ))
}

mcp_error_response <- function(id, code, message, data = NULL) {
  err <- list(code = code, message = message)
  if (!is.null(data)) err$data <- data
  list(jsonrpc = "2.0", id = id, error = err)
}

# JSON-RPC code for a shared handler error (see R/errors.R).
mcp_code_for <- function(err) {
  switch(ontapi_error_kind(err) %||% "internal",
    bad_request = MCP_INVALID_PARAMS,
    not_found   = MCP_INVALID_PARAMS,
    forbidden   = MCP_FORBIDDEN,
    MCP_INTERNAL_ERROR
  )
}
