MCP_PROTOCOL_VERSION <- "2024-11-05"

#' Dispatch a single JSON-RPC message
#'
#' @param message Parsed list (from `jsonlite::fromJSON(simplifyVector = FALSE)`).
#' @param server An MCP server from [mcp_server()].
#' @return The JSON-RPC response as a list, or `NULL` for notifications.
#' @noRd
handle_message <- function(message, server) {
  if (!is.list(message)) {
    return(mcp_error_response(NULL, MCP_INVALID_REQUEST, "Request must be a JSON object"))
  }
  id     <- message$id
  method <- message$method
  params <- message$params %||% list()

  if (is.null(method)) {
    return(mcp_error_response(id, MCP_INVALID_REQUEST, "Missing method"))
  }

  # Notifications (no id, or the notifications/ namespace) get no response.
  if (startsWith(method, "notifications/")) {
    return(invisible(NULL))
  }

  tryCatch(
    {
      result <- switch(method,
        "initialize"     = handle_initialize(params, server),
        "tools/list"     = handle_tools_list(params, server),
        "tools/call"     = handle_tools_call(params, server),
        "resources/list" = handle_resources_list(params, server),
        "resources/read" = handle_resources_read(params, server),
        "ping"           = json_object(list()),
        mcp_error(MCP_METHOD_NOT_FOUND, paste0("Unknown method: ", method))
      )
      list(jsonrpc = "2.0", id = id, result = result)
    },
    mcp_error = function(e) {
      mcp_error_response(id, e$code, e$message, e$data)
    },
    ontapi_error = function(e) {
      mcp_error_response(id, mcp_code_for(e), conditionMessage(e),
                         list(kind = ontapi_error_kind(e)))
    },
    error = function(e) {
      mcp_error_response(id, MCP_INTERNAL_ERROR, conditionMessage(e))
    }
  )
}

handle_initialize <- function(params, server) {
  list(
    protocolVersion = MCP_PROTOCOL_VERSION,
    capabilities = list(
      tools     = list(listChanged = FALSE),
      resources = list(subscribe = FALSE, listChanged = FALSE)
    ),
    serverInfo = list(
      name    = server$name,
      version = as.character(utils::packageVersion("ontologyAPI"))
    )
  )
}

handle_tools_list <- function(params, server) {
  list(tools = unname(lapply(server$tools, function(t) {
    list(name = t$name, description = t$description, inputSchema = t$input_schema)
  })))
}

handle_tools_call <- function(params, server) {
  tool_name <- params$name
  if (is.null(tool_name)) mcp_error(MCP_INVALID_PARAMS, "Tool name required")
  args <- params$arguments %||% list()

  tool <- server$tools[[tool_name]]
  if (is.null(tool)) {
    mcp_error(MCP_METHOD_NOT_FOUND, paste0("Unknown tool: ", tool_name))
  }
  call_tool(tool, args, server)
}

# Run a tool's handler and normalise the result to an MCP tool result. A
# handler may take `(args)` or `(args, server)`, and may return a ready MCP
# result (a list with `content`) or any value, which is sent as JSON text.
call_tool <- function(tool, args, server) {
  fn <- tool$handler
  n_args <- length(formals(fn))
  out <- if (n_args >= 2L || "..." %in% names(formals(fn))) fn(args, server) else fn(args)
  if (is.list(out) && !is.null(out$content)) out else mcp_text(to_json(out, pretty = TRUE))
}

mcp_text <- function(text) {
  list(content = list(list(type = "text", text = as.character(text))))
}
