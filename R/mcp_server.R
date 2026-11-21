#' Build an MCP server for an ontology service
#'
#' Creates the tool registry with the ten built-in tools
#' (`get_ontology_summary`, `list_object_types`, `list_link_types`,
#' `query_objects`, `count_objects`, `get_object`, `traverse_links`,
#' `list_concepts`, `evaluate_concept`, `submit_action`), each a thin wrapper
#' around the same shared handlers the REST routes use. Add more with
#' [mcp_add_tool()] and start it with [serve_mcp()].
#'
#' The server has reference semantics (it is an environment): adding a tool
#' changes the server in place, so code that ignores the value returned by
#' [mcp_add_tool()] still sees the new tool.
#'
#' @param svc An `OntologyService` from [ontology_service()].
#' @param name Server name reported in the MCP `initialize` handshake.
#' @return An `ontologyAPI_mcp_server` object.
#' @export
mcp_server <- function(svc, name = "ontologyAPI") {
  assert_service(svc)
  if (!is.character(name) || length(name) != 1L || !nzchar(name)) {
    rlang::abort("`name` must be a non-empty string.")
  }
  server <- new.env(parent = emptyenv())
  server$svc <- svc
  server$name <- name
  server$tools <- list()
  class(server) <- "ontologyAPI_mcp_server"

  for (tool in builtin_tools()) {
    mcp_add_tool(server, tool$name, tool$description, tool$input_schema, tool$handler)
  }
  server
}

#' Add a tool to an MCP server
#'
#' @param server An MCP server from [mcp_server()].
#' @param name Tool name. Must be unique on the server.
#' @param description Tool description shown to the model.
#' @param input_schema JSON Schema (as an R list) for the tool's arguments.
#'   An empty `properties` list is sent as `{}`.
#' @param handler Function called with the tool's arguments (a list). It may
#'   take one argument, `function(params)`, or two, `function(params, server)`
#'   to reach `server$svc`. It may return an MCP tool result (a list with
#'   `content`), or any other value, which is returned as JSON text. Errors
#'   raised with the `ontapi_bad_request` / `ontapi_not_found` /
#'   `ontapi_forbidden` classes become the matching JSON-RPC errors; any other
#'   error becomes -32603.
#' @return The server, invisibly (it is also modified in place).
#' @export
mcp_add_tool <- function(server, name, description, input_schema, handler) {
  assert_mcp_server(server)
  if (!is.character(name) || length(name) != 1L || !nzchar(name)) {
    rlang::abort("Tool `name` must be a non-empty string.")
  }
  if (!is.null(server$tools[[name]])) {
    rlang::abort(
      paste0("An MCP tool named '", name, "' is already registered."),
      class = "ontapi_duplicate_tool"
    )
  }
  if (!is.character(description) || length(description) != 1L) {
    rlang::abort("Tool `description` must be a string.")
  }
  if (!is.list(input_schema)) {
    rlang::abort("`input_schema` must be a list (a JSON Schema object).")
  }
  if (!is.function(handler)) {
    rlang::abort("`handler` must be a function.")
  }
  input_schema$type <- input_schema$type %||% "object"
  if (is.null(input_schema$properties) || length(input_schema$properties) == 0L) {
    input_schema$properties <- json_object(list())
  }

  tools <- server$tools
  tools[[name]] <- list(
    name = name,
    description = description,
    input_schema = input_schema,
    handler = handler
  )
  server$tools <- tools
  invisible(server)
}

#' Serve the ontology over the Model Context Protocol
#'
#' @param server An MCP server from [mcp_server()], or an `OntologyService`
#'   (wrapped with `mcp_server()`).
#' @param transport `"stdio"` (for desktop clients that launch the process) or
#'   `"sse"` (HTTP + server-sent events; needs httpuv and later).
#' @param port Port for the SSE transport. Default 3000.
#' @details With stdio, stdout carries only JSON-RPC messages: all R console
#'   output, messages and `cli` output are sent to stderr while serving.
#' @export
serve_mcp <- function(server, transport = c("stdio", "sse"), port = 3000L) {
  transport <- match.arg(transport)
  if (is_ontology_service(server)) server <- mcp_server(server)
  assert_mcp_server(server)

  switch(transport,
    stdio = run_stdio(server),
    sse   = run_sse(server, port)
  )
  invisible(NULL)
}

#' @export
print.ontologyAPI_mcp_server <- function(x, ...) {
  cat(sprintf("<ontologyAPI MCP server> %s, %d tools: %s\n",
              x$name, length(x$tools), paste(names(x$tools), collapse = ", ")))
  invisible(x)
}

assert_mcp_server <- function(server, call = rlang::caller_env()) {
  if (!inherits(server, "ontologyAPI_mcp_server")) {
    rlang::abort("Expected an MCP server from `mcp_server()`.", call = call)
  }
  invisible(server)
}
