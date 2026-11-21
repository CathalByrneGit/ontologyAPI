# MCP over HTTP: GET /sse opens the event stream and announces the POST
# endpoint; POST /message carries one JSON-RPC message and returns the
# response in the HTTP body. Moved in from ontologyMCP.

run_sse <- function(server, port) {
  require_pkg("httpuv", "the MCP SSE transport")
  require_pkg("later", "the MCP SSE transport")

  # Each SSE client gets a queue of pending messages.
  clients <- new.env(parent = emptyenv())
  clients$.counter <- 0L

  app <- list(
    call = function(req) {
      path   <- req$PATH_INFO
      method <- req$REQUEST_METHOD

      if (path == "/sse" && method == "GET") {
        handle_sse_connect(req, clients, server)
      } else if (path == "/message" && method == "POST") {
        handle_sse_post(req, clients, server)
      } else {
        list(status = 404L,
             headers = list("Content-Type" = "text/plain"),
             body    = "Not Found")
      }
    },
    onWSOpen  = NULL,
    onWSClose = NULL
  )

  http <- httpuv::startServer("0.0.0.0", port, app)
  log_stderr(paste0(server$name, " MCP SSE transport on port ", port))

  on.exit(httpuv::stopServer(http), add = TRUE)

  while (TRUE) {
    later::run_now(0.1)
    httpuv::service(0L)
  }
}

handle_sse_connect <- function(req, clients, server) {
  clients$.counter <- clients$.counter + 1L
  client_id <- paste0("c", clients$.counter)

  # SSE response uses a streaming body (httpuv handles this via a connection)
  queue <- list()
  clients[[client_id]] <- list(queue = queue, active = TRUE)

  # Send endpoint event so the client knows where to POST
  endpoint_evt <- sse_event("endpoint", paste0("/message?client_id=", client_id))

  list(
    status  = 200L,
    headers = list(
      "Content-Type"  = "text/event-stream",
      "Cache-Control" = "no-cache",
      "Connection"    = "keep-alive",
      "Access-Control-Allow-Origin" = "*"
    ),
    body = endpoint_evt
  )
}

handle_sse_post <- function(req, clients, server) {
  body <- req$rook.input$read_lines()
  body <- paste(body, collapse = "\n")

  message <- tryCatch(
    jsonlite::fromJSON(body, simplifyVector = FALSE),
    error = function(e) NULL
  )

  if (is.null(message)) {
    return(list(status = 400L,
                headers = list("Content-Type" = "application/json"),
                body = '{"error":"Parse error"}'))
  }

  response <- handle_message(message, server)
  if (is.null(response)) {
    return(list(status = 202L, headers = list("Access-Control-Allow-Origin" = "*"),
                body = ""))
  }
  json <- to_json(response)

  list(
    status  = 200L,
    headers = list("Content-Type" = "application/json",
                   "Access-Control-Allow-Origin" = "*"),
    body    = as.character(json)
  )
}

sse_event <- function(event, data) {
  paste0("event: ", event, "\ndata: ", data, "\n\n")
}
