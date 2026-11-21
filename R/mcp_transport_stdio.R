#' Run the MCP stdio transport
#'
#' Reads one JSON-RPC message per line from `input` and writes one response
#' per line to `output`, until end of input.
#'
#' Anything else written to stdout would corrupt the protocol, so while it
#' runs R's standard output is diverted to stderr (`sink()`), and messages,
#' warnings and `cli` output go to stderr too. Responses are written to the
#' process's C-level stdout (`file("stdout")`), which `sink()` does not touch.
#'
#' @param server An MCP server.
#' @param input,output Connections; default the process's stdin and stdout.
#' @noRd
run_stdio <- function(server, input = NULL, output = NULL) {
  if (is.null(input)) {
    input <- file("stdin", open = "r", blocking = TRUE)
    on.exit(close(input), add = TRUE)
  }
  if (is.null(output)) {
    output <- file("stdout", open = "w")
    on.exit(close(output), add = TRUE)
  }

  sink(stderr(), type = "output")
  on.exit(sink(type = "output"), add = TRUE)
  old <- options(cli.default_handler = stderr_cli_handler, warn = 1)
  on.exit(options(old), add = TRUE)

  log_stderr(paste0(server$name, " MCP stdio transport ready"))

  repeat {
    line <- tryCatch(readLines(input, n = 1L, warn = FALSE),
                     error = function(e) character(0L))
    if (length(line) == 0L) break
    line <- trimws(line)
    if (!nzchar(line)) next

    response <- process_line(line, server)
    if (!is.null(response)) write_message(response, output)
  }

  invisible(NULL)
}

# Parse one line and dispatch it, returning the response list (or NULL).
process_line <- function(line, server) {
  message <- tryCatch(
    jsonlite::fromJSON(line, simplifyVector = FALSE),
    error = function(e) NULL
  )
  if (is.null(message)) {
    return(mcp_error_response(NULL, MCP_PARSE_ERROR, "Parse error"))
  }
  withCallingHandlers(
    handle_message(message, server),
    message = function(m) {
      cat(conditionMessage(m), file = stderr())
      invokeRestart("muffleMessage")
    }
  )
}

write_message <- function(response, output) {
  writeLines(as.character(to_json(response)), output, useBytes = TRUE)
  flush(output)
}

log_stderr <- function(text) {
  cat(text, "\n", sep = "", file = stderr())
}

# cli handler that writes everything to stderr.
stderr_cli_handler <- function(msg) {
  text <- tryCatch(cli::ansi_strip(paste(msg$args$text, collapse = "")),
                   error = function(e) "")
  if (nzchar(text)) cat(text, "\n", sep = "", file = stderr())
  invisible()
}
