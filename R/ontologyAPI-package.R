#' ontologyAPI: REST and MCP Servers for Ontology-Based R Packages
#'
#' Serves an ontologySpecR bundle and its data over two protocols from one set
#' of handlers: a JSON REST API (plumber) and a Model Context Protocol server
#' (stdio or SSE). Build an [ontology_service()] once, then pass it to
#' [serve_ontology()] and/or [serve_mcp()].
#'
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom rlang %||%
## usethis namespace: end
NULL
