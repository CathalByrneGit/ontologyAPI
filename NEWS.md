# ontologyAPI 0.2.0

ontologyAPI absorbs **ontologyMCP**. One shared set of handlers now serves
both REST and MCP.

## Breaking changes

* Every entry point now takes a single `OntologyService` built by the new
  `ontology_service(bundle, connection, actor = NULL, auth = NULL)`:
  `ontology_router(svc)`, `serve_ontology(svc, host, port, docs)`,
  `mcp_server(svc)` and `serve_mcp(server_or_svc, transport, port)`. The old
  `bundle`, `connection`, `action_ctx` and `concept_ctx` arguments are gone.
  `ontology_router()` and `serve_ontology()` still take an optional `api_key`.
* Concept and action endpoints are always available. The service builds the
  `conceptR` (with audit tables) and `actionTypesR` contexts itself.
* The `search` parameter of `POST /v1/objects/{type}/query` has been removed.
  It relied on `objectSetsR::os_search()`, which does not exist. Full-text
  search may come back later if there's a real need.
* Errors are now `ontapi_bad_request` (400), `ontapi_not_found` (404) and
  `ontapi_forbidden` (403). The old 422 responses are gone: invalid action
  parameters are 400, unknown action types 404, and a missing context can no
  longer happen. Error bodies gain `meta.kind`.
* `GET /v1/concepts/{id}/evaluate` now requires `scope`.
* Traversal from `traverse` / `traverse_links` follows the bundle's link
  direction. A link that doesn't start at the given type is a 400; use
  `search_around` (REST) or `direction = "reverse"` (MCP) for the other way.
* The REST serialiser auto-unboxes scalars and writes `NA` as `null`.

## New

* MCP server, moved in from ontologyMCP: `mcp_server()`, `serve_mcp()`
  (stdio and SSE), and the exported `mcp_add_tool()` (which ontologyMCP
  never actually had), as used by ontologyDiscoverR. With stdio, all
  console, message and `cli` output goes to stderr.
* `auth` hook on the service for per-operation authorisation in both protocols.
* `POST /v1/objects/{type}/count` with filters, and `GET /v1/bundle/summary`.
* Composite primary keys are supported in `get_object` and traversal.
* Query paging happens in the database (`objectSetsR::os_slice()`, ordered by
  primary key), so pages are stable.

## Fixes

* Both packages called 17 core functions that don't exist and read bundle
  keys the spec never writes. Every handler now reads the bundle only through
  `ontologySpecR` accessors and calls only exported core functions (checked by
  `tools/check-cross-package.R`).
* The `contains` and `starts_with` filters work on DuckDB. They are translated
  to escaped regular expressions, because dbplyr cannot translate
  `grepl(fixed = TRUE)` or `startsWith()` there.
* REST errors raised inside mounted routers now get the JSON envelope and the
  right status, instead of plumber's default 500.
* The REST and MCP filter syntax is the same: plumber's data-frame form of a
  JSON array of filters is now handled.
* Empty MCP `inputSchema.properties` are sent as `{}` rather than `[]`.
