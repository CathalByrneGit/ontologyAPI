# ontologyAPI

Serve an [`ontologySpecR`](https://github.com/CathalByrneGit/ontologySpecR) bundle and its data over **two protocols from one set of handlers**:

- a **JSON REST API**, via plumber, for Python, TypeScript, curl and any other HTTP client;
- a **Model Context Protocol (MCP) server**, over stdio or SSE, for LLM agents such as Claude Desktop.

Both read data through [`objectSetsR`](https://github.com/CathalByrneGit/objectSetsR), concepts through [`conceptR`](https://github.com/CathalByrneGit/conceptR) and actions through [`actionTypesR`](https://github.com/CathalByrneGit/actionTypesR). A query sent over REST and the same query sent over MCP return the same rows, and the test suite checks that.

```
ontologySpecR ─┐                         ┌─► REST  (plumber)    ─► HTTP clients
objectSetsR   ─┤                         │
conceptR      ─┼─► ontology_service() ─► handlers (R/handlers.R)
actionTypesR  ─┘                         │
                                         └─► MCP   (stdio / SSE) ─► LLM agents
```

> ontologyAPI 0.2.0 absorbs the former **ontologyMCP** package. `serve_mcp()` and `mcp_add_tool()` now live here.

## Installation

```r
# install.packages("remotes")
remotes::install_github("CathalByrneGit/ontologyAPI")
```

Required: `ontologySpecR`, `objectSetsR (>= 0.3.1)`, `conceptR`, `actionTypesR`, `jsonlite`, `rlang`, `cli`.

Each server's own dependencies are optional, so installing for MCP doesn't pull in plumber:

| You want | Also install |
|---|---|
| REST server | `plumber` |
| MCP over stdio | nothing more |
| MCP over SSE | `httpuv`, `later` |

## Quick start

Build one service, then serve it over either protocol, or both:

```r
library(ontologyAPI)

con <- DBI::dbConnect(duckdb::duckdb(), "aviation.duckdb")
svc <- ontology_service("bundle.json", con)   # or an ontology_bundle

# REST on http://127.0.0.1:8080 (Swagger UI at /__docs__/)
serve_ontology(svc, port = 8080L)

# MCP over stdio, the transport Claude Desktop uses
serve_mcp(svc)
```

`ontology_service(bundle, connection, actor = NULL, auth = NULL)` runs `ontologySpecR::as_bundle()` once and builds the `objectSetsR`, `conceptR` and `actionTypesR` contexts. `conceptR` and `actionTypesR` create their own tables in the connection if they are missing.

Register R action handlers on the service before serving:

```r
svc$action_ctx <- actionTypesR::register_handler(
  svc$action_ctx, "UpdateAirportStatus",
  function(connection, action, params, target_ids) { ... }
)
```

### Claude Desktop

```json
{
  "mcpServers": {
    "aviation": {
      "command": "Rscript",
      "args": ["-e", "con <- DBI::dbConnect(duckdb::duckdb(), 'aviation.duckdb'); ontologyAPI::serve_mcp(ontologyAPI::ontology_service('bundle.json', con))"]
    }
  }
}
```

With stdio, stdout carries only JSON-RPC. While the server runs, all console output, messages and `cli` output go to stderr.

## Access control

| Layer | How | Failure |
|---|---|---|
| REST API key | `ONTOLOGY_API_KEY` env var, or `ontology_router(svc, api_key = )`. Clients send `Authorization: Bearer <key>`. Unset means development mode, with a warning. | `401` |
| Per-operation authorisation (both protocols) | `ontology_service(auth = function(actor, operation, resource) ...)`. `operation` is `"read"`, `"evaluate_concept"` or `"submit_action"`. Return `TRUE` to allow. | `403` / JSON-RPC `-32001` |

An approval gate that denies an action (`actionTypesR::register_approval_gate()`) is also reported as forbidden.

## Errors

The handlers raise three error classes once, and each protocol maps them to its own codes:

| Class | REST | MCP (JSON-RPC) |
|---|---|---|
| `ontapi_bad_request` (bad filter, unknown property, invalid params) | 400 | -32602 |
| `ontapi_not_found` (unknown type, link, concept, action or object) | 404 | -32602 |
| `ontapi_forbidden` | 403 | -32001 |
| anything else | 500 | -32603 |

The REST error body is `{"error": "...", "meta": {"status": "error", "kind": "not_found", ...}}`. MCP errors carry the same message, with `data.kind` set to the same kind.

## Filters

REST and MCP accept the same filter syntax: an array of `{property, op, value}` objects, ANDed together.

| `op` | Meaning |
|---|---|
| `eq` / `neq` | equal / not equal |
| `gt` / `gte` / `lt` / `lte` | comparisons |
| `contains` | literal substring (case-sensitive) |
| `starts_with` | literal prefix |
| `in` / `not_in` | value in / not in an array |
| `is_null` / `is_not_null` | null checks (no `value`) |

Property names are checked against the object type. Values reach the database only as values injected into `objectSetsR::os_filter()`, never pasted into SQL.

## REST API

Every response uses one envelope: `{"data": ..., "meta": {"status": "ok", "took_ms": 3}}`.

```
GET  /v1/health
GET  /v1/bundle                      full bundle JSON
GET  /v1/bundle/objects              object types (properties, primary key, source table)
GET  /v1/bundle/links                link types (endpoints, join keys, cardinality)
GET  /v1/bundle/actions              action types (parameters, targets, impl kind)
GET  /v1/bundle/summary              ids, row counts, concept count

POST /v1/objects/{type}/query        {filters, select, limit = 100, offset = 0}
GET  /v1/objects/{type}/count
POST /v1/objects/{type}/count        {filters}
GET  /v1/objects/{type}/{pk}
POST /v1/objects/{type}/{pk}/traverse/{link}        {limit = 50, select}   follow the link forward
POST /v1/objects/{type}/{pk}/search_around/{link}   {limit = 50, select}   reverse: who links to me

GET  /v1/concepts
GET  /v1/concepts/{id}
GET  /v1/concepts/{id}/evaluate?scope=&version=&stage=&limit=

POST /v1/actions/{id}/submit         {params, target_ids, dry_run = false}
GET  /v1/actions/log?n=100&action_type_id=
```

`query` returns `{rows, total_count, has_more, limit, offset}`. Rows are paged in the database with `objectSetsR::os_slice()`, ordered by primary key, so pages are stable.

For a composite primary key, the `{pk}` path segment joins the key parts with `|`, in key order.

## MCP tools

`mcp_server(svc)` registers ten tools, each a thin wrapper over a handler:

| Tool | Handler |
|---|---|
| `get_ontology_summary` | `h_summary` |
| `list_object_types` / `list_link_types` | `h_object_types` / `h_link_types` |
| `query_objects` / `count_objects` / `get_object` | `h_query` / `h_count` / `h_get` |
| `traverse_links` (`direction = forward / reverse`) | `h_traverse` |
| `list_concepts` / `evaluate_concept` | `h_concepts` / `h_evaluate_concept` |
| `submit_action` (**`dry_run` defaults to `true`**) | `h_submit_action` |

The server also offers resources: `ontology://bundle`, `ontology://schema` and `ontology://objects/{type}/schema`.

### Adding tools

```r
server <- mcp_server(svc, name = "aviation")
mcp_add_tool(server, "busiest_airport", "Airport with the most departing routes",
  input_schema = list(type = "object", properties = list()),
  handler = function(params, server) {
    # return an MCP result (list(content = ...)) or any value, sent as JSON
    ...
  })
serve_mcp(server, transport = "stdio")
```

A handler can take `function(params)` or `function(params, server)`. Duplicate names are an error. The server is modified in place, so code that ignores the returned value still sees the new tool, as `ontologyDiscoverR::dis_add_mcp_tool()` relies on.

## Package structure

```
R/
  service.R            ontology_service(), the auth hook
  handlers.R           shared handlers h_*(), filter parsing, primary keys
  errors.R             ontapi_* error classes and the REST status mapping
  json.R               the JSON encoding both protocols share
  rest_server.R        ontology_router(), serve_ontology()
  router_*.R           REST routes: parse request, call handler, format response
  rest_response.R      response envelope
  rest_error_handler.R error class to HTTP status
  auth.R               Bearer-token filter (REST only)
  mcp_server.R         mcp_server(), mcp_add_tool(), serve_mcp()
  mcp_tool_registry.R  built-in tool schemas
  mcp_tools.R          built-in tools: unpack arguments, call handler
  mcp_protocol.R       JSON-RPC dispatch
  mcp_errors.R         JSON-RPC codes and error class mapping
  mcp_resources.R      ontology:// resources
  mcp_transport_*.R    stdio and SSE transports
tools/
  check-cross-package.R  checks every core pkg::fn exists and the bundle is never read by key
```

## Testing

```r
devtools::test()
Rscript tools/check-cross-package.R   # prints nothing when clean
```

The fixtures use `ontologySpecR::spec_example_bundle()` and an in-memory DuckDB database built to match it. Most tests call the handlers directly. REST routes run in-process through plumber, MCP runs through `handle_message()` and the stdio transport, and one test checks that the same query returns identical rows over both. A single live-HTTP test (via `callr`) runs only when the package is installed.
