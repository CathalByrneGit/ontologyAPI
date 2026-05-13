# ontologyAPI

A plumber-based REST API server that exposes [`objectSetsR`](https://github.com/cathalbyrnegit/objectSetsR), [`actionTypesR`](https://github.com/cathalbyrnegit/actionTypesR), and [`conceptR`](https://github.com/cathalbyrnegit/conceptR) over HTTP. Lets Python, TypeScript, curl, and LLM agents query object sets, traverse links, evaluate concepts, and submit actions — no R required on the client side.

```
ontologySpecR ─┐
objectSetsR   ─┤
actionTypesR  ─┼─► ontologyAPI ──► HTTP clients (Python, TS, curl, agents)
conceptR      ─┘              └──► ontologyMCP  (next layer)
```

## Installation

```r
# Install from source (dependencies must be available)
devtools::install_local("path/to/ontologyAPI")
```

Required packages: `ontologySpecR`, `objectSetsR`, `actionTypesR`, `plumber (>= 1.2.0)`, `jsonlite`, `rlang`, `cli`

Optional: `conceptR`, `auditR`, `duckdb`

## Quick start

```r
library(ontologyAPI)
library(duckdb)

con    <- DBI::dbConnect(duckdb::duckdb(), "mydata.duckdb")
bundle <- readRDS("bundle.rds")   # ontologySpecR bundle

# Starts on http://127.0.0.1:8080 — Swagger UI at /docs
serve_ontology(bundle, con, port = 8080L)
```

Set `ONTOLOGY_API_KEY` before starting to require Bearer-token auth on every request:

```sh
ONTOLOGY_API_KEY=my-secret Rscript -e \
  "ontologyAPI::serve_ontology(bundle, con, port = 8080L)"
```

## Authentication

| `ONTOLOGY_API_KEY` set? | Behaviour |
|---|---|
| No | Development mode — all requests pass, startup warning is logged |
| Yes | Every request must include `Authorization: Bearer <key>`, otherwise `401` |

## API reference

All responses use a standard envelope:

```json
{ "data": ..., "meta": { "status": "ok",   "took_ms": 12 } }
{ "error": "...", "meta": { "status": "error", "took_ms": 5  } }
```

Base path: `/v1`

---

### Health

```
GET /v1/health
```

```json
{ "status": "ok", "bundle_id": "aviation-demo", "bundle_version": "1.0.0" }
```

---

### Bundle

```
GET /v1/bundle                → full bundle as JSON
GET /v1/bundle/objects        → [{ id, display_name, property_count, source_table }]
GET /v1/bundle/links          → [{ id, from, to, cardinality, directed }]
GET /v1/bundle/actions        → [{ id, display_name, parameter_count, targets }]
```

---

### Object sets

```
POST /v1/objects/{object_type_id}/query
GET  /v1/objects/{object_type_id}/count
GET  /v1/objects/{object_type_id}/{pk_value}
```

**Query request body:**

```json
{
  "filters": [
    { "property": "country",  "op": "eq",      "value": "Ireland" },
    { "property": "capacity", "op": "gte",     "value": 1000      },
    { "property": "name",     "op": "contains","value": "Dublin"  }
  ],
  "search": "optional free text",
  "limit": 100,
  "offset": 0,
  "select": ["airport_id", "name", "country"]
}
```

**Supported filter operators:**

| `op` | Meaning |
|---|---|
| `eq` / `neq` | equal / not equal |
| `gt` / `gte` | greater than / greater than or equal |
| `lt` / `lte` | less than / less than or equal |
| `contains` | substring (fixed, case-sensitive) |
| `starts_with` | prefix match |
| `in` / `not_in` | value in / not in array |
| `is_null` / `is_not_null` | null check |

Filter values are passed through rlang quosures into `objectSetsR::os_filter()` — never interpolated into SQL strings. SQL injection is not possible.

**Query response:**

```json
{
  "data": { "rows": [...], "total_count": 42, "has_more": false },
  "meta": { "status": "ok", "took_ms": 8 }
}
```

---

### Link traversal

```
POST /v1/objects/{object_type_id}/{pk_value}/traverse/{link_type_id}
POST /v1/objects/{object_type_id}/{pk_value}/search_around/{link_type_id}
```

Optional body: `{ "limit": 50, "select": ["id", "name"] }`

`traverse` follows the link forward; `search_around` is the reverse direction.

---

### Actions

```
POST /v1/actions/{action_type_id}/submit
GET  /v1/actions/log?n=100&action_type_id=UpdateAirportStatus
```

**Submit body:**

```json
{
  "params":     { "new_status": "maintenance" },
  "target_ids": ["DUB"],
  "dry_run":    false
}
```

`dry_run: true` returns a plan object without executing anything:

```json
{
  "data": {
    "plan": {
      "action_type_id": "UpdateAirportStatus",
      "params":         { "new_status": "maintenance" },
      "target_ids":     ["DUB"],
      "impl_kind":      "r"
    }
  }
}
```

Live submission returns:

```json
{
  "data": {
    "submission_id":  "a1b2c3d4-...",
    "status":         "success",
    "action_type_id": "UpdateAirportStatus"
  }
}
```

---

### Concepts _(optional — requires `conceptR`)_

```
GET /v1/concepts
GET /v1/concepts/{concept_id}
GET /v1/concepts/{concept_id}/evaluate?scope=flow&stage=production
```

Evaluate response includes per-row results plus `true_count` / `false_count`.

---

## Calling from Python

```python
import requests

BASE    = "http://127.0.0.1:8080"
HEADERS = {"Authorization": "Bearer my-secret"}

# Query with filters
resp = requests.post(f"{BASE}/v1/objects/Airport/query", headers=HEADERS, json={
    "filters": [{"property": "country", "op": "eq", "value": "Ireland"}],
    "limit": 50,
})
airports = resp.json()["data"]["rows"]

# Single object
dub = requests.get(f"{BASE}/v1/objects/Airport/DUB", headers=HEADERS).json()

# Traverse a link
routes = requests.post(
    f"{BASE}/v1/objects/Airport/DUB/traverse/FlightRoute",
    headers=HEADERS, json={"limit": 10}
).json()["data"]["rows"]
```

## Calling from R (httr2)

```r
library(httr2)

base <- "http://127.0.0.1:8080"

request(paste0(base, "/v1/objects/Airport/query")) |>
  req_body_json(list(
    filters = list(list(property = "country", op = "eq", value = "Ireland"))
  )) |>
  req_perform() |>
  resp_body_json()
```

## Package structure

```
R/
  server.R           # ontology_router(), serve_ontology()
  auth.R             # Bearer-token plumber filter
  error_handler.R    # global error → JSON envelope; http_abort()
  filters.R          # translate_filter(), apply_filters(), build_filter_expr()
  response.R         # ok_response(), error_response(), elapsed_ms(), df_to_list()
  router_health.R    # GET /v1/health
  router_bundle.R    # GET /v1/bundle/**
  router_objects.R   # POST|GET /v1/objects/**
  router_actions.R   # POST /v1/actions/**
  router_concepts.R  # GET /v1/concepts/** (optional)
  ontologyAPI-package.R

tests/testthat/
  setup.R                   # shared fixtures, start_test_server()
  test-filters.R            # filter expression building, SQL injection resistance
  test-auth.R               # unit + integration auth tests
  test-error-handling.R     # http_abort, error envelope, 404/400 from live server
  test-bundle-endpoints.R   # /bundle/** integration tests
  test-objects-endpoints.R  # query, count, single-object, 404, has_more
  test-traversal.R          # traverse, search_around
  test-actions-endpoints.R  # submit, dry_run, log

vignettes/
  quickstart.Rmd
```

## Testing

Unit tests (filters, auth, error handler) run without any server:

```r
devtools::test()
```

Integration tests spin up a real plumber server via `callr::r_bg()` against a DuckDB `:memory:` database seeded with aviation demo data (6 airports, 4 routes). They are skipped automatically if `callr`, `httr2`, or the ontology packages are not installed.

## Security

- **SQL injection** — filter values never reach the query as strings. They are captured as R objects and injected via rlang quosures into `objectSetsR::os_filter()`, which translates them to parameterised dbplyr expressions.
- **Auth** — when `ONTOLOGY_API_KEY` is set, the plumber filter rejects any request missing a matching `Authorization: Bearer <key>` header with `401` before it reaches any route handler.
- **Error leakage** — the global error handler returns the condition message only; no stack traces are exposed to clients.
