# Shared fixtures. The bundle is ontologySpecR's example aviation bundle; the
# DuckDB database is created to match it (tables `airports`, `airlines`,
# `routes`). Tests must not hand-roll bundle lists: the spec owns the layout.

skip_if_no_backend <- function() {
  testthat::skip_if_not_installed("duckdb")
  testthat::skip_if_not_installed("DBI")
}

example_bundle <- function() {
  suppressWarnings(ontologySpecR::spec_example_bundle())
}

# Create the example tables. Airport's source names schema "public", which
# objectSetsR reads as public.airports; conceptR reads the bare table name, so
# a `main.airports` view points at the same rows.
seed_example_db <- function(con) {
  DBI::dbExecute(con, "CREATE SCHEMA IF NOT EXISTS public")
  DBI::dbExecute(con, "
    CREATE TABLE public.airports (
      airport_id   VARCHAR PRIMARY KEY,
      name         VARCHAR NOT NULL,
      city         VARCHAR,
      country      VARCHAR,
      latitude     DOUBLE,
      longitude    DOUBLE,
      elevation_ft INTEGER
    )")
  DBI::dbExecute(con, "
    INSERT INTO public.airports VALUES
      ('DUB', 'Dublin Airport',    'Dublin',   'Ireland', 53.421, -6.270,  242),
      ('ORK', 'Cork Airport',      'Cork',     'Ireland', 51.841, -8.491,  502),
      ('SNN', 'Shannon Airport',   'Shannon',  'Ireland', 52.702, -8.925,   46),
      ('LHR', 'London Heathrow',   'London',   'UK',      51.470, -0.454,   83),
      ('CDG', 'Charles de Gaulle', 'Paris',    'France',  49.010,  2.548,  392),
      ('JFK', 'John F. Kennedy',   'New York', 'USA',     40.641, -73.778,  13)")
  DBI::dbExecute(con, "CREATE VIEW main.airports AS SELECT * FROM public.airports")

  DBI::dbExecute(con, "
    CREATE TABLE airlines (
      airline_id VARCHAR PRIMARY KEY,
      name       VARCHAR NOT NULL,
      country    VARCHAR,
      active     BOOLEAN
    )")
  DBI::dbExecute(con, "
    INSERT INTO airlines VALUES
      ('EI', 'Aer Lingus',      'Ireland', TRUE),
      ('FR', 'Ryanair',         'Ireland', TRUE),
      ('BA', 'British Airways', 'UK',      TRUE)")

  DBI::dbExecute(con, "
    CREATE TABLE routes (
      route_id       VARCHAR PRIMARY KEY,
      origin_id      VARCHAR NOT NULL,
      destination_id VARCHAR NOT NULL,
      airline_id     VARCHAR NOT NULL,
      stops          INTEGER,
      equipment      VARCHAR
    )")
  DBI::dbExecute(con, "
    INSERT INTO routes VALUES
      ('R001', 'DUB', 'LHR', 'EI', 0, 'A320'),
      ('R002', 'DUB', 'CDG', 'FR', 0, 'B738'),
      ('R003', 'DUB', 'JFK', 'EI', 0, 'A330'),
      ('R004', 'ORK', 'LHR', 'FR', 0, 'B738'),
      ('R005', 'LHR', 'DUB', 'BA', 0, 'A319')")
  invisible(con)
}

new_example_con <- function() {
  con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  seed_example_db(con)
  con
}

# A service over a fresh in-memory database. The connection is closed when the
# calling test (or `env`) exits.
local_service <- function(env = parent.frame(), auth = NULL, actor = "tester") {
  skip_if_no_backend()
  con <- new_example_con()
  withr_defer(DBI::dbDisconnect(con, shutdown = TRUE), env)
  ontology_service(example_bundle(), con, actor = actor, auth = auth)
}

# Minimal defer without a withr dependency.
withr_defer <- function(expr, env) {
  thunk <- as.call(list(function() expr))
  do.call(base::on.exit, list(thunk, TRUE, FALSE), envir = env)
}

# Define and activate a concept: Irish airports.
add_irish_concept <- function(svc, stage = NULL) {
  ctx <- svc$concept_ctx
  conceptR::cpt_define(ctx, "irish_airport", "Airport", description = "Airport in Ireland")
  v <- conceptR::cpt_add_version(ctx, "irish_airport", "operations",
                                 "country = 'Ireland'", status = "active")
  invisible(v)
}

# Register an R handler for the example's UpdateAirportStatus action.
with_status_handler <- function(svc, calls = new.env()) {
  calls$n <- 0L
  svc$action_ctx <- actionTypesR::register_handler(
    svc$action_ctx, "UpdateAirportStatus",
    function(connection, action, params, target_ids) {
      calls$n <- calls$n + 1L
      calls$last <- list(params = params, target_ids = target_ids)
      list(updated = length(target_ids))
    }
  )
  svc
}

# ---- MCP helpers ----

mcp_call <- function(server, method, params = list(), id = 1L) {
  handle_message(list(jsonrpc = "2.0", id = id, method = method, params = params), server)
}

mcp_tool <- function(server, name, arguments = list(), id = 1L) {
  mcp_call(server, "tools/call", list(name = name, arguments = arguments), id = id)
}

# Parse the JSON text of a successful tools/call response.
mcp_tool_json <- function(resp) {
  testthat::expect_null(resp$error)
  jsonlite::fromJSON(resp$result$content[[1]]$text, simplifyVector = FALSE)
}

# ---- REST helpers (in-process, no HTTP server) ----

# Send one request through a plumber router and return list(status, body).
rest_request <- function(pr, method, path, body = NULL, query = "", headers = list()) {
  testthat::skip_if_not_installed("plumber")
  req <- new.env()
  req$REQUEST_METHOD <- method
  req$PATH_INFO <- path
  req$QUERY_STRING <- query
  req$HTTP_CONTENT_TYPE <- "application/json"
  req$CONTENT_TYPE <- "application/json"
  for (h in names(headers)) assign(h, headers[[h]], envir = req)
  payload <- if (is.null(body)) "" else as.character(jsonlite::toJSON(body, auto_unbox = TRUE))
  raw_body <- charToRaw(payload)
  req$rook.input <- Rook_input(raw_body)
  req$HTTP_CONTENT_LENGTH <- as.character(length(raw_body))
  res <- pr$call(req)
  list(
    status = res$status,
    body = if (nzchar(res$body)) jsonlite::fromJSON(res$body, simplifyVector = FALSE) else NULL
  )
}

Rook_input <- function(raw_body) {
  pos <- 0L
  list(
    read = function(l = -1L) {
      if (pos >= length(raw_body)) return(raw())
      out <- raw_body[(pos + 1L):length(raw_body)]
      pos <<- length(raw_body)
      out
    },
    read_lines = function() rawToChar(raw_body),
    rewind = function() pos <<- 0L
  )
}

local_router <- function(svc, api_key = "") {
  testthat::skip_if_not_installed("plumber")
  suppressWarnings(ontology_router(svc, api_key = api_key))
}
