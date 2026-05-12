# ---------------------------------------------------------------------------
# Shared test helpers
# ---------------------------------------------------------------------------

# Skip the calling test if any required package is absent.
skip_deps <- function(extra = character()) {
  pkgs <- c("duckdb", "DBI", "ontologySpecR", "objectSetsR", extra)
  for (pkg in pkgs) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      testthat::skip(paste0("Package '", pkg, "' not available"))
    }
  }
}

# Seed an in-memory DuckDB connection with aviation demo data.
seed_test_db <- function(con) {
  DBI::dbExecute(con,
    "CREATE TABLE airports (
       airport_id VARCHAR PRIMARY KEY,
       name       VARCHAR,
       country    VARCHAR,
       city       VARCHAR,
       capacity   INTEGER
     )"
  )
  DBI::dbExecute(con,
    "INSERT INTO airports VALUES
       ('DUB', 'Dublin Airport',    'Ireland', 'Dublin',   35000000),
       ('ORK', 'Cork Airport',      'Ireland', 'Cork',      2500000),
       ('SNN', 'Shannon Airport',   'Ireland', 'Shannon',   1500000),
       ('LHR', 'London Heathrow',   'UK',      'London',   80000000),
       ('CDG', 'Charles de Gaulle', 'France',  'Paris',    75000000),
       ('JFK', 'John F. Kennedy',   'USA',     'New York', 62000000)"
  )
  DBI::dbExecute(con,
    "CREATE TABLE flight_routes (
       route_id     VARCHAR PRIMARY KEY,
       from_airport VARCHAR,
       to_airport   VARCHAR,
       airline      VARCHAR
     )"
  )
  DBI::dbExecute(con,
    "INSERT INTO flight_routes VALUES
       ('R001', 'DUB', 'LHR', 'Aer Lingus'),
       ('R002', 'DUB', 'CDG', 'Ryanair'),
       ('R003', 'DUB', 'JFK', 'Aer Lingus'),
       ('R004', 'ORK', 'LHR', 'Ryanair')"
  )
  invisible(con)
}

# Build the aviation test bundle (requires ontologySpecR).
make_test_bundle <- function() {
  b <- ontologySpecR::new_bundle(id = "aviation-test", version = "0.1.0")

  b <- ontologySpecR::add_object_type(b,
    id           = "Airport",
    display_name = "Airport",
    source_table = "airports",
    properties   = list(
      ontologySpecR::new_property("airport_id", "character", primary_key = TRUE),
      ontologySpecR::new_property("name",       "character"),
      ontologySpecR::new_property("country",    "character"),
      ontologySpecR::new_property("city",       "character"),
      ontologySpecR::new_property("capacity",   "integer")
    )
  )

  b <- ontologySpecR::add_object_type(b,
    id           = "FlightRoute",
    display_name = "Flight Route",
    source_table = "flight_routes",
    properties   = list(
      ontologySpecR::new_property("route_id",     "character", primary_key = TRUE),
      ontologySpecR::new_property("from_airport", "character"),
      ontologySpecR::new_property("to_airport",   "character"),
      ontologySpecR::new_property("airline",      "character")
    )
  )

  b <- ontologySpecR::add_link_type(b,
    id          = "FlightRoute",
    from        = "Airport",
    to          = "FlightRoute",
    cardinality = "one_to_many",
    directed    = TRUE
  )

  b
}

# ---------------------------------------------------------------------------
# Integration-test server helper
#
# Starts the API in a callr background process on a random high port.
# Returns a list: $url (character), $stop() (function).
# Automatically registers a testthat withr-style cleanup via on.exit().
# ---------------------------------------------------------------------------
start_test_server <- function(api_key = NULL) {
  skip_deps(extra = c("callr", "httr2"))

  port <- sample(49152L:65534L, 1L)

  proc <- callr::r_bg(
    func = function(port, api_key) {
      library(ontologyAPI)   # nolint
      library(duckdb)        # nolint
      library(DBI)           # nolint
      library(ontologySpecR) # nolint

      con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
      DBI::dbExecute(con,
        "CREATE TABLE airports (
           airport_id VARCHAR PRIMARY KEY,
           name VARCHAR, country VARCHAR, city VARCHAR, capacity INTEGER)")
      DBI::dbExecute(con,
        "INSERT INTO airports VALUES
           ('DUB','Dublin Airport','Ireland','Dublin',35000000),
           ('ORK','Cork Airport','Ireland','Cork',2500000),
           ('SNN','Shannon Airport','Ireland','Shannon',1500000),
           ('LHR','London Heathrow','UK','London',80000000),
           ('CDG','Charles de Gaulle','France','Paris',75000000),
           ('JFK','John F. Kennedy','USA','New York',62000000)")
      DBI::dbExecute(con,
        "CREATE TABLE flight_routes (
           route_id VARCHAR PRIMARY KEY,
           from_airport VARCHAR, to_airport VARCHAR, airline VARCHAR)")
      DBI::dbExecute(con,
        "INSERT INTO flight_routes VALUES
           ('R001','DUB','LHR','Aer Lingus'),
           ('R002','DUB','CDG','Ryanair'),
           ('R003','DUB','JFK','Aer Lingus'),
           ('R004','ORK','LHR','Ryanair')")

      b <- ontologySpecR::new_bundle(id = "aviation-test", version = "0.1.0")
      b <- ontologySpecR::add_object_type(b, id = "Airport",
        display_name = "Airport", source_table = "airports",
        properties = list(
          ontologySpecR::new_property("airport_id", "character", primary_key = TRUE),
          ontologySpecR::new_property("name",    "character"),
          ontologySpecR::new_property("country", "character"),
          ontologySpecR::new_property("city",    "character"),
          ontologySpecR::new_property("capacity","integer")))
      b <- ontologySpecR::add_object_type(b, id = "FlightRoute",
        display_name = "Flight Route", source_table = "flight_routes",
        properties = list(
          ontologySpecR::new_property("route_id",     "character", primary_key = TRUE),
          ontologySpecR::new_property("from_airport", "character"),
          ontologySpecR::new_property("to_airport",   "character"),
          ontologySpecR::new_property("airline",      "character")))
      b <- ontologySpecR::add_link_type(b, id = "FlightRoute",
        from = "Airport", to = "FlightRoute",
        cardinality = "one_to_many", directed = TRUE)

      pr <- ontology_router(b, con, api_key = api_key)
      plumber::pr_run(pr, host = "127.0.0.1", port = port, quiet = TRUE)
    },
    args      = list(port = port, api_key = api_key),
    supervise = TRUE
  )

  base_url <- paste0("http://127.0.0.1:", port)

  # Poll until the health endpoint responds (up to 10 s).
  ready <- FALSE
  for (i in seq_len(50L)) {
    Sys.sleep(0.2)
    ready <- tryCatch({
      httr2::request(paste0(base_url, "/v1/health")) |>
        httr2::req_timeout(1) |>
        httr2::req_perform()
      TRUE
    }, error = function(e) FALSE)
    if (ready) break
  }

  if (!ready) {
    proc$kill()
    testthat::skip("Test API server did not become ready in time")
  }

  list(
    url  = base_url,
    stop = function() proc$kill()
  )
}

# Convenience: perform a GET and parse the JSON body.
api_get <- function(base_url, path, api_key = NULL, ...) {
  req <- httr2::request(paste0(base_url, path))
  if (!is.null(api_key)) {
    req <- httr2::req_headers(req, Authorization = paste("Bearer", api_key))
  }
  resp <- httr2::req_perform(req)
  httr2::resp_body_json(resp, ...)
}

# Convenience: POST JSON body and parse response.
api_post <- function(base_url, path, body = list(), api_key = NULL, ...) {
  req <- httr2::request(paste0(base_url, path)) |>
    httr2::req_body_json(body)
  if (!is.null(api_key)) {
    req <- httr2::req_headers(req, Authorization = paste("Bearer", api_key))
  }
  resp <- httr2::req_perform(req)
  httr2::resp_body_json(resp, ...)
}
