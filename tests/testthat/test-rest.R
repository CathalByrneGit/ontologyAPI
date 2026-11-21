# REST routes, run in-process through plumber (no HTTP server needed).

test_that("GET /v1/health returns bundle metadata", {
  pr <- local_router(local_service())
  r <- rest_request(pr, "GET", "/v1/health")
  expect_equal(r$status, 200L)
  expect_equal(r$body$status, "ok")
  expect_equal(r$body$bundle_id, "aviation-demo")
})

test_that("bundle routes return the bundle and type summaries", {
  pr <- local_router(local_service())
  b <- rest_request(pr, "GET", "/v1/bundle")
  expect_equal(b$body$data$bundleId, "aviation-demo")

  objs <- rest_request(pr, "GET", "/v1/bundle/objects")$body
  expect_named(objs, c("data", "meta"))
  expect_named(objs$meta, c("status", "took_ms"), ignore.order = TRUE)
  airport <- Filter(function(x) x$id == "Airport", objs$data)[[1]]
  expect_equal(airport$source_table, "airports")
  expect_type(airport$property_count, "integer")

  links <- rest_request(pr, "GET", "/v1/bundle/links")$body$data
  expect_true(all(c("id", "from", "to", "cardinality", "directed") %in% names(links[[1]])))

  actions <- rest_request(pr, "GET", "/v1/bundle/actions")$body$data
  expect_equal(actions[[1]]$id, "UpdateAirportStatus")
  expect_equal(actions[[1]]$targets, list("Airport"))

  s <- rest_request(pr, "GET", "/v1/bundle/summary")$body$data
  expect_equal(s$bundle_id, "aviation-demo")
})

test_that("POST /v1/objects/{type}/query filters, pages and selects", {
  pr <- local_router(local_service())
  r <- rest_request(pr, "POST", "/v1/objects/Airport/query", list(
    filters = list(list(property = "country", op = "eq", value = "Ireland"))
  ))
  expect_equal(r$status, 200L)
  expect_equal(r$body$data$total_count, 3)

  # several filters arrive from plumber as a data frame
  r2 <- rest_request(pr, "POST", "/v1/objects/Airport/query", list(filters = list(
    list(property = "country", op = "eq", value = "Ireland"),
    list(property = "city", op = "neq", value = "Cork")
  )))
  expect_equal(r2$body$data$total_count, 2)

  p1 <- rest_request(pr, "POST", "/v1/objects/Airport/query", list(limit = 2, offset = 0))$body$data
  p2 <- rest_request(pr, "POST", "/v1/objects/Airport/query", list(limit = 2, offset = 2))$body$data
  ids <- function(p) vapply(p$rows, function(r) r$airport_id, character(1))
  expect_length(intersect(ids(p1), ids(p2)), 0L)
  expect_true(p1$has_more)

  sel <- rest_request(pr, "POST", "/v1/objects/Airport/query",
                      list(select = list("airport_id", "name"), limit = 1))$body$data
  expect_named(sel$rows[[1]], c("airport_id", "name"))
})

test_that("count, get, traverse and search_around routes", {
  pr <- local_router(local_service())
  expect_equal(rest_request(pr, "GET", "/v1/objects/Airport/count")$body$data$count, 6)
  expect_equal(
    rest_request(pr, "POST", "/v1/objects/Airport/count", list(
      filters = list(list(property = "country", op = "eq", value = "UK"))
    ))$body$data$count,
    1
  )
  dub <- rest_request(pr, "GET", "/v1/objects/Airport/DUB")$body$data
  expect_equal(dub$name, "Dublin Airport")

  fwd <- rest_request(pr, "POST", "/v1/objects/FlightRoute/R002/traverse/RouteDestination")
  expect_equal(fwd$body$data$rows[[1]]$airport_id, "CDG")

  rev <- rest_request(pr, "POST", "/v1/objects/Airport/DUB/search_around/RouteOrigin",
                      list(limit = 2, select = list("route_id")))
  expect_length(rev$body$data$rows, 2L)
  expect_named(rev$body$data$rows[[1]], "route_id")
})

test_that("REST maps shared errors to 400 / 404 / 403", {
  pr <- local_router(local_service())
  expect_equal(rest_request(pr, "GET", "/v1/objects/Nope/count")$status, 404L)
  missing <- rest_request(pr, "GET", "/v1/objects/Airport/ZZZ")
  expect_equal(missing$status, 404L)
  expect_equal(missing$body$meta$status, "error")
  expect_equal(missing$body$meta$kind, "not_found")
  expect_equal(
    rest_request(pr, "POST", "/v1/objects/Airport/query", list(
      filters = list(list(property = "country", op = "BADOP", value = "x"))
    ))$status,
    400L
  )
  expect_equal(rest_request(pr, "POST", "/v1/objects/Airport/DUB/traverse/RouteOrigin")$status, 400L)
  expect_equal(rest_request(pr, "POST", "/v1/objects/Airport/XXXX/search_around/RouteOrigin")$status, 404L)

  denied <- local_router(local_service(auth = function(actor, operation, resource) FALSE))
  r <- rest_request(denied, "GET", "/v1/objects/Airport/count")
  expect_equal(r$status, 403L)
  expect_equal(r$body$meta$kind, "forbidden")
})

test_that("concept routes", {
  svc <- local_service()
  add_irish_concept(svc)
  pr <- local_router(svc)
  expect_equal(rest_request(pr, "GET", "/v1/concepts")$body$data[[1]]$id, "irish_airport")
  expect_equal(rest_request(pr, "GET", "/v1/concepts/irish_airport")$body$data$object_type_id, "Airport")
  expect_equal(rest_request(pr, "GET", "/v1/concepts/nope")$status, 404L)
  ev <- rest_request(pr, "GET", "/v1/concepts/irish_airport/evaluate",
                     query = "scope=operations&limit=2")$body$data
  expect_equal(ev$true_count, 3L)
  expect_length(ev$rows, 2L)
  expect_equal(rest_request(pr, "GET", "/v1/concepts/irish_airport/evaluate")$status, 400L)
})

test_that("action routes submit, dry-run and log", {
  calls <- new.env()
  svc <- with_status_handler(local_service(), calls)
  pr <- local_router(svc)
  dry <- rest_request(pr, "POST", "/v1/actions/UpdateAirportStatus/submit", list(
    params = list(new_status = "closed"), target_ids = list("DUB"), dry_run = TRUE
  ))
  expect_equal(dry$status, 200L)
  expect_true(dry$body$data$dry_run)
  expect_equal(dry$body$data$plan$impl_kind, "r")
  expect_equal(calls$n, 0L)

  run <- rest_request(pr, "POST", "/v1/actions/UpdateAirportStatus/submit", list(
    params = list(new_status = "closed"), target_ids = list("DUB")
  ))
  expect_equal(run$body$data$status, "success")
  expect_equal(calls$n, 1L)

  log <- rest_request(pr, "GET", "/v1/actions/log", query = "n=10")$body$data
  expect_length(log, 1L)
  expect_equal(log[[1]]$submission_id, run$body$data$submission_id)

  expect_equal(rest_request(pr, "POST", "/v1/actions/Nope/submit", list(params = list()))$status, 404L)
  expect_equal(
    rest_request(pr, "POST", "/v1/actions/UpdateAirportStatus/submit", list(params = list()))$status,
    400L
  )
})

test_that("ontology_router requires an OntologyService", {
  expect_error(ontology_router(list()), "OntologyService")
})
