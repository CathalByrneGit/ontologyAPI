# Handler-level tests: no server, no protocol. Most coverage lives here.

test_that("h_summary reports bundle metadata, row counts and concept count", {
  svc <- local_service()
  s <- h_summary(svc)
  expect_equal(s$bundle_id, "aviation-demo")
  expect_equal(s$bundle_version, "0.1.0")
  counts <- vapply(s$object_types, function(x) x$row_count, numeric(1))
  names(counts) <- vapply(s$object_types, function(x) x$id, character(1))
  expect_equal(counts[["Airport"]], 6)
  expect_equal(counts[["FlightRoute"]], 5)
  expect_length(s$link_types, 3L)
  expect_equal(s$action_types[[1]]$id, "UpdateAirportStatus")
  expect_equal(s$concept_count, 0L)
})

test_that("h_object_types describes types through the spec accessors", {
  svc <- local_service()
  types <- h_object_types(svc)
  ids <- vapply(types, function(x) x$id, character(1))
  expect_setequal(ids, c("Airport", "Airline", "FlightRoute"))
  airport <- types[[which(ids == "Airport")]]
  expect_equal(airport$source_table, "airports")
  expect_equal(airport$primary_key, list("airport_id"))
  expect_equal(airport$property_count, 7L)
  expect_equal(airport$display_name, "Airport")
  expect_false(airport$properties[[1]]$nullable)
})

test_that("h_link_types and h_action_types expose endpoints, keys and impl kind", {
  svc <- local_service()
  links <- h_link_types(svc)
  origin <- Filter(function(l) l$id == "RouteOrigin", links)[[1]]
  expect_equal(origin$from, "FlightRoute")
  expect_equal(origin$to, "Airport")
  expect_equal(origin$from_keys, list("origin_id"))
  expect_true(origin$directed)

  actions <- h_action_types(svc)
  expect_equal(actions[[1]]$impl_kind, "r")
  expect_equal(actions[[1]]$targets, list("Airport"))
  expect_equal(actions[[1]]$parameters[[1]]$id, "new_status")
  expect_true(actions[[1]]$parameters[[1]]$required)
})

test_that("h_query filters, counts and pages in primary-key order", {
  svc <- local_service()
  all <- h_query(svc, "Airport")
  expect_equal(all$total_count, 6)
  expect_false(all$has_more)
  ids <- vapply(all$rows, function(r) r$airport_id, character(1))
  expect_equal(ids, sort(ids))

  irish <- h_query(svc, "Airport",
    filters = list(list(property = "country", op = "eq", value = "Ireland")))
  expect_equal(irish$total_count, 3)
  expect_true(all(vapply(irish$rows, function(r) r$country, character(1)) == "Ireland"))

  p1 <- h_query(svc, "Airport", limit = 2, offset = 0)
  p2 <- h_query(svc, "Airport", limit = 2, offset = 2)
  p3 <- h_query(svc, "Airport", limit = 2, offset = 4)
  page_ids <- function(p) vapply(p$rows, function(r) r$airport_id, character(1))
  expect_equal(c(page_ids(p1), page_ids(p2), page_ids(p3)), ids)
  expect_true(p1$has_more)
  expect_false(p3$has_more)
  expect_length(h_query(svc, "Airport", limit = 2, offset = 10)$rows, 0L)
})

test_that("h_query select limits the returned properties", {
  svc <- local_service()
  res <- h_query(svc, "Airport", select = list("airport_id", "name"), limit = 1)
  expect_named(res$rows[[1]], c("airport_id", "name"))
})

test_that("h_query rejects bad input with ontapi_bad_request / not_found", {
  svc <- local_service()
  expect_error(h_query(svc, "Nope"), class = "ontapi_not_found")
  expect_error(h_query(svc, "Airport", limit = -1), class = "ontapi_bad_request")
  expect_error(h_query(svc, "Airport", offset = "x"), class = "ontapi_bad_request")
  expect_error(h_query(svc, "Airport", select = "nope"), class = "ontapi_bad_request")
  expect_error(
    h_query(svc, "Airport", filters = list(list(property = "nope", op = "eq", value = 1))),
    class = "ontapi_bad_request"
  )
  expect_error(
    h_query(svc, "Airport", filters = list(list(property = "country", op = "BAD", value = 1))),
    class = "ontapi_bad_request"
  )
  expect_error(
    h_query(svc, "Airport", filters = list(list(property = "country", op = "eq"))),
    class = "ontapi_bad_request"
  )
})

test_that("h_count counts with and without filters", {
  svc <- local_service()
  expect_equal(h_count(svc, "FlightRoute")$count, 5)
  expect_equal(
    h_count(svc, "FlightRoute",
            filters = list(list(property = "origin_id", op = "eq", value = "DUB")))$count,
    3
  )
})

test_that("h_get fetches one object and 404s on a missing key", {
  svc <- local_service()
  dub <- h_get(svc, "Airport", "DUB")
  expect_equal(dub$name, "Dublin Airport")
  expect_equal(dub$elevation_ft, 242L)
  expect_error(h_get(svc, "Airport", "ZZZ"), class = "ontapi_not_found")
  expect_error(h_get(svc, "Airport", NULL), class = "ontapi_bad_request")
})

test_that("split_pk_value handles single and composite keys", {
  svc <- local_service()
  ot <- ontologySpecR::bundle_object_type(svc$bundle, "Airport")
  expect_equal(split_pk_value(ot, "airport_id", "DUB"), list(airport_id = "DUB"))
  pk <- c("a", "b")
  expect_equal(split_pk_value(ot, pk, list(b = "2", a = "1")), list(a = "1", b = "2"))
  expect_equal(split_pk_value(ot, pk, c("1", "2")), list(a = "1", b = "2"))
  expect_equal(split_pk_value(ot, pk, "1|2"), list(a = "1", b = "2"))
  expect_error(split_pk_value(ot, pk, list(a = "1")), class = "ontapi_bad_request")
  expect_error(split_pk_value(ot, pk, "1|2|3"), class = "ontapi_bad_request")
})

test_that("get_by_pk coerces key values to the property type", {
  svc <- local_service()
  ot <- ontologySpecR::bundle_object_type(svc$bundle, "Airport")
  expect_equal(coerce_to_property(ot, "elevation_ft", "242"), 242L)
  expect_error(coerce_to_property(ot, "elevation_ft", "abc"), class = "ontapi_bad_request")
})

test_that("h_traverse follows a link forward and in reverse", {
  svc <- local_service()
  fwd <- h_traverse(svc, "FlightRoute", "R001", "RouteOrigin")
  expect_equal(fwd$object_type_id, "Airport")
  expect_equal(vapply(fwd$rows, function(r) r$airport_id, character(1)), "DUB")

  rev <- h_search_around(svc, "Airport", "DUB", "RouteOrigin")
  expect_equal(rev$object_type_id, "FlightRoute")
  ids <- vapply(rev$rows, function(r) r$route_id, character(1))
  expect_equal(ids, c("R001", "R002", "R003"))

  limited <- h_search_around(svc, "Airport", "DUB", "RouteOrigin", limit = 1,
                             select = list("route_id", "airline_id"))
  expect_length(limited$rows, 1L)
  expect_named(limited$rows[[1]], c("route_id", "airline_id"))
})

test_that("h_traverse errors are classed", {
  svc <- local_service()
  expect_error(h_traverse(svc, "Airport", "DUB", "Nope"), class = "ontapi_not_found")
  expect_error(h_traverse(svc, "Airport", "DUB", "RouteOrigin"), class = "ontapi_bad_request")
  expect_error(h_search_around(svc, "FlightRoute", "R001", "RouteOrigin"),
               class = "ontapi_bad_request")
  expect_error(h_traverse(svc, "FlightRoute", "R999", "RouteOrigin"),
               class = "ontapi_not_found")
})

test_that("concept handlers list, describe and evaluate", {
  svc <- local_service()
  expect_equal(h_concepts(svc), list())
  add_irish_concept(svc)

  cs <- h_concepts(svc)
  expect_equal(cs[[1]]$id, "irish_airport")
  expect_equal(cs[[1]]$version_count, 1)

  c1 <- h_concept(svc, "irish_airport")
  expect_equal(c1$object_type_id, "Airport")
  expect_length(c1$versions, 1L)

  ev <- h_evaluate_concept(svc, "irish_airport", "operations")
  expect_equal(ev$version, 1L)
  expect_equal(ev$stage, "development")
  expect_equal(ev$true_count, 3L)
  expect_equal(ev$false_count, 3L)
  expect_length(ev$rows, 6L)
  expect_length(h_evaluate_concept(svc, "irish_airport", "operations", limit = 2)$rows, 2L)
  expect_equal(h_evaluate_concept(svc, "irish_airport", "operations", version = 1)$version, 1L)
})

test_that("concept handler errors are classed", {
  svc <- local_service()
  add_irish_concept(svc)
  expect_error(h_concept(svc, "nope"), class = "ontapi_not_found")
  expect_error(h_evaluate_concept(svc, "irish_airport", NULL), class = "ontapi_bad_request")
  expect_error(h_evaluate_concept(svc, "irish_airport", "other"), class = "ontapi_not_found")
  expect_error(h_evaluate_concept(svc, "irish_airport", "operations", version = 9),
               class = "ontapi_not_found")
  expect_error(h_evaluate_concept(svc, "irish_airport", "operations", stage = "production"),
               class = "ontapi_not_found")
  expect_error(h_evaluate_concept(svc, "irish_airport", "operations", stage = "bogus"),
               class = "ontapi_bad_request")
})

test_that("h_submit_action dry run returns the plan without running", {
  svc <- local_service()
  calls <- new.env()
  svc <- with_status_handler(svc, calls)
  res <- h_submit_action(svc, "UpdateAirportStatus",
                         params = list(new_status = "closed"),
                         target_ids = list("DUB"), dry_run = TRUE)
  expect_true(res$dry_run)
  expect_equal(res$plan$impl_kind, "r")
  expect_equal(res$plan$target_ids, list("DUB"))
  expect_equal(calls$n, 0L)
  expect_length(h_action_log(svc), 0L)
})

test_that("h_submit_action runs the handler and logs it", {
  svc <- local_service()
  calls <- new.env()
  svc <- with_status_handler(svc, calls)
  res <- h_submit_action(svc, "UpdateAirportStatus",
                         params = list(new_status = "closed"),
                         target_ids = list("DUB", "ORK"))
  expect_false(res$dry_run)
  expect_equal(res$status, "success")
  expect_equal(calls$n, 1L)
  expect_equal(unlist(calls$last$target_ids), c("DUB", "ORK"))

  log <- h_action_log(svc)
  expect_length(log, 1L)
  expect_equal(log[[1]]$submission_id, res$submission_id)
  expect_length(h_action_log(svc, action_type_id = "UpdateAirportStatus"), 1L)
  expect_length(h_action_log(svc, action_type_id = "Other"), 0L)
})

test_that("h_submit_action errors are classed", {
  svc <- local_service()
  expect_error(h_submit_action(svc, "Nope", list()), class = "ontapi_not_found")
  expect_error(h_submit_action(svc, "UpdateAirportStatus", list()),
               class = "ontapi_bad_request")
  expect_error(h_submit_action(svc, "UpdateAirportStatus",
                               list(new_status = "x", bogus = 1)),
               class = "ontapi_bad_request")
  expect_error(h_submit_action(svc, "UpdateAirportStatus", list("unnamed")),
               class = "ontapi_bad_request")
})

test_that("an approval gate denial becomes ontapi_forbidden", {
  svc <- with_status_handler(local_service())
  svc$action_ctx <- actionTypesR::register_approval_gate(
    svc$action_ctx, function(action, params, target_ids, ctx) "denied"
  )
  expect_error(
    h_submit_action(svc, "UpdateAirportStatus", list(new_status = "x"), list("DUB")),
    class = "ontapi_forbidden"
  )
})

test_that("the service auth hook gates every handler", {
  seen <- new.env()
  seen$calls <- list()
  auth <- function(actor, operation, resource) {
    seen$calls[[length(seen$calls) + 1L]] <- list(actor, operation, resource)
    operation == "read"
  }
  svc <- local_service(auth = auth, actor = "alice")
  expect_equal(h_count(svc, "Airport")$count, 6)
  expect_equal(seen$calls[[length(seen$calls)]], list("alice", "read", "Airport"))
  expect_error(
    h_submit_action(svc, "UpdateAirportStatus", list(new_status = "x"), dry_run = TRUE),
    class = "ontapi_forbidden"
  )
})

test_that("ontology_service validates its arguments", {
  skip_if_no_backend()
  con <- new_example_con()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  expect_error(ontology_service(example_bundle(), con, auth = "yes"), "auth")
  expect_error(ontology_service(example_bundle(), con, actor = ""), "actor")
  svc <- ontology_service(example_bundle(), con, actor = "a")
  expect_s3_class(svc, "OntologyService")
  expect_s3_class(svc$object_ctx, "OntologyContext")
  expect_s3_class(svc$concept_ctx, "ConceptContext")
  expect_s3_class(svc$action_ctx, "ActionContext")
  expect_output(print(svc), "aviation-demo")
})
