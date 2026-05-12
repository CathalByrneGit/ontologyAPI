test_that("GET /v1/health returns ok with bundle metadata", {
  skip_deps(extra = c("callr", "httr2"))
  srv <- start_test_server()
  on.exit(srv$stop(), add = TRUE)

  body <- api_get(srv$url, "/v1/health")
  expect_equal(body$status, "ok")
  expect_equal(body$bundle_id, "aviation-test")
  expect_type(body$bundle_version, "character")
})

test_that("GET /v1/bundle returns JSON with bundle_id", {
  skip_deps(extra = c("callr", "httr2"))
  srv <- start_test_server()
  on.exit(srv$stop(), add = TRUE)

  body <- api_get(srv$url, "/v1/bundle")
  expect_equal(body$meta$status, "ok")
  expect_true(!is.null(body$data))
  expect_equal(body$data$id, "aviation-test")
})

test_that("GET /v1/bundle/objects returns object type summaries", {
  skip_deps(extra = c("callr", "httr2"))
  srv <- start_test_server()
  on.exit(srv$stop(), add = TRUE)

  body  <- api_get(srv$url, "/v1/bundle/objects")
  types <- body$data
  expect_gte(length(types), 1L)

  ids <- vapply(types, `[[`, character(1L), "id")
  expect_true("Airport" %in% ids)

  airport <- types[[which(ids == "Airport")]]
  expect_equal(airport$display_name, "Airport")
  expect_equal(airport$source_table, "airports")
  expect_type(airport$property_count, "integer")
})

test_that("GET /v1/bundle/links returns link type summaries", {
  skip_deps(extra = c("callr", "httr2"))
  srv <- start_test_server()
  on.exit(srv$stop(), add = TRUE)

  body  <- api_get(srv$url, "/v1/bundle/links")
  links <- body$data
  expect_gte(length(links), 1L)

  link <- links[[1L]]
  expect_true(all(c("id", "from", "to", "cardinality", "directed") %in% names(link)))
})

test_that("GET /v1/bundle/actions returns empty list when no actions configured", {
  skip_deps(extra = c("callr", "httr2"))
  srv  <- start_test_server()
  on.exit(srv$stop(), add = TRUE)

  body <- api_get(srv$url, "/v1/bundle/actions")
  expect_equal(body$meta$status, "ok")
  expect_type(body$data, "list")
})

test_that("response envelope always has data + meta.status + meta.took_ms", {
  skip_deps(extra = c("callr", "httr2"))
  srv <- start_test_server()
  on.exit(srv$stop(), add = TRUE)

  body <- api_get(srv$url, "/v1/bundle/objects")
  expect_named(body, c("data", "meta"), ignore.order = TRUE)
  expect_named(body$meta, c("status", "took_ms"), ignore.order = TRUE)
  expect_equal(body$meta$status, "ok")
  expect_type(body$meta$took_ms, "integer")
})
