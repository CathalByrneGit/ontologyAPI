test_that("POST /v1/objects/Airport/query returns all airports with no filters", {
  skip_deps(extra = c("callr", "httr2"))
  srv <- start_test_server()
  on.exit(srv$stop(), add = TRUE)

  body <- api_post(srv$url, "/v1/objects/Airport/query", list())
  expect_equal(body$meta$status, "ok")
  expect_gte(body$data$total_count, 6L)
  expect_length(body$data$rows, min(body$data$total_count, 100L))
})

test_that("POST /v1/objects/Airport/query filters by country eq Ireland", {
  skip_deps(extra = c("callr", "httr2"))
  srv <- start_test_server()
  on.exit(srv$stop(), add = TRUE)

  body <- api_post(srv$url, "/v1/objects/Airport/query", list(
    filters = list(list(property = "country", op = "eq", value = "Ireland"))
  ))
  expect_equal(body$meta$status, "ok")
  expect_equal(body$data$total_count, 3L)
  countries <- vapply(body$data$rows, `[[`, character(1L), "country")
  expect_true(all(countries == "Ireland"))
})

test_that("POST /v1/objects/Airport/query respects limit and offset", {
  skip_deps(extra = c("callr", "httr2"))
  srv <- start_test_server()
  on.exit(srv$stop(), add = TRUE)

  body1 <- api_post(srv$url, "/v1/objects/Airport/query",
    list(limit = 2L, offset = 0L))
  body2 <- api_post(srv$url, "/v1/objects/Airport/query",
    list(limit = 2L, offset = 2L))

  expect_length(body1$data$rows, 2L)
  expect_length(body2$data$rows, 2L)

  ids1 <- vapply(body1$data$rows, `[[`, character(1L), "airport_id")
  ids2 <- vapply(body2$data$rows, `[[`, character(1L), "airport_id")
  expect_equal(length(intersect(ids1, ids2)), 0L)
})

test_that("POST /v1/objects/Airport/query selects specific columns", {
  skip_deps(extra = c("callr", "httr2"))
  srv <- start_test_server()
  on.exit(srv$stop(), add = TRUE)

  body <- api_post(srv$url, "/v1/objects/Airport/query",
    list(select = c("airport_id", "name")))
  row <- body$data$rows[[1L]]
  expect_true(all(c("airport_id", "name") %in% names(row)))
  expect_false("country" %in% names(row))
})

test_that("POST /v1/objects/Airport/query gte filter on capacity", {
  skip_deps(extra = c("callr", "httr2"))
  srv <- start_test_server()
  on.exit(srv$stop(), add = TRUE)

  body <- api_post(srv$url, "/v1/objects/Airport/query", list(
    filters = list(list(property = "capacity", op = "gte", value = 30000000L))
  ))
  caps <- vapply(body$data$rows, `[[`, integer(1L), "capacity")
  expect_true(all(caps >= 30000000L))
})

test_that("GET /v1/objects/Airport/count returns correct total", {
  skip_deps(extra = c("callr", "httr2"))
  srv <- start_test_server()
  on.exit(srv$stop(), add = TRUE)

  body <- api_get(srv$url, "/v1/objects/Airport/count")
  expect_equal(body$meta$status, "ok")
  expect_gte(body$data$count, 6L)
})

test_that("GET /v1/objects/Airport/DUB returns Dublin airport", {
  skip_deps(extra = c("callr", "httr2"))
  srv <- start_test_server()
  on.exit(srv$stop(), add = TRUE)

  body <- api_get(srv$url, "/v1/objects/Airport/DUB")
  expect_equal(body$meta$status, "ok")
  expect_equal(body$data$airport_id, "DUB")
  expect_equal(body$data$country, "Ireland")
})

test_that("GET /v1/objects/Airport/INVALID returns 404", {
  skip_deps(extra = c("callr", "httr2"))
  srv <- start_test_server()
  on.exit(srv$stop(), add = TRUE)

  resp <- httr2::request(paste0(srv$url, "/v1/objects/Airport/INVALID")) |>
    httr2::req_error(is_error = \(r) FALSE) |>
    httr2::req_perform()
  expect_equal(httr2::resp_status(resp), 404L)

  body <- httr2::resp_body_json(resp)
  expect_equal(body$meta$status, "error")
})

test_that("POST /v1/objects/Airport/query has_more flag is correct", {
  skip_deps(extra = c("callr", "httr2"))
  srv <- start_test_server()
  on.exit(srv$stop(), add = TRUE)

  body_all  <- api_post(srv$url, "/v1/objects/Airport/query", list(limit = 1000L))
  body_page <- api_post(srv$url, "/v1/objects/Airport/query",
    list(limit = 1L, offset = 0L))

  expect_false(body_all$data$has_more)
  if (body_all$data$total_count > 1L) {
    expect_true(body_page$data$has_more)
  }
})
