test_that("POST traverse FlightRoute from DUB returns routes departing Dublin", {
  skip_deps(extra = c("callr", "httr2"))
  srv <- start_test_server()
  on.exit(srv$stop(), add = TRUE)

  body <- api_post(srv$url, "/v1/objects/Airport/DUB/traverse/FlightRoute", list())
  expect_equal(body$meta$status, "ok")
  expect_gte(length(body$data$rows), 1L)

  from_airports <- vapply(body$data$rows, `[[`, character(1L), "from_airport")
  expect_true(all(from_airports == "DUB"))
})

test_that("POST traverse respects limit", {
  skip_deps(extra = c("callr", "httr2"))
  srv <- start_test_server()
  on.exit(srv$stop(), add = TRUE)

  body <- api_post(srv$url, "/v1/objects/Airport/DUB/traverse/FlightRoute",
    list(limit = 1L))
  expect_lte(length(body$data$rows), 1L)
})

test_that("POST traverse respects select", {
  skip_deps(extra = c("callr", "httr2"))
  srv <- start_test_server()
  on.exit(srv$stop(), add = TRUE)

  body <- api_post(srv$url, "/v1/objects/Airport/DUB/traverse/FlightRoute",
    list(select = c("route_id", "airline")))
  if (length(body$data$rows) > 0L) {
    row <- body$data$rows[[1L]]
    expect_true(all(c("route_id", "airline") %in% names(row)))
    expect_false("from_airport" %in% names(row))
  }
})

test_that("POST traverse for unknown source object returns 404", {
  skip_deps(extra = c("callr", "httr2"))
  srv <- start_test_server()
  on.exit(srv$stop(), add = TRUE)

  resp <- httr2::request(
    paste0(srv$url, "/v1/objects/Airport/XXXX/traverse/FlightRoute")
  ) |>
    httr2::req_method("POST") |>
    httr2::req_body_json(list()) |>
    httr2::req_error(is_error = \(r) FALSE) |>
    httr2::req_perform()
  expect_equal(httr2::resp_status(resp), 404L)
})

test_that("POST search_around returns routes arriving at DUB (reverse traversal)", {
  skip_deps(extra = c("callr", "httr2"))
  srv <- start_test_server()
  on.exit(srv$stop(), add = TRUE)

  body <- api_post(
    srv$url,
    "/v1/objects/Airport/DUB/search_around/FlightRoute",
    list()
  )
  expect_equal(body$meta$status, "ok")
  expect_type(body$data$rows, "list")
})
