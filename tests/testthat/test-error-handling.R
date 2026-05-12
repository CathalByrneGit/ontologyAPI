make_mock_res <- function() {
  res        <- new.env(parent = emptyenv())
  res$status <- 200L
  res
}

test_that("http_abort throws with http_400 class by default", {
  expect_error(
    http_abort("bad input"),
    class = "http_400"
  )
})

test_that("http_abort throws with http_404 class for status 404", {
  expect_error(
    http_abort("not found", status = 404L),
    class = "http_404"
  )
})

test_that("http_abort throws with http_422 class for status 422", {
  expect_error(
    http_abort("unprocessable", status = 422L),
    class = "http_422"
  )
})

test_that("make_error_handler maps http_400 to 400 status", {
  handler <- make_error_handler()
  req     <- new.env(parent = emptyenv())
  res     <- make_mock_res()
  err     <- tryCatch(http_abort("bad"), error = function(e) e)

  result  <- handler(req, res, err)
  expect_equal(res$status, 400L)
  expect_equal(result$error[[1]], "bad")
})

test_that("make_error_handler maps http_404 to 404 status", {
  handler <- make_error_handler()
  req     <- new.env(parent = emptyenv())
  res     <- make_mock_res()
  err     <- tryCatch(http_abort("gone", status = 404L), error = function(e) e)

  handler(req, res, err)
  expect_equal(res$status, 404L)
})

test_that("make_error_handler defaults to 500 for unknown errors", {
  handler <- make_error_handler()
  req     <- new.env(parent = emptyenv())
  res     <- make_mock_res()
  err     <- simpleError("something exploded")

  result <- handler(req, res, err)
  expect_equal(res$status, 500L)
  expect_match(result$error[[1]], "something exploded")
})

test_that("error response has correct envelope shape", {
  r <- error_response("oops", took_ms = 5L)
  expect_named(r, c("error", "meta"))
  expect_equal(r$meta$status[[1]], "error")
  expect_equal(r$meta$took_ms[[1]], 5L)
})

# ---------------------------------------------------------------------------
# Integration: unknown object type → 404
# ---------------------------------------------------------------------------
test_that("GET /v1/objects/NonExistentType/count returns 404", {
  skip_deps(extra = c("callr", "httr2"))
  srv <- start_test_server()
  on.exit(srv$stop(), add = TRUE)

  resp <- httr2::request(paste0(srv$url, "/v1/objects/NonExistentType/count")) |>
    httr2::req_error(is_error = \(r) FALSE) |>
    httr2::req_perform()
  expect_equal(httr2::resp_status(resp), 404L)
  body <- httr2::resp_body_json(resp)
  expect_true(!is.null(body$error))
  expect_equal(body$meta$status, "error")
})

test_that("POST /v1/objects/Airport/query with invalid op returns 400", {
  skip_deps(extra = c("callr", "httr2"))
  srv <- start_test_server()
  on.exit(srv$stop(), add = TRUE)

  resp <- httr2::request(paste0(srv$url, "/v1/objects/Airport/query")) |>
    httr2::req_body_json(list(
      filters = list(list(property = "country", op = "BADOP", value = "Ireland"))
    )) |>
    httr2::req_error(is_error = \(r) FALSE) |>
    httr2::req_perform()
  expect_equal(httr2::resp_status(resp), 400L)
})
