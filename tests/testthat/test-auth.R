make_mock_req <- function(auth_header = NULL) {
  req <- new.env(parent = emptyenv())
  if (!is.null(auth_header)) req$HTTP_AUTHORIZATION <- auth_header
  req
}

make_mock_res <- function() {
  res        <- new.env(parent = emptyenv())
  res$status <- 200L
  res
}

test_that("auth filter is a no-op when api_key is NULL", {
  filter <- make_auth_filter(NULL)
  req    <- make_mock_req()
  res    <- make_mock_res()

  # Should forward without setting an error status.
  # (plumber::forward() is a no-op outside a plumber context so we just check
  #  that no error is thrown and status stays 200.)
  expect_silent(filter(req, res))
  expect_equal(res$status, 200L)
})

test_that("auth filter is a no-op when api_key is empty string", {
  filter <- make_auth_filter("")
  req    <- make_mock_req()
  res    <- make_mock_res()
  expect_silent(filter(req, res))
  expect_equal(res$status, 200L)
})

test_that("auth filter returns 401 when Authorization header is missing", {
  filter   <- make_auth_filter("secret-key")
  req      <- make_mock_req()
  res      <- make_mock_res()
  result   <- filter(req, res)
  expect_equal(res$status, 401L)
  expect_true(!is.null(result$error))
})

test_that("auth filter returns 401 when header is not Bearer scheme", {
  filter <- make_auth_filter("secret-key")
  req    <- make_mock_req("Basic dXNlcjpwYXNz")
  res    <- make_mock_res()
  result <- filter(req, res)
  expect_equal(res$status, 401L)
})

test_that("auth filter returns 401 for wrong key", {
  filter <- make_auth_filter("correct-key")
  req    <- make_mock_req("Bearer wrong-key")
  res    <- make_mock_res()
  result <- filter(req, res)
  expect_equal(res$status, 401L)
})

test_that("auth filter passes for correct key", {
  filter <- make_auth_filter("my-secret")
  req    <- make_mock_req("Bearer my-secret")
  res    <- make_mock_res()
  # No error, status stays 200
  expect_silent(filter(req, res))
  expect_equal(res$status, 200L)
})

# ---------------------------------------------------------------------------
# Integration: auth enforcement via live server
# ---------------------------------------------------------------------------
test_that("requests without key return 401 when ONTOLOGY_API_KEY is set", {
  skip_deps(extra = c("callr", "httr2"))
  srv <- start_test_server(api_key = "test-key-123")
  on.exit(srv$stop(), add = TRUE)

  resp <- httr2::request(paste0(srv$url, "/v1/health")) |>
    httr2::req_error(is_error = \(r) FALSE) |>
    httr2::req_perform()
  expect_equal(httr2::resp_status(resp), 401L)
})

test_that("requests with correct key succeed", {
  skip_deps(extra = c("callr", "httr2"))
  srv <- start_test_server(api_key = "test-key-123")
  on.exit(srv$stop(), add = TRUE)

  resp <- httr2::request(paste0(srv$url, "/v1/health")) |>
    httr2::req_headers(Authorization = "Bearer test-key-123") |>
    httr2::req_perform()
  expect_equal(httr2::resp_status(resp), 200L)
})
