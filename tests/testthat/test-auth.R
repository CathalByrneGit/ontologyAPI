make_mock_req <- function(auth_header = NULL) {
  req <- new.env(parent = emptyenv())
  if (!is.null(auth_header)) req$HTTP_AUTHORIZATION <- auth_header
  req
}

make_mock_res <- function() {
  res <- new.env(parent = emptyenv())
  res$status <- 200L
  res
}

test_that("auth filter is a no-op when api_key is NULL or empty", {
  skip_if_not_installed("plumber")
  for (key in list(NULL, "")) {
    res <- make_mock_res()
    expect_silent(make_auth_filter(key)(make_mock_req(), res))
    expect_equal(res$status, 200L)
  }
})

test_that("auth filter returns 401 for missing, malformed or wrong keys", {
  skip_if_not_installed("plumber")
  filter <- make_auth_filter("secret-key")
  for (hdr in list(NULL, "Basic dXNlcjpwYXNz", "Bearer wrong-key")) {
    res <- make_mock_res()
    out <- filter(make_mock_req(hdr), res)
    expect_equal(res$status, 401L)
    expect_false(is.null(out$error))
  }
})

test_that("auth filter passes for the correct key", {
  skip_if_not_installed("plumber")
  res <- make_mock_res()
  expect_silent(make_auth_filter("my-secret")(make_mock_req("Bearer my-secret"), res))
  expect_equal(res$status, 200L)
})

test_that("the router enforces the API key on every route", {
  pr <- local_router(local_service(), api_key = "test-key-123")
  expect_equal(rest_request(pr, "GET", "/v1/health")$status, 401L)
  expect_equal(rest_request(pr, "GET", "/v1/objects/Airport/count")$status, 401L)
  ok <- rest_request(pr, "GET", "/v1/objects/Airport/count",
                     headers = list(HTTP_AUTHORIZATION = "Bearer test-key-123"))
  expect_equal(ok$status, 200L)
})

test_that("ontology_router warns when no API key is configured", {
  skip_if_not_installed("plumber")
  svc <- local_service()
  withr_env <- Sys.getenv("ONTOLOGY_API_KEY", unset = NA)
  Sys.unsetenv("ONTOLOGY_API_KEY")
  on.exit(if (!is.na(withr_env)) Sys.setenv(ONTOLOGY_API_KEY = withr_env), add = TRUE)
  expect_warning(ontology_router(svc), "development mode")
})
