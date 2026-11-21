test_that("ontapi_* helpers raise classed conditions", {
  expect_error(ontapi_bad_request("x"), class = "ontapi_bad_request")
  expect_error(ontapi_not_found("x"), class = "ontapi_not_found")
  expect_error(ontapi_forbidden("x"), class = "ontapi_forbidden")
  expect_error(ontapi_not_found("x"), class = "ontapi_error")
})

test_that("one error maps to matching REST and MCP codes", {
  catch <- function(expr) tryCatch(expr, error = function(e) e)
  bad <- catch(ontapi_bad_request("b"))
  missing <- catch(ontapi_not_found("m"))
  denied <- catch(ontapi_forbidden("d"))
  other <- catch(stop("boom"))

  expect_equal(rest_status_for(bad), 400L)
  expect_equal(rest_status_for(missing), 404L)
  expect_equal(rest_status_for(denied), 403L)
  expect_equal(rest_status_for(other), 500L)

  expect_equal(mcp_code_for(bad), -32602L)
  expect_equal(mcp_code_for(missing), -32602L)
  expect_equal(mcp_code_for(denied), -32001L)
  expect_equal(mcp_code_for(other), -32603L)
})

test_that("the REST error handler sets status and the error envelope", {
  handler <- make_error_handler()
  res <- new.env()
  out <- handler(NULL, res, tryCatch(ontapi_not_found("gone"), error = identity))
  expect_equal(res$status, 404L)
  expect_equal(out$error, "gone")
  expect_equal(out$meta$status, "error")
  expect_equal(out$meta$kind, "not_found")

  res2 <- new.env()
  out2 <- suppressMessages(handler(NULL, res2, simpleError("exploded")))
  expect_equal(res2$status, 500L)
  expect_equal(out2$meta$kind, "internal")
})
