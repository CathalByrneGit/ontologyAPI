# Ported from ontologyMCP's test-error-codes.R. Handler errors are mapped
# from the shared ontapi_* classes.

test_that("unknown tool returns -32601", {
  server <- mcp_server(local_service())
  expect_equal(mcp_tool(server, "nonexistent_tool")$error$code, -32601L)
  expect_equal(mcp_call(server, "tools/call", list())$error$code, -32602L)
})

test_that("unknown object type returns -32602 with kind not_found", {
  server <- mcp_server(local_service())
  resp <- mcp_tool(server, "query_objects", list(object_type_id = "DoesNotExist"))
  expect_equal(resp$error$code, -32602L)
  expect_equal(resp$error$data$kind, "not_found")
})

test_that("missing object returns -32602", {
  server <- mcp_server(local_service())
  resp <- mcp_tool(server, "get_object", list(object_type_id = "Airport", pk_value = "ZZZ"))
  expect_equal(resp$error$code, -32602L)
})

test_that("missing required argument and bad filter op return -32602", {
  server <- mcp_server(local_service())
  expect_equal(mcp_tool(server, "query_objects", list())$error$code, -32602L)
  resp <- mcp_tool(server, "query_objects", list(
    object_type_id = "Airport",
    filters = list(list(property = "country", op = "LIKE", value = "x"))
  ))
  expect_equal(resp$error$code, -32602L)
  expect_equal(resp$error$data$kind, "bad_request")
})

test_that("forbidden returns -32001", {
  server <- mcp_server(local_service(auth = function(actor, operation, resource) FALSE))
  resp <- mcp_tool(server, "count_objects", list(object_type_id = "Airport"))
  expect_equal(resp$error$code, -32001L)
  expect_equal(resp$error$data$kind, "forbidden")
})

test_that("an internal failure returns -32603", {
  svc <- local_service()
  DBI::dbExecute(svc$connection, "DROP TABLE routes")
  server <- mcp_server(svc)
  resp <- mcp_tool(server, "count_objects", list(object_type_id = "FlightRoute"))
  expect_equal(resp$error$code, -32603L)
})
