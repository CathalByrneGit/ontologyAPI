# Ported from ontologyMCP's test-dry-run.R: MCP submits default to dry runs.

test_that("submit_action with dry_run omitted previews without running", {
  calls <- new.env()
  svc <- with_status_handler(local_service(), calls)
  server <- mcp_server(svc)
  parsed <- mcp_tool_json(mcp_tool(server, "submit_action", list(
    action_type_id = "UpdateAirportStatus",
    params = list(new_status = "closed"),
    target_ids = list("DUB")
  )))
  expect_true(parsed$dry_run)
  expect_equal(parsed$plan$action_type_id, "UpdateAirportStatus")
  expect_equal(calls$n, 0L)
})

test_that("submit_action with dry_run = FALSE runs the action", {
  calls <- new.env()
  svc <- with_status_handler(local_service(), calls)
  server <- mcp_server(svc)
  parsed <- mcp_tool_json(mcp_tool(server, "submit_action", list(
    action_type_id = "UpdateAirportStatus",
    params = list(new_status = "closed"),
    target_ids = list("DUB"),
    dry_run = FALSE
  )))
  expect_false(parsed$dry_run)
  expect_equal(parsed$status, "success")
  expect_equal(calls$n, 1L)
})

test_that("submit_action with invalid params returns -32602", {
  server <- mcp_server(local_service())
  resp <- mcp_tool(server, "submit_action", list(
    action_type_id = "UpdateAirportStatus", params = list(bogus = 1)
  ))
  expect_equal(resp$error$code, -32602L)
})
