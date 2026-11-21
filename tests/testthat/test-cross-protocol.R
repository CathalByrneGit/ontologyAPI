# The same query through REST and through MCP must return the same rows: both
# call h_query() and serialise with the same JSON options.

test_that("REST and MCP stdio return identical results for the same query", {
  svc <- local_service()
  query <- list(
    filters = list(
      list(property = "country", op = "in", value = list("Ireland", "UK")),
      list(property = "elevation_ft", op = "gte", value = 50)
    ),
    select = list("airport_id", "name", "elevation_ft"),
    limit = 2,
    offset = 1
  )

  # REST, through the plumber router
  pr <- local_router(svc)
  rest <- rest_request(pr, "POST", "/v1/objects/Airport/query", query)
  expect_equal(rest$status, 200L)

  # MCP, as a JSON-RPC line through the stdio transport
  rpc <- list(jsonrpc = "2.0", id = 1L, method = "tools/call", params = list(
    name = "query_objects",
    arguments = c(list(object_type_id = "Airport"), query)
  ))
  input <- textConnection(as.character(jsonlite::toJSON(rpc, auto_unbox = TRUE)))
  out <- character()
  output <- textConnection("out", "w", local = TRUE)
  run_stdio(mcp_server(svc), input = input, output = output)
  close(output)
  close(input)
  mcp_resp <- jsonlite::fromJSON(out[[1]], simplifyVector = FALSE)
  expect_null(mcp_resp$error)
  mcp <- jsonlite::fromJSON(mcp_resp$result$content[[1]]$text, simplifyVector = FALSE)

  expect_equal(mcp$rows, rest$body$data$rows)
  expect_equal(mcp$total_count, rest$body$data$total_count)
  expect_equal(mcp$has_more, rest$body$data$has_more)

  # and both match the handler itself
  direct <- jsonlite::fromJSON(
    as.character(to_json(do.call(h_query, c(list(svc, "Airport"), query)))),
    simplifyVector = FALSE
  )
  expect_equal(mcp$rows, direct$rows)
  ids <- vapply(mcp$rows, function(r) r$airport_id, character(1))
  expect_equal(ids, c("LHR", "ORK"))
})

test_that("REST and MCP report the same error kind for the same mistake", {
  svc <- local_service()
  pr <- local_router(svc)
  rest <- rest_request(pr, "GET", "/v1/objects/Airport/ZZZ")
  mcp <- mcp_tool(mcp_server(svc), "get_object",
                  list(object_type_id = "Airport", pk_value = "ZZZ"))
  expect_equal(rest$body$meta$kind, mcp$error$data$kind)
  expect_equal(rest$body$error, mcp$error$message)
})
