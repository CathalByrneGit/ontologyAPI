# Ported from ontologyMCP's test-tools.R, against the example bundle.

test_that("list_object_types returns all types", {
  server <- mcp_server(local_service())
  parsed <- mcp_tool_json(mcp_tool(server, "list_object_types"))
  ids <- vapply(parsed, function(x) x$id, character(1))
  expect_setequal(ids, c("Airport", "Airline", "FlightRoute"))
})

test_that("list_link_types returns the example links", {
  server <- mcp_server(local_service())
  parsed <- mcp_tool_json(mcp_tool(server, "list_link_types"))
  expect_length(parsed, 3L)
  expect_true("RouteOrigin" %in% vapply(parsed, function(x) x$id, character(1)))
})

test_that("query_objects with an eq filter returns matching rows", {
  server <- mcp_server(local_service())
  parsed <- mcp_tool_json(mcp_tool(server, "query_objects", list(
    object_type_id = "Airport",
    filters = list(list(property = "country", op = "eq", value = "UK"))
  )))
  expect_equal(parsed$total_count, 1)
  expect_equal(parsed$rows[[1]]$airport_id, "LHR")
})

test_that("query_objects with a contains filter works", {
  server <- mcp_server(local_service())
  parsed <- mcp_tool_json(mcp_tool(server, "query_objects", list(
    object_type_id = "Airport",
    filters = list(list(property = "name", op = "contains", value = "Airport"))
  )))
  expect_equal(parsed$total_count, 3)
})

test_that("query_objects pages with limit and offset", {
  server <- mcp_server(local_service())
  p <- mcp_tool_json(mcp_tool(server, "query_objects",
                              list(object_type_id = "Airport", limit = 2, offset = 2)))
  expect_length(p$rows, 2L)
  expect_true(p$has_more)
  expect_equal(p$offset, 2L)
})

test_that("get_object fetches a single airport", {
  server <- mcp_server(local_service())
  parsed <- mcp_tool_json(mcp_tool(server, "get_object",
                                   list(object_type_id = "Airport", pk_value = "LHR")))
  expect_equal(parsed$name, "London Heathrow")
})

test_that("count_objects counts with filters", {
  server <- mcp_server(local_service())
  parsed <- mcp_tool_json(mcp_tool(server, "count_objects", list(
    object_type_id = "FlightRoute",
    filters = list(list(property = "airline_id", op = "eq", value = "FR"))
  )))
  expect_equal(parsed$count, 2)
})

test_that("traverse_links follows links forward and in reverse", {
  server <- mcp_server(local_service())
  fwd <- mcp_tool_json(mcp_tool(server, "traverse_links", list(
    object_type_id = "FlightRoute", pk_value = "R004", link_type_id = "RouteOperator"
  )))
  expect_equal(fwd$rows[[1]]$airline_id, "FR")

  rev <- mcp_tool_json(mcp_tool(server, "traverse_links", list(
    object_type_id = "Airport", pk_value = "LHR",
    link_type_id = "RouteDestination", direction = "reverse"
  )))
  ids <- vapply(rev$rows, function(r) r$route_id, character(1))
  expect_equal(ids, c("R001", "R004"))
})

test_that("get_ontology_summary returns bundle id and object types", {
  server <- mcp_server(local_service())
  parsed <- mcp_tool_json(mcp_tool(server, "get_ontology_summary"))
  expect_equal(parsed$bundle_id, "aviation-demo")
  expect_length(parsed$object_types, 3L)
})

test_that("concept tools list and evaluate", {
  svc <- local_service()
  add_irish_concept(svc)
  server <- mcp_server(svc)
  cs <- mcp_tool_json(mcp_tool(server, "list_concepts"))
  expect_equal(cs[[1]]$id, "irish_airport")
  ev <- mcp_tool_json(mcp_tool(server, "evaluate_concept",
                               list(concept_id = "irish_airport", scope = "operations")))
  expect_equal(ev$true_count, 3L)
  expect_length(ev$rows, 6L)
})

test_that("SQL injection in a filter value leaves the table intact", {
  server <- mcp_server(local_service())
  mcp_tool(server, "query_objects", list(
    object_type_id = "Airport",
    filters = list(list(property = "country", op = "eq",
                        value = "'; DROP TABLE airports; --"))
  ))
  parsed <- mcp_tool_json(mcp_tool(server, "count_objects", list(object_type_id = "Airport")))
  expect_equal(parsed$count, 6)
})
