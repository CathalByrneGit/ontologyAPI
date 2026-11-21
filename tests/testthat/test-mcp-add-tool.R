test_that("mcp_add_tool registers a tool that tools/list and tools/call see", {
  server <- mcp_server(local_service())
  out <- mcp_add_tool(server, "echo", "Echo the input",
    input_schema = list(
      type = "object",
      properties = list(text = list(type = "string")),
      required = list("text")
    ),
    handler = function(params) list(content = list(list(type = "text", text = params$text)))
  )
  expect_identical(out, server)

  names <- vapply(mcp_call(server, "tools/list")$result$tools, function(t) t$name, character(1))
  expect_true("echo" %in% names)
  resp <- mcp_tool(server, "echo", list(text = "hello"))
  expect_equal(resp$result$content[[1]]$text, "hello")
})

test_that("mcp_add_tool mutates in place, as ontologyDiscoverR relies on", {
  server <- mcp_server(local_service())
  # dis_add_mcp_tool() ignores mcp_add_tool()'s return value
  register <- function(srv) {
    mcp_add_tool(srv, "build_ontology", "Build",
                 list(type = "object", properties = list(instruction = list(type = "string")),
                      required = list("instruction")),
                 function(params) list(content = list(list(type = "text", text = "Done"))))
    srv
  }
  register(server)
  expect_false(is.null(server$tools$build_ontology))
  expect_length(server$tools, 11L)
})

test_that("mcp_add_tool errors on a duplicate name", {
  server <- mcp_server(local_service())
  expect_error(
    mcp_add_tool(server, "query_objects", "dup", list(type = "object"), function(p) NULL),
    class = "ontapi_duplicate_tool"
  )
  mcp_add_tool(server, "mine", "once", list(type = "object"), function(p) NULL)
  expect_error(
    mcp_add_tool(server, "mine", "twice", list(type = "object"), function(p) NULL),
    "already registered"
  )
})

test_that("mcp_add_tool validates its arguments", {
  server <- mcp_server(local_service())
  expect_error(mcp_add_tool(list(), "x", "d", list(), function(p) NULL), "mcp_server")
  expect_error(mcp_add_tool(server, "", "d", list(), function(p) NULL), "name")
  expect_error(mcp_add_tool(server, "x", 1, list(), function(p) NULL), "description")
  expect_error(mcp_add_tool(server, "x", "d", "schema", function(p) NULL), "input_schema")
  expect_error(mcp_add_tool(server, "x", "d", list(), "fn"), "handler")
})

test_that("added tools can reach the service, return plain values, and raise classed errors", {
  server <- mcp_server(local_service())
  mcp_add_tool(server, "airport_names", "Names of airports", list(),
    function(params, server) {
      rows <- h_query(server$svc, "Airport", select = "name")$rows
      vapply(rows, function(r) r$name, character(1))
    })
  mcp_add_tool(server, "always_missing", "Fails", list(),
    function(params) ontapi_not_found("nothing here"))
  mcp_add_tool(server, "crashes", "Fails", list(), function(params) stop("kaboom"))

  names <- jsonlite::fromJSON(mcp_tool(server, "airport_names")$result$content[[1]]$text)
  expect_true("Dublin Airport" %in% names)
  expect_equal(mcp_tool(server, "always_missing")$error$code, -32602L)
  crash <- mcp_tool(server, "crashes")
  expect_equal(crash$error$code, -32603L)
  expect_match(crash$error$message, "kaboom")

  json <- as.character(to_json(mcp_call(server, "tools/list")))
  expect_match(json, '"name":"airport_names","description":"Names of airports","inputSchema":{"type":"object","properties":{}}', fixed = TRUE)
})

test_that("serve_mcp rejects anything but a server or service", {
  expect_error(serve_mcp(list()), "mcp_server")
  expect_output(print(mcp_server(local_service())), "10 tools")
})
