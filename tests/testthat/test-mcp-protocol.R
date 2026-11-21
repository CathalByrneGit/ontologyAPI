# Ported from ontologyMCP's test-protocol.R.

test_that("initialize returns protocol version and server info", {
  server <- mcp_server(local_service(), name = "test-server")
  resp <- mcp_call(server, "initialize")
  expect_equal(resp$jsonrpc, "2.0")
  expect_equal(resp$id, 1L)
  expect_equal(resp$result$protocolVersion, "2024-11-05")
  expect_equal(resp$result$serverInfo$name, "test-server")
  expect_equal(resp$result$serverInfo$version, as.character(utils::packageVersion("ontologyAPI")))
  expect_false(is.null(resp$result$capabilities$tools))
})

test_that("tools/list returns the ten built-in tools with object schemas", {
  server <- mcp_server(local_service())
  tools <- mcp_call(server, "tools/list")$result$tools
  names <- vapply(tools, function(t) t$name, character(1))
  expect_setequal(names, c(
    "get_ontology_summary", "list_object_types", "list_link_types",
    "query_objects", "count_objects", "get_object", "traverse_links",
    "list_concepts", "evaluate_concept", "submit_action"
  ))
  for (t in tools) {
    expect_equal(t$inputSchema$type, "object", info = t$name)
    expect_false(is.null(t$inputSchema$properties), info = t$name)
  }
})

test_that("empty inputSchema properties serialise as a JSON object", {
  server <- mcp_server(local_service())
  json <- as.character(to_json(mcp_call(server, "tools/list")))
  expect_match(json, '"name":"list_object_types"', fixed = TRUE)
  expect_false(grepl('"properties":[]', json, fixed = TRUE))
  expect_match(json, '"properties":{}', fixed = TRUE)
})

test_that("ping responds and notifications get no response", {
  server <- mcp_server(local_service())
  resp <- mcp_call(server, "ping")
  expect_null(resp$error)
  expect_equal(as.character(to_json(resp$result)), "{}")
  expect_null(handle_message(list(jsonrpc = "2.0", method = "notifications/initialized"), server))
})

test_that("unknown method returns -32601 and missing method -32600", {
  server <- mcp_server(local_service())
  expect_equal(mcp_call(server, "banana/split")$error$code, -32601L)
  expect_equal(handle_message(list(jsonrpc = "2.0", id = 1L), server)$error$code, -32600L)
})

test_that("resources list and read through the accessors", {
  server <- mcp_server(local_service())
  uris <- vapply(mcp_call(server, "resources/list")$result$resources,
                 function(r) r$uri, character(1))
  expect_true(all(c("ontology://bundle", "ontology://schema",
                    "ontology://objects/Airport/schema") %in% uris))

  bundle <- mcp_call(server, "resources/read", list(uri = "ontology://bundle"))
  parsed <- jsonlite::fromJSON(bundle$result$contents[[1]]$text, simplifyVector = FALSE)
  expect_equal(parsed$bundleId, "aviation-demo")

  schema <- mcp_call(server, "resources/read", list(uri = "ontology://objects/Airport/schema"))
  s <- jsonlite::fromJSON(schema$result$contents[[1]]$text, simplifyVector = FALSE)
  expect_equal(s$properties$airport_id$type, "string")
  expect_equal(s$required, list("airport_id"))

  expect_equal(mcp_call(server, "resources/read", list(uri = "ontology://nope"))$error$code, -32602L)
  expect_equal(
    mcp_call(server, "resources/read", list(uri = "ontology://objects/Nope/schema"))$error$code,
    -32602L
  )
})

test_that("the stdio transport answers line by line and survives bad input", {
  server <- mcp_server(local_service())
  input <- textConnection(c(
    '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{}}',
    '{"jsonrpc":"2.0","method":"notifications/initialized"}',
    "",
    "this is not json",
    '{"jsonrpc":"2.0","id":2,"method":"tools/call","params":{"name":"count_objects","arguments":{"object_type_id":"Airport"}}}'
  ))
  out <- character()
  output <- textConnection("out", "w", local = TRUE)
  run_stdio(server, input = input, output = output)
  close(output)
  close(input)

  expect_length(out, 3L)
  msgs <- lapply(out, jsonlite::fromJSON, simplifyVector = FALSE)
  expect_equal(msgs[[1]]$id, 1L)
  expect_equal(msgs[[2]]$error$code, -32700L)
  expect_equal(jsonlite::fromJSON(msgs[[3]]$result$content[[1]]$text)$count, 6)
})

test_that("stdio keeps console output and messages off the protocol stream", {
  server <- mcp_server(local_service())
  mcp_add_tool(server, "noisy", "Prints", list(type = "object"), function(params) {
    print("console noise")
    cat("more noise\n")
    message("a message")
    cli::cli_alert_info("cli noise")
    list(ok = TRUE)
  })
  input <- textConnection(
    '{"jsonrpc":"2.0","id":7,"method":"tools/call","params":{"name":"noisy","arguments":{}}}'
  )
  out <- character()
  output <- textConnection("out", "w", local = TRUE)
  utils::capture.output(run_stdio(server, input = input, output = output), type = "message")
  close(output)
  close(input)
  expect_length(out, 1L)
  expect_equal(jsonlite::fromJSON(out[[1]])$id, 7L)
})
