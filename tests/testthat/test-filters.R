test_that("build_filter_expr: eq produces equality expression", {
  col  <- rlang::sym("country")
  expr <- build_filter_expr(col, "eq", "Ireland")
  expect_equal(rlang::expr_text(expr), 'country == "Ireland"')
})

test_that("build_filter_expr: neq", {
  col  <- rlang::sym("country")
  expr <- build_filter_expr(col, "neq", "Ireland")
  expect_equal(rlang::expr_text(expr), 'country != "Ireland"')
})

test_that("build_filter_expr: numeric comparisons", {
  col <- rlang::sym("capacity")
  expect_match(rlang::expr_text(build_filter_expr(col, "gt",  1000L)), "capacity > ")
  expect_match(rlang::expr_text(build_filter_expr(col, "gte", 1000L)), "capacity >= ")
  expect_match(rlang::expr_text(build_filter_expr(col, "lt",  5000L)), "capacity < ")
  expect_match(rlang::expr_text(build_filter_expr(col, "lte", 5000L)), "capacity <= ")
})

test_that("build_filter_expr: contains uses grepl with fixed = TRUE", {
  col  <- rlang::sym("name")
  expr <- build_filter_expr(col, "contains", "Dublin")
  txt  <- rlang::expr_text(expr)
  expect_match(txt, "grepl")
  expect_match(txt, "fixed = TRUE")
})

test_that("build_filter_expr: starts_with", {
  col  <- rlang::sym("name")
  expr <- build_filter_expr(col, "starts_with", "Dub")
  expect_match(rlang::expr_text(expr), "startsWith")
})

test_that("build_filter_expr: in / not_in", {
  col   <- rlang::sym("country")
  val   <- c("Ireland", "UK")
  expr_in  <- build_filter_expr(col, "in",     val)
  expr_nin <- build_filter_expr(col, "not_in", val)
  expect_match(rlang::expr_text(expr_in),  "%in%")
  expect_match(rlang::expr_text(expr_nin), "%in%")
  expect_match(rlang::expr_text(expr_nin), "^!")
})

test_that("build_filter_expr: is_null / is_not_null ignore value", {
  col <- rlang::sym("city")
  expect_equal(rlang::expr_text(build_filter_expr(col, "is_null",     NULL)), "is.na(city)")
  expect_equal(rlang::expr_text(build_filter_expr(col, "is_not_null", NULL)), "!is.na(city)")
})

test_that("translate_filter aborts with http_400 for unknown op", {
  mock_os <- structure(list(), class = "ObjectSet")
  expect_error(
    translate_filter(mock_os, list(property = "country", op = "INVALID", value = "x")),
    class = "http_400"
  )
})

test_that("apply_filters returns os unchanged for empty/NULL filters", {
  os <- structure(list(type = "test"), class = "ObjectSet")
  expect_identical(apply_filters(os, NULL),    os)
  expect_identical(apply_filters(os, list()),  os)
})

test_that("SQL injection in filter value is harmless (DuckDB integration)", {
  skip_deps()

  con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  seed_test_db(con)

  bundle     <- make_test_bundle()
  object_ctx <- objectSetsR::ontology_context(bundle, con)
  os         <- objectSetsR::os_get(object_ctx, "Airport")

  injection  <- "'; DROP TABLE airports; --"
  os_filtered <- translate_filter(os, list(
    property = "country",
    op       = "eq",
    value    = injection
  ))

  rows <- objectSetsR::os_collect(os_filtered)
  expect_equal(nrow(rows), 0L)

  # Confirm the table still exists (injection did not execute)
  expect_true(DBI::dbExistsTable(con, "airports"))
  expect_gt(nrow(DBI::dbGetQuery(con, "SELECT * FROM airports")), 0L)
})
