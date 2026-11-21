test_that("build_filter_expr: comparisons", {
  col <- rlang::sym("country")
  expect_equal(rlang::expr_text(build_filter_expr(col, "eq", "Ireland")), 'country == "Ireland"')
  expect_equal(rlang::expr_text(build_filter_expr(col, "neq", "Ireland")), 'country != "Ireland"')
  cap <- rlang::sym("capacity")
  expect_match(rlang::expr_text(build_filter_expr(cap, "gt", 1L)), "capacity > ")
  expect_match(rlang::expr_text(build_filter_expr(cap, "gte", 1L)), "capacity >= ")
  expect_match(rlang::expr_text(build_filter_expr(cap, "lt", 1L)), "capacity < ")
  expect_match(rlang::expr_text(build_filter_expr(cap, "lte", 1L)), "capacity <= ")
})

test_that("build_filter_expr: contains / starts_with use escaped regex", {
  col <- rlang::sym("name")
  expect_equal(rlang::expr_text(build_filter_expr(col, "contains", "a.b")),
               'grepl("a\\\\.b", name)')
  expect_match(rlang::expr_text(build_filter_expr(col, "starts_with", "Dub")), '"\\^Dub"')
})

test_that("build_filter_expr: in / not_in / null checks", {
  col <- rlang::sym("country")
  expect_match(rlang::expr_text(build_filter_expr(col, "in", c("IE", "UK"))), "%in%")
  expect_match(rlang::expr_text(build_filter_expr(col, "not_in", c("IE", "UK"))), "^!")
  expect_equal(rlang::expr_text(build_filter_expr(col, "is_null", NULL)), "is.na(country)")
  expect_equal(rlang::expr_text(build_filter_expr(col, "is_not_null", NULL)), "!is.na(country)")
})

test_that("escape_regex escapes every metacharacter", {
  special <- "a.b*c+d?e^f$g(h)i[j]k{l}m|n\\o"
  expect_true(grepl(escape_regex(special), special))
  expect_false(grepl(escape_regex("a.b"), "axb"))
})

test_that("normalize_filters accepts the shapes both protocols produce", {
  expect_equal(normalize_filters(NULL), list())
  one <- list(property = "country", op = "eq", value = "IE")
  expect_equal(normalize_filters(one), list(one))
  expect_equal(normalize_filters(list(one)), list(one))
  # plumber parses a JSON array of objects into a data frame
  df <- data.frame(property = c("country", "city"), op = c("eq", "neq"),
                   value = c("IE", "Cork"), stringsAsFactors = FALSE)
  out <- normalize_filters(df)
  expect_length(out, 2L)
  expect_equal(out[[2]], list(property = "city", op = "neq", value = "Cork"))
  expect_error(normalize_filters("x"), class = "ontapi_bad_request")
})

test_that("every filter op runs against DuckDB", {
  svc <- local_service()
  n <- function(...) h_count(svc, "Airport", filters = list(list(...)))$count
  expect_equal(n(property = "country", op = "eq", value = "Ireland"), 3)
  expect_equal(n(property = "country", op = "neq", value = "Ireland"), 3)
  expect_equal(n(property = "elevation_ft", op = "gt", value = 242), 2)
  expect_equal(n(property = "elevation_ft", op = "gte", value = 242), 3)
  expect_equal(n(property = "elevation_ft", op = "lt", value = 83), 2)
  expect_equal(n(property = "elevation_ft", op = "lte", value = 83), 3)
  expect_equal(n(property = "name", op = "contains", value = "Airport"), 3)
  expect_equal(n(property = "name", op = "contains", value = "."), 1)
  expect_equal(n(property = "name", op = "starts_with", value = "C"), 2)
  expect_equal(n(property = "country", op = "in", value = list("UK", "France")), 2)
  expect_equal(n(property = "country", op = "not_in", value = list("UK", "France")), 4)
  expect_equal(n(property = "city", op = "is_null"), 0)
  expect_equal(n(property = "city", op = "is_not_null"), 6)
})

test_that("SQL injection in a filter value is harmless", {
  svc <- local_service()
  res <- h_query(svc, "Airport", filters = list(list(
    property = "country", op = "eq", value = "'; DROP TABLE airports; --"
  )))
  expect_equal(res$total_count, 0)
  res2 <- h_count(svc, "Airport", filters = list(list(
    property = "name", op = "contains", value = "%' OR 1=1 --"
  )))
  expect_equal(res2$count, 0)
  expect_equal(h_count(svc, "Airport")$count, 6)
})

test_that("an injected property name is rejected before reaching the database", {
  svc <- local_service()
  expect_error(
    h_query(svc, "Airport", filters = list(list(property = "1; DROP TABLE x", op = "eq", value = 1))),
    class = "ontapi_bad_request"
  )
})
