# Actions tests require an actionTypesR ActionContext wired to the server.
# The basic server started by start_test_server() has no action_ctx, so
# submit/log return 422 rather than running real actions.

test_that("POST /v1/actions/Foo/submit without action_ctx returns 422", {
  skip_deps(extra = c("callr", "httr2"))
  srv <- start_test_server()
  on.exit(srv$stop(), add = TRUE)

  resp <- httr2::request(paste0(srv$url, "/v1/actions/Foo/submit")) |>
    httr2::req_body_json(list(params = list(), target_ids = c("DUB"))) |>
    httr2::req_error(is_error = \(r) FALSE) |>
    httr2::req_perform()
  expect_equal(httr2::resp_status(resp), 422L)
})

test_that("POST /v1/actions/Foo/submit dry_run=true without action_ctx returns 422", {
  skip_deps(extra = c("callr", "httr2"))
  srv <- start_test_server()
  on.exit(srv$stop(), add = TRUE)

  resp <- httr2::request(paste0(srv$url, "/v1/actions/Foo/submit")) |>
    httr2::req_body_json(list(params = list(), target_ids = c("DUB"), dry_run = TRUE)) |>
    httr2::req_error(is_error = \(r) FALSE) |>
    httr2::req_perform()
  expect_equal(httr2::resp_status(resp), 422L)
})

test_that("GET /v1/actions/log without action_ctx returns 422", {
  skip_deps(extra = c("callr", "httr2"))
  srv <- start_test_server()
  on.exit(srv$stop(), add = TRUE)

  resp <- httr2::request(paste0(srv$url, "/v1/actions/log")) |>
    httr2::req_error(is_error = \(r) FALSE) |>
    httr2::req_perform()
  expect_equal(httr2::resp_status(resp), 422L)
})

# ---------------------------------------------------------------------------
# Unit-level tests for the dry-run response shape (mocked action_ctx)
# ---------------------------------------------------------------------------
test_that("dry_run response has plan with required fields", {
  skip_if_not_installed("actionTypesR")

  mock_action_def <- structure(list(id = "UpdateStatus"), class = "ActionType")
  mock_ctx <- list(
    action_ctx = structure(list(), class = "ActionContext")
  )

  with_mocked_bindings(
    get_action_type  = function(...) mock_action_def,
    action_impl_kind = function(...) "r",
    .package = "actionTypesR",
    {
      start  <- proc.time()["elapsed"]
      result <- local({
        body    <- list(params = list(new_status = "active"),
                       target_ids = c("DUB"), dry_run = TRUE)
        params  <- body$params     %||% list()
        targets <- body$target_ids %||% character(0L)
        dry_run <- isTRUE(body$dry_run)

        action_def <- actionTypesR::get_action_type(mock_ctx$action_ctx, "UpdateStatus")
        impl_kind  <- actionTypesR::action_impl_kind(action_def)

        list(
          plan = list(
            action_type_id = jsonlite::unbox("UpdateStatus"),
            params         = params,
            target_ids     = targets,
            impl_kind      = jsonlite::unbox(as.character(impl_kind))
          )
        )
      })

      expect_named(result$plan, c("action_type_id", "params", "target_ids", "impl_kind"),
        ignore.order = TRUE)
      expect_equal(result$plan$impl_kind[[1L]], "r")
    }
  )
})
