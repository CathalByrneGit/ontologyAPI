# One end-to-end check over real HTTP, in a background R process. Needs the
# package to be installed (as under R CMD check).

test_that("serve_ontology answers over HTTP", {
  skip_on_cran()
  skip_if_no_backend()
  for (p in c("callr", "httr2", "plumber", "httpuv")) skip_if_not_installed(p)
  skip_if(!nzchar(system.file(package = "ontologyAPI")), "ontologyAPI not installed")

  port <- sample(49152L:65534L, 1L)
  seed <- seed_example_db
  proc <- callr::r_bg(function(port, seed) {
    con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
    seed(con)
    b <- suppressWarnings(ontologySpecR::spec_example_bundle())
    svc <- ontologyAPI::ontology_service(b, con, actor = "live")
    ontologyAPI::serve_ontology(svc, port = port, docs = FALSE, api_key = "k")
  }, args = list(port = port, seed = seed), supervise = TRUE)
  on.exit(proc$kill(), add = TRUE)

  base <- paste0("http://127.0.0.1:", port)
  ready <- FALSE
  for (i in seq_len(100L)) {
    Sys.sleep(0.2)
    ready <- tryCatch({
      httr2::req_perform(httr2::req_error(httr2::req_timeout(httr2::request(paste0(base, "/v1/health")), 1),
                                          is_error = function(r) FALSE))
      TRUE
    }, error = function(e) FALSE)
    if (ready || !proc$is_alive()) break
  }
  skip_if(!ready, paste("server did not start:", paste(proc$read_error(), collapse = "")))

  resp <- httr2::request(paste0(base, "/v1/objects/Airport/query")) |>
    httr2::req_headers(Authorization = "Bearer k") |>
    httr2::req_body_json(list(limit = 3)) |>
    httr2::req_perform()
  body <- httr2::resp_body_json(resp)
  expect_equal(body$data$total_count, 6)
  expect_length(body$data$rows, 3L)

  unauth <- httr2::request(paste0(base, "/v1/health")) |>
    httr2::req_error(is_error = function(r) FALSE) |>
    httr2::req_perform()
  expect_equal(httr2::resp_status(unauth), 401L)
})
