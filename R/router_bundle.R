#' Build the /v1/bundle plumber sub-router
#'
#' @param ctx Server context list created by `ontology_router()`.
#' @return A configured `plumber::Plumber` object.
bundle_router <- function(ctx) {
  pr <- plumber::pr()

  plumber::pr_get(pr, "/", function(req, res) {
    start    <- proc.time()["elapsed"]
    json_str <- ontologySpecR::bundle_to_json(ctx$bundle)
    data     <- jsonlite::fromJSON(json_str, simplifyVector = FALSE)
    ok_response(data, elapsed_ms(start))
  })

  plumber::pr_get(pr, "/objects", function(req, res) {
    start <- proc.time()["elapsed"]
    types <- ontologySpecR::bundle_object_types(ctx$bundle)
    rows  <- lapply(types, function(ot) {
      list(
        id             = jsonlite::unbox(ot$id),
        display_name   = jsonlite::unbox(ot$display_name),
        property_count = jsonlite::unbox(length(ot$properties)),
        source_table   = jsonlite::unbox(ot$source_table)
      )
    })
    ok_response(rows, elapsed_ms(start))
  })

  plumber::pr_get(pr, "/links", function(req, res) {
    start <- proc.time()["elapsed"]
    types <- ontologySpecR::bundle_link_types(ctx$bundle)
    rows  <- lapply(types, function(lt) {
      list(
        id          = jsonlite::unbox(lt$id),
        from        = jsonlite::unbox(lt$from),
        to          = jsonlite::unbox(lt$to),
        cardinality = jsonlite::unbox(lt$cardinality),
        directed    = jsonlite::unbox(isTRUE(lt$directed))
      )
    })
    ok_response(rows, elapsed_ms(start))
  })

  plumber::pr_get(pr, "/actions", function(req, res) {
    start <- proc.time()["elapsed"]
    types <- ontologySpecR::bundle_action_types(ctx$bundle)
    rows  <- lapply(types, function(at) {
      list(
        id              = jsonlite::unbox(at$id),
        display_name    = jsonlite::unbox(at$display_name),
        parameter_count = jsonlite::unbox(length(at$parameters)),
        targets         = at$targets
      )
    })
    ok_response(rows, elapsed_ms(start))
  })

  pr
}
