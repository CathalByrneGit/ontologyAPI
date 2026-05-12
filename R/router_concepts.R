#' Build the /v1/concepts plumber sub-router
#'
#' Only mounted when `concept_ctx` is provided to `ontology_router()`.
#'
#' @param ctx Server context list created by `ontology_router()`.
#' @return A configured `plumber::Plumber` object.
concepts_router <- function(ctx) {
  pr <- plumber::pr()

  # GET /v1/concepts
  plumber::pr_get(pr, "/", function(req, res) {
    start    <- proc.time()["elapsed"]
    concepts <- conceptR::list_concepts(ctx$concept_ctx)
    rows <- lapply(concepts, function(c) {
      list(
        id           = jsonlite::unbox(c$id),
        display_name = jsonlite::unbox(c$display_name %||% c$id),
        version      = jsonlite::unbox(as.integer(c$version %||% 1L))
      )
    })
    ok_response(rows, elapsed_ms(start))
  })

  # GET /v1/concepts/{concept_id}
  plumber::pr_get(pr, "/<concept_id>", function(concept_id, req, res) {
    start   <- proc.time()["elapsed"]
    concept <- tryCatch(
      conceptR::get_concept(ctx$concept_ctx, concept_id),
      error = function(e) {
        http_abort(paste0("Concept '", concept_id, "' not found"), status = 404L)
      }
    )
    ok_response(concept, elapsed_ms(start))
  })

  # GET /v1/concepts/{concept_id}/evaluate
  plumber::pr_get(pr, "/<concept_id>/evaluate",
    function(concept_id, req, res, scope = NULL, stage = NULL) {
      start  <- proc.time()["elapsed"]
      result <- tryCatch(
        conceptR::evaluate_concept(
          ctx$concept_ctx,
          concept_id = concept_id,
          scope      = scope,
          stage      = stage
        ),
        error = function(e) http_abort(conditionMessage(e), status = 422L)
      )

      rows        <- df_to_list(result$rows)
      true_count  <- sum(vapply(rows, function(r) isTRUE(r[[concept_id]]), logical(1L)))
      false_count <- length(rows) - true_count

      ok_response(
        list(
          concept_id  = jsonlite::unbox(concept_id),
          scope       = jsonlite::unbox(scope  %||% "default"),
          version     = jsonlite::unbox(as.integer(result$version %||% 1L)),
          stage       = jsonlite::unbox(stage  %||% "production"),
          rows        = rows,
          true_count  = jsonlite::unbox(true_count),
          false_count = jsonlite::unbox(false_count)
        ),
        elapsed_ms(start)
      )
    }
  )

  pr
}
