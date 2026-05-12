#' Build the /v1/objects plumber sub-router
#'
#' @param ctx Server context list created by `ontology_router()`.
#' @return A configured `plumber::Plumber` object.
objects_router <- function(ctx) {
  pr <- plumber::pr()

  # POST /v1/objects/{object_type_id}/query
  plumber::pr_post(pr, "/<object_type_id>/query",
    function(object_type_id, req, res) {
      start       <- proc.time()["elapsed"]
      body        <- req$body %||% list()
      filters     <- body$filters
      search      <- body$search
      limit       <- as.integer(body$limit  %||% 100L)
      offset      <- as.integer(body$offset %||% 0L)
      select_cols <- body$select

      os          <- get_os_or_404(ctx$object_ctx, object_type_id)
      os          <- apply_filters(os, filters)

      if (!is.null(search) && nzchar(search)) {
        os <- objectSetsR::os_search(os, search)
      }

      total_count <- objectSetsR::os_count(os)
      os          <- objectSetsR::os_slice(os, offset = offset, n = limit)

      if (!is.null(select_cols) && length(select_cols) > 0L) {
        os <- objectSetsR::os_select(os, select_cols)
      }

      rows <- df_to_list(objectSetsR::os_collect(os))

      ok_response(
        list(
          rows        = rows,
          total_count = jsonlite::unbox(total_count),
          has_more    = jsonlite::unbox((offset + limit) < total_count)
        ),
        elapsed_ms(start)
      )
    }
  )

  # GET /v1/objects/{object_type_id}/count
  plumber::pr_get(pr, "/<object_type_id>/count",
    function(object_type_id, req, res) {
      start <- proc.time()["elapsed"]
      os    <- get_os_or_404(ctx$object_ctx, object_type_id)
      count <- objectSetsR::os_count(os)
      ok_response(list(count = jsonlite::unbox(count)), elapsed_ms(start))
    }
  )

  # GET /v1/objects/{object_type_id}/{pk_value}
  plumber::pr_get(pr, "/<object_type_id>/<pk_value>",
    function(object_type_id, pk_value, req, res) {
      start <- proc.time()["elapsed"]
      os    <- get_os_or_404(ctx$object_ctx, object_type_id)
      obj   <- objectSetsR::os_get_by_pk(os, pk_value)

      if (is.null(obj) || (is.data.frame(obj) && nrow(obj) == 0L)) {
        http_abort(
          paste0("Object '", pk_value, "' not found in '", object_type_id, "'"),
          status = 404L
        )
      }

      row <- if (is.data.frame(obj)) df_to_list(obj)[[1L]] else as.list(obj)
      ok_response(row, elapsed_ms(start))
    }
  )

  # POST /v1/objects/{object_type_id}/{pk_value}/traverse/{link_type_id}
  plumber::pr_post(pr, "/<object_type_id>/<pk_value>/traverse/<link_type_id>",
    function(object_type_id, pk_value, link_type_id, req, res) {
      start       <- proc.time()["elapsed"]
      body        <- req$body %||% list()
      limit       <- as.integer(body$limit %||% 50L)
      select_cols <- body$select

      linked <- tryCatch(
        objectSetsR::os_traverse(
          ctx$object_ctx, object_type_id, pk_value, link_type_id
        ),
        error = function(e) http_abort(conditionMessage(e), status = 404L)
      )

      if (!is.null(select_cols) && length(select_cols) > 0L) {
        linked <- objectSetsR::os_select(linked, select_cols)
      }

      linked <- objectSetsR::os_slice(linked, offset = 0L, n = limit)
      rows   <- df_to_list(objectSetsR::os_collect(linked))

      ok_response(list(rows = rows), elapsed_ms(start))
    }
  )

  # POST /v1/objects/{object_type_id}/{pk_value}/search_around/{link_type_id}
  plumber::pr_post(pr, "/<object_type_id>/<pk_value>/search_around/<link_type_id>",
    function(object_type_id, pk_value, link_type_id, req, res) {
      start       <- proc.time()["elapsed"]
      body        <- req$body %||% list()
      limit       <- as.integer(body$limit %||% 50L)
      select_cols <- body$select

      linked <- tryCatch(
        objectSetsR::os_search_around(
          ctx$object_ctx, object_type_id, pk_value, link_type_id
        ),
        error = function(e) http_abort(conditionMessage(e), status = 404L)
      )

      if (!is.null(select_cols) && length(select_cols) > 0L) {
        linked <- objectSetsR::os_select(linked, select_cols)
      }

      linked <- objectSetsR::os_slice(linked, offset = 0L, n = limit)
      rows   <- df_to_list(objectSetsR::os_collect(linked))

      ok_response(list(rows = rows), elapsed_ms(start))
    }
  )

  pr
}

# Retrieve an ObjectSet for a type, aborting with 404 if the type is unknown.
get_os_or_404 <- function(object_ctx, object_type_id) {
  tryCatch(
    objectSetsR::os_get(object_ctx, object_type_id),
    error = function(e) {
      http_abort(
        paste0("Unknown object type: '", object_type_id, "'"),
        status = 404L
      )
    }
  )
}
