#' Build the /v1/objects plumber sub-router
#'
#' Routes only parse the request and call a shared handler (R/handlers.R).
#'
#' @param svc An `OntologyService`.
#' @return A `plumber::Plumber` object.
#' @noRd
objects_router <- function(svc) {
  pr <- rest_sub_router()

  # POST /v1/objects/{object_type_id}/query
  pr <- plumber::pr_post(pr, "/<object_type_id>/query",
    function(object_type_id, req, res) {
      body <- request_body(req)
      rest_ok(h_query(svc, object_type_id,
        filters = body$filters,
        select  = body$select,
        limit   = body$limit,
        offset  = body$offset
      ))
    }
  )

  # GET /v1/objects/{object_type_id}/count
  pr <- plumber::pr_get(pr, "/<object_type_id>/count",
    function(object_type_id, req, res) {
      rest_ok(h_count(svc, object_type_id))
    }
  )

  # POST /v1/objects/{object_type_id}/count (with filters)
  pr <- plumber::pr_post(pr, "/<object_type_id>/count",
    function(object_type_id, req, res) {
      rest_ok(h_count(svc, object_type_id, filters = request_body(req)$filters))
    }
  )

  # GET /v1/objects/{object_type_id}/{pk_value}
  pr <- plumber::pr_get(pr, "/<object_type_id>/<pk_value>",
    function(object_type_id, pk_value, req, res) {
      rest_ok(h_get(svc, object_type_id, pk_value))
    }
  )

  # POST /v1/objects/{object_type_id}/{pk_value}/traverse/{link_type_id}
  pr <- plumber::pr_post(pr, "/<object_type_id>/<pk_value>/traverse/<link_type_id>",
    function(object_type_id, pk_value, link_type_id, req, res) {
      body <- request_body(req)
      rest_ok(h_traverse(svc, object_type_id, pk_value, link_type_id,
        limit = body$limit, select = body$select
      ))
    }
  )

  # POST /v1/objects/{object_type_id}/{pk_value}/search_around/{link_type_id}
  pr <- plumber::pr_post(pr, "/<object_type_id>/<pk_value>/search_around/<link_type_id>",
    function(object_type_id, pk_value, link_type_id, req, res) {
      body <- request_body(req)
      rest_ok(h_search_around(svc, object_type_id, pk_value, link_type_id,
        limit = body$limit, select = body$select
      ))
    }
  )

  pr
}

# The parsed JSON body as a list (empty list when there is none).
request_body <- function(req) {
  body <- req$body
  if (is.null(body) || !is.list(body)) list() else body
}

# Query-string parameters as a list.
request_query <- function(req) {
  q <- req$argsQuery
  if (is.null(q)) list() else q
}
