# Shared handler layer.
#
# Every function here takes the OntologyService first and returns a plain R
# list. None of them knows whether it is answering REST or MCP: the routers in
# R/router_*.R and the tools in R/mcp_tools.R only parse their protocol's
# request, call one of these, and format the result.
#
# Rules:
# * The bundle is read only through ontologySpecR accessors, never by key.
# * Failures the caller can fix raise ontapi_bad_request / ontapi_not_found /
#   ontapi_forbidden (R/errors.R). Anything else propagates and becomes a
#   500 / -32603.
# * Arrays are returned as unnamed lists so they serialise as JSON arrays even
#   with one element; scalars are length-1 vectors (serialised unboxed).

# ---------------------------------------------------------------------------
# Bundle metadata
# ---------------------------------------------------------------------------

#' @rdname handlers
#' @title Shared request handlers
#' @description Plain functions behind every REST route and MCP tool. They
#'   take an [ontology_service()] and return R lists, and raise classed errors
#'   (`ontapi_bad_request`, `ontapi_not_found`, `ontapi_forbidden`) for
#'   problems the caller can fix.
#' @param svc An `OntologyService` from [ontology_service()].
#' @keywords internal
h_summary <- function(svc) {
  authorize(svc, "read")
  b <- svc$bundle
  object_types <- lapply(ontologySpecR::bundle_object_types(b), function(ot) {
    count <- tryCatch(
      as_count(objectSetsR::os_count(objectSetsR::object_set(svc$object_ctx, ot$id))),
      error = function(e) NA_real_
    )
    list(id = ot$id, display_name = display_name(ot), row_count = count)
  })
  link_types <- lapply(ontologySpecR::bundle_link_types(b), function(lt) {
    ends <- ontologySpecR::link_endpoints(lt)
    list(id = lt$id, from = ends$from, to = ends$to)
  })
  action_types <- lapply(ontologySpecR::bundle_action_types(b), function(at) {
    list(id = at$id, display_name = display_name(at))
  })
  list(
    bundle_id      = ontologySpecR::bundle_id(b),
    bundle_version = ontologySpecR::bundle_version(b),
    object_types   = unname(object_types),
    link_types     = unname(link_types),
    action_types   = unname(action_types),
    concept_count  = nrow(conceptR::cpt_list(svc$concept_ctx))
  )
}

#' @rdname handlers
h_health <- function(svc) {
  list(
    status         = "ok",
    bundle_id      = ontologySpecR::bundle_id(svc$bundle),
    bundle_version = ontologySpecR::bundle_version(svc$bundle)
  )
}

#' @rdname handlers
h_bundle <- function(svc) {
  authorize(svc, "read")
  jsonlite::fromJSON(ontologySpecR::bundle_to_json(svc$bundle), simplifyVector = FALSE)
}

#' @rdname handlers
h_object_types <- function(svc) {
  authorize(svc, "read")
  unname(lapply(ontologySpecR::bundle_object_types(svc$bundle), describe_object_type))
}

#' @rdname handlers
h_link_types <- function(svc) {
  authorize(svc, "read")
  unname(lapply(ontologySpecR::bundle_link_types(svc$bundle), function(lt) {
    ends <- ontologySpecR::link_endpoints(lt)
    keys <- tryCatch(ontologySpecR::link_join_keys(lt), error = function(e) NULL)
    list(
      id           = lt$id,
      display_name = display_name(lt),
      description  = description(lt),
      from         = ends$from,
      to           = ends$to,
      cardinality  = lt$cardinality %||% NA_character_,
      directed     = !identical(lt$directed, FALSE),
      from_keys    = as.list(keys$from),
      to_keys      = as.list(keys$to)
    )
  }))
}

#' @rdname handlers
h_action_types <- function(svc) {
  authorize(svc, "read")
  unname(lapply(ontologySpecR::bundle_action_types(svc$bundle), function(at) {
    params <- lapply(at$parameters %||% list(), function(p) {
      list(id = p$id, type = p$type %||% NA_character_, required = isTRUE(p$required))
    })
    list(
      id              = at$id,
      display_name    = display_name(at),
      description     = description(at),
      targets         = as.list(unlist(at$targets, use.names = FALSE)),
      parameters      = unname(params),
      parameter_count = length(params),
      impl_kind       = action_impl_kind(svc, at$id)
    )
  }))
}

# ---------------------------------------------------------------------------
# Objects
# ---------------------------------------------------------------------------

#' @rdname handlers
#' @param object_type_id Object type id.
#' @param filters Filters: a list of `list(property, op, value)` (or a data
#'   frame with those columns, as plumber parses a JSON array of objects).
#' @param select Character vector of property ids to return, or `NULL`.
#' @param limit,offset Paging. Rows are ordered by primary key.
h_query <- function(svc, object_type_id, filters = NULL, select = NULL,
                    limit = NULL, offset = NULL) {
  ot <- get_object_type(svc, object_type_id)
  authorize(svc, "read", ot$id)
  limit  <- parse_count(limit %||% 100L, "limit")
  offset <- parse_count(offset %||% 0L, "offset")
  select <- parse_select(ot, select)

  os <- objectSetsR::object_set(svc$object_ctx, ot$id)
  os <- apply_filters(os, ot, filters)
  total <- as_count(objectSetsR::os_count(os))
  page <- objectSetsR::os_slice(os, offset = offset, n = limit)
  page <- select_props(page, select)

  list(
    rows        = df_rows(objectSetsR::os_collect(page)),
    total_count = total,
    has_more    = (offset + limit) < total,
    limit       = limit,
    offset      = offset
  )
}

#' @rdname handlers
h_count <- function(svc, object_type_id, filters = NULL) {
  ot <- get_object_type(svc, object_type_id)
  authorize(svc, "read", ot$id)
  os <- apply_filters(objectSetsR::object_set(svc$object_ctx, ot$id), ot, filters)
  list(count = as_count(objectSetsR::os_count(os)))
}

#' @rdname handlers
#' @param pk_value Primary key value. A scalar for single-column keys; for
#'   composite keys a named list/vector (by property id), an unnamed vector in
#'   key order, or one string with the parts separated by `"|"`.
h_get <- function(svc, object_type_id, pk_value) {
  ot <- get_object_type(svc, object_type_id)
  authorize(svc, "read", ot$id)
  rows <- objectSetsR::os_collect(get_by_pk(svc, ot, pk_value))
  if (nrow(rows) == 0L) {
    ontapi_not_found(paste0(
      "No ", ot$id, " with primary key ", format_pk(pk_value), "."
    ))
  }
  df_rows(rows[1L, , drop = FALSE])[[1L]]
}

#' @rdname handlers
#' @param link_type_id Link type id.
#' @param direction `"forward"` follows the link from `object_type_id` (its
#'   `from` end); `"reverse"` finds objects whose link points at it.
h_traverse <- function(svc, object_type_id, pk_value, link_type_id,
                       limit = NULL, select = NULL,
                       direction = c("forward", "reverse")) {
  direction <- match.arg(direction)
  ot <- get_object_type(svc, object_type_id)
  lt <- get_link_type(svc, link_type_id)
  ends <- ontologySpecR::link_endpoints(lt)
  authorize(svc, "read", ot$id)
  limit <- parse_count(limit %||% 50L, "limit")

  if (direction == "forward" && !identical(ends$from, ot$id)) {
    ontapi_bad_request(paste0(
      "Link '", lt$id, "' does not originate from '", ot$id,
      "' (it runs ", ends$from, " -> ", ends$to, ")."
    ))
  }
  if (direction == "reverse" && !identical(ends$to, ot$id)) {
    ontapi_bad_request(paste0(
      "Link '", lt$id, "' does not point to '", ot$id,
      "' (it runs ", ends$from, " -> ", ends$to, ")."
    ))
  }
  target_id <- if (direction == "forward") ends$to else ends$from
  target <- get_object_type(svc, target_id)
  select <- parse_select(target, select)

  source <- get_by_pk(svc, ot, pk_value)
  if (as_count(objectSetsR::os_count(source)) == 0) {
    ontapi_not_found(paste0("No ", ot$id, " with primary key ", format_pk(pk_value), "."))
  }

  linked <- if (direction == "forward") {
    objectSetsR::os_traverse(source, lt$id)
  } else {
    objectSetsR::os_search_around(source, lt$id)
  }
  linked <- objectSetsR::os_slice(linked, offset = 0L, n = limit)
  linked <- select_props(linked, select)

  list(
    object_type_id = target$id,
    link_type_id   = lt$id,
    direction      = direction,
    rows           = df_rows(objectSetsR::os_collect(linked))
  )
}

#' @rdname handlers
h_search_around <- function(svc, object_type_id, pk_value, link_type_id,
                            limit = NULL, select = NULL) {
  h_traverse(svc, object_type_id, pk_value, link_type_id,
             limit = limit, select = select, direction = "reverse")
}

# ---------------------------------------------------------------------------
# Concepts
# ---------------------------------------------------------------------------

#' @rdname handlers
h_concepts <- function(svc) {
  authorize(svc, "read")
  df <- conceptR::cpt_list(svc$concept_ctx)
  lapply(df_rows(df), function(r) {
    list(
      id             = r$concept_id,
      object_type_id = r$object_type_id,
      description    = r$description,
      version_count  = as_count(r$version_count)
    )
  })
}

#' @rdname handlers
#' @param concept_id Concept id.
h_concept <- function(svc, concept_id) {
  def <- get_concept_def(svc, concept_id)
  authorize(svc, "read", concept_id)
  versions <- conceptR::cpt_versions_by_stage(svc$concept_ctx, concept_id = concept_id)
  list(
    id             = def$concept_id,
    object_type_id = def$object_type_id,
    description    = def$description,
    versions       = df_rows(versions)
  )
}

#' @rdname handlers
#' @param scope Concept scope (required).
#' @param version Explicit version, or `NULL` to resolve one.
#' @param stage Optional stage (`"development"`, `"staging"`, `"production"`)
#'   to resolve the version from. Without `version` or `stage` the active
#'   version is chosen as `conceptR` does: production, then staging, then
#'   development.
h_evaluate_concept <- function(svc, concept_id, scope, version = NULL,
                               stage = NULL, limit = NULL) {
  def <- get_concept_def(svc, concept_id)
  authorize(svc, "evaluate_concept", concept_id)
  if (is.null(scope) || !is.character(scope) || length(scope) != 1L || !nzchar(scope)) {
    ontapi_bad_request("`scope` is required to evaluate a concept.")
  }
  stages <- c("development", "staging", "production")
  if (!is.null(stage) && !(stage %in% stages)) {
    ontapi_bad_request(paste0(
      "`stage` must be one of ", paste(stages, collapse = ", "), "."
    ))
  }
  if (!is.null(limit)) limit <- parse_count(limit, "limit")

  ver <- resolve_concept_version(svc, concept_id, scope, version, stage)
  result <- conceptR::cpt_evaluate(svc$concept_ctx, concept_id, scope,
                                   version = ver$version)
  flags <- result[[concept_id]]
  true_count <- sum(flags %in% TRUE)
  rows <- if (is.null(limit)) result else utils::head(result, limit)

  list(
    concept_id  = concept_id,
    scope       = scope,
    version     = as.integer(ver$version),
    stage       = ver$stage,
    rows        = df_rows(rows),
    total_count = nrow(result),
    true_count  = true_count,
    false_count = nrow(result) - true_count
  )
}

# ---------------------------------------------------------------------------
# Actions
# ---------------------------------------------------------------------------

#' @rdname handlers
#' @param action_type_id Action type id.
#' @param params Named list of action parameters.
#' @param target_ids Target object ids.
#' @param dry_run If `TRUE`, validate and return the plan without running it.
h_submit_action <- function(svc, action_type_id, params = NULL,
                            target_ids = NULL, dry_run = FALSE) {
  at <- get_action_type(svc, action_type_id)
  authorize(svc, "submit_action", at$id)
  params <- params %||% list()
  if (!is.list(params) || (length(params) > 0L && is.null(names(params)))) {
    ontapi_bad_request("`params` must be an object of named parameters.")
  }
  target_ids <- if (length(target_ids) == 0L) NULL else as.character(unlist(target_ids))

  tryCatch(
    actionTypesR::validate_params(svc$action_ctx, at$id, params),
    error = function(e) ontapi_bad_request(conditionMessage(e))
  )

  result <- tryCatch(
    actionTypesR::submit_action(svc$action_ctx, at$id, params,
                                target_ids = target_ids,
                                .dry_run = isTRUE(dry_run)),
    ontapi_error = function(e) rlang::cnd_signal(e),
    error = function(e) {
      if (grepl("denied", conditionMessage(e), fixed = TRUE)) {
        ontapi_forbidden(conditionMessage(e))
      }
      rlang::cnd_signal(e)
    }
  )

  if (isTRUE(dry_run)) {
    return(list(
      dry_run = TRUE,
      plan = list(
        action_type_id  = result$action_type_id,
        impl_kind       = result$impl_kind %||% NA_character_,
        impl_entrypoint = result$impl_entrypoint %||% NA_character_,
        params          = json_object(params),
        target_ids      = as.list(result$target_ids)
      )
    ))
  }

  list(
    dry_run        = FALSE,
    submission_id  = result$submission_id,
    action_type_id = result$action_type_id,
    status         = result$status,
    timestamp      = format_time(result$timestamp)
  )
}

#' @rdname handlers
#' @param n Maximum number of log entries.
h_action_log <- function(svc, n = NULL, action_type_id = NULL) {
  authorize(svc, "read", action_type_id)
  n <- parse_count(n %||% 100L, "n")
  if (is.null(action_type_id)) {
    log <- actionTypesR::action_log(svc$action_ctx, n = n)
  } else {
    log <- actionTypesR::action_log(svc$action_ctx, n = .Machine$integer.max)
    log <- utils::head(log[log$action_type_id == action_type_id, , drop = FALSE], n)
  }
  df_rows(log)
}

# ---------------------------------------------------------------------------
# Lookups (accessor-based, raising ontapi_not_found)
# ---------------------------------------------------------------------------

get_object_type <- function(svc, id) {
  id <- parse_id(id, "object_type_id")
  types <- ontologySpecR::bundle_object_types(svc$bundle)
  if (is.null(types[[id]])) {
    ontapi_not_found(unknown_msg("object type", id, names(types)))
  }
  ontologySpecR::bundle_object_type(svc$bundle, id)
}

get_link_type <- function(svc, id) {
  id <- parse_id(id, "link_type_id")
  types <- ontologySpecR::bundle_link_types(svc$bundle)
  if (is.null(types[[id]])) {
    ontapi_not_found(unknown_msg("link type", id, names(types)))
  }
  ontologySpecR::bundle_link_type(svc$bundle, id)
}

get_action_type <- function(svc, id) {
  id <- parse_id(id, "action_type_id")
  types <- ontologySpecR::bundle_action_types(svc$bundle)
  if (is.null(types[[id]])) {
    ontapi_not_found(unknown_msg("action type", id, names(types)))
  }
  ontologySpecR::bundle_action_type(svc$bundle, id)
}

# Implementation kind of an action type ("r", "sql", "http", "plugin").
action_impl_kind <- function(svc, action_type_id) {
  ontologySpecR::bundle_action_type(svc$bundle, action_type_id)$implementation$kind %||%
    NA_character_
}

get_concept_def <- function(svc, concept_id) {
  concept_id <- parse_id(concept_id, "concept_id")
  defs <- conceptR::cpt_list(svc$concept_ctx)
  row <- defs[defs$concept_id == concept_id, , drop = FALSE]
  if (nrow(row) == 0L) {
    ontapi_not_found(unknown_msg("concept", concept_id, defs$concept_id))
  }
  df_rows(row)[[1L]]
}

# Pick the version cpt_evaluate() should run, so the response can report it.
resolve_concept_version <- function(svc, concept_id, scope, version, stage) {
  versions <- conceptR::cpt_versions_by_stage(svc$concept_ctx,
                                              concept_id = concept_id,
                                              scope = scope)
  if (nrow(versions) == 0L) {
    ontapi_not_found(paste0("Concept '", concept_id, "' has no versions in scope '", scope, "'."))
  }
  if (!is.null(version)) {
    v <- suppressWarnings(as.integer(version))
    if (length(v) != 1L || is.na(v)) ontapi_bad_request("`version` must be an integer.")
    row <- versions[versions$version == v, , drop = FALSE]
    if (nrow(row) == 0L) {
      ontapi_not_found(paste0("Concept '", concept_id, "' / '", scope, "' has no version ", v, "."))
    }
    if (identical(row$status[[1L]], "deprecated")) {
      ontapi_bad_request(paste0("Version ", v, " of '", concept_id, "' / '", scope, "' is deprecated."))
    }
    return(list(version = v, stage = row$stage[[1L]]))
  }
  active <- versions[versions$status == "active", , drop = FALSE]
  candidates <- if (is.null(stage)) c("production", "staging", "development") else stage
  for (s in candidates) {
    rows <- active[active$stage == s, , drop = FALSE]
    if (nrow(rows) > 0L) {
      return(list(version = max(rows$version), stage = s))
    }
  }
  ontapi_not_found(paste0(
    "Concept '", concept_id, "' / '", scope, "' has no active version",
    if (!is.null(stage)) paste0(" in stage '", stage, "'") else "", "."
  ))
}

# ---------------------------------------------------------------------------
# Primary keys
# ---------------------------------------------------------------------------

# Lazy ObjectSet holding the object(s) whose primary key equals `pk_value`.
# Handles composite keys; values are coerced to the key properties' types.
get_by_pk <- function(svc, ot, pk_value) {
  pk <- ontologySpecR::object_pk(ot)
  values <- split_pk_value(ot, pk, pk_value)
  os <- objectSetsR::object_set(svc$object_ctx, ot$id)
  for (col in pk) {
    value <- coerce_to_property(ot, col, values[[col]])
    os <- rlang::inject(objectSetsR::os_filter(os, !!rlang::sym(col) == !!value))
  }
  os
}

split_pk_value <- function(ot, pk, pk_value) {
  if (is.null(pk_value) || length(pk_value) == 0L) {
    ontapi_bad_request("`pk_value` is required.")
  }
  if (is.list(pk_value)) pk_value <- unlist(pk_value)
  nm <- names(pk_value)
  if (length(pk) == 1L && length(pk_value) == 1L) {
    return(stats_set_names(list(pk_value[[1L]]), pk))
  }
  if (!is.null(nm) && all(nzchar(nm))) {
    missing <- setdiff(pk, nm)
    if (length(missing) > 0L) {
      ontapi_bad_request(paste0(
        "Primary key of '", ot$id, "' needs: ", paste(pk, collapse = ", "),
        ". Missing: ", paste(missing, collapse = ", "), "."
      ))
    }
    return(as.list(pk_value[pk]))
  }
  if (length(pk_value) == 1L && length(pk) > 1L) {
    pk_value <- strsplit(as.character(pk_value), "|", fixed = TRUE)[[1L]]
  }
  if (length(pk_value) != length(pk)) {
    ontapi_bad_request(paste0(
      "Primary key of '", ot$id, "' has ", length(pk), " part(s) (",
      paste(pk, collapse = ", "), ") but ", length(pk_value), " were given."
    ))
  }
  stats_set_names(as.list(unname(pk_value)), pk)
}

coerce_to_property <- function(ot, prop_id, value) {
  type <- ontologySpecR::object_property(ot, prop_id)$type %||% "string"
  out <- switch(type,
    integer = suppressWarnings(as.integer(value)),
    number  = ,
    numeric = ,
    double  = suppressWarnings(as.numeric(value)),
    boolean = ,
    logical = as.logical(value),
    as.character(value)
  )
  if (length(out) != 1L || is.na(out)) {
    ontapi_bad_request(paste0(
      "Value '", format(value), "' is not a valid ", type, " for '", prop_id, "'."
    ))
  }
  out
}

format_pk <- function(pk_value) {
  v <- unlist(pk_value)
  if (!is.null(names(v)) && all(nzchar(names(v)))) {
    paste0(names(v), "=", v, collapse = ", ")
  } else {
    paste0("'", paste(v, collapse = "|"), "'")
  }
}

# ---------------------------------------------------------------------------
# Filters (shared by REST and MCP)
#
# Security-critical: filter values reach the database only as values injected
# into objectSetsR::os_filter() quosures, never by string interpolation.
# Property names are checked against the object type before use.
# ---------------------------------------------------------------------------

VALID_OPS <- c(
  "eq", "neq", "gt", "gte", "lt", "lte",
  "contains", "starts_with", "in", "not_in",
  "is_null", "is_not_null"
)

# Escape regex metacharacters so `contains` / `starts_with` match literally.
# (dbplyr cannot translate grepl(fixed = TRUE) or startsWith() for every
# backend, but plain grepl() maps to each backend's regex match.)
escape_regex <- function(x) {
  gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", as.character(x))
}

#' Build the dbplyr expression for one filter
#' @param col An rlang symbol.
#' @param op One of `VALID_OPS`.
#' @param value Comparison value.
#' @noRd
build_filter_expr <- function(col, op, value) {
  switch(op,
    eq          = rlang::expr(!!col == !!value),
    neq         = rlang::expr(!!col != !!value),
    gt          = rlang::expr(!!col > !!value),
    gte         = rlang::expr(!!col >= !!value),
    lt          = rlang::expr(!!col < !!value),
    lte         = rlang::expr(!!col <= !!value),
    contains    = rlang::expr(grepl(!!escape_regex(value), !!col)),
    starts_with = rlang::expr(grepl(!!paste0("^", escape_regex(value)), !!col)),
    `in`        = rlang::expr(!!col %in% !!value),
    not_in      = rlang::expr(!(!!col %in% !!value)),
    is_null     = rlang::expr(is.na(!!col)),
    is_not_null = rlang::expr(!is.na(!!col))
  )
}

# Normalise filters from either protocol to a list of list(property, op, value).
# Accepts NULL, a list of filter objects, a single filter object, or the data
# frame plumber produces from a JSON array of objects.
normalize_filters <- function(filters) {
  if (is.null(filters) || length(filters) == 0L) return(list())
  if (is.data.frame(filters)) {
    return(lapply(seq_len(nrow(filters)), function(i) {
      row <- lapply(filters, function(col) col[[i]])
      row
    }))
  }
  if (!is.list(filters)) {
    ontapi_bad_request("`filters` must be an array of {property, op, value} objects.")
  }
  if (!is.null(filters[["property"]]) || !is.null(filters[["op"]])) {
    return(list(filters))
  }
  filters
}

validate_filter <- function(ot, f) {
  if (!is.list(f)) {
    ontapi_bad_request("Each filter must be an object with `property` and `op`.")
  }
  property <- f[["property"]]
  op <- f[["op"]]
  if (!is.character(property) || length(property) != 1L || !nzchar(property)) {
    ontapi_bad_request("Each filter needs a `property` string.")
  }
  if (!is.character(op) || length(op) != 1L || !(op %in% VALID_OPS)) {
    ontapi_bad_request(paste0(
      "Unknown filter op '", paste(format(op), collapse = ""), "'. Valid ops: ",
      paste(VALID_OPS, collapse = ", ")
    ))
  }
  props <- ontologySpecR::object_property_ids(ot)
  if (!(property %in% props)) {
    ontapi_bad_request(paste0(
      "Unknown property '", property, "' on '", ot$id, "'. Valid properties: ",
      paste(props, collapse = ", ")
    ))
  }
  value <- f[["value"]]
  if (is.list(value)) value <- unlist(value, use.names = FALSE)
  if (op %in% c("is_null", "is_not_null")) {
    value <- NULL
  } else if (op %in% c("in", "not_in")) {
    if (length(value) == 0L) {
      ontapi_bad_request(paste0("Filter op '", op, "' needs a non-empty array `value`."))
    }
  } else if (length(value) != 1L || is.na(value)) {
    ontapi_bad_request(paste0("Filter op '", op, "' needs a single, non-null `value`."))
  }
  list(property = property, op = op, value = value)
}

# Apply filters to an ObjectSet of object type `ot`.
apply_filters <- function(os, ot, filters) {
  for (f in normalize_filters(filters)) {
    f <- validate_filter(ot, f)
    expr <- build_filter_expr(rlang::sym(f$property), f$op, f$value)
    os <- rlang::inject(objectSetsR::os_filter(os, !!expr))
  }
  os
}

# ---------------------------------------------------------------------------
# Small parsing / formatting helpers
# ---------------------------------------------------------------------------

parse_id <- function(x, arg) {
  if (is.list(x)) x <- unlist(x)
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    ontapi_bad_request(paste0("`", arg, "` must be a non-empty string."))
  }
  x
}

parse_count <- function(x, arg) {
  if (is.list(x)) x <- unlist(x)
  v <- suppressWarnings(as.numeric(x))
  if (length(v) != 1L || is.na(v) || v < 0 || v != trunc(v) || v > .Machine$integer.max) {
    ontapi_bad_request(paste0("`", arg, "` must be a non-negative integer."))
  }
  as.integer(v)
}

parse_select <- function(ot, select) {
  if (is.null(select) || length(select) == 0L) return(NULL)
  select <- as.character(unlist(select, use.names = FALSE))
  props <- ontologySpecR::object_property_ids(ot)
  unknown <- setdiff(select, props)
  if (length(unknown) > 0L) {
    ontapi_bad_request(paste0(
      "Unknown propert", if (length(unknown) == 1L) "y" else "ies", " in `select` for '",
      ot$id, "': ", paste(unknown, collapse = ", "), "."
    ))
  }
  unique(select)
}

select_props <- function(os, select) {
  if (is.null(select)) return(os)
  rlang::inject(objectSetsR::os_select(os, !!!rlang::syms(select)))
}

# Counts come back as doubles from some backends (BIGINT); keep them numeric
# but whole.
as_count <- function(x) {
  x <- as.numeric(x)
  if (length(x) == 0L) return(0)
  x[[1L]]
}

# Data frame -> list of row lists (unnamed list, so it is always a JSON array).
df_rows <- function(df) {
  if (!is.data.frame(df) || nrow(df) == 0L) return(list())
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  for (nm in names(df)) {
    col <- df[[nm]]
    if (inherits(col, "POSIXt")) df[[nm]] <- format_time(col)
    else if (inherits(col, "Date")) df[[nm]] <- format(col)
    else if (is.factor(col)) df[[nm]] <- as.character(col)
  }
  lapply(seq_len(nrow(df)), function(i) {
    lapply(df, function(col) col[[i]])
  })
}

format_time <- function(x) {
  if (is.null(x) || length(x) == 0L) return(NA_character_)
  if (inherits(x, "POSIXt")) {
    out <- format(x, "%Y-%m-%dT%H:%M:%OS3%z")
    out[is.na(x)] <- NA_character_
    return(out)
  }
  as.character(x)
}

display_name <- function(x) {
  x$display$name %||% x$display_name %||% x$id
}

description <- function(x) {
  x$display$description %||% x$description %||% NA_character_
}

describe_object_type <- function(ot) {
  props <- lapply(ontologySpecR::object_properties(ot), function(p) {
    list(
      id       = p$id,
      type     = p$type %||% NA_character_,
      nullable = !identical(p$nullable, FALSE)
    )
  })
  src <- ontologySpecR::object_source(ot)
  list(
    id             = ot$id,
    display_name   = display_name(ot),
    description    = description(ot),
    source_kind    = src$kind %||% "table",
    source_table   = if (identical(src$kind, "query")) NA_character_ else ontologySpecR::object_table(ot),
    primary_key    = as.list(ontologySpecR::object_pk(ot)),
    property_count = length(props),
    properties     = unname(props)
  )
}

unknown_msg <- function(what, id, valid) {
  valid <- valid[nzchar(valid)]
  if (length(valid) == 0L) {
    return(paste0("Unknown ", what, ": '", id, "'. There are none defined."))
  }
  paste0("Unknown ", what, ": '", id, "'. Valid ids: ", paste(valid, collapse = ", "), ".")
}

stats_set_names <- function(x, nm) {
  names(x) <- nm
  x
}
