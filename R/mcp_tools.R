# Built-in MCP tools. Each one unpacks its JSON arguments and calls one shared
# handler (R/handlers.R); the result is sent back as JSON text by call_tool().

tool_get_ontology_summary <- function(args, server) {
  h_summary(server$svc)
}

tool_list_object_types <- function(args, server) {
  h_object_types(server$svc)
}

tool_list_link_types <- function(args, server) {
  h_link_types(server$svc)
}

tool_query_objects <- function(args, server) {
  h_query(server$svc,
    object_type_id = require_arg(args, "object_type_id"),
    filters = args$filters,
    select  = args$select,
    limit   = args$limit,
    offset  = args$offset
  )
}

tool_count_objects <- function(args, server) {
  h_count(server$svc,
    object_type_id = require_arg(args, "object_type_id"),
    filters = args$filters
  )
}

tool_get_object <- function(args, server) {
  h_get(server$svc,
    object_type_id = require_arg(args, "object_type_id"),
    pk_value = require_arg(args, "pk_value")
  )
}

tool_traverse_links <- function(args, server) {
  direction <- args$direction %||% "forward"
  if (!(direction %in% c("forward", "reverse"))) {
    ontapi_bad_request("`direction` must be 'forward' or 'reverse'.")
  }
  h_traverse(server$svc,
    object_type_id = require_arg(args, "object_type_id"),
    pk_value     = require_arg(args, "pk_value"),
    link_type_id = require_arg(args, "link_type_id"),
    limit        = args$limit,
    select       = args$select,
    direction    = direction
  )
}

tool_list_concepts <- function(args, server) {
  h_concepts(server$svc)
}

tool_evaluate_concept <- function(args, server) {
  h_evaluate_concept(server$svc,
    concept_id = require_arg(args, "concept_id"),
    scope   = require_arg(args, "scope"),
    version = args$version,
    stage   = args$stage,
    limit   = args$limit %||% 100L
  )
}

tool_submit_action <- function(args, server) {
  # MCP defaults to a dry run so agents preview before changing data.
  dry_run <- if (is.null(args$dry_run)) TRUE else isTRUE(args$dry_run)
  h_submit_action(server$svc,
    action_type_id = require_arg(args, "action_type_id"),
    params     = require_arg(args, "params"),
    target_ids = args$target_ids,
    dry_run    = dry_run
  )
}

require_arg <- function(args, name) {
  value <- args[[name]]
  if (is.null(value)) ontapi_bad_request(paste0("`", name, "` is required."))
  value
}
