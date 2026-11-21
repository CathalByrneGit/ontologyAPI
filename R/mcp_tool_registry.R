# Input schemas and descriptions for the built-in MCP tools. Each tool's
# implementation (R/mcp_tools.R) only unpacks its arguments and calls a shared
# handler from R/handlers.R.

no_args_schema <- function() {
  list(type = "object", properties = json_object(list()), required = list())
}

filters_schema <- function() {
  list(
    type = "array",
    description = paste0(
      "Optional property filters, ANDed together. Same syntax as the REST API."
    ),
    items = list(
      type = "object",
      properties = list(
        property = list(type = "string", description = "Property id"),
        op = list(type = "string", `enum` = as.list(VALID_OPS)),
        value = list(description = "Comparison value; an array for 'in' / 'not_in'; omit for 'is_null' / 'is_not_null'")
      ),
      required = list("property", "op")
    )
  )
}

select_schema <- function() {
  list(
    type = "array",
    items = list(type = "string"),
    description = "Property ids to include. Omit for all properties."
  )
}

pk_schema <- function() {
  list(
    type = c("string", "number", "object"),
    description = paste0(
      "Primary key value. For composite keys pass an object keyed by property id, ",
      "or one string with the parts separated by '|'."
    )
  )
}

builtin_tools <- function() {
  list(
    list(
      name = "get_ontology_summary",
      description = "Get a high-level summary of the ontology: object type names, row counts, link and action types, and the number of concepts. Use this as your first call to orient yourself.",
      input_schema = no_args_schema(),
      handler = tool_get_ontology_summary
    ),
    list(
      name = "list_object_types",
      description = "List all object types in the ontology with their properties, primary keys and source tables. Use this to understand what data is available before querying.",
      input_schema = no_args_schema(),
      handler = tool_list_object_types
    ),
    list(
      name = "list_link_types",
      description = "List all relationship types between object types. Shows how objects are connected, including cardinality and direction.",
      input_schema = no_args_schema(),
      handler = tool_list_link_types
    ),
    list(
      name = "query_objects",
      description = "Query objects of a specific type with optional filters. Returns {rows, total_count, has_more, limit, offset}; rows are ordered by primary key, so offset paging is stable. Example: Airport objects where country equals 'Ireland'.",
      input_schema = list(
        type = "object",
        properties = list(
          object_type_id = list(type = "string", description = "The object type to query, e.g. 'Airport'"),
          filters = filters_schema(),
          select  = select_schema(),
          limit   = list(type = "integer", default = 100L, minimum = 0L),
          offset  = list(type = "integer", default = 0L, minimum = 0L)
        ),
        required = list("object_type_id")
      ),
      handler = tool_query_objects
    ),
    list(
      name = "count_objects",
      description = "Count objects matching optional filters. Faster than query_objects when you only need the count.",
      input_schema = list(
        type = "object",
        properties = list(
          object_type_id = list(type = "string"),
          filters = filters_schema()
        ),
        required = list("object_type_id")
      ),
      handler = tool_count_objects
    ),
    list(
      name = "get_object",
      description = "Fetch a single object by its primary key value.",
      input_schema = list(
        type = "object",
        properties = list(
          object_type_id = list(type = "string"),
          pk_value = pk_schema()
        ),
        required = list("object_type_id", "pk_value")
      ),
      handler = tool_get_object
    ),
    list(
      name = "traverse_links",
      description = "Follow a link from one object to its related objects. forward follows the link from its 'from' type; reverse finds objects of the 'from' type that link to this object. Example: from FlightRoute 'R001' forward over RouteOrigin, or from Airport 'DUB' reverse over RouteOrigin to find routes departing Dublin.",
      input_schema = list(
        type = "object",
        properties = list(
          object_type_id = list(type = "string"),
          pk_value = pk_schema(),
          link_type_id = list(type = "string"),
          direction = list(
            type = "string",
            `enum` = list("forward", "reverse"),
            default = "forward"
          ),
          limit  = list(type = "integer", default = 50L, minimum = 0L),
          select = select_schema()
        ),
        required = list("object_type_id", "pk_value", "link_type_id")
      ),
      handler = tool_traverse_links
    ),
    list(
      name = "list_concepts",
      description = "List all concept definitions with their object type and number of versions.",
      input_schema = no_args_schema(),
      handler = tool_list_concepts
    ),
    list(
      name = "evaluate_concept",
      description = "Evaluate a concept definition against all objects of its type. Returns which objects satisfy the concept (true/false per object) plus counts. Without version or stage, the active production version is used, then staging, then development.",
      input_schema = list(
        type = "object",
        properties = list(
          concept_id = list(type = "string"),
          scope = list(type = "string"),
          version = list(type = "integer"),
          stage = list(type = "string", `enum` = list("development", "staging", "production")),
          limit = list(type = "integer", default = 100L, minimum = 0L)
        ),
        required = list("concept_id", "scope")
      ),
      handler = tool_evaluate_concept
    ),
    list(
      name = "submit_action",
      description = "Submit an ontology action to modify objects. dry_run defaults to true and returns the validated plan without running it; set dry_run=false to execute.",
      input_schema = list(
        type = "object",
        properties = list(
          action_type_id = list(type = "string"),
          params = list(type = "object"),
          target_ids = list(type = "array", items = list(type = "string")),
          dry_run = list(type = "boolean", default = TRUE)
        ),
        required = list("action_type_id", "params")
      ),
      handler = tool_submit_action
    )
  )
}
