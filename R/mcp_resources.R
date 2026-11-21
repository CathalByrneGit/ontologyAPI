handle_resources_list <- function(params, server) {
  svc <- server$svc
  base_resources <- list(
    list(
      uri         = "ontology://bundle",
      name        = "Full bundle JSON",
      description = "Complete ontology bundle definition",
      mimeType    = "application/json"
    ),
    list(
      uri         = "ontology://schema",
      name        = "Bundle schema",
      description = "JSON Schema for the bundle format",
      mimeType    = "application/json"
    )
  )

  type_resources <- lapply(ontologySpecR::bundle_object_types(svc$bundle), function(ot) {
    list(
      uri         = paste0("ontology://objects/", ot$id, "/schema"),
      name        = paste0(display_name(ot), " schema"),
      description = paste0("Property schema for object type: ", ot$id),
      mimeType    = "application/json"
    )
  })

  list(resources = c(base_resources, unname(type_resources)))
}

handle_resources_read <- function(params, server) {
  svc <- server$svc
  uri <- params$uri
  if (is.null(uri)) mcp_error(MCP_INVALID_PARAMS, "uri required")

  text <- if (identical(uri, "ontology://bundle")) {
    to_json(h_bundle(svc), pretty = TRUE)
  } else if (identical(uri, "ontology://schema")) {
    bundle_json_schema()
  } else if (grepl("^ontology://objects/.+/schema$", uri)) {
    ot_id <- sub("^ontology://objects/(.+)/schema$", "\\1", uri)
    object_json_schema(svc, ot_id)
  } else {
    mcp_error(MCP_INVALID_PARAMS, paste0("Unknown resource URI: ", uri))
  }

  list(contents = list(list(uri = uri, mimeType = "application/json",
                            text = as.character(text))))
}

object_json_schema <- function(svc, object_type_id) {
  ot <- get_object_type(svc, object_type_id)
  authorize(svc, "read", ot$id)
  desc <- describe_object_type(ot)
  json_types <- c(string = "string", integer = "integer", number = "number",
                  boolean = "boolean")
  props <- lapply(desc$properties, function(p) {
    type <- if (isTRUE(p$type %in% names(json_types))) json_types[[p$type]] else "string"
    list(type = if (p$nullable) list(type, "null") else type)
  })
  names(props) <- vapply(desc$properties, function(p) p$id, character(1))
  to_json(list(
    `$schema`  = "https://json-schema.org/draft/2020-12/schema",
    title      = desc$display_name,
    type       = "object",
    properties = json_object(props),
    required   = desc$primary_key
  ), pretty = TRUE)
}

bundle_json_schema <- function() {
  to_json(list(
    `$schema` = "https://json-schema.org/draft/2020-12/schema",
    title = "OntologyBundle",
    type  = "object",
    properties = list(
      specVersion   = list(type = "string"),
      bundleId      = list(type = "string"),
      bundleVersion = list(type = "string"),
      objects       = list(type = "array"),
      links         = list(type = "array"),
      interfaces    = list(type = "array"),
      actions       = list(type = "array"),
      queries       = list(type = "array"),
      concepts      = list(type = "array"),
      templates     = list(type = "array")
    ),
    required = list("specVersion", "bundleId", "bundleVersion", "objects")
  ), pretty = TRUE)
}
