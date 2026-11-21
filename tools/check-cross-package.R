# Static checks for ontologyAPI's use of the core packages. Prints nothing and
# exits 0 when clean.
#
#   Rscript tools/check-cross-package.R
#
# 1. Every `pkg::fn` used in R/ for a core package must be an export of the
#    installed package (catches calls to functions that don't exist).
# 2. No `bundle$` / `$bundle$` key access in R/: bundles are read only through
#    ontologySpecR accessors.

core <- c("ontologySpecR", "objectSetsR", "conceptR", "actionTypesR")
files <- list.files("R", pattern = "[.]R$", full.names = TRUE)
problems <- character()

for (f in files) {
  lines <- readLines(f, warn = FALSE)
  code <- sub("#.*$", "", lines)
  for (i in seq_along(code)) {
    m <- regmatches(code[[i]], gregexpr("\\b([A-Za-z][A-Za-z0-9.]*)::([A-Za-z_.][A-Za-z0-9_.]*)", code[[i]]))[[1]]
    for (call in m) {
      parts <- strsplit(call, "::", fixed = TRUE)[[1]]
      if (parts[[1]] %in% core && !(parts[[2]] %in% getNamespaceExports(parts[[1]]))) {
        problems <- c(problems, sprintf("%s:%d: %s is not exported by %s", f, i, call, parts[[1]]))
      }
    }
    if (grepl("bundle\\$", code[[i]])) {
      problems <- c(problems, sprintf("%s:%d: direct bundle$ key access: %s", f, i, trimws(lines[[i]])))
    }
  }
}

if (length(problems) > 0) {
  writeLines(problems)
  quit(status = 1)
}
