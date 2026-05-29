# Homogenize raw factor strings against a (raw -> canonical) lookup table.
#
# Args:
#   x      : character vector of raw factor labels.
#   lookup : data.frame / data.table with columns `raw` and `canonical`.
#
# Behaviour:
#   - Whitespace-trimmed and case-folded to lower for matching.
#   - Entries not found in `lookup$raw` are returned unchanged (trimmed +
#     lower-cased), so the table only needs to list factors that differ
#     from their canonical form.
#   - NA in, NA out.
#
# Returns: character vector of the same length as x.
homogenize_factors_fun <- function(x, lookup) {
  stopifnot(all(c("raw", "canonical") %in% names(lookup)))
  key <- tolower(trimws(as.character(x)))
  raw <- tolower(trimws(as.character(lookup$raw)))
  out <- lookup$canonical[match(key, raw)]
  ifelse(is.na(out), key, out)
}
