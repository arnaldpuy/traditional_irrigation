# Smith's salience index S for free-listing data.
#
# In a free-listing exercise, an item mentioned early in a respondent's list
# is more cognitively salient than one mentioned late. Smith's S combines
# how OFTEN and how EARLY a concept is mentioned into a single index.
#
# Args:
#   dat : data.table / data.frame with columns
#           respondent : interviewee id
#           group      : the group the respondent belongs to
#           concept    : the listed concept (already cleaned/homogenized)
#           ord        : within-respondent listing order (1 = first mentioned)
#
# Method:
#   For each respondent, rank concepts by first mention (1 = first); a list
#   of L distinct concepts gives the item at rank k a score (L - k + 1)/L.
#   S(concept, group) = mean of that score over ALL respondents in the group,
#   with non-mentioners contributing 0.
#
# Returns: data.table with columns group, concept, sum_score, N, S.
smith_salience_fun <- function(dat) {
  stopifnot(all(c("respondent", "group", "concept", "ord") %in% names(dat)))
  d <- data.table::as.data.table(dat)

  # First mention of each distinct concept per respondent.
  first_occ <- d[, .(ord = min(ord)), by = .(respondent, group, concept)]
  first_occ[, rank := data.table::frank(ord, ties.method = "first"),
            by = respondent]
  first_occ[, L := .N, by = respondent]
  first_occ[, score := (L - rank + 1) / L]

  # Group size = number of respondents (denominator includes non-mentioners).
  n_resp <- d[, .(N = data.table::uniqueN(respondent)), by = group]

  sal <- first_occ[, .(sum_score = sum(score)), by = .(group, concept)]
  sal <- merge(sal, n_resp, by = "group")
  sal[, S := sum_score / N]
  sal[]
}
