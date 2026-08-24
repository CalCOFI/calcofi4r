# quality flags: which measurement_qual codes a consumer should exclude ---------
#
# `obs.measurement_qual` carries each dataset's OWN vocabulary, uninterpreted:
# the bottle database's (6 = ok-from-CTD, 8 = suspect, 9 = missing), the CTD
# cast files' (1/2 = use primary/secondary sensor, 8 = questionable, 9 = bad or
# missing) and DIC's WOCE flags (2 = good, 3 = questionable, 4 = bad, 9 = missing).
# Every consumer that plotted `obs` averaged the flagged values in with the rest
# — the bottle oxygen that put a 2.18 ml/L spike at 1,144 m on the station
# portal in August 2026 had carried `O_qual = 8` since 1955. This is the one
# predicate to apply; the codes are documented in
# CalCOFI/workflows `metadata/measurement_qual.csv`.

#' Quality codes a consumer should exclude, per dataset
#'
#' Named list: `dataset_key` -> the `measurement_qual` codes that mean suspect,
#' bad or missing in that dataset's vocabulary. Datasets not listed carry no
#' usable flag and are never excluded.
#' @export
#' @concept quality
CC_QUAL_EXCLUDE <- list(
  "calcofi_bottle"   = c("8", "9"),
  "calcofi_ctd-cast" = c("8", "9"),
  "calcofi_dic"      = c("3", "4", "9"))

#' SQL predicate keeping only observations whose quality flag is not bad
#'
#' Returns a boolean SQL fragment over `dataset_key` and `measurement_qual`
#' that is `TRUE` for unflagged rows (NULL), for datasets without a flag
#' vocabulary, and for codes not in [CC_QUAL_EXCLUDE]; `FALSE` for suspect /
#' bad / missing. Bottle codes were written as `"8.0"` through v2026.08.14, so
#' the trailing `.0` is stripped before comparison. Append it to any `WHERE`
#' over `obs`, `obs_ctd_full`, `sample_measurement` or `ctd_thin`.
#'
#' @param alias Optional table alias to prefix the columns with (e.g. `"o"`).
#' @return A single string.
#' @examples
#' cc_qual_ok_sql("o")
#' @export
#' @concept quality
cc_qual_ok_sql <- function(alias = NULL) {
  p <- if (is.null(alias) || !nzchar(alias)) "" else paste0(alias, ".")
  q <- sprintf("regexp_replace(%smeasurement_qual, '\\.0+$', '')", p)
  arms <- vapply(names(CC_QUAL_EXCLUDE), function(dk) sprintf(
    "(%sdataset_key = '%s' AND %s IN (%s))", p, dk, q,
    paste0("'", CC_QUAL_EXCLUDE[[dk]], "'", collapse = ", ")), character(1))
  # COALESCE: a NULL flag must KEEP the row, and NOT(NULL) is NULL
  sprintf("COALESCE(NOT (%s), TRUE)", paste(arms, collapse = " OR "))
}

#' Is each observation's quality flag acceptable? (vectorised, in R)
#'
#' The in-memory twin of [cc_qual_ok_sql()], for data already collected.
#' @param dataset_key,measurement_qual Character vectors of equal length.
#' @return Logical, `TRUE` where the row should be kept.
#' @export
#' @concept quality
cc_qual_ok <- function(dataset_key, measurement_qual) {
  q <- sub("\\.0+$", "", as.character(measurement_qual))
  bad <- vapply(seq_along(q), function(i) {
    codes <- CC_QUAL_EXCLUDE[[dataset_key[i]]]
    !is.null(codes) && !is.na(q[i]) && q[i] %in% codes
  }, logical(1))
  !bad
}
