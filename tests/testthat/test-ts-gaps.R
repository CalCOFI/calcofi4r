# A species time series is mostly zeros, and Highcharts connects consecutive
# points — so an unsampled stretch renders as a flat line along zero, which reads
# as "we looked and found none" when the truth is "nobody looked".
#
# The worked case: cdfw_dungeness-crab's sorted-archive effort exists in only 9
# years (1984, 1988, 1998, 2004-2009). The chart drew a continuous zero from 1984
# to 2008, asserting measured absence across ~20 years in which not one jar was
# opened.

crab <- function() data.frame(
  time = as.Date(paste0(c(1984, 1988, 1998, 2004, 2005, 2006, 2007, 2008, 2009), "-01-01")),
  name = "Metacarcinus magister",
  avg  = c(0, 0, 0, 0, 0, 0, 0, 0.03, 0.03),
  std  = 0, n = 10L, upr = 0, lwr = 0, stringsAsFactors = FALSE)

test_that("unsampled years become NA so the line breaks", {
  g <- calcofi4r:::.ts_gaps(crab(), "year")
  yrs <- as.integer(format(g$time, "%Y"))

  expect_equal(sum(!is.na(g$avg)), 9L)                 # every measured year kept
  expect_true(all(is.na(g$avg[yrs %in% 1985:1987])))   # the real gaps
  expect_true(all(is.na(g$avg[yrs %in% 1989:1997])))
  expect_true(all(is.na(g$avg[yrs %in% 1999:2003])))
  expect_false(any(is.na(g$avg[yrs %in% c(1984, 1988, 1998, 2004:2009)])))
})

test_that("a gap is NA, never 0 — zero is a measurement", {
  g <- calcofi4r:::.ts_gaps(crab(), "year")
  gap <- g[is.na(g$avg), ]
  expect_gt(nrow(gap), 0)
  # this is the whole point: the bug was gaps reading as measured zeros
  expect_false(any(gap$avg %in% 0, na.rm = TRUE))
  expect_true(all(is.na(gap$std)), )
  expect_true(all(gap$n == 0))                          # tells the two apart
})

test_that("it does not pad beyond the observed range", {
  g <- calcofi4r:::.ts_gaps(crab(), "year")
  expect_equal(min(format(g$time, "%Y")), "1984")
  expect_equal(max(format(g$time, "%Y")), "2009")
})

test_that("a fully-sampled series is returned unchanged", {
  d <- data.frame(time = as.Date(paste0(2000:2004, "-01-01")), name = "x",
                  avg = 1:5, std = 0, n = 1L, upr = 0, lwr = 0)
  expect_equal(nrow(calcofi4r:::.ts_gaps(d, "year")), 5L)
})

test_that("each series gets its own range — one taxon's gap is not another's", {
  d <- rbind(
    data.frame(time = as.Date(c("2000-01-01", "2003-01-01")), name = "a",
               avg = 1, std = 0, n = 1L, upr = 0, lwr = 0),
    data.frame(time = as.Date(c("2010-01-01", "2011-01-01")), name = "b",
               avg = 1, std = 0, n = 1L, upr = 0, lwr = 0))
  g <- calcofi4r:::.ts_gaps(d, "year")
  # `a` gains 2001-2002; `b` is contiguous and gains nothing. Crucially `b` is
  # NOT padded back to 2000 — the series do not share a range.
  expect_equal(sum(g$name == "a"), 4L)
  expect_equal(sum(g$name == "b"), 2L)
})

test_that("climatology cycles are left alone", {
  # quarter/month/day are CYCLES, not axes: every bin is populated by
  # construction and an absent one means something different
  q <- data.frame(time = as.Date(c("2000-01-01", "2000-07-01")), name = "x",
                  avg = c(1, 2), std = 0, n = 1L, upr = 0, lwr = 0)
  expect_equal(nrow(calcofi4r:::.ts_gaps(q, "quarter")), 2L)
  m <- data.frame(time = c(1, 6), name = "x", avg = c(1, 2),
                  std = 0, n = 1L, upr = 0, lwr = 0)   # month = integer, not Date
  expect_equal(nrow(calcofi4r:::.ts_gaps(m, "month")), 2L)
})
