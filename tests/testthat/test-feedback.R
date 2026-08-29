test_that("the feedback Sheet header and the Apps Script agree", {
  hdr <- cc_feedback_header()
  expect_equal(hdr[1], "ts")
  expect_true(all(c("app", "url", "release", "viewport", "theme", "text", "email", "image_url", "issue_url", "id", "status") %in% hdr))

  gs <- cc_feedback_script()
  # the script writes exactly the header's columns, in order, and one setValues() per submission
  expect_true(grepl(as.character(jsonlite::toJSON(hdr)), gs, fixed = TRUE))
  expect_true(grepl("setValues([COLS.map", gs, fixed = TRUE))
  expect_false(grepl("appendRow", gs, fixed = TRUE))
  # a GET health check, a POST handler
  expect_true(grepl("function doGet(e)", gs, fixed = TRUE))
  expect_true(grepl("function doPost(e)", gs, fixed = TRUE))
})

test_that("the four steps are there: Drive image, Sheet row, recipients mail, public issue", {
  gs <- cc_feedback_script()
  expect_true(grepl("createFile(Utilities.newBlob(bytes, \"image/png\"", gs, fixed = TRUE))
  expect_true(grepl("_tab(\"recipients\")", gs, fixed = TRUE))       # the editable recipient list, no redeploy
  expect_true(grepl("MailApp.sendEmail", gs, fixed = TRUE))
  expect_true(grepl("/issues\"", gs, fixed = TRUE))                    # POST /repos/{repo}/issues
  expect_true(grepl("/contents/\" + path", gs, fixed = TRUE))          # the screenshot committed so the issue can embed it
  expect_true(grepl("raw.githubusercontent.com", gs, fixed = TRUE))
  # without a token the issue step is skipped and the row says so — never an error for the submitter
  expect_true(grepl("issue skipped: no GITHUB_TOKEN", gs, fixed = TRUE))
})

test_that("the mail carries the screenshot inline (1.14.1), gated on the copy Drive stored", {
  gs <- cc_feedback_script()
  expect_true(grepl("mail.inlineImages = inline", gs, fixed = TRUE))
  expect_true(grepl('src=\\"cid:shot\\"', gs, fixed = TRUE))                      # the <img> names the inline blob
  expect_true(grepl("(image_url && bytes && bytes.length) ?", gs, fixed = TRUE))  # no Drive copy (too large, absent) -> no inline
  expect_true(grepl("MailApp.sendEmail(mail)", gs, fixed = TRUE))
})

test_that("the email never reaches the public issue", {
  gs <- cc_feedback_script()
  fn <- regmatches(gs, regexpr("function _openIssue\\([^)]*\\) \\{.*?\\n\\}", gs))
  expect_true(nzchar(fn))
  expect_false(grepl("email", fn, fixed = TRUE))
  # the mail to the team may carry it; the Sheet keeps it
  expect_true(grepl("row.email", gs, fixed = TRUE))
})

test_that("spam guards: honeypot and an hourly cap", {
  gs <- cc_feedback_script(max_per_hour = 7)
  expect_true(grepl("if (b.website) return", gs, fixed = TRUE))
  expect_true(grepl("var MAX_PER_HOUR = 7;", gs, fixed = TRUE))
  expect_true(grepl("CacheService.getScriptCache()", gs, fixed = TRUE))
})

test_that("repos, label and branch are parameters and are validated", {
  gs <- cc_feedback_script(repos = c(explore = "CalCOFI/explore", hex = "CalCOFI/db-viz-hex"), label = "bug", branch = "gh-pages")
  expect_true(grepl('"hex":"CalCOFI/db-viz-hex"', gs, fixed = TRUE))
  expect_true(grepl('var LABEL = "bug";', gs, fixed = TRUE))
  expect_true(grepl('var BRANCH = "gh-pages";', gs, fixed = TRUE))
  expect_error(cc_feedback_script(repos = "CalCOFI/explore"))            # unnamed: no app -> repo mapping
  expect_error(cc_feedback_script(repos = c(explore = "explore")))       # not owner/repo
  expect_error(cc_feedback_script(max_per_hour = 0))
})
