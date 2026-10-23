# The release-notes filter lives with the CI scripts, which are not part of the
# built package, so these tests only run from a source checkout.
script <- test_path("..", "..", ".github", "scripts", "filter-app-news.R")
skip_if_not(file.exists(script), "release script is not in the built package")
filter_env <- new.env()
sys.source(script, envir = filter_env)

entry <- function(text, sha) c(paste("-", text), sprintf("  ([%s](https://github.com/o/r/tree/%s))", substr(sha, 1, 7), sha))
sha <- function(letter) strrep(letter, 40)

commits <- list(
  a = list(subject = "feat(app): add a tab", files = c("app/R/a.R", ".Rbuildignore")),
  b = list(subject = "fix: change a thing", files = c("app/R/b.R", "app/DESCRIPTION")),
  c = list(subject = "feat(bootstrap): add progress", files = c("R/bootstrap.R", "app/R/c.R")),
  d = list(subject = "fix(bootstrap): handle limit", files = "R/bootstrap.R"),
  e = list(subject = "docs: readme", files = "README.md")
)
inspect <- function(sha) commits[[substr(sha, 1, 1)]]
run <- function(lines) filter_env$filter_app_news(lines, inspect)

test_that("entries are dropped for the app scope or app-only changes, and kept otherwise", {
  news <- c(
    "## singlehit v1.0.0 (2026-01-01)", "", "#### New features", "",
    entry("add a tab", sha("a")),
    entry("change a thing", sha("b")),
    entry("add progress", sha("c")),
    entry("handle limit", sha("d")),
    "", "#### Docs", "", entry("readme", sha("e"))
  )
  out <- run(news)
  expect_false(any(grepl(sha("a"), out)))
  expect_false(any(grepl(sha("b"), out)))
  expect_true(any(grepl(sha("c"), out)))
  expect_true(any(grepl(sha("d"), out)))
  expect_true(any(grepl(sha("e"), out)))
})

test_that("a section left empty is removed along with its blank line", {
  news <- c(
    "## singlehit v1.0.0 (2026-01-01)", "",
    "#### New features", "", entry("add a tab", sha("a")), "",
    "#### Docs", "", entry("readme", sha("e"))
  )
  out <- run(news)
  expect_false("#### New features" %in% out)
  expect_equal(out, c("## singlehit v1.0.0 (2026-01-01)", "", "#### Docs", "", entry("readme", sha("e"))))
})

test_that("a section that keeps any entry keeps its heading", {
  news <- c("#### New features", "", entry("add a tab", sha("a")), entry("add progress", sha("c")))
  expect_equal(run(news), c("#### New features", "", entry("add progress", sha("c"))))
})

test_that("entries whose commit cannot be inspected are kept", {
  news <- c("#### Fixes", "", entry("mystery", sha("z")), entry("no link", sha("a"))[1])
  expect_equal(run(news), news)
})

test_that("text without app entries comes back unchanged", {
  news <- c("", "", "## singlehit v1.0.0 (2026-01-01)", "", "#### Docs", "", entry("readme", sha("e")), "")
  expect_equal(run(news), news)
})
