# The release-notes filter lives with the CI scripts, which are not part of the
# built package, so these tests only run from a source checkout.
script <- test_path("..", "..", ".github", "scripts", "filter-app-news.R")
skip_if_not(file.exists(script), "release script is not in the built package")
filter_env <- new.env()
sys.source(script, envir = filter_env)

entry <- function(text, sha) {
  c(paste("-", text), sprintf("  ([%s](https://github.com/o/r/tree/%s))", substr(sha, 1, 7), sha))
}
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

test_that("app-scoped and app-only entries move to a Shiny app section; others stay", {
  news <- c(
    "## singlehit v1.0.0 (2026-01-01)",
    "",
    "#### New features",
    "",
    entry("add a tab", sha("a")),
    entry("change a thing", sha("b")),
    entry("add progress", sha("c")),
    entry("handle limit", sha("d")),
    "",
    "#### Docs",
    "",
    entry("readme", sha("e")),
    "",
    "Full set of changes:",
    "[`v0.9.0...v1.0.0`](https://example.org)",
    ""
  )
  expect_equal(
    run(news),
    c(
      "## singlehit v1.0.0 (2026-01-01)",
      "",
      "#### New features",
      "",
      entry("add progress", sha("c")),
      entry("handle limit", sha("d")),
      "",
      "#### Docs",
      "",
      entry("readme", sha("e")),
      "",
      "#### Shiny app",
      "",
      entry("add a tab", sha("a")),
      entry("change a thing", sha("b")),
      "",
      "Full set of changes:",
      "[`v0.9.0...v1.0.0`](https://example.org)",
      ""
    )
  )
})

test_that("app entries stay within their own version", {
  news <- c(
    "## singlehit v2.0.0 (2026-02-01)",
    "",
    "#### Docs",
    "",
    entry("readme", sha("e")),
    "",
    "## singlehit v1.0.0 (2026-01-01)",
    "",
    "#### New features",
    "",
    entry("add a tab", sha("a")),
    ""
  )
  expect_equal(
    run(news),
    c(
      "## singlehit v2.0.0 (2026-02-01)",
      "",
      "#### Docs",
      "",
      entry("readme", sha("e")),
      "",
      "## singlehit v1.0.0 (2026-01-01)",
      "",
      "#### Shiny app",
      "",
      entry("add a tab", sha("a")),
      ""
    )
  )
})

test_that("a section left empty is removed along with its blank line", {
  news <- c(
    "## singlehit v1.0.0 (2026-01-01)",
    "",
    "#### New features",
    "",
    entry("add a tab", sha("a")),
    "",
    "#### Docs",
    "",
    entry("readme", sha("e"))
  )
  expect_equal(
    run(news),
    c(
      "## singlehit v1.0.0 (2026-01-01)",
      "",
      "#### Docs",
      "",
      entry("readme", sha("e")),
      "",
      "#### Shiny app",
      "",
      entry("add a tab", sha("a"))
    )
  )
})

test_that("entries whose commit cannot be inspected stay where they are", {
  news <- c("#### Fixes", "", entry("mystery", sha("z")), entry("no link", sha("a"))[1])
  expect_equal(run(news), news)
})

test_that("text without app entries comes back unchanged", {
  news <- c("", "", "## singlehit v1.0.0 (2026-01-01)", "", "#### Docs", "", entry("readme", sha("e")), "")
  expect_equal(run(news), news)
})
