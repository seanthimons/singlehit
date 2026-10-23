# Drop entries for app-only commits from NEWS.md.
#
# The release workflow runs this after autonewsmd writes NEWS.md. autonewsmd
# lists every conventional commit, but the Shiny app in app/ is a separate
# package with its own audience, so its changes do not belong in the singlehit
# release notes. An entry is dropped when its commit has the conventional scope
# `app` (for example `feat(app): ...`) or changed only files under app/. Entries
# whose commit cannot be inspected are kept.

news_entry_sha <- function(entry) {
  match <- regmatches(entry, regexpr("/tree/[0-9a-f]{40}", entry))
  if (length(match) == 0L) NA_character_ else sub("^/tree/", "", match[[1L]])
}

# The subject and changed paths of a commit, or NULL when git cannot show it.
inspect_commit <- function(sha) {
  git <- function(...) {
    out <- suppressWarnings(system2("git", c(...), stdout = TRUE, stderr = FALSE))
    if (is.null(attr(out, "status"))) out else NULL
  }
  subject <- git("show", "-s", "--format=%s", sha)
  files <- git("show", "--no-commit-id", "--name-only", "--format=", sha)
  if (is.null(subject) || is.null(files)) NULL else list(subject = subject[[1L]], files = files[nzchar(files)])
}

is_app_commit <- function(commit) {
  if (is.null(commit)) {
    return(FALSE)
  }
  scoped <- grepl("^[a-z]+\\(app\\)!?:", commit$subject)
  only_app <- length(commit$files) > 0L && all(startsWith(commit$files, "app/"))
  scoped || only_app
}

# `inspect` maps a commit sha to its subject and files; a parameter so the
# filter can be tested without a repository.
filter_app_news <- function(lines, inspect = inspect_commit) {
  keep <- rep(TRUE, length(lines))
  i <- 1L
  while (i <= length(lines)) {
    if (startsWith(lines[[i]], "- ")) {
      end <- i
      while (end < length(lines) && startsWith(lines[[end + 1L]], "  ")) end <- end + 1L
      sha <- news_entry_sha(paste(lines[i:end], collapse = "\n"))
      if (!is.na(sha) && is_app_commit(inspect(sha))) keep[i:end] <- FALSE
      i <- end + 1L
    } else {
      i <- i + 1L
    }
  }
  lines <- lines[keep]

  # Remove a "#### Section" heading left with no entries, with the blank line
  # that follows it.
  is_heading <- grepl("^#{2,4} ", lines)
  drop <- logical(length(lines))
  for (k in which(startsWith(lines, "#### "))) {
    later <- which(is_heading & seq_along(lines) > k)
    last <- if (length(later) > 0L) later[[1L]] - 1L else length(lines)
    if (!any(startsWith(lines[seq.int(k + 1L, length.out = max(0L, last - k))], "- "))) {
      drop[[k]] <- TRUE
      after <- k + 1L
      while (after <= last && !nzchar(trimws(lines[[after]]))) {
        drop[[after]] <- TRUE
        after <- after + 1L
      }
    }
  }
  lines[!drop]
}

if (sys.nframe() == 0L) {
  news <- readLines("NEWS.md", warn = FALSE, encoding = "UTF-8")
  filtered <- filter_app_news(news)
  removed <- sum(startsWith(news, "- ")) - sum(startsWith(filtered, "- "))
  writeLines(filtered, "NEWS.md", useBytes = TRUE)
  message(sprintf("Removed %d app-only entries from NEWS.md.", removed))
}
