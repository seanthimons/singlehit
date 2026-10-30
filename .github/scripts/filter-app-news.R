# Move entries for app-only commits in NEWS.md into a "Shiny app" section.
#
# The release workflow runs this after autonewsmd writes NEWS.md. autonewsmd
# lists every conventional commit, but the Shiny app in app/ is a separate
# package released in step with singlehit (see sync-app-release.R), so its
# changes would otherwise read as singlehit features. Each version's app
# entries are gathered under "#### Shiny app" at the end of that version. An
# entry counts as the app's when its commit has the conventional scope `app`
# (for example `feat(app): ...`) or changed only files under app/. Entries whose
# commit cannot be inspected stay where they are.

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

# Remove each "#### Section" heading left with no entries, with the blank line
# that follows it.
drop_empty_sections <- function(lines) {
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

# `inspect` maps a commit sha to its subject and files; a parameter so the
# filter can be tested without a repository.
filter_app_news <- function(lines, inspect = inspect_commit) {
  app <- logical(length(lines))
  i <- 1L
  while (i <= length(lines)) {
    if (startsWith(lines[[i]], "- ")) {
      end <- i
      while (end < length(lines) && startsWith(lines[[end + 1L]], "  ")) {
        end <- end + 1L
      }
      sha <- news_entry_sha(paste(lines[i:end], collapse = "\n"))
      if (!is.na(sha) && is_app_commit(inspect(sha))) {
        app[i:end] <- TRUE
      }
      i <- end + 1L
    } else {
      i <- i + 1L
    }
  }
  if (!any(app)) {
    return(lines)
  }

  # Each "## " version heading starts a block; the app section goes after the
  # block's last entry, before autonewsmd's "Full set of changes" link.
  starts <- unique(c(1L, which(startsWith(lines, "## "))))
  ends <- c(starts[-1L] - 1L, length(lines))
  blocks <- Map(
    function(from, to) {
      rows <- seq.int(from, to)
      moved <- lines[rows][app[rows]]
      block <- drop_empty_sections(lines[rows][!app[rows]])
      if (length(moved) == 0L) {
        return(block)
      }
      full <- match(TRUE, startsWith(block, "Full set of changes"), nomatch = length(block) + 1L)
      filled <- which(nzchar(trimws(block[seq_len(full - 1L)])))
      after <- if (length(filled) > 0L) max(filled) else 0L
      append(block, c("", "#### Shiny app", "", moved), after = after)
    },
    starts,
    ends
  )
  unlist(blocks, use.names = FALSE)
}

if (sys.nframe() == 0L) {
  news <- readLines("NEWS.md", warn = FALSE, encoding = "UTF-8")
  filtered <- filter_app_news(news)
  writeLines(filtered, "NEWS.md", useBytes = TRUE)
  message(sprintf("Moved app entries into %d Shiny app sections of NEWS.md.", sum(filtered == "#### Shiny app")))
}
