# Release the Shiny app in app/ in step with singlehit.
#
# The release workflow runs this after bumping DESCRIPTION and before the
# release commit, so these edits land in that commit and the release tag holds
# both packages at the same version. The app is pinned to singlehit's release
# tag, so `pak::pak("seanthimons/singlehit/app@vX")` installs the matching
# singlehit. When publishing nothing is committed, so the script only checks
# that the merged files already match.

# `check_only` stops if anything is out of step instead of editing it.
sync_app_release <- function(version, app = "app", check_only = FALSE) {
  d <- desc::desc(file.path(app, "DESCRIPTION"))
  remotes <- d$get_remotes()
  i <- grep("/singlehit(@.*)?$", remotes)
  if (length(i) != 1L) {
    stop("app/DESCRIPTION needs exactly one singlehit entry in Remotes.")
  }
  remotes[[i]] <- paste0(sub("@.*$", "", remotes[[i]]), "@v", version)
  requirement <- paste(">=", version)

  config_path <- file.path(app, "inst", "golem-config.yml")
  old_config <- readLines(config_path, warn = FALSE)
  config <- sub("^(\\s*golem_version:).*$", paste("\\1", version), old_config)

  deps <- d$get_deps()
  synced <- identical(as.character(d$get_version()), version) &&
    identical(deps$version[deps$package == "singlehit" & deps$type == "Imports"], requirement) &&
    identical(d$get_remotes(), remotes) &&
    identical(old_config, config)
  if (check_only) {
    if (!synced) {
      stop(sprintf(
        "app/ is not released in step with singlehit %s: set its Version, singlehit (%s), Remotes %s, and golem_version.",
        version,
        requirement,
        remotes[[i]]
      ))
    }
    return(invisible(FALSE))
  }

  d$set_version(version)
  d$set_dep("singlehit", "Imports", requirement)
  d$set_remotes(remotes)
  d$write()
  # desc leaves a trailing space after "Remotes:".
  path <- file.path(app, "DESCRIPTION")
  writeLines(sub("[ \t]+$", "", readLines(path, warn = FALSE)), path)
  writeLines(config, config_path)
  invisible(!synced)
}

if (sys.nframe() == 0L) {
  version <- read.dcf("DESCRIPTION", fields = "Version")[[1L]]
  publishing <- identical(Sys.getenv("RELEASE_MODE"), "publish")
  sync_app_release(version, check_only = publishing)
  message(sprintf("app/ %s singlehit %s.", if (publishing) "matches" else "set to", version))
}
