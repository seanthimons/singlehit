# GitHub Actions workflows

This directory contains the package CI, build, and release workflows.

- `r-cmd-check.yaml` runs `R CMD check` on current release R for Linux,
  Windows, and macOS on pushes and pull requests targeting `main`.
- `test-coverage.yaml` reports package test coverage and uploads the detailed
  `covr` result as a workflow artifact.
- `build-package.yaml` manually builds and uploads a source-package artifact
  without creating a GitHub Release.
- `publish-rolling-package.yaml` manually creates or updates the
  `package-latest` prerelease with a source package built from `main`.
- `release.yaml` manually bumps the package version, checks and builds the
  package, generates normalized `NEWS.md` from Conventional Commits, pushes an
  immutable version tag, and creates a stable GitHub Release.
- `commit-lint.yaml` enforces Conventional Commit subjects on pull requests.
- `gitleaks.yaml` scans repository history for committed secrets.

The stable release workflow uses `RELEASE_PAT` when configured and otherwise
falls back to `GITHUB_TOKEN`. A PAT may be required when branch protection does
not allow the Actions token to push the release commit and tag.

R package jobs set `R_PROFILE_USER=/dev/null` so the package is built and tested
without the repository's optional boosterpak/renv development startup profile.

Stable releases use `autonewsmd` and normalize version headings for pkgdown
in the workflow; `NEWS.md` does not need to be maintained manually.
