# GitHub Actions workflows

These callers use [seanthimons/baseline v1.3.0](https://github.com/seanthimons/baseline/tree/733a6e4554870ab2fa8dd7495fa67bea029bb38c),
pinned to `733a6e4554870ab2fa8dd7495fa67bea029bb38c`. Dependabot proposes
grouped weekly updates with a `ci:` commit prefix.

| Caller | Behavior |
| --- | --- |
| `gitleaks.yaml` | Scan pushes, PRs, and repository history every Monday at 06:43 UTC, including bot commits. |
| `commit-lint.yaml` | Check PR commit subjects, PR titles, and Conventional Branch names. Scopes use ASCII letters, numbers, and underscores. |
| `lint-workflows.yaml` | Run actionlint and zizmor when `.github/` changes. |
| `r-cmd-check.yaml` | Check release R on Linux, Windows, and macOS, plus `oldrel-1` on Linux; also test development-loaded mirai bootstraps on Windows. |
| `test-coverage.yaml` | Report coverage in the job summary and upload `cobertura.xml`; Codecov is disabled. |
| `pkgdown.yaml` | Build PRs with Contents read permission; deploy non-PR builds through GitHub Pages artifacts. |
| `build-package.yaml` | Manually build and upload a source package without publishing. |
| `publish-rolling-package.yaml` | Manually publish a source package from `main` to `package-latest`; assets include the version and commit SHA. |
| `release.yaml` | Prepare a checked version-and-NEWS PR, then publish the merged version without pushing main. |

The reusable workflows own job timeouts, action pins, and R profile isolation.
Coverage requires `id-token: write` even without Codecov because the reusable
job declares that permission. Pages permissions are granted to the caller so
the reusable deploy job can use them; the reusable build job explicitly keeps
`contents: read`. Release and rolling publication concurrency belong to the
reusable workflows; callers must not reuse those concurrency groups.

## Release setup and validation

Create the Actions secret `RELEASE_PAT` with a fine-grained PAT scoped to this
repository, with Contents and Pull requests read/write. Pass it explicitly;
there is no fallback to `GITHUB_TOKEN`. Checkout and package installs use the
read-only job token, with checkout credentials disabled. Baseline exposes the
PAT only to the final ref-push and PR-creation steps after checks.

The active `protect-default-branch` ruleset requires PRs with no bypass actors.
The release caller respects that policy:

1. From `main`, dispatch Release with `release-mode: prepare-pr`, a version
   component, and `dry-run: false`. It checks the source tarball, pushes a
   `release/vVERSION` branch, and opens a PR. It does not push main or a tag.
2. Review the release PR and wait for its checks, then merge it normally.
3. From the merged `main`, dispatch Release with `release-mode: publish` and
   `dry-run: false`. It checks the merged source, creates the version tag, and
   publishes the release. It does not bump the version or regenerate NEWS.

Keep `dry-run: true` for validation; it remains the manual default. PRs that
change `release.yaml` automatically validate preparation with dry-run enabled.
Fork PRs skip this job because they cannot receive `RELEASE_PAT`. The old
`version_type` input is replaced by `version-type`; `none` is unsupported.
No missing NEWS postprocessor is referenced.

If branch push succeeds but PR creation fails, open a PR from the existing
`release/vVERSION` branch. If publication fails after the tag push, rerun
**failed jobs only** in the same Actions run to reuse the checked artifact.
Publishing can also rerun from the same merged commit: baseline accepts an
existing version tag only when it points to that commit. It never force-moves
version tags. Keep the build artifact until publication succeeds.

## Pages cutover

Before the first merged Actions deployment, change Settings > Pages > Build
and deployment > Source to **GitHub Actions**. In the `github-pages` environment,
allow deployments from branch `main` and tags `v*` so both push and published
release triggers can deploy. After the first successful
deployment, verify <https://seanthimons.github.io/singlehit/> and only then
delete the old `gh-pages` branch and its deployment policy. PR runs validate
the build without deploying.

## Checks and remaining verification

Reusable jobs report names such as `R CMD check / ubuntu-latest (release)`
and `pkgdown / Build site`. The current ruleset has no required status checks.
If checks are added later, use the names reported by the migrated PR runs.

Run `actionlint .github/workflows/*.yaml` and `zizmor .github/workflows/`
locally, then confirm the PR jobs pass, the release dry-run builds successfully,
Pages deploys after cutover, and the rolling workflow updates `package-latest`.
The latter publishes assets and moves a tag, so run it when ready to publish.

Baseline v1.3.0 lints bot-named commits, leaves `TESTTHAT_PARALLEL` unset outside
the Windows opt-out, and scopes the release PAT to post-check write steps.
These fixes are covered by baseline's workflow regression checks.
