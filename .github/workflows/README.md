# GitHub Actions workflows

These callers use [seanthimons/baseline v1.2.0](https://github.com/seanthimons/baseline/tree/c99a0a60eed152a08d1e34841d45573fef812af2),
pinned to `c99a0a60eed152a08d1e34841d45573fef812af2`. Dependabot proposes
grouped weekly updates with a `ci:` commit prefix.

| Caller | Behavior |
| --- | --- |
| `gitleaks.yaml` | Scan pushes, PRs, and repository history every Monday at 06:43 UTC, including bot commits. |
| `commit-lint.yaml` | Check PR commit subjects, PR titles, and Conventional Branch names. Scopes use ASCII letters, numbers, and underscores. |
| `lint-workflows.yaml` | Run actionlint and zizmor when `.github/` changes. |
| `r-cmd-check.yaml` | Check release R on Linux, Windows, and macOS, plus `oldrel-1` on Linux. |
| `test-coverage.yaml` | Report coverage in the job summary and upload `cobertura.xml`; Codecov is disabled. |
| `pkgdown.yaml` | Build PRs with Contents read permission; deploy non-PR builds through GitHub Pages artifacts. |
| `build-package.yaml` | Manually build and upload a source package without publishing. |
| `publish-rolling-package.yaml` | Manually publish a source package from `main` to `package-latest`; assets include the version and commit SHA. |
| `release.yaml` | Generate NEWS with autonewsmd, build and check the tagged source tarball, atomically push the release commit and tag, then publish. |

The reusable workflows own job timeouts, action pins, and R profile isolation.
Coverage requires `id-token: write` even without Codecov because the reusable
job declares that permission. Pages permissions are granted to the caller so
the reusable deploy job can use them; the reusable build job explicitly keeps
`contents: read`. Release and rolling publication concurrency belong to the
reusable workflows; callers must not reuse those concurrency groups.

## Release setup and validation

Create the Actions secret `RELEASE_PAT` with a fine-grained PAT scoped to this
repository and Contents read/write. It must belong to an account allowed to
push release commits directly to `main`. Pass it explicitly; there is no
fallback to `GITHUB_TOKEN`. The package-install token is the read-only job token.
Baseline currently persists the PAT through checkout for its final push, so
earlier build steps can still access the checkout credential.

The active `protect-default-branch` ruleset requires pull requests with no
bypass actors. A PAT alone does not bypass that rule. Before a real release,
provide an appropriate release identity with bypass access or change baseline
to publish from a merged release PR. Do not remove branch protection just to
run a release.

Release validation runs with `dry-run: true` on same-repository PRs that change
`release.yaml`. Fork PRs skip it because they cannot receive `RELEASE_PAT`.
Manual dispatch also defaults to dry-run; use `version-type: patch` for the
first validation. The baseline inputs replace the old `version_type` input and
do not support `none`. Set `dry-run: false` only after validation and release
access are resolved. No missing NEWS postprocessor is referenced.

If the atomic push succeeds but publishing fails, rerun **failed jobs only**
in the same Actions run so the publish job reuses `release-dist`. A new full
run would bump the version again. Keep the successful build artifact until
publication succeeds. Baseline verifies its checksums and publishes via a
draft release.

## Pages cutover

Before the first merged Actions deployment, change Settings > Pages > Build
and deployment > Source to **GitHub Actions**. After the first successful
deployment, verify <https://seanthimons.github.io/singlehit/> and only then
delete the old `gh-pages` branch. PR runs validate the build without deploying.

## Checks and remaining verification

Reusable jobs report names such as `R CMD check / ubuntu-latest (release)`
and `pkgdown / Build site`. The current ruleset has no required status checks.
If checks are added later, use the names reported by the migrated PR runs.

Run `actionlint .github/workflows/*.yaml` and `zizmor .github/workflows/`
locally, then confirm the PR jobs pass, the release dry-run builds successfully,
Pages deploys after cutover, and the rolling workflow updates `package-latest`.
The latter publishes assets and moves a tag, so run it when ready to publish.

Issue #10 remains partially unresolved upstream: baseline v1.2.0 fixes the
scope regex but still skips commits based on author name. Removing that skip
in baseline and updating the pin is required to remove the spoofing bypass.
The release checkout credential exposure also remains an upstream limitation.
Do not treat caller lint success as verification of the reusable implementation.
