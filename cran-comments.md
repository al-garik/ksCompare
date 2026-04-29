## R CMD check results

0 errors | 0 warnings | 0 notes

* This is a new release.

## Test environments

* local: Windows, R 4.4
* GitHub Actions: ubuntu-latest (devel, release, oldrel-1), macOS-latest, windows-latest
* win-builder: devel, release

## Reverse dependencies

This is a new package, so there are no reverse dependencies.

## Notes for CRAN

* All packages used in examples and vignettes guarded by
  `requireNamespace()` / Suggests are properly listed.
* All `\dontrun{}` examples interact with files on disk; we use
  `tempfile()` in tests to avoid touching the user's home directory.
* No external network access is performed during package load,
  examples, or tests.
