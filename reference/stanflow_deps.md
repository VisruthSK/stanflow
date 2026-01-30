# List all stanflow dependencies

Returns a data frame of Stan workflow packages and their local/remote
versions. When `check_updates = FALSE`, remote versions are not queried
and the `remote` and `behind` columns are `NA` and `FALSE`,
respectively.

## Usage

``` r
stanflow_deps(recursive = FALSE, dev = FALSE, check_updates = TRUE)
```

## Arguments

- recursive:

  If `TRUE`, will also list dependencies of dependencies. When
  `check_updates = TRUE`, the recursive traversal follows only "strong"
  dependencies (Depends/Imports/LinkingTo), so Suggests are not expanded
  recursively.

- dev:

  If `FALSE` (default), checks for updates in the R-multiverse or CRAN
  (stable releases). If `TRUE`, checks the Stan R-universe (dev
  versions). This is only cogent for Stan packages, and cannot compare
  two dev versions.

- check_updates:

  Logical. If `FALSE`, skips checking for remote versions and only
  reports locally installed package versions.

## Value

A data frame with columns:

- package:

  Package name.

- remote:

  Repository version (character, `NA` when not queried).

- local:

  Installed version (character, `"0"` if not installed).

- behind:

  Logical; `TRUE` when `remote` is newer than `local`.

## Examples

``` r
if (FALSE) { # \dontrun{
# Full dependency check with remote versions
stanflow_deps(recursive = TRUE)

# Local-only inventory (fast, no network)
stanflow_deps(check_updates = FALSE)
} # }
```
