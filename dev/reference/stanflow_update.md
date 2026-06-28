# Update stanflow packages

Checks for outdated Stan workflow packages and installs updates. This
function requires an interactive R session for installation unless
`dry_run = TRUE`. Dry runs check repositories and preview installs
without prompting or installing packages. Adapted from
`tidyverse::tidyverse_update()`.

## Usage

``` r
stanflow_update(recursive = FALSE, dev = FALSE, dry_run = FALSE)
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

- dry_run:

  Logical. If `TRUE`, previews update steps without installing packages
  or prompting.

## Value

Invisibly returns a data frame of outdated packages (same columns as
[`stanflow_deps`](https://visruthsk.github.io/stanflow/dev/reference/stanflow_deps.md)).
Returns `NULL` invisibly when no updates are needed.

## Examples

``` r
if (FALSE) { # \dontrun{
# Update direct dependencies only
stanflow_update()

# Update full dependency tree (including suggests)
stanflow_update(recursive = TRUE)
} # }
```
