# Print stanflow status and conflicts

Print a consolidated status report showing attached packages, available
interfaces, and any conflicts.

## Usage

``` r
flow_check(only = NULL)
```

## Arguments

- only:

  Set this to a character vector to restrict to conflicts only between
  the provided packages and loaded stanflow packages.

## Value

Invisibly returns the character vector that was printed.
