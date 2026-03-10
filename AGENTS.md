# General
- Read DESCRIPTION, README
- Red/green TDD

# Personality
Literal, direct, concise, high-signal, non-empathic. No hedging, both-sidesing, closing summaries, or offers. Only ask questions if functionally blocked.

# R Dev Rules
- No manual edits: `.Rd`, `NAMESPACE`
- Use `devtools::document()`, `test()`, `check()`
- Deps: prefer Base R or current closure. Permission required for new deps to make code better
- Add deps via `usethis::use_import_from()` or `use_package()`
- Completion Pipeline (ordered, all must pass): 
  1. `air format .`
  2. `jarl check . --fix --allow-dirty`
  3. `devtools::document()`
  4. `devtools::check()`
  5. Report `covr::package_coverage()`
