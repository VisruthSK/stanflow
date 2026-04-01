# Portions of this file are adapted from the tidyverse package.
# Copyright (c) 2024 tidyverse authors.
# License: MIT; full notice preserved in LICENSE.note.
# Source: https://github.com/tidyverse/tidyverse/blob/0231aafbc56914ee5371dd6c7b60677f168d7154/R/zzz.R

.onAttach <- function(...) {
  wrapped_startup(core_attach_message())
  wrapped_startup(backends_attach_message())

  conflicts <- stanflow_conflicts()

  if (length(conflicts) > 0) {
    wrapped_startup(stanflow_conflict_message(conflicts))
  }
}
