# Portions of this file are adapted from the tidyverse package.
# Copyright (c) 2021 tidyverse authors. Licensed under the MIT license.
# See LICENSE.note for details.
.onAttach <- function(...) {
  wrapped_startup(core_attach_message())
  wrapped_startup(backends_attach_message())

  conflicts <- stanflow_conflicts()

  if (length(conflicts) > 0) {
    wrapped_startup(stanflow_conflict_message(conflicts))
  }
}
