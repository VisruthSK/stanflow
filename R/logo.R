# Portions of this file are adapted from the tidyverse package.
# Copyright (c) 2024 tidyverse authors.
# License: MIT; full notice preserved in LICENSE.note.
# Source: https://github.com/tidyverse/tidyverse/blob/0231aafbc56914ee5371dd6c7b60677f168d7154/R/logo.R

#' Print the Stan ASCII art logo
#'
#' @return Invisibly returns the character vector that was printed.
#' @export
#' @examples
#' stan_logo()
stan_logo <- function() {
  lines <- c(
    "           G08GLG80G           ",
    "        G80LLLLLLLLL08G        ",
    "    G08GLLLLLLLLLLLLLLLG80G    ",
    " G80LLLLLLLLLLLLLLCG08@@@@@@8G ",
    "8LLLLLLLLLLLC8@@@@@@@@@@@@@@@@@",
    "8LLLLLLLLL8@@@@@@@@@@@@@@800GC8",
    "8LLLLLLLL8@@@@80GCLLLLLLLLLLLL8",
    "8LLLLLLLGtiiiitfLLLLLLLLLLLLLL8",
    "8LLLLLLLfiiiiiiiii1fLLLLLLLLLL8",
    "8LLLLLLLLLLLLftiiiiiifLLLLLLLL8",
    "8LLLLLLLLLLLLLLLLLLLGLLLLLLLLL8",
    "8LLLLLLLLLLLLLG0@@0CLLLLLLLLLL8",
    "8LLLLCG0880GCLLLLLLLLLLLLLLLLL8",
    " G80LLLLLLLLLLLLLLLLLLLLGCG08G ",
    "    G08GLLLLLLLLLLLLCGCG80G    ",
    "        G80LLLLLLLCL08G        ",
    "           G08GLG80G "
  )
  cli::cat_line(lines)
  invisible(lines)
}

#' Print the stanflow ASCII art logo
#'
#' Adapted from [tidyverse::tidyverse_logo()].
#'
#' @return Invisibly returns the character vector that was printed.
#' @export
#' @examples
#' stanflow_logo()
stanflow_logo <- function() {
  lines <- c(
    "",
    "          \u2588\u2588\u2588\u2588\u2588                            \u2588\u2588\u2588\u2588\u2588\u2588  \u2588\u2588\u2588\u2588                          ",
    "         \u2591\u2591\u2588\u2588\u2588                            \u2588\u2588\u2588\u2591\u2591\u2588\u2588\u2588\u2591\u2591\u2588\u2588\u2588                          ",
    "  \u2588\u2588\u2588\u2588\u2588  \u2588\u2588\u2588\u2588\u2588\u2588\u2588    \u2588\u2588\u2588\u2588\u2588\u2588   \u2588\u2588\u2588\u2588\u2588\u2588\u2588\u2588    \u2591\u2588\u2588\u2588 \u2591\u2591\u2591  \u2591\u2588\u2588\u2588   \u2588\u2588\u2588\u2588\u2588\u2588  \u2588\u2588\u2588\u2588\u2588 \u2588\u2588\u2588 \u2588\u2588\u2588\u2588\u2588",
    " \u2588\u2588\u2588\u2591\u2591  \u2591\u2591\u2591\u2588\u2588\u2588\u2591    \u2591\u2591\u2591\u2591\u2591\u2588\u2588\u2588 \u2591\u2591\u2588\u2588\u2588\u2591\u2591\u2588\u2588\u2588  \u2588\u2588\u2588\u2588\u2588\u2588\u2588    \u2591\u2588\u2588\u2588  \u2588\u2588\u2588\u2591\u2591\u2588\u2588\u2588\u2591\u2591\u2588\u2588\u2588 \u2591\u2588\u2588\u2588\u2591\u2591\u2588\u2588\u2588 ",
    "\u2591\u2591\u2588\u2588\u2588\u2588\u2588   \u2591\u2588\u2588\u2588      \u2588\u2588\u2588\u2588\u2588\u2588\u2588  \u2591\u2588\u2588\u2588 \u2591\u2588\u2588\u2588 \u2591\u2591\u2591\u2588\u2588\u2588\u2591     \u2591\u2588\u2588\u2588 \u2591\u2588\u2588\u2588 \u2591\u2588\u2588\u2588 \u2591\u2588\u2588\u2588 \u2591\u2588\u2588\u2588 \u2591\u2588\u2588\u2588 ",
    " \u2591\u2591\u2591\u2591\u2588\u2588\u2588  \u2591\u2588\u2588\u2588 \u2588\u2588\u2588 \u2588\u2588\u2588\u2591\u2591\u2588\u2588\u2588  \u2591\u2588\u2588\u2588 \u2591\u2588\u2588\u2588   \u2591\u2588\u2588\u2588      \u2591\u2588\u2588\u2588 \u2591\u2588\u2588\u2588 \u2591\u2588\u2588\u2588 \u2591\u2591\u2588\u2588\u2588\u2588\u2588\u2588\u2588\u2588\u2588\u2588\u2588  ",
    " \u2588\u2588\u2588\u2588\u2588\u2588   \u2591\u2591\u2588\u2588\u2588\u2588\u2588 \u2591\u2591\u2588\u2588\u2588\u2588\u2588\u2588\u2588\u2588 \u2588\u2588\u2588\u2588 \u2588\u2588\u2588\u2588\u2588  \u2588\u2588\u2588\u2588\u2588     \u2588\u2588\u2588\u2588\u2588\u2591\u2591\u2588\u2588\u2588\u2588\u2588\u2588   \u2591\u2591\u2588\u2588\u2588\u2588\u2591\u2588\u2588\u2588\u2588   ",
    "\u2591\u2591\u2591\u2591\u2591\u2591     \u2591\u2591\u2591\u2591\u2591   \u2591\u2591\u2591\u2591\u2591\u2591\u2591\u2591 \u2591\u2591\u2591\u2591 \u2591\u2591\u2591\u2591\u2591  \u2591\u2591\u2591\u2591\u2591     \u2591\u2591\u2591\u2591\u2591  \u2591\u2591\u2591\u2591\u2591\u2591     \u2591\u2591\u2591\u2591 \u2591\u2591\u2591\u2591    "
  )
  cli::cat_line(lines)
  invisible(lines)
}
