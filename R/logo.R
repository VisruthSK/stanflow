#' Print the Stan logo to the console.
#'
#' @export
stan_logo <- function() {
  # generated using https://ascii-kit.pigeonposse.com/?tab=form-text
  # and shrunk using https://github.com/Salih2000/ascii-shrinker
  cli::cat_line(c(
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
  ))
}

#' Print the stanflow logo to the console.
#'
#' @export
stanflow_logo <- function() {
  # generated using https://www.asciiart.eu/text-to-ascii-art with the DOS Rebel font
  cli::cat_line(c(
    "",
    "          █████                            ██████  ████                          ",
    "         ░░███                            ███░░███░░███                          ",
    "  █████  ███████    ██████   ████████    ░███ ░░░  ░███   ██████  █████ ███ █████",
    " ███░░  ░░░███░    ░░░░░███ ░░███░░███  ███████    ░███  ███░░███░░███ ░███░░███ ",
    "░░█████   ░███      ███████  ░███ ░███ ░░░███░     ░███ ░███ ░███ ░███ ░███ ░███ ",
    " ░░░░███  ░███ ███ ███░░███  ░███ ░███   ░███      ░███ ░███ ░███ ░░███████████  ",
    " ██████   ░░█████ ░░████████ ████ █████  █████     █████░░██████   ░░████░████   ",
    "░░░░░░     ░░░░░   ░░░░░░░░ ░░░░ ░░░░░  ░░░░░     ░░░░░  ░░░░░░     ░░░░ ░░░░    "
  ))
}
