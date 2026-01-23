assign_shinystan <- function(funs, entries) {
  for (fun in funs) {
    .stan_citation_funs[[paste0("shinystan::", fun)]] <- entries
  }
}

assign_shinystan(
  c("launch_shinystan"),
  muth2018
)

assign_shinystan(
  c("shinystan"),
  muth2018
)
