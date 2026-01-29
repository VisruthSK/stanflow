assign_shinystan <- function(funs, entries) {
  for (fun in funs) {
    .stan_citation_funs[[paste0("shinystan::", fun)]] <- entries
  }
}

# TODO: should the muth be a package citation?
assign_shinystan(
  c("launch_shinystan", "shinystan"),
  c(muth2018, gabry2019_vis)
)
