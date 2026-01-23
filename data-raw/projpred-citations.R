assign_projpred <- function(funs, entries) {
  for (fun in funs) {
    .stan_citation_funs[[paste0("projpred::", fun)]] <- entries
  }
}

assign_projpred(
  c("cv_varsel"),
  c(
    magnusson2020_large_data,
    mclatchie2025_projpred,
    piironen2020_projpred,
    vehtari2017_loo,
    vehtari2024_psis
  )
)
