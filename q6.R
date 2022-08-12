
ATE.ests = unname(sapply(results$aiptw.values, function(x) mean(x$EY1-x$EY0)))
ATE.ses = unname(sapply(results$aiptw.values, function(x) sd(x$EY1-x$EY0) / sqrt(length(x$EY1))))
ATE.cis = lapply(results$aiptw.values, function(x){
  ATE = mean(x$EY1-x$EY0)
  D1 = x$EY1 - mean(x$EY1)
  D0 = x$EY0 - mean(x$EY0)
  n = length(x$EY1)
  varHat_AIPTW = var(D1 - D0) / n
  ATE + c(-1, 1) * 1.96 * sqrt(varHat_AIPTW)
})
ATE.cis = unname(sapply(ATE.cis,custom_pad))

Q6_table = knitr::kable(
  data.frame(
    technique = c("Tutorial", "Superlearner Default", "Superlearner Custom"),
    pointestimate = ATE.ests,
    ses = ATE.ses,
    CI95 = ATE.cis
  ),
  col.names = c("Technique", "$\\widehat{ATE}_{AIPTW}$", "SE", "95% CI"),
  align = c("l", "c", "c", "c"),
  format = "simple",
  digits = 3
)

results[["Q6_table"]] = Q6_table
if(clean_all) clean_env(keep_in_env)