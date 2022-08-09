TMLE2 = tmle(
  Y = nhefs.nmv$wt82_71_bin,
  A = nhefs.nmv$qsmk,
  W = nhefs.nmv[preds.ds[preds.ds != "qsmk"]],
  family = "binomial"
)

superdefault.est = TMLE2$estimates$ATE$psi
superdefault.se = sqrt(TMLE2$estimates$ATE$var.psi)
superdefault.ci = TMLE2$estimates$ATE$CI
