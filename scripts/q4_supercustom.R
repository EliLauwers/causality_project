
SL.library <-
  c(
    "SL.glm",
    "SL.step",
    "SL.step.interaction",
    "SL.glm.interaction",
    "SL.gam",
    "SL.randomForest",
    "SL.rpart"
  )
TMLE3 <-
  tmle(
    Y = nhefs.nmv$wt82_71_bin,
    A = nhefs.nmv$qsmk,
    W = nhefs.nmv[preds.ds[preds.ds != "qsmk"]],
    family = "binomial",
    Q.SL.library = SL.library,
    g.SL.library = SL.library
  )
ATEtmle3 <- TMLE3$estimates$ATE$psi
supercustom.est = ATEtmle3
supercustom.se = sqrt(TMLE3$estimates$ATE$var.psi)
supercustom.ci = TMLE3$estimates$ATE$CI
