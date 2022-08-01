library(SuperLearner)
library(tmle)
preds = names(nhefs.nmv)[!names(nhefs.nmv) %in% c("wt82_71_bin", "qsmk")]
TMLE2 = tmle(
  Y = nhefs.nmv$wt82_71_bin,
  A = nhefs.nmv$qsmk,
  W = nhefs.nmv[preds],
  family = "binomial"
)

ATEtmle2 <- TMLE2$estimates$ATE$psi
ATEtmle2
TMLE2$estimates$ATE$CI
MORtmle2 <- TMLE2$estimates$OR$psi
MORtmle2
TMLE2$estimates$OR$CI
#ATEtmle2 (95%CI): 20.8% (17.5, 24.1)
#MORtmle2 (95%CI): 2.8 (2.3, 3.4)

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
    W = nhefs.nmv[preds],
    family = "binomial",
    Q.SL.library = SL.library,
    g.SL.library = SL.library
  )
ATEtmle3 <- TMLE3$estimates$ATE$psi
ATEtmle3
TMLE3$estimates$ATE$CI
MORtmle3 <- TMLE3$estimates$OR$psi
MORtmle3
TMLE3$estimates$OR$CI