n = nrow(nhefs.nmv)
newdata = bind_rows(nhefs.nmv,
                    mutate(nhefs.nmv, qsmk = 1),
                    mutate(nhefs.nmv, qsmk = 0))

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


# outcome model
Qinit = SuperLearner(
  Y = nhefs.nmv$wt82_71_bin,
  X = nhefs.nmv[preds.ds],
  SL.library = SL.library,
  newX = newdata[preds.ds],
  family = "binomial"
)
QbarAW = Qinit$SL.predict[1:n]
Qbar1W = Qinit$SL.predict[(n + 1):(2 * n)]
Qbar0W = Qinit$SL.predict[(2 * n + 1):(3 * n)]
# The propensities
ginit = SuperLearner(
  Y = nhefs.nmv$wt82_71_bin,
  X = nhefs.nmv[preds.ds[preds.ds != "qsmk"]],
  newX = newdata[preds.ds[preds.ds != "qsmk"]],
  SL.library = SL.library,
  family = "binomial"
)
gbarW = ginit$SL.predict[1:n]


IC1 = (nhefs.nmv$qsmk / gbarW) * (nhefs.nmv$wt82_71_bin - QbarAW) + Qbar1W
IC0 = ((1 - nhefs.nmv$qsmk) / (1 - gbarW)) * (nhefs.nmv$wt82_71_bin - QbarAW) + Qbar0W

EY1.supercustom.val = IC1
EY1.supercustom.est = mean(IC1)
EY1.supercustom.se = sd(IC1) / sqrt(n)
EY1.supercustom.ci = EY1.supercustom.est + c(-1, 1) * 1.96 * EY1.supercustom.se

EY0.supercustom.val = IC0
EY0.supercustom.est = mean(IC0)
EY0.supercustom.se = sd(IC0) / sqrt(n)
EY0.supercustom.ci = EY0.supercustom.est + c(-1, 1) * 1.96 * EY0.supercustom.se
