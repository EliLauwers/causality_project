library(SuperLearner)

n = nrow(nhefs.nmv)
newdata = bind_rows(nhefs.nmv,
                    mutate(nhefs.nmv, qsmk = 1),
                    mutate(nhefs.nmv, qsmk = 0))
SL.default = c("SL.glm", "tmle.SL.dbarts.k.5", "SL.gam", "tmle.SL.dbarts2")
# outcome model
Qinit = SuperLearner(
  Y = nhefs.nmv$wt82_71_bin,
  X = nhefs.nmv[preds.ds],
  newX = newdata[preds.ds],
  SL.library = SL.default,
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
  SL.library = SL.default,
  family = "binomial"
)
gbarW = ginit$SL.predict[1:n]


IC1 = (nhefs.nmv$qsmk / gbarW) * (nhefs.nmv$wt82_71_bin - QbarAW) + Qbar1W
IC0 = ((1 - nhefs.nmv$qsmk) / (1 - gbarW)) * (nhefs.nmv$wt82_71_bin - QbarAW) + Qbar0W

EY1.superdefault.val = IC1
EY1.superdefault.est = mean(IC1)
EY1.superdefault.se = sd(IC1) / sqrt(n)
EY1.superdefault.ci = EY1.superdefault.est + c(-1, 1) * 1.96 * EY1.superdefault.se

EY0.superdefault.val = IC0
EY0.superdefault.est = mean(IC0)
EY0.superdefault.se = sd(IC0) / sqrt(n)
EY0.superdefault.ci = EY0.superdefault.est + c(-1, 1) * 1.96 * EY0.superdefault.se
