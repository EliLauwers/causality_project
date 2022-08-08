# Step 1: the outcome model
formula.outcome = as.formula(paste0("wt82_71_bin~", paste0(preds.ds, collapse = "+")))
m = glm(
  data = nhefs.nmv,
  formula = formula.outcome,
  family = binomial
)

Q1W = predict(m, newdata = mutate(nhefs.nmv, qsmk = 1), type = "response")
Q0W = predict(m, newdata = mutate(nhefs.nmv, qsmk = 0), type = "response")

# Step 2: propensity score
formula.propensity = as.formula(paste0("qsmk~", paste0(preds.ds[preds.ds!="qsmk"], collapse = "+")))
psm = glm(
  data = nhefs.nmv,
  formula = formula.propensity,
  family = binomial
)
gW = predict(psm, type = "response")

n = nrow(nhefs.nmv)

EY1.tutorial.val = (nhefs.nmv$qsmk) * (nhefs.nmv$wt82_71_bin  - Q1W) / gW + Q1W
EY1.tutorial.est = mean(EY1.tutorial.val)
EY1.tutorial.se = sd(EY1.tutorial.val) / sqrt(n)
EY1.tutorial.ci = EY1.tutorial.est + c(-1, 1) * 1.96 * EY1.tutorial.se

EY0.tutorial.val = (1 - nhefs.nmv$qsmk) * (nhefs.nmv$wt82_71_bin - Q0W) / (1 - gW) + Q0W
EY0.tutorial.est = mean(EY0.tutorial.val)
EY0.tutorial.se = sd(EY0.tutorial.val) / sqrt(n)
EY0.tutorial.ci = EY0.tutorial.est + c(-1, 1) * 1.96 * EY0.tutorial.se