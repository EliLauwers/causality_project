# Step 1: the outcome model
n = nrow(nhefs.nmv)
formula.outcome = as.formula(paste0("wt82_71_bin~", paste0(preds.ds, collapse = "+")))
m = glm(
  data = nhefs.nmv,
  formula = formula.outcome,
  family = binomial
)

QAW = predict(m, type = "response")
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
# step 3: The clever covariate
H1W = nhefs.nmv$qsmk/gW
H0W = (1-nhefs.nmv$qsmk)/(1-gW)
# fluctuation parameter
epsilon = coef(glm(nhefs.nmv$wt82_71_bin~-1+H0W+H1W+offset(qlogis(QAW)), family = binomial))
# Step 4: update of original outcome model
Q0W_1 = plogis(qlogis(Q0W)+ epsilon[1]/(1 - gW))
Q1W_1 = plogis(qlogis(Q1W) + epsilon[2]/gW)
ATEtmle1 = mean(Q1W_1 - Q0W_1)
EY1tmle1 = mean(Q1W_1)
EY0tmle1 = mean(Q0W_1)


EY1.tutorial.val = Q1W_1
EY1.tutorial.est = mean(EY1.tutorial.val)
EY1.tutorial.se = sd(EY1.tutorial.val) / sqrt(n)
EY1.tutorial.ci = EY1.tutorial.est + c(-1, 1) * 1.96 * EY1.tutorial.se

EY0.tutorial.val = Q0W_1
EY0.tutorial.est = mean(EY0.tutorial.val)
EY0.tutorial.se = sd(EY0.tutorial.val) / sqrt(n)
EY0.tutorial.ci = EY0.tutorial.est + c(-1, 1) * 1.96 * EY0.tutorial.se


