# Step 1: the outcome model
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
# Step 6: Inference and CI
#ATE efficient influence curve (EIC)
D1 <- nhefs.nmv$qsmk/gW*(nhefs.nmv$wt82_71_bin - Q1W_1) + Q1W_1 - EY1tmle1
D0 <- (1 - nhefs.nmv$qsmk)/(1 - gW)*(nhefs.nmv$wt82_71_bin - Q0W_1) + Q0W_1 - EY0tmle1
EIC <- D1 - D0
#ATE variance
n <- nrow(nhefs.nmv)
varHat.IC <- var(EIC)/n
#ATE 95%CI
ATEtmle1_CI <- ATEtmle1 + c(1, -1) * 1.96 * sqrt(varHat.IC)
tutorial.est = ATEtmle1
tutorial.se = sqrt(varHat.IC)
tutorial.ci = ATEtmle1_CI
