# Step 1: the outcome model
preds = names(nhefs.nmv)[names(nhefs.nmv) != "wt82_71_bin"]
m = step_backwards(
  nhefs.nmv,
  outcome = "wt82_71_bin",
  treatment = "qsmk",
  predictors = preds,
  cutoff = .1,
  verbose = 1
)
QAW = predict(m, type = "response")
Q1W = predict(m, newdata = mutate(nhefs.nmv, qsmk = 1), type = "response")
Q0W = predict(m, newdata = mutate(nhefs.nmv, qsmk = 0), type = "response")
mean(Q1W - Q0W)
# Step 2: propensity score
preds = names(nhefs.nmv)[!names(nhefs.nmv) %in% c("qsmk")]
psm = step_backwards(
  nhefs.nmv,
  outcome = "qsmk",
  predictors = preds,
  cutoff = .1,
  verbose = 1
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
EY1tmle1=mean(Q1W_1)
EY0tmle1=mean(Q0W_1)
MORtmle1 = (EY1tmle1 * (1-EY0tmle1)) / ((1-EY1tmle1) * EY0tmle1)
# Step 6: Inference and CI
#ATE efficient influence curve (EIC)
D1 <- nhefs.nmv$qsmk/gW*(nhefs.nmv$wt82_71_bin - Q1W_1) + Q1W_1 - EY1tmle1
D0 <- (1 - nhefs.nmv$qsmk)/(1 - gW)*(nhefs.nmv$wt82_71_bin - Q0W_1) + Q0W_1 - EY0tmle1
EIC <- D1 - D0
#ATE variance
n <- nrow(nhefs.nmv)
varHat.IC <- var(EIC)/n
#ATE 95%CI
ATEtmle1_CI <- c(ATEtmle1 - 1.96*sqrt(varHat.IC), ATEtmle1 + 1.96*sqrt(varHat.IC)); ATEtmle1;
ATEtmle1_CI
#ATEtmle1_CI(95%CI): 22.1% (15.1, 29.0)
#MOR EIC
EIC <- (1 - EY0tmle1) / EY0tmle1 / (1 - EY1tmle1)^2 * D1 - EY1tmle1 / (1 - EY1tmle1) / EY0tmle1^2 * D0
varHat.IC <- var(EIC)/n
#MOR 95%CI
MORtmle1_CI <- c(MORtmle1 - 1.96*sqrt(varHat.IC), MORtmle1 + 1.96*sqrt(varHat.IC)); MORtmle1;
MORtmle1_CI