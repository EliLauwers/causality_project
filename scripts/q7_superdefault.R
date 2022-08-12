# Step 1: the outcome model
n = nrow(nhefs.nmv)
newdata = bind_rows(nhefs.nmv,
                    mutate(nhefs.nmv, qsmk = 1),
                    mutate(nhefs.nmv, qsmk = 0))

formula.outcome = as.formula(paste0("wt82_71_bin~", paste0(preds.ds, collapse = "+")))


SL.default = c("SL.glm", "tmle.SL.dbarts.k.5", "SL.gam", "tmle.SL.dbarts2")


# outcome model
Qinit = SuperLearner(
  Y = nhefs.nmv$wt82_71_bin,
  X = nhefs.nmv[preds.ds],
  SL.library = SL.default,
  newX = newdata[preds.ds],
  family = "binomial"
)
QAW = Qinit$SL.predict[1:n]
Q1W = Qinit$SL.predict[(n + 1):(2 * n)]
Q0W = Qinit$SL.predict[(2 * n + 1):(3 * n)]

# Step 2: propensity score
formula.propensity = as.formula(paste0("qsmk~", paste0(preds.ds[preds.ds!="qsmk"], collapse = "+")))
psm = SuperLearner(
  Y = nhefs.nmv$qsmk,
  X = nhefs.nmv[preds.ds[preds.ds!="qsmk"]],
  SL.library = SL.default,
  newX = newdata[preds.ds[preds.ds!="qsmk"]],
  family = "binomial"
)
gW = psm$SL.predict[1:n]
# step 3: The clever covariate
H1W = nhefs.nmv$qsmk/gW
H0W = (1-nhefs.nmv$qsmk)/(1-gW)
# fluctuation parameter
epsilon = coef(glm(nhefs.nmv$wt82_71_bin~-1+H0W+H1W+offset(qlogis(QAW)), family = binomial))
# Step 4: update of original outcome model
Q0W_1 = plogis(qlogis(Q0W)+ epsilon[1]/(1 - gW))
Q1W_1 = plogis(qlogis(Q1W) + epsilon[2]/gW)

EY1.superdefault.val = Q1W_1
EY1.superdefault.est = mean(EY1.superdefault.val)
EY1.superdefault.se = sd(EY1.superdefault.val) / sqrt(n)
EY1.superdefault.ci = EY1.superdefault.est + c(-1, 1) * 1.96 * EY1.superdefault.se

EY0.superdefault.val = Q0W_1
EY0.superdefault.est = mean(EY0.superdefault.val)
EY0.superdefault.se = sd(EY0.superdefault.val) / sqrt(n)
EY0.superdefault.ci = EY0.superdefault.est + c(-1, 1) * 1.96 * EY0.superdefault.se


