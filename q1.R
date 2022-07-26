
# Q1: Estimate the marginal effect using a logistical regression outcome model
# steps:
# 1: build logistic regression model using step backwards
# 2: predict counterfactuals
# 3: Estimate risk difference and relative risk
# 
# Next, chrosscheck with {stdReg::stdGlm}
# 4: Build model
# 5: Extract estimates 
# 6: Calculate risk difference and relative risk
# 
# Next, store results and clean environment

# STEP 1: build logistic regression model using step backwards
predictors.all = names(nhefs.nmv)[names(nhefs.nmv) != "wt82_71_bin"]
model = step_backwards(
  data = nhefs.nmv,
  outcome = "wt82_71_bin",
  treatment = "qsmk",
  predictors = predictors.all,
  cutoff = .1,
  verbose = 1
)

# STEP 2: Predict counterfactuals
p0 = mean(predict(model, na.omit(mutate(nhefs.nmv, qsmk = 0)), type = "response"))
p1 = mean(predict(model, na.omit(mutate(nhefs.nmv, qsmk = 1)), type = "response"))

# STEP 3: Estimate risk difference and relative risk
rd.g = p1 - p0
rr.g = p1 / p0

# STEP 4: build stdReg Model
model.std = stdReg::stdGlm(model,
                           data = nhefs.nmv,
                           X = "qsmk",
                           x = c(0, 1))

# STEP 5: Extract estimates
model.est.table = summary(model.std, reference = 0)$est.table
p0 = model.est.table[1,1]
p1 = model.est.table[2,1]

# STEP 6: calculate risk difference and relative risk
rd.stdreg = p1 - p0
rr.stdreg = p1 / p0

# Store results and clean Environment
Q1_table=knitr::kable(data.frame(
  first = c( "Manual", "stdReg"),
  second = c( rd.g, rd.stdreg),
  third = c( rr.g, rr.stdreg)
), col.names = c("", "Risk Difference", "Relative Risk"), format = "simple", digits = 3)


results[["Q1_table"]] = Q1_table
if(clean_all) clean_env(keep_in_env)
