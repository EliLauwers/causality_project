source("step_backwards.R")
# verbose 0 => do not print anything
predictors.all = names(nhefs.nmv)[names(nhefs.nmv) != "wt82_71_bin"]
model = step_backwards(
  data = nhefs.nmv,
  outcome = "wt82_71_bin",
  treatment = "qsmk",
  predictors = predictors.all,
  cutoff = .1,
  verbose = 1
)
# counterfactual outcomes on data without any missingness
p0 = mean(predict(model, na.omit(mutate(nhefs.nmv, qsmk = 0)), type = "response"))
p1 = mean(predict(model, na.omit(mutate(nhefs.nmv, qsmk = 1)), type = "response"))
rd.man = p1 - p0
rr.man = p1 / p0
# crosscheck results
model.std = stdReg::stdGlm(model,
                           data = nhefs.nmv,
                           X = "qsmk",
                           x = c(0, 1))
# difference estimate
model.std.diff = summary(model.std, contrast = "difference", reference = 0)
rd.stdreg = model.std.diff$est.table[2, "Estimate"]
# Ratio estimate
model.std.ratio = summary(model.std, contrast = "ratio", reference = 0)
rr.stdreg = model.std.ratio$est.table[2, "Estimate"]

knitr::kable(data.frame(
  first = c("Manual", "stdReg"),
  second = c(rd.man, rd.stdreg),
  third = c(rr.man, rr.stdreg)
), col.names = c("", "Risk Difference", "Relative Risk"), format = "simple", digits = 3)
