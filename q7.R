predictors.all = names(nhefs.nmv)[names(nhefs.nmv) != "wt82_71_bin"]
m = step_backwards(
  nhefs.nmv,
  outcome = "wt82_71_bin",
  treatment = "qsmk",
  predictors = predictors.all,
  cutoff = .1,
  verbose = 0
)
predictors.outcome = names(coef(m))[-1]

predictors.all = names(nhefs.nmv)[!names(nhefs.nmv) %in% c("wt82_71_bin", "qsmk")]
m = step_backwards(
  nhefs.nmv,
  outcome = "qsmk",
  predictors = predictors.all,
  cutoff = .1,
  verbose = 0
)
predictors.treatment = names(coef(m))[-1]
preds.ds = unique(c(predictors.outcome, predictors.treatment))

EY1.supercustom.est = NULL
EY1.supercustom.se = NULL
EY1.supercustom.ci = NULL
EY1.supercustom.val = NULL

EY0.supercustom.est = NULL
EY0.supercustom.se = NULL
EY0.supercustom.ci = NULL
EY0.supercustom.val = NULL

source("scripts/q7_supercustom.R")

EY1.ests = c(EY1.supercustom.est)
EY1.ses = c(EY1.supercustom.se)
EY1.cis = list(EY1.supercustom.ci)
EY1.cis = sapply(EY1.cis, custom_pad)

EY0.ests = c(EY0.supercustom.est)
EY0.ses = c(EY0.supercustom.se)
EY0.cis = list(EY0.supercustom.ci)
EY0.cis = sapply(EY0.cis, custom_pad)


Q7_table = knitr::kable(
  data.frame(
    technique = c("Superlearner Custom"),
    EY1.ests = EY1.ests,
    EY1.se = EY1.ses,
    EY1.ci = EY1.cis,
    EY0.ests = EY0.ests,
    EY0.se = EY0.ses,
    EY0.ci = EY0.cis
  ),
  col.names = c("Technique", "$P(Y^1=1)$", "SE" ,"95% CI", "$P(Y^0=1)$", "SE" ,"95% CI"),
  align = c("l", rep("c", 6)),
  format = "simple",
  digits = 3
)

results[["Q7_table"]] = Q7_table
clean_env(keep_in_env)

