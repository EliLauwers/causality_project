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

EY1.tutorial.est = EY1.superdefault.est = EY1.supercustom.est = NULL
EY1.tutorial.se = EY1.superdefault.se = EY1.supercustom.se = NULL
EY1.tutorial.ci = EY1.superdefault.ci = EY1.supercustom.ci = NULL
EY1.tutorial.val = EY1.superdefault.val = EY1.supercustom.val = NULL

EY0.tutorial.est = EY0.superdefault.est = EY0.supercustom.est = NULL
EY0.tutorial.se = EY0.superdefault.se = EY0.supercustom.se = NULL
EY0.tutorial.ci = EY0.superdefault.ci = EY0.supercustom.ci = NULL
EY0.tutorial.val = EY0.superdefault.val = EY0.supercustom.val = NULL

source("scripts/q5_tutorial.R")
source("scripts/q5_superdefault.R")
source("scripts/q5_supercustom.R")


if (is.null(EY1.superdefault.est)) {
  EY1.superdefault.est = EY1.tutorial.est
  EY1.superdefault.se = EY1.tutorial.se
  EY1.superdefault.ci = EY1.tutorial.ci
  
  EY0.superdefault.est = EY0.tutorial.est
  EY0.superdefault.se = EY0.tutorial.se
  EY0.superdefault.ci = EY0.tutorial.ci
}

if (is.null(EY1.supercustom.est)) {
  EY1.supercustom.est = EY1.tutorial.est
  EY1.supercustom.se = EY1.tutorial.se
  EY1.supercustom.ci = EY1.tutorial.ci
  
  EY0.supercustom.est = EY0.tutorial.est
  EY0.supercustom.se = EY0.tutorial.se
  EY0.supercustom.ci = EY0.tutorial.ci
}


EY1.ests = c(EY1.tutorial.est, EY1.superdefault.est, EY1.supercustom.est)
EY1.ses = c(EY1.tutorial.se, EY1.superdefault.se, EY1.supercustom.se)
EY1.cis = list(EY1.tutorial.ci, EY1.superdefault.ci, EY1.supercustom.ci)
EY1.cis = sapply(EY1.cis, custom_pad)

EY0.ests = c(EY0.tutorial.est, EY0.superdefault.est, EY0.supercustom.est)
EY0.ses = c(EY0.tutorial.se, EY0.superdefault.se, EY0.supercustom.se)
EY0.cis = list(EY0.tutorial.ci, EY0.superdefault.ci, EY0.supercustom.ci)
EY0.cis = sapply(EY0.cis, custom_pad)


Q5_table = knitr::kable(
  data.frame(
    technique = c("Tutorial", "Superlearner Default", "Superlearner Custom"),
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

results[["Q5_table"]] = Q5_table
results[["aiptw.values"]] = list(
  tutorial = list(
    EY1 = EY1.tutorial.val,
    EY0 = EY0.tutorial.val
  ),
  superdefault = list(
    EY1 = EY1.superdefault.val,
    EY0 = EY0.superdefault.val
  ),
  supercustom = list(
    EY1 = EY1.supercustom.val,
    EY0 = EY0.supercustom.val
  )
)
clean_env(keep_in_env)

