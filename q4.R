predictors.all = preds = names(nhefs.nmv)[names(nhefs.nmv) != "wt82_71_bin"]
m = step_backwards(
  nhefs.nmv,
  outcome = "wt82_71_bin",
  treatment = "qsmk",
  predictors = preds,
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


superdefault.est = supercustom.est = tutorial.est = NULL
superdefault.se = supercustom.se = tutorial.se = NULL
superdefault.ci = supercustom.ci = tutorial.ci = NULL

source("scripts/q4_tutorial.R")
source("scripts/q4_superdefault.R")
source("scripts/q4_supercustom.R")

if (is.null(superdefault.est)) {
  superdefault.est = tutorial.est
  superdefault.ci = tutorial.ci
  superdefault.se = tutorial.se
}

if (is.null(supercustom.est)) {
  supercustom.est = tutorial.est
  supercustom.ci = tutorial.ci
  supercustom.se = tutorial.se
}

ATE.ests = c(tutorial.est, superdefault.est, supercustom.est)
ATE.ses = c(tutorial.se, superdefault.se, supercustom.se)
ATE.cis = list(tutorial.ci, superdefault.ci, supercustom.ci)
ATE.cis = sapply(ATE.cis,custom_pad)

Q4_table = knitr::kable(
  data.frame(
    technique = c("Tutorial", "Superlearner Default", "Superlearner Custom"),
    pointestimate = ATE.ests,
    ses = ATE.ses,
    CI95 = ATE.cis
  ),
  col.names = c("Technique", "$\\widehat{ATE}_{TMLE}$", "SE", "95% CI"),
  align = c("l", "c", "c", "c"),
  format = "simple",
  digits = 3
)

results[["Q4_table"]] = Q4_table
if(clean_all) clean_env(keep_in_env)