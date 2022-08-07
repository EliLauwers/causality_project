library("readxl")
nhefs <- read_excel("nhefs.xls")
# Ignore patients with missing outcome
nhefs.nmv <- nhefs[which(!is.na(nhefs$wt82_71)), ]
# Dichotomize the outcome variable
nhefs.nmv$wt82_71_bin <- ifelse(nhefs.nmv$wt82_71 > 0, 1, 0)
# Select reduced dataset
nhefs.nmv <-
  nhefs.nmv[, (
    names(nhefs.nmv) %in% c(
      # Weigth gain between 72 and 82
      # => 1 if weight gain is present, 0 otherwise
      "wt82_71_bin", 
      "qsmk", # quit smoking 
      "sex",
      "dbp",
      "race",
      "sbp",
      "age",
      "education",
      "smokeintensity",
      "smokeyrs",
      "tax71_82",
      "exercise",
      "active",
      "wt71"
    )
  )]

nhefs.nmv = nhefs.nmv[complete.cases(nhefs.nmv),]