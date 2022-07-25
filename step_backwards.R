step_backwards = function(data, outcome, predictors, treatment = NULL, verbose=0,cutoff = .5) {
  predictors.removed = c()
  while (T) {
    # Select in scope predictors for this iteration
    predictors.quest = predictors[!predictors %in% predictors.removed]
    # create a formula based on all in scope predictors
    formula.str = paste(outcome, paste(predictors.quest, collapse = "+"), sep = "~")
    # fit the modes
    model = glm(
      data = data,
      formula = as.formula(formula.str),
      family = binomial
    )
    # retrueve p values of all coefficients axcept for the intercept
    p.values = summary(model)$coefficients[-1, 4]
    # retrieve the max predictor
    predictor.max = names(p.values[which(p.values == max(p.values))])
    # break if the highest predictor is the treatment
    if (!is.null(treatment) && predictor.max == treatment)
      break
    if (max(p.values) <= cutoff)
      break
    # if not, than do a new iteration
    predictors.removed = c(predictors.removed, predictor.max)
  }
  
  if(verbose >= 2){
    print(summary(model))
  }
  
  if(verbose >= 1){
    
    kept.pred.str = paste("  Kept:", paste(predictors.quest, collapse = ", "))
    rem.pred.str = paste("  Removed:", paste(predictors.removed, collapse = ", "))
    
    cat(c(glue::glue("Predictors in backwards procedure for {outcome}:"),
          "\n",
          kept.pred.str,
          "\n",
          rem.pred.str,
          "\n"))
  }
  
  return(model)
}

