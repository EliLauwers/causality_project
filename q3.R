ps.predictors = names(nhefs.nmv)[!names(nhefs.nmv) %in% c("wt82_71_bin", "qsmk")]
model = step_backwards(
  data = nhefs.nmv,
  outcome = "qsmk",
  predictors = ps.predictors,
  cutoff = .1,
  verbose = 0
)
ps.scores = fitted(model)

ggplot(data = data.frame(treated = as.factor(nhefs.nmv$qsmk), ps = ps.scores),
       aes(x = ps, group = treated, color = treated)) +
  geom_density() +
  xlim(0, 1) +
  theme_classic() +
  scale_color_manual(values = c("black", "red")) +
  labs(x = "Propensity score",
       y = "Density",
       title = "Distribution of Propensity scores across levels of treatment") +
  theme(axis.title.y = element_text(
    angle = 0,
    vjust = .5,
    hjust = 0
  ))
