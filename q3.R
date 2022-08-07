ps.predictors = names(nhefs.nmv)[!names(nhefs.nmv) %in% c("wt82_71_bin", "qsmk")]
model = step_backwards(
  data = nhefs.nmv,
  outcome = "qsmk",
  predictors = ps.predictors,
  cutoff = .1,
  verbose = 3
)
ps.scores = fitted(model)

Q3_plot1 = ggplot(data = data.frame(treated = as.factor(nhefs.nmv$qsmk), ps = ps.scores),
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



data = nhefs.nmv[names(nhefs.nmv) %in% c("qsmk", names(coef(model)))] %>%
  pivot_longer(-qsmk, names_to = "var") %>%
  mutate(treated = as.factor(qsmk))

continuous = c("age", "dbp", "smokeintensity", "smokeyrs")
categorical = c("education", "exercise", "race", "sex")

Q3_plot_continuous = data %>%
  filter(var %in% continuous) %>%
  ggplot(aes(group = treated, color = treated, x = value)) +
  geom_density() +
  theme_classic() +
  scale_color_manual(values = c("black", "red")) +
  
  labs(x = "",
       y = "Density",
       title = "") +
  theme(axis.title.y = element_text(
    angle = 0,
    vjust = .5,
    hjust = 0
  )) +
  facet_wrap(var ~ ., scales = "free")

Q3_plot_categorical = data %>%
  filter(var %in% categorical) %>%
  group_by(treated, var, value) %>%
  summarize(absFreq = n()) %>%
  group_by(treated, var) %>% 
  mutate(relFreq = absFreq / sum(absFreq)) %>%
ggplot(aes(group = treated, color = treated, x = value, y = relFreq, fill = treated)) +
  geom_bar(stat="identity",position = "dodge")+
  theme_classic() +
  scale_color_manual(values = c("black", "red")) +
  scale_fill_manual(values = c("black","red"))+
  labs(x = "",
       y = "Density",
       title = "") +
  theme(axis.title.y = element_text(
    angle = 0,
    vjust = .5,
    hjust = 0
  )) +
  facet_wrap(var ~ ., scales = "free")

results[["Q3_plot1"]] = Q3_plot1
results[["Q3_plot_continuous"]] = Q3_plot_continuous
results[["Q3_plot_categorical"]] = Q3_plot_categorical
clean_env(keep_in_env)


