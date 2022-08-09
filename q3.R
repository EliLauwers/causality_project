# Q3 estimate propensity scores
# Steps:
# 1: build logistic regression model for treatment using backwards step regression
# 2: extract fitted values as propensity scores
# 3: Create propensity plot
# 4: preprocess data for easy plotting
# 5: Create density plots for continuous data
# 6: Create histograms for categorical data

# STEP 1: build logistic regression model for treatment using backwards step regression
ps.predictors = names(nhefs.nmv)[!names(nhefs.nmv) %in% c("wt82_71_bin", "qsmk")]
model = step_backwards(
  data = nhefs.nmv,
  outcome = "qsmk",
  predictors = ps.predictors,
  cutoff = .1,
  verbose = 3
)

# STEP 2: extract fitted values as propensity scores
ps.scores = fitted(model)

# STEP 3: Create propensity plot
Q3_plot1 = ggplot(data = data.frame(treated = as.factor(nhefs.nmv$qsmk), ps = ps.scores),
                  aes(x = ps, group = treated, color = treated)) +
  geom_density() +
  xlim(0, 1) +
  theme_classic() +
  scale_color_manual(values = c("black", "red")) +
  labs(x = "Propensity score",
       y = "Density",
       title = "Distribution of Propensity scores across levels of treatment \n and distributions of underlying variables") +
  theme(axis.title.y = element_text(
    angle = 90,
    vjust = .5,
    hjust = .5
  ),
  legend.position = "none")

d1 = density(ps.scores[nhefs.nmv$qsmk==1], from = 0, to = 1)
d0 = density(ps.scores[nhefs.nmv$qsmk==0], from = 0, to = 1)
difference_in = data.frame(x = d1$x, dif = d1$y - d0$y, d1y = d1$y, d0y=d0$y) %>% 
  ggplot(aes(x = x)) + 
  geom_hline(yintercept = 0, color = "red", linetype ="dashed") + 
  geom_line(aes(y=.01 * dif)) + 
  labs(y = "Difference in density(%)",
       x= "Propensity score")+
  scale_y_continuous(labels = scales::percent, limits = .01 * c(-3,3))+
  # geom_density(aes(x = x, y = d1y), stat = "identity") + 
  # geom_density(aes(x = x, y = d0y), stat = "identity", color = "red") +
  theme_classic()




# STEP 4: Preprocess data for easy plotting
continuous = c("age","dbp","smokeyrs","smokeintensity")
categorical = names(coef(model))[!names(coef(model)) %in% c(continuous, "(Intercept)")]

plots = list()
for (col in continuous) {
  plots[[col]] = ggplot(
    mutate(nhefs.nmv, treated = as.factor(qsmk)),
    aes_string(x = col,
               group = "treated",
               color = "treated")
  ) +
    geom_density() +
    labs(title = col,
         y = "Density",
         x = element_blank()) +
    theme_classic() +
    scale_color_manual(values = c("black", "red")) +
    theme(
      legend.position = "none",
      plot.title = element_text(hjust = .1, size = 15),
      plot.margin = margin(6, 0, 6, 0)
    )
}

for (col in categorical) {
  plots[[col]] = ggplot(
    mutate(nhefs.nmv, treated = as.factor(qsmk)),
    aes_string(
      x = col,
      y = "..prop..",
      group = "treated",
      color = "treated",
      fill = "treated"
    )
  ) +
    geom_bar(position = "dodge2", alpha = .2) +
    labs(title = col,
         y = "density",
         x = element_blank()) +
    scale_fill_manual(values = c("black", "red")) +
    scale_x_continuous(breaks = sort(unique(nhefs.nmv[[col]]))) +
    theme_classic() +
    scale_color_manual(values = c("black", "red")) +
    theme(
      legend.position = "none",
      plot.title = element_text(hjust = .1, size = 15),
      plot.margin = margin(6, 0, 6, 0)
    )
}


grid = ggpubr::ggarrange(
  plots[[continuous[1]]]  + ylab(NULL),
  plots[[continuous[2]]]  + ylab(NULL),
  plots[[continuous[3]]]  + ylab(NULL),
  plots[[continuous[4]]]  + ylab(NULL),
  plots[[categorical[1]]]  + ylab(NULL),
  plots[[categorical[2]]]  + ylab(NULL),
  plots[[categorical[3]]]  + ylab(NULL),
  plots[[categorical[4]]]  + ylab(NULL),
  common.legend = T,
  ncol = 2,
  nrow = 4,
  legend = "bottom",
  font.label = list(
    size = 10,
    color = "black",
    face = "bold",
    family = NULL,
    position = "top"
  )
)

grid = ggpubr::annotate_figure(grid,
                               left = grid::textGrob(
                                 "Density",
                                 gp = grid::gpar(cex = 1),
                                 rot = 90,
                                 vjust = .5
                               ))

grid


# Store results and clean environment
results[["Q3_plot1"]] = Q3_plot1
results[["Q3_plot_grid"]] = grid
# results[["Q3_plot_categorical"]] = Q3_plot_categorical
clean_env(keep_in_env)
