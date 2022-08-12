


estimators = c("aiptw.values", "TMLE.values")
data = NULL
for (estim in estimators) {
  techniques = names(results[[estim]])
  for (technique in techniques) {
    estimates = names(results[[estim]][[technique]])
    for (est in estimates) {
      values = unname(unlist(results[[estim]][[technique]][[est]]))
      tmp = data.frame(
        val = values,
        technique = technique,
        estimator = estim,
        estimate = est
      )
      if (!is.null(data)) {
        data = bind_rows(data, tmp)
      } else {
        data = tmp
      }
    }
  }
}


plots = list(aiptw = list(), tmle = list())

for (estim in c("aiptw.values", "TMLE.values")) {
  techniques = c("tutorial", "superdefault", "supercustom")
  for (techn in techniques) {
    print(glue::glue("{estim}: {technique}"))
    tmp = filter(data, estimator == estim, technique == techn)
    ey1_mean = mean(tmp[tmp$estimate == "EY1",]$val)
    ey0_mean = mean(tmp[tmp$estimate == "EY0",]$val)
    p = ggplot(data = tmp,
               aes(
                 x = val,
                 group = estimate,
                 color = estimate,
                 fill = estimate
               )) + geom_density(alpha = 0) +
      theme_classic() +
      scale_color_manual(values = c("black", "red")) +
      theme(
        legend.position = "none",
        plot.title = element_text(hjust = .1, size = 15),
        plot.margin = margin(6, 0, 6, 0)
      ) +
      geom_vline(
        xintercept = ey1_mean,
        color = "red",
        show.legend = F,
        linetype = "dashed"
      ) +
      geom_vline(
        xintercept = ey0_mean,
        color = "black",
        show.legend = F,
        linetype = "dashed"
      )
    
    if (estim == "TMLE.values") {
      p = p + xlim(0, 1)
    }
    
    plots[[estim]][[techn]] = p
    
  }
}

grid = ggpubr::ggarrange(
  plots[["aiptw.values"]][["tutorial"]]     + labs(y = element_blank(), x = element_blank(), title = "tutorial"),
  plots[["TMLE.values"]][["tutorial"]]      + labs(y = element_blank(), x = element_blank(), title = element_blank()),
  plots[["aiptw.values"]][["superdefault"]] + labs(y = element_blank(), x = element_blank(), title = "superdefault"),
  plots[["TMLE.values"]][["superdefault"]]  + labs(y = element_blank(), x = element_blank(), title = element_blank()),
  plots[["aiptw.values"]][["supercustom"]]  + labs(y = element_blank(), x = element_blank(), title = "supercustom"),
  plots[["TMLE.values"]][["supercustom"]]   + labs(y = element_blank(), x = element_blank(), title = element_blank()),
  
  
  common.legend = T,
  ncol = 2,
  nrow = 3,
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
results[["discussion_plot"]] = grid
