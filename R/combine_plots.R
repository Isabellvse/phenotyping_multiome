source(here::here("R/weight.R"))
source(here::here("R/beta_cell_function.R"))
source(here::here("R/GTT.R"))
source(here::here("R/ITT.R"))
source(here::here("R/fasted_glucose.R"))

weight
glucose
insulin
spina_gr
spina_gbeta
gtt_perc
gtt_auc_perc
itt_perc
itt_auc_perc

# Load the library
library(ggplot2)
library(patchwork)

# Arrange plots with patchwork
layout <- (weight | glucose | insulin | spina_gr | spina_gbeta | gtt_perc | gtt_auc_perc | itt_perc | itt_auc_perc)
pdf(here::here("data/combined_plots.pdf"),
    width = 25,
    height = 5)
layout + plot_layout(widths = c(0.75, 0.75, 0.75, 0.75, 0.75, 2, 0.75, 2, 0.75))
dev.off()
