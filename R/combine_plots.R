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
gtt_raw
itt_raw

# Load the library
library(ggplot2)
library(patchwork)

my_theme <- function() {
    ggplot2::theme_classic(base_size = 7) +
        ggplot2::theme(
            panel.grid.major = ggplot2::element_line(color = "grey85"),
            panel.grid.minor = ggplot2::element_line(color = "grey95")
        )
}

layout <- (plot_spacer() | weight | glucose | insulin | spina_gr | spina_gbeta | gtt_perc | gtt_auc_perc | itt_perc | itt_auc_perc) & my_theme() +
    ggplot2::theme(legend.position = "none", strip.text = ggplot2::element_blank())
pdf(here::here("data/combined_plots.pdf"),
    width = 12,
    height = 2)
layout  + plot_layout(widths = c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 1, 0.5, 1, 0.5))
dev.off()

pdf(here::here("data/combined_plots_raw.pdf"),
    width = 4,
    height = 2)
(gtt_raw | itt_raw) & my_theme() +
    ggplot2::theme(legend.position = "none", strip.text = ggplot2::element_blank())
dev.off()

