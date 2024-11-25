# Description -------------------------------------------------------------
# Fasted glucose levels with either low-fat- or high-fat-diet
# some of these measurement also contain data from high sucrose fed group, but these
# will be filtered out when merging with the meta file
# Setup -------------------------------------------------------------------
source(here::here("R/package_load.R"))
set.seed(100)

# Load --------------------------------------------------------------------
gtt <- readxl::read_xlsx(path = here::here("data-raw/all_raw_data.xlsx"),
                          sheet = "GTT",
                          col_names = TRUE,
                          col_types = c("text", rep("numeric", 7))) %>%
    dplyr::select(ID, week, `0`) %>%
    dplyr::mutate(test = "gtt")

itt <- readxl::read_xlsx(path = here::here("data-raw/all_raw_data.xlsx"),
                         sheet = "ITT",
                         col_names = TRUE,
                         col_types = c("text", rep("numeric", 7))) %>%
    dplyr::select(ID, week, `0`) %>%
    dplyr::mutate(test = "itt")

homa <- readxl::read_xlsx(path = here::here("data-raw/all_raw_data.xlsx"),
                         sheet = "HOMA_bg",
                         col_names = TRUE,
                         col_types = c("text", rep("numeric", 2))) %>%
    dplyr::mutate(test = "homa")

meta <- readxl::read_xlsx(path = here::here("data-raw/all_raw_data.xlsx"),
                          sheet = "overview",
                          col_names = TRUE,
                          col_types = c("numeric", rep("text", 5)),
                          range=cell_cols(1:6))


# Prepare data ------------------------------------------------------------
## combine data ----
data <- dplyr::bind_rows(gtt, itt, homa)
colnames(data) <- c("ID", "week", "bg", "test")

## Add diet info to weight data
fasted_bg <- data %>%
    dplyr::inner_join(y = dplyr::select(meta, Diet, ID),
                      by = "ID",
                      relationship = "many-to-many") %>%
    dplyr::relocate(c(Diet), .after = ID) %>%
    dplyr::mutate(Diet = factor(Diet, levels = c("LFD", "HFD")))

# Perform non parametric Wilcoxon test ------------------------------------
## Get summary statistics - median and interquartile range ----
fasted_bg %>%
    dplyr::select(ID, Diet, week, bg) %>%
    dplyr::group_by(week, Diet) %>%
    rstatix::get_summary_stats(type = "median_iqr")

## Statistic test ----
# Is there any significant difference between LFD and HFD median fasted blood glucose levels?
stats <- fasted_bg %>%
    dplyr::group_by(week) %>%
    rstatix::wilcox_test(bg ~ Diet,
                         ref.group = "LFD") %>%
    rstatix::adjust_pvalue(method = "fdr") %>%
    rstatix::add_significance("p") %>%
    rstatix::add_x_position(x = "week", group = "Diet") %>%
    rstatix::add_y_position(step.increase = 0.1, fun = "max")

## Effect size ----
# How large the effect is compared to random noise
fasted_bg%>%
    dplyr::group_by(week) %>%
    rstatix::wilcox_effsize(bg ~ Diet,
                            ref.group = "LFD")


# Plot --------------------------------------------------------------------
diet_color <- c("LFD" = "#F8Ad4B", "HFD" = "#004B7A")

pdf(here::here("data/fasted_blood_glucose_boxplot.pdf"),
    width = 5,
    height = 9,18)
fasted_bg %>%
    ggpubr::ggboxplot(x = "week",
                      y = "bg",
                      fill = "Diet",
                      color = "Diet",
                      add = "jitter",
                      size = 0.5,
                      palette = diet_color,
                      lwd = 1,
                      fatten = 0.5) +
    ggplot2::scale_color_manual(values = c("black", "black")) +
    ggplot2::labs(
        y = "Fasted Blood Glucose (mmol / L)",
        x = "Weeks on diet",
        fill = "Diet") +
    ggplot2::geom_text(
        data = stats,
        aes(x = x, y = y.position + 1, label = paste("n =", n1, ", n =", n2)),
        size = 3) +
    ggpubr::stat_pvalue_manual(stats,  label = "p.signif", bracket.size = 1, size = 5) +
    ggprism::theme_prism(border = TRUE,
                         base_size = 15,
                         base_fontface = "plain") +
    ggplot2::theme(legend.position = "top") +
    ggplot2::coord_cartesian(clip = "off")
dev.off()

## 1 week and 3 weeks ----
stats <- fasted_bg %>%
    dplyr::filter(week %in% c("1", "3")) %>%
    dplyr::group_by(week) %>%
    rstatix::wilcox_test(bg ~ Diet,
                         ref.group = "LFD") %>%
    rstatix::adjust_pvalue(method = "fdr") %>%
    rstatix::add_significance("p") %>%
    rstatix::add_x_position(x = "week", group = "Diet") %>%
    rstatix::add_y_position(step.increase = 0.5, fun = "max")

# Stats bg between time
stats_time <- fasted_bg %>%
    dplyr::filter(week %in% c("1", "3")) %>%
    dplyr::group_by(Diet) %>%
    rstatix::wilcox_test(bg ~ week,
                         ref.group = "1") %>%
    rstatix::adjust_pvalue(method = "fdr") %>%
    rstatix::add_significance("p") %>%
    rstatix::add_x_position(x = "week", group = "Diet") %>%
    rstatix::add_y_position(step.increase = 3, fun = "mean_sd")

pdf(here::here("data/fasted_blood_glucose_boxplot_1_and_3.pdf"),
    width = 5,
    height = 9,18)
glucose <- fasted_bg %>%
    dplyr::filter(week %in% c("1", "3")) %>%
    ggpubr::ggboxplot(x = "week",
                      y = "bg",
                      fill = "Diet",
                      color = "Diet",
                      add = "jitter",
                      size = 0.5,
                      palette = diet_color,
                      lwd = 1,
                      fatten = 0.5,
                      title = "Fasted glucose") +
    ggplot2::scale_color_manual(values = c("black", "black")) +
    ggplot2::labs(
        y = "Fasted Blood Glucose (mmol / L)",
        x = "Weeks on diet",
        fill = "Diet") +
    ggplot2::scale_y_continuous(breaks = seq(0, 13, by = 2)) +
    ggplot2::expand_limits(y = 0) +
    ggplot2::geom_text(
        data = stats,
        aes(x = x, y = y.position + 0.5, label = paste("n =", n1, ", n =", n2)),
        size = 3) +
    ggpubr::stat_pvalue_manual(stats,  label = "p.signif", bracket.size = 1, size = 5) +
    ggpubr::stat_pvalue_manual(stats_time,  label = "p.signif", bracket.size = 1, size = 5) +
    ggprism::theme_prism(border = TRUE,
                         base_size = 15,
                         base_fontface = "plain") +
    ggplot2::theme(legend.position = "top") +
    ggplot2::coord_cartesian(clip = "off")
print(glucose)
dev.off()


# save --------------------------------------------------------------------
openxlsx::write.xlsx(fasted_bg, here::here("data/documents/fasted_bg.xlsx"))

