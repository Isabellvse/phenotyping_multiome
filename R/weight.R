# Description -------------------------------------------------------------
# Weight of mice fed with either low-fat- or high-fat-diet
# some of these measurement also contain data from high sucrose fed group, but these
# will be filtered out when merging with the meta file

# Setup -------------------------------------------------------------------
source(here::here("R/package_load.R"))
set.seed(100)


# Load --------------------------------------------------------------------
data <- readxl::read_xlsx(path = here::here("data-raw/all_raw_data.xlsx"),
                          sheet = "weight",
                          col_names = TRUE,
                          col_types = c("text", rep("numeric", 6)))

meta <- readxl::read_xlsx(path = here::here("data-raw/all_raw_data.xlsx"),
                          sheet = "overview",
                          col_names = TRUE,
                          col_types = c("numeric", rep("text", 5)),
                          range=cell_cols(1:6))

# Prepare data ------------------------------------------------------------
## Add diet info to weight data
data_1 <- data %>%
    dplyr::inner_join(y = dplyr::select(meta, Cohort, Diet, ID),
                      by = "ID") %>%
    dplyr::relocate(c(Cohort, Diet), .after = ID)

## Calculate % weight gain
weight_gain <- data_1 %>%
    dplyr::mutate(dplyr::across(.cols = c(`1`, `2`, `3`, `5`, `6`),
                                .fns = ~ ifelse(is.na(`0`) | is.na(.), NA, ((. - `0`) / `0`) * 100),
                                .names = "week_{.col}"))

## to save
weight_gain_save <- weight_gain
colnames(weight_gain_save) <- c("ID", "Cohort", "Diet", "Week 0", "Week 1", "Week 2", "Week 3", "Week 5", "Week 6", "Weight gain wk 1", "Weight gain wk 3",
                           "Weight gain wk 3", "Weight gain wk 5", "Weight gain wk 6")

openxlsx::write.xlsx(weight_gain_save, here::here("data/documents/weight.xlsx"))
# Check assumptions for t.test --------------------------------------------
## Outliers ----
# week 1 has 3 outliers
outliers_list <- purrr::map(.x = colnames(weight_gain)[10:14],
                            .f = ~ rstatix::identify_outliers(weight_gain, variable = .))
# # A tibble: 3 × 16
# ID       Cohort Diet    `0`   `1`   `2`   `3`   `5`   `6` week_1 week_2 week_3 week_5 week_6 is.outlier
# <chr>     <dbl> <chr> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl> <lgl>
# 1 SMA-2473      3 LFD    29.8  27.7    NA    NA    NA    NA  -7.01     NA     NA     NA     NA TRUE
# 2 SMA-2473      6 LFD    29.8  27.7    NA    NA    NA    NA  -7.01     NA     NA     NA     NA TRUE
# 3 SMA-2474      3 LFD    26.0  31.3    NA    NA    NA    NA  20.3      NA     NA     NA     NA TRUE

## Normality assumption ----
# From the output, the p-value is NOT greater than the significance level 0.05
# indicating that the distribution of the data are significantly different from
# the normal distribution. In other words, we can NOT assume the normality.
normality_list <- purrr::map(.x = colnames(weight_gain)[10:14],
                            .f = ~ rstatix::shapiro_test(data = weight_gain,
                                                         vars = .))

# Perform non parametric Wilcoxon test ------------------------------------
## Get summary statistics - median and interquartile range
statistics_list <- purrr::map(.x = colnames(weight_gain)[10:14],
                              .f = ~ weight_gain[, c("ID", "Diet" , .)] %>%
                                  dplyr::group_by(Diet) %>%
                                  rstatix::get_summary_stats(., type = "median_iqr"))

## Make long  format ----
weight_gain_long <- weight_gain %>%
    dplyr::select("ID", "Diet", starts_with("week_")) %>%
    tidyr::pivot_longer(cols = starts_with("week_"),
                        names_to = "week",
                        values_to = "weight_gain") %>%
    rstatix::drop_na() %>%
    dplyr::mutate(Diet = factor(Diet, levels = c("LFD", "HFD")))

## Statistic test ----
# Is there any significant difference between LFD and HFD median weight gains?
stats <- weight_gain_long %>%
    dplyr::group_by(week) %>%
    rstatix::wilcox_test(weight_gain ~ Diet,
                         ref.group = "LFD") %>%
    rstatix::adjust_pvalue(method = "fdr") %>%
    rstatix::add_significance("p") %>%
    rstatix::add_x_position(x = "week", group = "Diet") %>%
    rstatix::add_y_position(step.increase = 0.1, fun = "max")

## Effect size ----
# How large the effect is compared to random noise
weight_gain_long %>%
    dplyr::group_by(week) %>%
    rstatix::wilcox_effsize(weight_gain ~ Diet,
                            ref.group = "LFD")

## Visualization
diet_color <- c("LFD" = "#F8Ad4B", "HFD" = "#004B7A")

pdf(here::here("data/weight_gain_boxplot.pdf"),
    width = 5,
    height = 9,18)
weight_gain_long %>%
    dplyr::mutate(week = as.numeric(stringr::str_remove(week, "week_"))) %>%
    ggpubr::ggboxplot(x = "week",
                      y = "weight_gain",
                      fill = "Diet",
                      color = "Diet",
                      add = "jitter",
                      size = 0.5,
                      palette = diet_color,
                      lwd = 1,
                      fatten = 0.5) +
    ggplot2::scale_color_manual(values = c("black", "black")) +
    ggplot2::labs(
        y = "% change from start weight (week 0)",
        x = "Weeks on diet",
        fill = "Diet",
        caption = "Wilcoxon test, ns: fdr > 0.05, *: fdr <= 0.05, **: fdr <= 0.01, ***: fdr <= 0.001, ****: fdr <= 0.0001") +
    ggplot2::geom_text(
        data = stats,
        aes(x = x, y = y.position + 5, label = paste("n =", n1, ", n =", n2)),
        size = 3) +
    ggpubr::stat_pvalue_manual(stats,  label = "p.signif", bracket.size = 1, size = 5) +
    ggprism::theme_prism(border = TRUE,
                         base_size = 15,
                         base_fontface = "plain") +
    ggplot2::theme(legend.position = "top") +
    ggplot2::coord_cartesian(clip = "off")
dev.off()


# 1-3 weeks ---------------------------------------------------------------
stats_filter <- stats %>%
    dplyr::filter(week %in% c("week_1", "week_2", "week_3"))

pdf(here::here("data/weight_gain_boxplot_1_to_3_weeks.pdf"),
    width = 5,
    height = 9,18)
weight_gain_long %>%
    dplyr::filter(week %in% c("week_1", "week_2", "week_3")) %>%
    dplyr::mutate(week = as.numeric(stringr::str_remove(week, "week_"))) %>%
    ggpubr::ggboxplot(x = "week",
                      y = "weight_gain",
                      fill = "Diet",
                      color = "Diet",
                      add = "jitter",
                      size = 0.5,
                      palette = diet_color,
                      lwd = 1,
                      fatten = 0.5) +
    ggplot2::scale_color_manual(values = c("black", "black")) +
    ggplot2::labs(
        y = "% change from start weight (week 0)",
        x = "Weeks on diet",
        fill = "Diet",
        caption = "Wilcoxon test, ns: fdr > 0.05, *: fdr <= 0.05, **: fdr <= 0.01, ***: fdr <= 0.001, ****: fdr <= 0.0001") +
    ggplot2::geom_text(
        data = stats_filter,
        aes(x = x, y = y.position + 5, label = paste("n =", n1, ", n =", n2)),
        size = 3) +
    ggpubr::stat_pvalue_manual(stats_filter,  label = "p.signif", bracket.size = 1, size = 5) +
    ggprism::theme_prism(border = TRUE,
                         base_size = 15,
                         base_fontface = "plain") +
    ggplot2::theme(legend.position = "top") +
    ggplot2::coord_cartesian(clip = "off")
dev.off()

# 1 and 3 weeks ---------------------------------------------------------------
stats_filter <- stats %>%
    dplyr::filter(week %in% c("week_1", "week_3")) %>%
    dplyr::mutate(x = c(1,2),
                  xmin = c(0.8, 1.8),
                  xmax = c(1.2, 2.2))


pdf(here::here("data/weight_gain_boxplot_1_and_3_weeks.pdf"),
    width = 5,
    height = 9,18)
weight <- weight_gain_long %>%
    dplyr::filter(week %in% c("week_1", "week_3")) %>%
    dplyr::mutate(week = as.numeric(stringr::str_remove(week, "week_"))) %>%
    ggpubr::ggboxplot(x = "week",
                      y = "weight_gain",
                      fill = "Diet",
                      color = "Diet",
                      add = "jitter",
                      size = 0.5,
                      palette = diet_color,
                      lwd = 1,
                      fatten = 0.5,
                      title = "Weight gain") +
    ggplot2::scale_color_manual(values = c("black", "black")) +
    ggplot2::expand_limits(y = 0) +
    ggplot2::labs(
        y = "% change from start weight (week 0)",
        x = "Weeks on diet",
        fill = "Diet") +
    ggplot2::geom_text(
        data = stats_filter,
        aes(x = x, y = y.position + 5, label = paste("n =", n1, ", n =", n2)),
        size = 3) +
    ggpubr::stat_pvalue_manual(stats_filter,  label = "p.signif", bracket.size = 1, size = 5) +
    ggprism::theme_prism(border = TRUE,
                         base_size = 15,
                         base_fontface = "plain") +
    ggplot2::theme(legend.position = "top") +
    ggplot2::coord_cartesian(clip = "off")
print(weight)
dev.off()

