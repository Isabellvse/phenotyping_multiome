# Description -------------------------------------------------------------
# Insulin tolerance test either low-fat- or high-fat-diet
# some of these measurement also contain data from high sucrose fed group, but these
# will be filtered out when merging with the meta file
# Setup -------------------------------------------------------------------
source(here::here("R/package_load.R"))
set.seed(100)

# Load --------------------------------------------------------------------
itt <- readxl::read_xlsx(path = here::here("data-raw/all_raw_data.xlsx"),
                         sheet = "ITT",
                         col_names = TRUE,
                         col_types = c("text", rep("numeric", 7))) %>%
    dplyr::filter(week %in% c(1,3))

## change colnames
colnames(itt) <- c("ID", "week", "min_0", "min_15", "min_30", "min_60", "min_90", "min_120")

meta <- readxl::read_xlsx(path = here::here("data-raw/all_raw_data.xlsx"),
                          sheet = "overview",
                          col_names = TRUE,
                          col_types = c("numeric", rep("text", 5)),
                          range=cell_cols(1:6))

# Prepare data ------------------------------------------------------------
## Combine with meta data ----
itt_2 <- itt %>%
    dplyr::inner_join(y = dplyr::select(meta, Diet, ID),
                      by = "ID",
                      relationship = "many-to-many") %>%
    dplyr::relocate(c(Diet), .after = ID) %>%
    dplyr::mutate(Diet = factor(Diet, levels = c("LFD", "HFD")))


## Calculate % blood glucose of baseline ----
itt_perc <- itt_2 %>%
    dplyr::mutate(dplyr::across(.cols = c("min_0", "min_15", "min_30", "min_60", "min_90", "min_120"),
                                .fns = ~ ifelse(is.na(min_0) | is.na(.), NA, ((. - min_0) / min_0) * 100),
                                .names = "perc_{.col}"))
## Convert to long format ----
# bg
itt_long <- itt_perc %>%
    dplyr::select(ID, Diet, week, starts_with("min_")) %>%
    tidyr::pivot_longer(cols = c(-ID, -Diet, -week),
                        names_to = "time",
                        values_to = "bg") %>%
    dplyr::mutate(time = as.numeric(stringr::str_remove(time, "min_")),
                  time = factor(time, levels = c("0","15","30","60","90","120")))
# % bg
itt_perc_long <- itt_perc %>%
    dplyr::select(ID, Diet, week, starts_with("perc_")) %>%
    tidyr::pivot_longer(cols = c(-ID, -Diet, -week),
                        names_to = "time",
                        values_to = "bg") %>%
    dplyr::mutate(time = as.numeric(stringr::str_remove(time, "perc_min_")),
                  time = factor(time, levels = c("0","15","30","60","90","120")))
## Statistic test ----
# Is there any significant difference between LFD and HFD median blood glucose levels during the itt?
# bg
stats <- itt_long %>%
    dplyr::group_by(week, time) %>%
    rstatix::wilcox_test(bg ~ Diet,
                         ref.group = "LFD") %>%
    rstatix::adjust_pvalue(method = "fdr") %>%
    rstatix::add_significance("p") %>%
    rstatix::add_x_position(x = "time") %>%
    rstatix::add_y_position(step.increase = 0.1, fun = "max", ref.group = "LFD")

# % bg
stats_perc <- itt_perc_long %>%
    dplyr::group_by(week, time) %>%
    dplyr::filter(!time == "0") %>%
    rstatix::wilcox_test(bg ~ Diet,
                         ref.group = "LFD") %>%
    rstatix::adjust_pvalue(method = "fdr") %>%
    rstatix::add_significance("p") %>%
    rstatix::add_x_position(x = "time", group = "Diet") %>%
    rstatix::add_y_position(step.increase = 0.1, fun = "max")


# plot --------------------------------------------------------------------
diet_color <- c("LFD" = "#F8Ad4B", "HFD" = "#004B7A")

## BG ----
# Create dataframe with number of observations in each plot
n_obs <- stats %>%
    dplyr::select(week, n1, n2, group1, group2) %>%
    dplyr::distinct()

pdf(here::here("data/insulin_tolerance_test_bg.pdf"),
    width = 11,
    height = 7)
ggpubr::ggline(itt_long,
               x = "time",
               y = "bg",
               color = "Diet",
               add =  c("mean_sd"),
               facet.by = "week",
               ncol = 4,
               size = 1,
               palette = diet_color,
               title = "itt - raw",
               ylab = "Blood Glucose (mmol / L)",
               xlab = "Time after insulin injection (min)") +
    ggpubr::stat_pvalue_manual(stats,
                               x = "x",
                               label = "p.signif",
                               remove.bracket = TRUE,
                               size = 5) +
    ggplot2::geom_jitter(data = itt_long,
                         aes(x = time,
                             y = bg,
                             color = Diet),
                         width = 0.1,
                         alpha = 0.5) +
    ggplot2::geom_text(
        data = n_obs,
        aes(x = Inf, y = Inf, label = paste("n(LFD) =", n1, ", n(HFD) =", n2)),
        size = 3,
        vjust = 2,
        hjust = 1.05) +
    ggplot2::scale_y_continuous(breaks = seq(0, 15, by = 2)) +
    ggprism::theme_prism(border = TRUE,
                         base_size = 15,
                         base_fontface = "plain") +
    ggplot2::theme(legend.position = "top") +
    ggplot2::coord_cartesian(clip = "off")
dev.off()

## without points
pdf(here::here("data/insulin_tolerance_test_bg_no_pt.pdf"),
    width = 10,
    height = 7)
ggpubr::ggline(itt_long,
               x = "time",
               y = "bg",
               color = "Diet",
               add =  c("mean_sd"),
               facet.by = "week",
               ncol = 4,
               size = 1,
               palette = diet_color,
               ylab = "Blood Glucose (mmol / L)",
               xlab = "Time after insulin injection (min)") +
    ggpubr::stat_pvalue_manual(stats,
                               x = "x",
                               label = "p.signif",
                               remove.bracket = TRUE,
                               size = 5) +
    ggplot2::geom_text(
        data = n_obs,
        aes(x = Inf, y = Inf, label = paste("n(LFD) =", n1, ", n(HFD) =", n2)),
        size = 3,
        vjust = 2,
        hjust = 1.05) +
    ggplot2::scale_y_continuous(breaks = seq(0, 15, by = 2)) +
    ggprism::theme_prism(border = TRUE,
                         base_size = 15,
                         base_fontface = "plain") +
    ggplot2::theme(legend.position = "top") +
    ggplot2::coord_cartesian(clip = "off")
dev.off()

## Perc ----
n_obs <- stats_perc %>%
    dplyr::select(week, n1, n2, group1, group2) %>%
    dplyr::distinct()

pdf(here::here("data/insulin_tolerance_test_perc.pdf"),
    width = 11,
    height = 7)
itt_perc <- ggpubr::ggline(itt_perc_long,
               x = "time",
               y = "bg",
               color = "Diet",
               add =  c("mean_sd"),
               facet.by = "week",
               ncol = 4,
               size = 1,
               palette = diet_color,
               title = "itt - normalized",
               ylab = "% Change in blood glucose from baseline (T = 0)",
               xlab = "Time after insulin injection (min)") +
    ggpubr::stat_pvalue_manual(stats_perc,
                               x = "x",
                               label = "p.signif",
                               remove.bracket = TRUE,
                               size = 5) +
    ggplot2::geom_jitter(data = itt_perc_long,
                         aes(x = time,
                             y = bg,
                             color = Diet),
                         width = 0.1,
                         alpha = 0.5) +
    ggplot2::geom_text(
        data = n_obs,
        aes(x = Inf, y = Inf, label = paste("n(LFD) =", n1, ", n(HFD) =", n2)),
        size = 3,
        vjust = 2,
        hjust = 1.05) +
    ggplot2::scale_y_continuous(breaks = seq(-80, 100, by = 20)) +
    ggprism::theme_prism(border = TRUE,
                         base_size = 15,
                         base_fontface = "plain") +
    ggplot2::theme(legend.position = "top") +
    ggplot2::coord_cartesian(clip = "off")
print(itt_perc)
dev.off()

# not pt
pdf(here::here("data/insulin_tolerance_test_perc_no_pt.pdf"),
    width = 15,
    height = 7)
ggpubr::ggline(itt_perc_long,
               x = "time",
               y = "bg",
               color = "Diet",
               add =  c("mean_sd"),
               facet.by = "week",
               ncol = 4,
               size = 1,
               palette = diet_color,
               ylab = "% change in blood glucose from baseline (T = 0)",
               xlab = "Time after insulin injection (min)") +
    ggpubr::stat_pvalue_manual(stats_perc,
                               x = "x",
                               label = "p.signif",
                               remove.bracket = TRUE,
                               size = 5) +
    ggplot2::geom_text(
        data = n_obs,
        aes(x = Inf, y = Inf, label = paste("n(LFD) =", n1, ", n(HFD) =", n2)),
        size = 3,
        vjust = 2,
        hjust = 1.05) +
    ggplot2::scale_y_continuous(breaks = seq(-80, 100, by = 20)) +
    ggprism::theme_prism(border = TRUE,
                         base_size = 15,
                         base_fontface = "plain") +
    ggplot2::theme(legend.position = "top") +
    ggplot2::coord_cartesian(clip = "off")
dev.off()


# AREA UNDER CURVE --------------------------------------------------------
## BG ----
itt_auc <- itt_long %>%
    dplyr::filter(week %in% c(1, 3)) %>%
    dplyr::mutate(time = as.numeric(time)) %>%
    dplyr::group_by(ID, Diet, week) %>%
    dplyr::summarise(auc = DescTools::AUC(x = time,
                                          y = bg))

## PERC ----
itt_perc_auc <- itt_perc_long %>%
    dplyr::filter(week %in% c(1, 3)) %>%
    dplyr::mutate(time = as.numeric(time)) %>%
    dplyr::group_by(Diet, week, ID ) %>%
    dplyr::summarise(auc = DescTools::AUC(x = time,
                                          y = bg)) %>%
    dplyr::group_by(Diet, week, ID)

## Statistic test ----
# Is there any significant difference between LFD and HFD median blood glucose levels during the itt?
# bg
stats_auc <- itt_auc %>%
    dplyr::filter(week %in% c(1,3)) %>%
    dplyr::group_by(week) %>%
    rstatix::wilcox_test(auc ~ Diet,
                         ref.group = "LFD") %>%
    rstatix::adjust_pvalue(method = "fdr") %>%
    rstatix::add_significance("p") %>%
    rstatix::add_x_position(x = "week") %>%
    rstatix::add_y_position(step.increase = 0.1, fun = "max", ref.group = "LFD")

# % bg
stats_perc_auc <- itt_perc_auc %>%
    dplyr::filter(week %in% c(1,3)) %>%
    dplyr::group_by(week) %>%
    rstatix::wilcox_test(auc ~ Diet,
                         ref.group = "LFD") %>%
    rstatix::adjust_pvalue(method = "fdr") %>%
    rstatix::add_significance("p") %>%
    rstatix::add_x_position(x = "week") %>%
    rstatix::add_y_position(step.increase = 0.1, fun = "max", ref.group = "LFD")

# plot --------------------------------------------------------------------
## BG ----
pdf(here::here("data/insulin_tolerance_test_AUC.pdf"),
    width = 9.18,
    height = 9.18)
itt_auc %>% dplyr::filter(week %in% c(1, 3)) %>%
    ungroup() %>%
    ggpubr::ggboxplot(x = "week",
                      y = "auc",
                      fill = "Diet",
                      color = "Diet",
                      add = "jitter",
                      size = 0.5,
                      palette = diet_color,
                      lwd = 1,
                      fatten = 0.5,
                      title = "itt - Raw data",
                      ylab = "Area under the Curve",
                      xlab = "Weeks on diet") +
    ggplot2::scale_color_manual(values = c("black", "black")) +
    ggpubr::stat_pvalue_manual(stats_auc,
                               x = "x",
                               label = "p.signif",
                               remove.bracket = TRUE,
                               size = 5) +
    ggprism::theme_prism(border = TRUE,
                         base_size = 15,
                         base_fontface = "plain") +
    ggplot2::theme(legend.position = "top") +
    ggplot2::coord_cartesian(clip = "off")

## PERC ----
itt_auc_perc <- itt_perc_auc %>% dplyr::filter(week %in% c(1, 3)) %>%
    ungroup() %>%
    ggpubr::ggboxplot(x = "week",
                      y = "auc",
                      fill = "Diet",
                      color = "Diet",
                      add = "jitter",
                      size = 0.5,
                      palette = diet_color,
                      lwd = 1,
                      fatten = 0.5,
                      title = "itt - Normalized",
                      ylab = "Area under the Curve",
                      xlab = "Weeks on diet") +
    ggplot2::scale_color_manual(values = c("black", "black")) +
    ggpubr::stat_pvalue_manual(stats_perc_auc,
                               x = "x",
                               label = "p.signif",
                               remove.bracket = TRUE,
                               size = 5) +
    ggprism::theme_prism(border = TRUE,
                         base_size = 15,
                         base_fontface = "plain") +
    ggplot2::theme(legend.position = "top") +
    ggplot2::coord_cartesian(clip = "off")
print(itt_auc_perc)
dev.off()


# save --------------------------------------------------------------------
openxlsx::write.xlsx(list("itt" = itt_long,
                          "itt_perc" = itt_perc_long,
                          "itt_auc" = itt_auc,
                          "itt_perc_auc" = itt_perc_auc), here::here("data/documents/itt.xlsx"))


openxlsx::write.xlsx(list("itt" = stats,
                          "itt_perc" = stats_perc,
                          "itt_auc" = stats_auc,
                          "itt_perc_auc" = stats_perc_auc), here::here("data/documents/itt_wilcox.xlsx"))
