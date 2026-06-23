# Description -------------------------------------------------------------
# Weight of mice fed with either low-fat- or high-fat-diet
# some of these measurement also contain data from high sucrose fed group, but these
# will be filtered out when merging with the meta file

# Setup -------------------------------------------------------------------
source(here::here("R/package_load.R"))
set.seed(100)
# Multiome samples
ids <- c("SMA-3344", "SMA-3345", "SMA-4671", "SMA-4662", "SMA-4663", "SMA-4811", "SMA-4814")

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
rev <- readxl::read_xlsx(path = here::here("data-raw/revisions_staining.xlsx"),
                         col_names = TRUE) |>
    dplyr::rename(`0` = `Weight (g) 19/8`,
                  `3` = `Weight (g) 9/9`) |>
    dplyr::select(Diet, ID, `0`, `3`)

# Prepare data ------------------------------------------------------------
## Add diet info to weight data
data_1 <- data %>%
    dplyr::inner_join(y = dplyr::select(meta, Cohort, Diet, ID),
                      by = "ID") %>%
    dplyr::relocate(c(Cohort, Diet), .after = ID)

# Test if multiome and imaging are outliers -----------------------------
df <- data_1 |>
    dplyr::full_join(rev) |>
    dplyr::mutate(mean_0 = mean(`0`, na.rm = TRUE),
                  dplyr::mutate(dplyr::across(.cols = c(`1`, `3`),
                                              .fns = ~ ifelse(is.na(mean_0) | is.na(.), NA, ((. - mean_0) / mean_0) * 100),
                                              .names = "wg_{.col}")))
id_rev <- rev$ID

# weight gain
outliers <- purrr::map(.x = c(1, 3, "wg_1", "wg_3"),
                            .f = ~ rstatix::identify_outliers(df, variable = .)) |>
    purrr::list_rbind(names_to = "groups") |>
    dplyr::select(ID, Cohort, Diet, outlier = "is.outlier")

df_2 <- df |>
    dplyr::left_join(outliers) |>
    tidyr::pivot_longer(c(wg_1, wg_3), names_to = "time", values_to = "weight_gain") |>
    tidyr::pivot_longer(c(`1`, `3`), names_to = "time_2", values_to = "weight") |>
    dplyr::mutate(is_outlier = dplyr::case_when(is.na(outlier) ~ FALSE,
                                                .default = outlier),
                  colors = dplyr::case_when(ID %in% ids ~ "multiome",
                                            ID %in% id_rev ~ "revisions",
                                            .default = as.character("not_multiome")),
                  colors_outlier = dplyr::case_when(outlier == TRUE ~ paste0(colors, "_isoutlier"),
                                                    .default = paste0(colors, "_notoutlier")),
                  Diet = factor(Diet, levels = c("LFD", "HFD"))) |>
    dplyr::filter(!(ID == "SMA-4671" & time == "wg_1")) |>
    dplyr::distinct()

p1 <- df_2 |>
    ggplot(aes(x = time, y = weight_gain)) +
    geom_boxplot(outlier.shape = NA) +
    geom_point(aes(color = colors_outlier), size = 0.5) +
    ggtitle("Weight gain from mean weight at 0") +
    geom_text(aes(label=ifelse(colors == "multiome",as.character(ID),'')),
              hjust=0,vjust=0, size = 0.5) +
    facet_wrap(~ Diet) +
    theme_classic(base_size = 7)


p2 <- df_2 |>
    ggplot(aes(x = time_2, y = weight)) +
    geom_boxplot(outlier.shape = TRUE) +
    geom_point(aes(color = colors_outlier), size = 0.5) +
    ggtitle("Weight") +
    geom_text(aes(label=ifelse(colors == "multiome",as.character(ID),'')),
              hjust=0,vjust=0, size = 0.5) +
    facet_wrap(~ Diet) +
    theme_classic(base_size = 7)

pdf(here::here("data/weight_outlier.pdf"), width = 4, height = 2)
p1 + p2 + patchwork::plot_layout(guides = "collect")
dev.off()


|> Q3 <- quantile(df$wg_3, 0.75, na.rm = TRUE)
Q1 <- quantile(df$wg_3, 0.25, na.rm = TRUE)
.IQR <- IQR(df$wg_3, na.rm = TRUE)
upper.limit <- Q3 + (1.5 * .IQR)
lower.limit <- Q1 - (1.5 * .IQR)
