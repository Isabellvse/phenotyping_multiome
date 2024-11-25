# Description -------------------------------------------------------------
# Fasted insulin levels and SPINA analysis with either low-fat- or high-fat-diet
# Setup -------------------------------------------------------------------
source(here::here("R/package_load.R"))
source(here::here("R/spina_functions.r"))
set.seed(100)


# Load --------------------------------------------------------------------
ins <- readxl::read_xlsx(path = here::here("data-raw/all_raw_data.xlsx"),
                         sheet = "HOMA_elisa",
                         col_names = TRUE,
                         col_types = c("text", "text", rep("numeric", 3)))

meta <- readxl::read_xlsx(path = here::here("data-raw/all_raw_data.xlsx"),
                          sheet = "overview",
                          col_names = TRUE,
                          col_types = c("numeric", rep("text", 5)),
                          range=cell_cols(1:6))


# Prepare data ------------------------------------------------------------

## Add dilutation factor to od
data <- ins %>%
    dplyr::mutate(od_dilutation = od_corrected*dilutation)


# plate 1 -----------------------------------------------------------------

## get standard for plate 1
standard_1 <- data %>%
    dplyr::filter(plate == 1 & week == "stanard")

## linear model lm(y~x)
model_1 <- lm(standard_1$od_dilutation ~ as.numeric(standard_1$ID))

# Get coefficients
intercept_1 <- coef(model_1)[1]
slope_1 <- coef(model_1)[2]

# calulcate insulin
ins_1 <- data %>%
    dplyr::filter(plate == 1 & !week == "stanard") %>%
    dplyr::mutate(insulin = (od_dilutation - intercept_1) / slope_1)

# plot
plot(as.numeric(standard_1$ID), standard_1$od_dilutation,
     xlab = "Insulin (ng/ml",
     ylab = "OD450-630",
     main = "Plate 1")
abline(model_1)

# plate 2 -----------------------------------------------------------------

## get standard for plate 2
standard_2 <- data %>%
    dplyr::filter(plate == 2 & week == "stanard")

## linear model lm(y~x)
model_2 <- lm(standard_2$od_dilutation ~ as.numeric(standard_2$ID))

# Get coefficients
intercept_2 <- coef(model_2)[1]
slope_2 <- coef(model_2)[2]

# calulcate insulin
ins_2 <- data %>%
    dplyr::filter(plate == 2 & !week == "stanard") %>%
    dplyr::mutate(insulin = (od_dilutation - intercept_2) / slope_2)

# plot
plot(as.numeric(standard_2$ID), standard_2$od_dilutation,
     xlab = "Insulin (ng/ml",
     ylab = "OD450-630",
     main = "Plate 2")
abline(model_2)

# plate 3 -----------------------------------------------------------------

## get standard for plate 3
standard_3 <- data %>%
    dplyr::filter(plate == 3 & week == "stanard")

## linear model lm(y~x)
model_3 <- lm(standard_3$od_dilutation ~ as.numeric(standard_3$ID))

# Get coefficients
intercept_3 <- coef(model_3)[1]
slope_3 <- coef(model_3)[2]

# calulcate insulin
ins_3 <- data %>%
    dplyr::filter(plate == 3 & !week == "stanard") %>%
    dplyr::mutate(insulin = (od_dilutation - intercept_3) / slope_3)

# plot
plot(as.numeric(standard_3$ID), standard_3$od_dilutation,
     xlab = "Insulin (ng/ml",
     ylab = "OD450-630",
     main = "Plate 3")
abline(model_3)

# plate 4 -----------------------------------------------------------------

## get standard for plate 4
standard_4 <- data %>%
    dplyr::filter(plate == 4 & week == "stanard")

## linear model lm(y~x)
model_4 <- lm(standard_4$od_dilutation ~ as.numeric(standard_4$ID))

# Get coefficients
intercept_4 <- coef(model_4)[1]
slope_4 <- coef(model_4)[2]

# calulcate insulin
ins_4 <- data %>%
    dplyr::filter(plate == 4 & !week == "stanard") %>%
    dplyr::mutate(insulin = (od_dilutation - intercept_4) / slope_4)

# plot
plot(as.numeric(standard_4$ID), standard_4$od_dilutation,
     xlab = "Insulin (ng/ml)",
     ylab = "OD450-630",
     main = "Plate 4")
abline(model_4)


# combine data ------------------------------------------------------------
# and add diet information
ins_final <- dplyr::bind_rows(list(ins_1,
                                   ins_2,
                                   ins_3,
                                   ins_4)) %>%
    dplyr::inner_join(y = dplyr::select(meta, Diet, ID, Cohort),
                      by = "ID",
                      relationship = "many-to-many") %>%
    dplyr::relocate(c(Diet), .after = ID) %>%
    dplyr::mutate(Diet = factor(Diet, levels = c("LFD", "HFD")),
                  week = as.numeric(week))

rstatix::identify_outliers(ins_final)

## Statistic test ----
# Is there any significant difference between LFD and HFD median fasted serum insulin levels?
stats <- ins_final %>%
    dplyr::group_by(week) %>%
    rstatix::wilcox_test(insulin ~ Diet,
                         ref.group = "LFD") %>%
    rstatix::add_significance("p") %>%
    rstatix::add_x_position(x = "week", group = "Diet") %>%
    rstatix::add_y_position(step.increase = 0.1, fun = "max")

## Effect size ----
# How large the effect is compared to random noise
ins_final %>%
    dplyr::group_by(week) %>%
    rstatix::wilcox_effsize(insulin ~ Diet,
                            ref.group = "LFD")

# plot --------------------------------------------------------------------
diet_color <- c("LFD" = "#F8Ad4B", "HFD" = "#004B7A")

pdf(here::here("data/fasted_serum_insulin_boxplot.pdf"),
    width = 5,
    height = 9,18)
insulin <- ins_final %>%
    ggpubr::ggboxplot(x = "week",
                      y = "insulin",
                      fill = "Diet",
                      color = "Diet",
                      add = "jitter",
                      size = 0.5,
                      palette = diet_color,
                      lwd = 1,
                      fatten = 0.5,
                      title = "Fasted insulin") +
    ggplot2::scale_color_manual(values = c("black", "black")) +
    ggplot2::labs(
        y = "Fasted serum insulin (ng / mL)",
        x = "Weeks on diet",
        fill = "Diet") +
    ggplot2::scale_y_continuous(breaks = seq(0, 6, by = 1)) +
    ggplot2::expand_limits(y = 0) +
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
print(insulin)
dev.off()


# HOMA-IR -----------------------------------------------------------------
# load bg data
# homa-ir citation
# https://pubmed.ncbi.nlm.nih.gov/3899825/
# https://pubmed.ncbi.nlm.nih.gov/7014307/
# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3114864/
# https://pubmed.ncbi.nlm.nih.gov/15161807/
# https://fbri.vtc.vt.edu/content/dam/fbri_vtc_vt_edu/yan-lab-protocols/HOMA-IR.v1.pdf

# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6501531/

# why you should measure insulin in serum and not plasma: https://www.sciencedirect.com/science/article/pii/S0140673670913486?via%3Dihub
bg <- readxl::read_xlsx(path = here::here("data-raw/all_raw_data.xlsx"),
                        sheet = "HOMA_bg",
                        col_names = TRUE,
                        col_types = c("text", rep("numeric", 2))) %>%
    dplyr::rename(bg = `0`)

# combine with insulin and calculate homa
ins_glu <- ins_final %>%
    dplyr::left_join(y = bg, by = c("ID", "week"))


# HOMA1-IR = (ins × bg)/22.5 for IR,
# respectively, where ins is fasting insulin concentration (uIU/ml)
# and bg is fasting glucose (mmol/l).
# 1 μU/mL = 6 pmol/L
# https://fbri.vtc.vt.edu/content/dam/fbri_vtc_vt_edu/yan-lab-protocols/HOMA-IR.v1.pdf
conv_insulin <- function(x){
    # ((((ng/ml * 1000 pg/ng) / 5808 mol/g insulin) * 1e12 pmol/mol) / 1e12 g/pg)/6
    output <- ((((x*1000*1000)/5808)*1e12)/1e12)/6
    return(output)
}

conv_insulin_pmol <- function(x){
    # ((((ng/ml * 1000 pg/ng) / 5808 mol/g insulin) * 1e12 pmol/mol) / 1e12 g/pg)/6
    output <- ((((x*1000*1000)/5808)*1e12)/1e12)
    return(output)
}

# Calcualte homa-IR and beta -----------------------------------------------
# when calculating homa_beta_percent, we will say that if bg-3.5 is below 0.1, the value will bet set to 0.1
# as otherwise the result will be negative.
df <- ins_glu%>%
    dplyr::mutate(insulin_pmol = conv_insulin_pmol(insulin),
                  homa_ir = HOMA.IR(insulin_pmol, bg),
                  homa_beta = HOMA.Beta(insulin_pmol, bg),
                  quicki = QUICKI(insulin_pmol, bg),
                  spina_gbeta = SPINA.GBeta(insulin_pmol, bg),
                  spina_gr = SPINA.GR(insulin_pmol, bg))


# Statistical test --------------------------------------------------------
stats_gr <- df %>%
    dplyr::select(ID, Diet, week, spina_gr) %>%
    tidyr::pivot_longer(-c(ID, Diet, week), names_to = "name", values_to = "value") %>%
    dplyr::mutate(week = factor(week, levels = c("1", "3")),
                  Diet = factor(Diet, levels = c("LFD", "HFD"))) %>%
    tidyr::drop_na() %>%
    dplyr::group_by(week) %>%
    rstatix::wilcox_test(value ~ Diet,
                         ref.group = "LFD") %>%
    rstatix::add_significance("p") %>%
    rstatix::add_x_position(x = "week") %>%
    rstatix::add_y_position(step.increase = 0.1, fun = "max")

stats_gb <- df %>%
    dplyr::select(ID, Diet, week, spina_gbeta) %>%
    tidyr::pivot_longer(-c(ID, Diet, week), names_to = "name", values_to = "value") %>%
    dplyr::mutate(week = factor(week, levels = c("1", "3")),
                  Diet = factor(Diet, levels = c("LFD", "HFD"))) %>%
    tidyr::drop_na() %>%
    dplyr::group_by(week) %>%
    rstatix::wilcox_test(value ~ Diet,
                         ref.group = "LFD") %>%
    rstatix::add_significance("p") %>%
    rstatix::add_x_position(x = "week") %>%
    rstatix::add_y_position(step.increase = 0.1, fun = "max")


# plot --------------------------------------------------------------------
diet_color <- c("LFD" = "#F8Ad4B", "HFD" = "#004B7A")


spina_gr <- df %>%
    dplyr::select(ID, Diet, week, spina_gr) %>%
    tidyr::pivot_longer(-c(ID, Diet, week), names_to = "name", values_to = "value") %>%
    dplyr::mutate(week = factor(week, levels = c("1", "3")),
                  Diet = factor(Diet, levels = c("LFD", "HFD"))) %>%
    tidyr::drop_na() %>%
    ggplot2::ggplot(aes(x = week, y = value, fill = Diet)) +
    ggplot2::geom_boxplot(outlier.shape = NA) +
    ggplot2::geom_point(position=position_dodge(width=0.75)) +
    ggplot2::scale_fill_manual(values = diet_color) +
    ggplot2::expand_limits(y = 0) +
    ggplot2::labs(title = "Insulin receptor gain",
                  y = "SPINA-GR",
                  x = "") +
    ggplot2::expand_limits(y = 0) +
    ggprism::theme_prism(border = TRUE,
                         base_size = 15,
                         base_fontface = "plain") +
    ggplot2::theme(legend.position = "top") +
    ggplot2::coord_cartesian(clip = "off")

spina_gbeta <- df %>%
    dplyr::select(ID, Diet, week, spina_gbeta) %>%
    tidyr::pivot_longer(-c(ID, Diet, week), names_to = "name", values_to = "value") %>%
    dplyr::mutate(week = factor(week, levels = c("1", "3")),
                  Diet = factor(Diet, levels = c("LFD", "HFD"))) %>%
    tidyr::drop_na() %>%
    ggplot2::ggplot(aes(x = week, y = value, fill = Diet)) +
    ggplot2::geom_boxplot(outlier.shape = NA) +
    ggplot2::geom_point(position=position_dodge(width=0.75)) +
    ggplot2::scale_fill_manual(values = diet_color) +
    ggplot2::expand_limits(y = 0) +
    ggplot2::labs(title = "Secretory capacity",
                  y = "SPINA-GBeta",
                  x = "") +
    ggplot2::expand_limits(y = 0) +
    ggprism::theme_prism(border = TRUE,
                         base_size = 15,
                         base_fontface = "plain") +
    ggplot2::theme(legend.position = "top") +
    ggplot2::coord_cartesian(clip = "off")

pdf(here::here("data/spina_boxplot.pdf"),
    width = 8,
    height = 9.18)
wrap_plots(spina_gr, spina_gbeta)
dev.off()


# save --------------------------------------------------------------------
openxlsx::write.xlsx(df, here::here("data/documents/fasted_insulin_beta_function.xlsx"))

openxlsx::write.xlsx(list("fasted_insulin" = stats,
                          "spina_gr" = stats_gr,
                          "spina_gb" = stats_gb), here::here("data/documents/fasted_insulin_beta_function_wilcox.xlsx"))

