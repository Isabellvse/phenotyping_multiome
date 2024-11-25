source(here::here("R/package_load.R"))
set.seed(100)
df <- readxl::read_xlsx(here::here("data/documents/fasted_insulin_beta_function.xlsx"))

df %>% dplyr::filter(Diet == "HFD", week == 3) %>% identify_outliers(insulin)
