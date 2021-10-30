# SELECT TIME SERIES TO BE PREDICTED
library(data.table)
library(dtplyr)
library(dplyr, warn.conflicts = FALSE)

# load auxiliar tables
df_countries <- readRDS('db/t_countries.rds')
df_ncm <- readRDS('db/t_ncm.rds')
df_unities <- readRDS('db/t_unities.rds')

# AGGREGATE IMPORTS SERIES
df_m <- readRDS('db/imports.rds')
df_m <- df_m %>% lazy_dt() %>%
  left_join(., {df_countries %>% select(CO_PAIS, NO_BLOCO_ING)}, by = "CO_PAIS") %>%
  left_join(., {df_ncm %>% select(CO_NCM, NO_SH4_ING)}, by = "CO_NCM") %>%
  group_by(TRADE = "IMPORTS", NO_SH4_ING, NO_BLOCO_ING, DT, CO_UNID) %>% 
  summarise(
    QT_ESTAT = sum(QT_ESTAT),
    VL_FOB = sum(VL_FOB)
  ) %>% as_tibble()

# AGGREGATE EXPORTS SERIES
df_x <- readRDS('db/exports.rds')
df_x <- df_x %>% lazy_dt() %>%
  left_join(., {df_countries %>% select(CO_PAIS, NO_BLOCO_ING)}, by = "CO_PAIS") %>%
  left_join(., {df_ncm %>% select(CO_NCM, NO_SH4_ING)}, by = "CO_NCM") %>%
  group_by(TRADE = "EXPORTS", NO_SH4_ING, NO_BLOCO_ING, DT, CO_UNID) %>% 
  summarise(
    QT_ESTAT = sum(QT_ESTAT),
    VL_FOB = sum(VL_FOB)
  ) %>% as_tibble()

# BIND SERIES
df_ts <- bind_rows(list(df_x, df_m)) %>% ungroup() %>%
  mutate(UNIT_PRICE_FOB = VL_FOB / QT_ESTAT)

rm(list = c("df_x", "df_m"))

# SAMPLE SERIES
n_series <- 100 # amount of series to consider (currently small to speed processing)
candidates_ts <- df_ts %>%
  group_by(TRADE, NO_SH4_ING, NO_BLOCO_ING, CO_UNID) %>%
  summarise(DTS = n_distinct(DT)) %>%
  ungroup() %>% filter(DTS == max(DTS)) %>% select(-DTS)

set.seed(193827)  # seed for random sampling reproducibility
chosen_ts <- candidates_ts %>% sample_n(n_series, replace = FALSE)
df_ts_sample <- inner_join(df_ts, chosen_ts)

saveRDS(df_ts_sample, 'db/df_ts_sample.rds')
