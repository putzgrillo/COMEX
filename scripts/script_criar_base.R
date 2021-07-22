library(tidyverse)
library(lubridate)

df_m <- data.table::fread("/mnt/2892419C92416F7E/aMestrado Estatística/Disciplinas/T4 - Series Temporais/Trabalho/IMP_COMPLETA.csv")
df_x <- data.table::fread("/mnt/2892419C92416F7E/aMestrado Estatística/Disciplinas/T4 - Series Temporais/Trabalho/EXP_COMPLETA.csv")
df_ncm <- data.table::fread("/mnt/2892419C92416F7E/R/Comercio Exterior/refs/refNcm.csv")

# DADOS DE IMPORTAÇÃO DO RS A PARTIR DO ANO 2000. FREQUÊNCIA TRIMESTRAL ----
df_m <- df_m %>%
  filter(SG_UF_NCM == "RS") %>%
  mutate(
    DT_REF = make_date(CO_ANO, CO_MES),
    DT_Q = floor_date(DT_REF, unit = "quarter")
    ) %>%
  filter(year(DT_REF) >= 2000) %>%
  group_by(DT_Q, CO_NCM) %>%
  summarise(
    KG_LIQUIDO = sum(KG_LIQUIDO),
    VL_FOB = sum(VL_FOB)
  )

df_x <- df_x %>%
  filter(SG_UF_NCM == "RS") %>%
  mutate(
    DT_REF = make_date(CO_ANO, CO_MES),
    DT_Q = floor_date(DT_REF, unit = "quarter")
  ) %>%
  filter(year(DT_REF) >= 2000) %>%
  group_by(DT_Q, CO_NCM) %>%
  summarise(
    KG_LIQUIDO = sum(KG_LIQUIDO),
    VL_FOB = sum(VL_FOB)
  )


# VERIFICAR SÉRIES COM TODAS OBSERVAÇÕES (completas tem 86 observações)
registros_ref_m <- df_m %>%
  group_by(CO_NCM) %>%
  summarise(Freq = n()) %>%
  ungroup() %>% #count(Freq) %>% ggplot(., aes(x = n)) +  geom_histogram() + theme_bw()
  filter(Freq == 86)

registros_ref_x <- df_x %>%
  group_by(CO_NCM) %>%
  summarise(Freq = n()) %>%
  ungroup() %>% #count(Freq) %>% ggplot(., aes(x = n)) +  geom_histogram() + theme_bw()
  filter(Freq == 86)

# FILTRAR SÉRIES DE INTERESSE ----
df_ts <- list(
  x = df_x %>% filter(CO_NCM %in% registros_ref_x$CO_NCM) %>% mutate(ORIGEM = "EXPORTACAO"),
  x = df_m %>% filter(CO_NCM %in% registros_ref_m$CO_NCM) %>% mutate(ORIGEM = "IMPORTACAO")
) %>% 
  bind_rows() %>%
  left_join(., {df_ncm %>% select(CO_NCM, NO_SH4_ING, NO_ISIC4_GRUPO)}, by = "CO_NCM") %>%
  mutate(
    VL_FOB = as.double(VL_FOB),
    KG_LIQUIDO = as.double(KG_LIQUIDO)
  ) %>%
  ungroup()

# SALVAR DADOS ----
saveRDS(df_ts, "/mnt/2892419C92416F7E/aMestrado Estatística/Disciplinas/T5 - Econometria III/Trabalho 1/df_ts.rds")
