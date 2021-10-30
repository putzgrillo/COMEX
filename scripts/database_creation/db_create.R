# GOAL: CREATE DIRECTORY WITH ALL DATASETS IN A COMPRESSED RDS FILE
library(data.table)
library(rbcb)
library(dplyr)
library(lubridate)
first_year <- 2006         # string to determine year of the earliest observation

# IF DIRECTORY IS NOT AVAILABLE, CREATE IT
if (!dir.exists('db')) {
  dir.create('db')
}

# DATA ----
    # # EXPORTS DATA ----
    # # # # -k eliminates the need for certificate; -O keeps the original filename; -X is for request
system(command = "curl -k -X GET https://balanca.economia.gov.br/balanca/bd/comexstat-bd/ncm/EXP_COMPLETA.zip -O", wait = TRUE)
df_x <- data.table::fread('unzip -p EXP_COMPLETA.zip', dec = ",")
unlink("EXP_COMPLETA.zip")
df_x <- within(df_x[df_x$CO_ANO >= first_year], {
  DT <- lubridate::ymd(paste(CO_ANO, sprintf("%02d", CO_MES), "01", sep = "-"))
  VL_FOB <- as.numeric(VL_FOB)
  KG_LIQUIDO <- as.numeric(KG_LIQUIDO)
  QT_ESTAT <- as.numeric(QT_ESTAT)
})
df_x <- df_x[, c(12, 3:11)] # eliminate year and month columns (pos: 1, 2)
saveRDS(df_x, file = "db/exports.rds")



    # # IMPORTS DATA ----
system(command = "curl -k -X GET https://balanca.economia.gov.br/balanca/bd/comexstat-bd/ncm/IMP_COMPLETA.zip -O", wait = TRUE)
df_m <- data.table::fread('unzip -p IMP_COMPLETA.zip', dec = ",")
unlink("IMP_COMPLETA.zip")
df_m <- within(df_m[df_m$CO_ANO >= first_year], {
  DT <- lubridate::ymd(paste(CO_ANO, sprintf("%02d", CO_MES), "01", sep = "-"))
  VL_FOB <- as.numeric(VL_FOB)
  KG_LIQUIDO <- as.numeric(KG_LIQUIDO)
  QT_ESTAT <- as.numeric(QT_ESTAT)
})
df_m <- df_m[, c(14, 3:13)] # eliminate year and month columns (pos: 1, 2)
saveRDS(df_m, file = "db/imports.rds")



    # # # NCM & SH CODE REFERENCE ----
system(command = "curl -k -X GET https://balanca.economia.gov.br/balanca/bd/tabelas/NCM.csv -O", wait = TRUE)
df_ncm <- data.table::fread('NCM.csv', dec = ",", encoding = "Latin-1")
unlink("NCM.csv")

system(command = "curl -k -X GET https://balanca.economia.gov.br/balanca/bd/tabelas/NCM_SH.csv -O", wait = TRUE)
df_sh <- data.table::fread('NCM_SH.csv', dec = ",", encoding = "Latin-1")
unlink("NCM_SH.csv")

df_ncm <- merge(x = df_ncm, y = df_sh, all.x = TRUE)
saveRDS(df_ncm, file = "db/t_ncm.rds")



    # # # STATISTICAL UNITIES REFERENCE ----
system(command = "curl -k -X GET https://balanca.economia.gov.br/balanca/bd/tabelas/NCM_UNIDADE.csv -O", wait = TRUE)
df_unid <- data.table::fread('NCM_UNIDADE.csv', dec = ",", encoding = "Latin-1")
unlink("NCM_UNIDADE.csv")
saveRDS(df_unid, file = "db/t_unities.rds")



    # # # MEANS OF TRANSPORTATION REFERENCE ----
system(command = "curl -k -X GET https://balanca.economia.gov.br/balanca/bd/tabelas/VIA.csv -O", wait = TRUE)
df_via <- data.table::fread('VIA.csv', dec = ",", encoding = "Latin-1")
unlink("VIA.csv")
saveRDS(df_via, file = "db/t_transportation_means.rds")



    # # # COUNTRIES REFERENCE ----
system(command = "curl -k -X GET https://balanca.economia.gov.br/balanca/bd/tabelas/PAIS.csv -O", wait = TRUE)
df_pais <- data.table::fread('PAIS.csv', dec = ",", encoding = "Latin-1")
unlink("PAIS.csv")

system(command = "curl -k -X GET https://balanca.economia.gov.br/balanca/bd/tabelas/PAIS_BLOCO.csv -O", wait = TRUE)
df_bloco <- data.table::fread('PAIS_BLOCO.csv', dec = ",", encoding = "Latin-1")
unlink("PAIS_BLOCO.csv")

df_bloco <- df_bloco[!(df_bloco$CO_BLOCO %in% c(53, 111, 22)), ] # remove duplicated entries as (EUROPA, EU), (South America, Mercosur)
df_pais <- merge(x = df_pais, y = df_bloco, all.x = TRUE, by = "CO_PAIS")

saveRDS(df_pais, file = "db/t_countries.rds")



    # # # CALENDAR TABLE ----
df_calendar <- data.frame(DT = sort(unique(df_m$DT)))
saveRDS(df_calendar, file = "db/t_calendar.rds")


    # # # EXCHANGE RATE TABLE (USD-EUR-GBP) ----
            # TIME INTERVAL
dates_currencies <- range(df_calendar$DT)
dates_currencies[2] <- lubridate::ceiling_date(x = dates_currencies[2], unit = "month") - 1
            # LIST OF CURRENCIES
symbol_currencies <- c("USD", "EUR", "GBP")
df_currencies <- vector('list', length(symbol_currencies))
names(df_currencies) <- symbol_currencies

for (w in seq_along(df_currencies)) {
  df_currencies[[w]] <- rbcb::get_currency(symbol = symbol_currencies[w],
                                           start_date = dates_currencies[1],
                                           end_date = dates_currencies[2])
}
#  bid is the price a dealer buys a currency
#  ask is the price a dealer sells a currency
df_currencies <- dplyr::bind_rows(df_currencies, .id = "INTO_CURRENCY") %>%
  dplyr::mutate(
    DT = lubridate::floor_date(date, unit = "month"),
    FROM_CURRENCY = "BRL",
    INVERSE_RATE = 1 / ask
  ) %>%
  dplyr::select(DT, DATE = date, FROM_CURRENCY, INTO_CURRENCY, RATE = ask, INVERSE_RATE)

            # # # # INCLUDE ALL CURRENCIES IN THE FROM_CURRENCY VARIABLE (SUITED FOR DATA THAT HAS ONLY ONE SOURCE_CURRENCY)
                      # CREATE ALL DESIRED COMBINATIONS
currency_combinations <- expand.grid(FROM_CURRENCY = symbol_currencies, 
                                     INTO_CURRENCY = symbol_currencies,
                                     stringsAsFactors = FALSE) %>% 
  filter(FROM_CURRENCY != INTO_CURRENCY)
              
df_currency_combinations <- vector("list", nrow(currency_combinations)) 

for (w in seq(nrow(currency_combinations))) {
  df_currency_combinations[[w]] <- 
    dplyr::left_join(
                    # DATA.FRAME WITH SOURCE_CURRENCY
                    x = {
                      df_currencies %>%
                        dplyr::filter(INTO_CURRENCY == currency_combinations$FROM_CURRENCY[w]) %>%
                        dplyr::mutate(FROM_CURRENCY = currency_combinations$FROM_CURRENCY[w]) %>%
                        dplyr::select(DT, DATE, FROM_CURRENCY, RATE_X = RATE)},
                    # DATA.FRAME WITH TARGET_CURRENCY
                    y = {
                      df_currencies %>%
                        dplyr::filter(INTO_CURRENCY == currency_combinations$INTO_CURRENCY[w]) %>%
                        dplyr::select(DT, DATE, INTO_CURRENCY, RATE_Y = RATE)},
                    # MERGING VARIABLES
                    by = c("DT", "DATE")
    ) %>%
    dplyr::mutate(
      RATE = RATE_X / RATE_Y,
      INVERSE_RATE = 1 / RATE
    ) %>%
    dplyr::select(DT, DATE, FROM_CURRENCY, INTO_CURRENCY, RATE, INVERSE_RATE)
}

df_currency_combinations <- bind_rows(df_currency_combinations)

df_currencies <- bind_rows(list(df_currencies,
                                df_currency_combinations))

saveRDS(df_currencies, 'db/t_currencies.rds')
