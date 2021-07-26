# this script has the following steps: 
  # download files > create .db file > delete downloaded files
library(data.table)
library(DBI)
library(RSQLite)
# string
first_year <- 2006
# create export table ----
                    # -k eliminates the need for certificate; -O keeps the original filename; -X is for request
system(command = "curl -k -X GET https://balanca.economia.gov.br/balanca/bd/comexstat-bd/ncm/EXP_COMPLETA.zip -O", wait = TRUE)
df_x <- data.table::fread('unzip -p EXP_COMPLETA.zip', dec = ",")
unlink("EXP_COMPLETA.zip")

df_x <- within(df_x[df_x$CO_ANO >= first_year], {
  DT <- as.character(paste(CO_ANO, sprintf("%02d", CO_MES), "01", sep = "/"))
  VL_FOB <- as.numeric(VL_FOB)
  KG_LIQUIDO <- as.numeric(KG_LIQUIDO)
  QT_ESTAT <- as.numeric(QT_ESTAT)
})

df_x <- df_x[, c(12, 3:11)] # eliminate year and month columns (pos: 1, 2)


# create import table ----
system(command = "curl -k -X GET https://balanca.economia.gov.br/balanca/bd/comexstat-bd/ncm/IMP_COMPLETA.zip -O", wait = TRUE)
df_m <- data.table::fread('unzip -p IMP_COMPLETA.zip', dec = ",")
unlink("IMP_COMPLETA.zip")

df_m <- within(df_m[df_m$CO_ANO >= first_year], {
  DT <- as.character(paste(CO_ANO, sprintf("%02d", CO_MES), "01", sep = "/"))
  VL_FOB <- as.numeric(VL_FOB)
  KG_LIQUIDO <- as.numeric(KG_LIQUIDO)
  QT_ESTAT <- as.numeric(QT_ESTAT)
})

df_m <- df_m[, c(12, 3:11)] # eliminate year and month columns (pos: 1, 2)


# create reference tables ----
  # # ncm code reference 
system(command = "curl -k -X GET https://balanca.economia.gov.br/balanca/bd/tabelas/NCM.csv -O", wait = TRUE)
df_ncm <- data.table::fread('NCM.csv', dec = ",", encoding = "Latin-1")
unlink("NCM.csv")

  # # sh code reference
system(command = "curl -k -X GET https://balanca.economia.gov.br/balanca/bd/tabelas/NCM_SH.csv -O", wait = TRUE)
df_sh <- data.table::fread('NCM_SH.csv', dec = ",", encoding = "Latin-1")
unlink("NCM_SH.csv")

  # # merge ncm and sh4 tables
df_ncm <- merge(x = df_ncm, y = df_sh, all.x = TRUE)
rm(df_sh)

  # # statistical unity
system(command = "curl -k -X GET https://balanca.economia.gov.br/balanca/bd/tabelas/NCM_UNIDADE.csv -O", wait = TRUE)
df_unid <- data.table::fread('NCM_UNIDADE.csv', dec = ",", encoding = "Latin-1")
unlink("NCM_UNIDADE.csv")

  # # mean of transportation
system(command = "curl -k -X GET https://balanca.economia.gov.br/balanca/bd/tabelas/VIA.csv -O", wait = TRUE)
df_via <- data.table::fread('VIA.csv', dec = ",", encoding = "Latin-1")
unlink("VIA.csv")

  # # country
system(command = "curl -k -X GET https://balanca.economia.gov.br/balanca/bd/tabelas/PAIS.csv -O", wait = TRUE)
df_pais <- data.table::fread('PAIS.csv', dec = ",", encoding = "Latin-1")
unlink("PAIS.csv")

  # # country block
system(command = "curl -k -X GET https://balanca.economia.gov.br/balanca/bd/tabelas/PAIS_BLOCO.csv -O", wait = TRUE)
df_bloco <- data.table::fread('PAIS_BLOCO.csv', dec = ",", encoding = "Latin-1")
unlink("PAIS_BLOCO.csv")

  # # merge countries and blocks
df_bloco <- df_bloco[df_bloco$NO_BLOCO %in% c(53, 111, 22), ] # remove duplicated entries as (EUROPA, EU), (South America, Mercosur)
df_pais <- merge(x = df_pais, y = df_bloco, all.x = TRUE)
rm(df_bloco)

  # # create calendar table
df_calendar <- data.frame(DT = sort(unique(df_m$DT)))


# conect to table ----
# unlink("db/comex_db.sqlite")
db <- dbConnect(SQLite(), dbname = "db/comex_db.sqlite") # if non-existent, it will create

    # # # NCM ----
sql_statement <- sprintf("CREATE TABLE %s(%s, PRIMARY KEY(%s))", 
                         "t_ncm",
                         paste(colnames(df_ncm), collapse = ", "),
                         "CO_NCM")
dbExecute(conn = db, statement = sql_statement)

dbWriteTable(conn = db, name = "t_ncm", value = df_ncm, row.names = FALSE, method = "POSIXct", append = TRUE)

    # # # STATISTICAL UNITIES ----
sql_statement <- sprintf("CREATE TABLE %s(%s, PRIMARY KEY(%s))", 
                         "t_units",
                         paste(colnames(df_unid), collapse = ", "),
                         "CO_UNID")
dbExecute(conn = db, statement = sql_statement)

dbWriteTable(conn = db, name = "t_units", value = df_unid, row.names = FALSE, method = "POSIXct", append = TRUE)

    # # # MEANS OF TRANSPORTATION ----
sql_statement <- sprintf("CREATE TABLE %s(%s, PRIMARY KEY(%s))", 
                         "t_means_transport",
                         paste(colnames(df_via), collapse = ", "),
                         "CO_VIA")
dbExecute(conn = db, statement = sql_statement)

dbWriteTable(conn = db, name = "t_means_transport", value = df_via, row.names = FALSE, method = "POSIXct", append = TRUE)

    # # # COUNTRIES ----
sql_statement <- sprintf("CREATE TABLE %s(%s, PRIMARY KEY(%s))", 
                         "t_countries",
                         paste(colnames(df_pais), collapse = ", "),
                         "CO_PAIS")
dbExecute(conn = db, statement = sql_statement)

dbWriteTable(conn = db, name = "t_countries", value = df_pais, row.names = FALSE, method = "POSIXct", append = TRUE)

    # # # DATES ----
dbWriteTable(conn = db, name = "t_calendar", value = df_calendar, row.names = FALSE, method = "POSIXct", append = TRUE)

    # # # EXPORTS and IMPORTS ----
foreign_references <- paste("FOREIGN KEY (CO_NCM) REFERENCES t_ncm(CO_NCM)", 
                            "FOREIGN KEY (CO_UNID) REFERENCES t_units(CO_UNID)",
                            "FOREIGN KEY (CO_VIA) REFERENCES t_means_transport(CO_VIA)",
                            "FOREIGN KEY (CO_PAIS) REFERENCES t_countries(CO_PAIS)",
                            sep = ", ")
              # # # # EXPORTS
sql_statement <- sprintf("CREATE TABLE %s(%s, PRIMARY KEY(%s), %s)", 
                         "t_exports",
                         paste(colnames(df_x), collapse = ", "),
                         "DT, CO_NCM, CO_UNID, CO_PAIS, SG_UF_NCM, CO_VIA, CO_URF",
                         foreign_references)

dbExecute(conn = db, statement = sql_statement)

dbWriteTable(conn = db, name = "t_exports", value = df_x, row.names = FALSE, method = "POSIXct", append = TRUE)

              # # # # IMPORTS
sql_statement <- sprintf("CREATE TABLE %s(%s, PRIMARY KEY(%s), %s)", 
                         "t_imports",
                         paste(colnames(df_m), collapse = ", "),
                         "DT, CO_NCM, CO_UNID, CO_PAIS, SG_UF_NCM, CO_VIA, CO_URF",
                         foreign_references)

dbExecute(conn = db, statement = sql_statement)

dbWriteTable(conn = db, name = "t_imports", value = df_m, row.names = FALSE, method = "POSIXct", append = TRUE)

  # # disconnect
dbDisconnect(db)
