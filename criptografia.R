library(dplyr)
library(ifpe.ae)

qacademico <- read_qacademico(c(
  "C://Users/dmmad/Desktop/Aria/qacademico/recife-outubro_1.csv",
  "C://Users/dmmad/Desktop/Aria/qacademico/recife-outubro_1_2.csv"
))

columns <- names(qacademico)

qacademico %>%
  dplyr::filter(ENTRADA > "2018") %>%
  change_name("COTA") %>%
  change_name("MATRICULA") %>%
  change_name("RENDA") %>%
  change_name("NOME") %>%
  change_name("CURSO") %>%
  change_name("CPF") %>%
  change_name("COR_RACA") %>%
  change_name("ESCOLA") %>%
  change_name("SITUACAO_MATRICULA") %>%
  change_name("SEXO") %>%
  change_name("TURNO_ENTRADA") %>%
  change_name("NIVEL") %>%
  change_name("CAMPUS") %>%
  glimpse()












