# qacademico <- read_qacademico("C://Users/dmmad/Desktop/Aria/qacademico/Outubro_2022_1.csv")
library(ifpe.ae)
library(dplyr)
library(stringr)
library(tidyr)


qacademico <- rbind(
  read.csv("C://Users/dmmad/Desktop/Aria/qacademico/recife-outubro_1.csv", sep = "") ,
  read.csv("C://Users/dmmad/Desktop/Aria/qacademico/recife-outubro_1_2.csv", sep = "")
)

glimpse(qacademico)


qtd_por_matricula <- qacademico %>%
  filter(Instituição == "IFPE - CAMPUS RECIFE") %>%
  group_by(
  Per..Let..Inigresso,
  Turno.Ingresso,
  Curso,
  Nível.Regime.de.Ensino,
  Situação.Matrícula
  ) %>%
  tally() %>%
  tidyr::separate(Per..Let..Inigresso, into = c("Ano", "Semestre"), sep = "/") %>%
  filter(Ano >= 2000) %>%
  rename(qtd_por_matricula = n) %>%
  ungroup()


qtd_total <- qacademico %>%
  filter(Instituição == "IFPE - CAMPUS RECIFE") %>%
  group_by(
    Per..Let..Inigresso,
    Turno.Ingresso,
    Curso,
    Nível.Regime.de.Ensino
  ) %>%
  tally() %>%
  tidyr::separate(Per..Let..Inigresso, into = c("Ano", "Semestre"), sep = "/") %>%
  filter(Ano >= 2000) %>%
  rename(qtd_total = n) %>%
  ungroup()

qacademico_matricula_situacao <- qtd_por_matricula %>%
  left_join(
    qtd_total,
    by = c(
      "Ano", "Semestre", "Turno.Ingresso",
      "Curso", "Nível.Regime.de.Ensino"
    )) %>%
  ungroup() %>%
  mutate(
    Situaçao_geral = case_when(
      str_detect(Situação.Matrícula, "Matriculado|Trancado|Intercâmbio|Afastado|Não") ~ "Em curso",
      str_detect(Situação.Matrícula, "Concluído|Formado|Colação") ~ "Concluído",
      str_detect(Situação.Matrícula, "Abandono|Cancelamento|Jubilado|Falecido") ~ "Abandono",
      str_detect(Situação.Matrícula, "Concludente|Vínculo|ENADE|Seminário|Certificado") ~ "Integralizado",
      TRUE ~ Situação.Matrícula
    )
  )

cursos_finalizados <- qacademico_matricula_situacao %>%
  mutate(
    Status_curso = if_else(
      Situaçao_geral == "Concluído", "Finalizado", "Não Finalizado"
    )
  ) %>%
  filter(Status_curso == "Finalizado") %>%
  select(
    Ano, Semestre, Turno.Ingresso, Curso,
    Nível.Regime.de.Ensino, Status_curso
  )

cursos_nao_finalizados <- qacademico_matricula_situacao %>%
  anti_join(cursos_finalzados,
            by = c("Ano", "Semestre", "Turno.Ingresso", "Curso", "Nível.Regime.de.Ensino")
        ) %>%
  select(Ano, Semestre, Turno.Ingresso, Curso, Nível.Regime.de.Ensino) %>%
  distinct()

  qacademico_matricula_situacao %>%
    semi_join(
      cursos_finalizados,
      by = c("Ano", "Semestre", "Turno.Ingresso", "Curso", "Nível.Regime.de.Ensino")
   ) %>%
    group_by(
      Ano, Semestre, Turno.Ingresso, Curso,
      Nível.Regime.de.Ensino, qtd_total, Situaçao_geral
    ) %>%
    summarise(qtd_alunos = sum(qtd_por_matricula)) %>%
    ungroup() %>%
    mutate(percent = qtd_alunos / qtd_total ) %>%
    select(-qtd_total, -qtd_alunos) %>%
    pivot_wider(
      names_from = Situaçao_geral, values_from = percent, values_fill = 0
    ) %>%
    mutate(
      IEA = Concluído + (Integralizado + `Em curso`) * (Concluído) / (Concluído + Abandono)
    ) %>%
    mutate(across(where(is.numeric), round, digits = 3))




