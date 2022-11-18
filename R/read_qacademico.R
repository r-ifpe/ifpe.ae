#' @export
read_qacademico <- function(x) {
  qacademico_files <- lapply(x, read_qacademico_file)
  do.call("rbind", qacademico_files)
}

#' @importFrom dplyr %>% sym syms
read_qacademico_file <- function(x) {
  utils::read.csv(x, sep = "") %>%
    dplyr::select(
      !!!syms(read_qacademico_columns())
    ) %>%
    dplyr::rename(
      MATRICULA = !!sym("Matrícula"),
      NOME = !!sym("Nome"),
      SEXO = !!sym("Sexo"),
      SITUACAO_MATRICULA = !!sym("Situação.Matrícula"),
      ENTRADA = !!sym("Per..Let..Inigresso"),
      CAMPUS = !!sym("Instituição"),
      CURSO = !!sym("Curso"),
      TURNO_ENTRADA = !!sym("Turno.Ingresso"),
      RENDA = !!sym("Renda.Familiar"),
      COTA = !!sym("Cota"),
      ESCOLA = !!sym("Escola.de.Origem"),
      CPF = !!sym("Cpf"),
      NIVEL = !!sym("Nível.Regime.de.Ensino"),
      COR_RACA = !!sym("Cor.Raça")
    )
}

read_qacademico_columns <- function() {
  x <- utils::read.csv(
    system.file(
      "qacademico_columns/qacademico_columns.csv",
      package = "ifpe.ae"
    )
  )

  x$columns
}
