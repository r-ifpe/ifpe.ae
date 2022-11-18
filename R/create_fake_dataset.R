#' @importFrom dplyr %>%
#' @export
change_name <- function(qacademico_table, column) {
  rnd_names_table <- qacademico_table %>%
    retrieve_unique_names(column) %>%
    create_rnd_name_table()

  rename_rnd_names_to_names(qacademico_table, rnd_names_table, column)
}


retrieve_unique_names <- function(x, column) {
  unique(x[column])
}

create_rnd_name_table <- function(x) {
  rnd_names <- unlist(
    lapply(1:nrow(x), function(e) {
      create_rnd_name()
    })
  )

  x$RND_NAME <- rnd_names
  x
}

#' @importFrom dplyr %>%
create_rnd_name <- function() {
  stringi::stri_rand_strings(1, 9, pattern = "[a-z]") %>%
    stringr::str_replace(
      pattern = "([a-z]{3})([a-z]{3})([a-z]{3})",
      replacement = "\\1 \\2 \\3"
    )
}

#' @importFrom dplyr %>%
rename_rnd_names_to_names <- function(qacademico_table, rnd_names, column) {
  dplyr::left_join(qacademico_table, rnd_names, by = column) %>%
    na.omit() %>%
    dplyr::rename(
      NOME_TEMP = !!sym(column),
      !!sym(column) := !!sym("RND_NAME")
    ) %>%
    within(rm("NOME_TEMP"))
}
