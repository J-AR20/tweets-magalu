library(readr)


# base_categorizada_completa <- read_rds("data/base_categorizada_completa.rds")
# 
# autores_unicos <- base_categorizada_completa |> 
#   dplyr::arrange(desc(like_count)) |> 
#   dplyr::distinct(author_id, author_username, .keep_all = TRUE)


url_google_sheet <- 'https://docs.google.com/spreadsheets/d/1z3oNAxrIYHZkjwVirRxLKiGMQAyxGE8cAemfXvusUUY/edit#gid=462148650'

base_users_contrarios <- googlesheets4::read_sheet(url_google_sheet, 'tweets_contrarios')

lista_buscar <- base_users_contrarios |> dplyr::distinct(id, .keep_all = TRUE)


users_baixados <- list.files("data/author_following/") |> 
  stringr::str_remove(".rds")


buscar_user_following <- function(id, username){
  df <- academictwitteR::get_user_following(id)
  readr::write_rds(df, paste0("data/author_following/", username, ".rds"))
}

lista_buscar |> 
  dplyr::filter(!autor_username %in% users_baixados) |> 
  dplyr::group_split(id) |> 
  purrr::walk(~buscar_user_following(id = .x$id, username =.x$autor_username))
  