library(readr)


base_categorizada_completa <- read_rds("data/base_categorizada_completa.rds")

lista_buscar <- base_categorizada_completa |>
  dplyr::filter(orientacao_categoria %in% c("favoravel", "contrario")) |> 
  dplyr::arrange(author_following_count) |>
  dplyr::distinct(author_id, author_username, .keep_all = TRUE)


# url_google_sheet <- 'https://docs.google.com/spreadsheets/d/1z3oNAxrIYHZkjwVirRxLKiGMQAyxGE8cAemfXvusUUY/edit#gid=462148650'

# base_users <- googlesheets4::read_sheet(url_google_sheet, 'tweets_favoraveis')

# lista_buscar <- base_users |> dplyr::distinct(id, .keep_all = TRUE)


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
  
