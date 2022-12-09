library(readr)


base_categorizada_completa <- read_rds("data/base_categorizada_completa.rds")

lista_buscar <- base_categorizada_completa |>
  filter(orientacao_categoria != 'outros', !cod_tweets_magalu %in% c("98", "99")) %>% 
  dplyr::distinct(author_id, author_username, .keep_all = FALSE)


# url_google_sheet <- 'https://docs.google.com/spreadsheets/d/1z3oNAxrIYHZkjwVirRxLKiGMQAyxGE8cAemfXvusUUY/edit#gid=462148650'

# base_users <- googlesheets4::read_sheet(url_google_sheet, 'tweets_favoraveis')

# lista_buscar <- base_users |> dplyr::distinct(id, .keep_all = TRUE)


users_baixados <- list.files("data/busca_seguidores/") |> 
  stringr::str_remove(".rds")


buscar_user_following <- function(id, username){
  df <- academictwitteR::get_user_following(id)
  readr::write_rds(df, paste0("data/busca_seguidores/", username, ".rds"))
}

lista_buscar |> 
  dplyr::filter(!author_username %in% users_baixados) |> 
  dplyr::group_split(author_username) |> 
  purrr::walk(~buscar_user_following(id = .x$author_id, username =.x$author_username))
  
