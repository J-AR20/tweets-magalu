library(readr)
library(tidyverse)

base_categorizada_completa <- read_rds("data/base_categorizada_completa.rds")

lista_buscar <- base_categorizada_completa |>
  filter(orientacao_categoria != 'outros', !cod_tweets_magalu %in% c("98", "99")) %>% 
  dplyr::distinct(author_id, author_username, author_following_count, .keep_all = FALSE) |> 
  tidyr::drop_na(author_id)

users_baixados <- list.files("data/busca_seguidores/") |> 
  stringr::str_remove(".rds")


buscar_user_following <- function(id, username){
  df <- academictwitteR::get_user_following(id)
  readr::write_rds(df, paste0("data/busca_seguidores/", username, ".rds"))
  usethis::ui_done("feito para {username}")
}

lista_buscar |> 
  dplyr::filter(!author_username %in% users_baixados) |> 
  dplyr::arrange(author_following_count) |> 
  tidyr::drop_na(author_id) |> 
  tibble::rowid_to_column() |> 
  dplyr::group_split(rowid) |> 
  purrr::walk(~buscar_user_following(id = .x$author_id, username =.x$author_username))
  
"OK"
