library(readr)


base_categorizada_completa <- read_rds("data/base_categorizada_completa.rds")

autores_unicos <- base_categorizada_completa |> 
  dplyr::arrange(desc(like_count)) |> 
  dplyr::distinct(author_id, author_username, .keep_all = TRUE)


autores_baixados <- list.files("data/author_following/") |> 
  stringr::str_remove(".rds")


buscar_user_following <- function(id, username){
  df <- academictwitteR::get_user_following(id)
  readr::write_rds(df, paste0("data/author_following/", username, ".rds"))
}

autores_unicos |> 
  dplyr::filter(!author_username %in% autores_baixados) |> 
  dplyr::group_split(author_username) |> 
  purrr::walk(~buscar_user_following(.x$author_id, .x$author_username))
  

