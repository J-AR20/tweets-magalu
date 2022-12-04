# DETECTANDO A EXTREMA-DIREITA --------------------------------------------

base_categorizada_completa <- read_rds("data/base_categorizada_completa.rds")

extrem_words <- c('patriota', 'bolsonaro', 'armamentista', 'anti-esquerda', 
                  'cristao de direita', 'cristao', 'deus, familia, patria','direita', 'anti comunas', 
                  'desesquerdizando', 'liberdade', 'bolsonarista', 'brasil acima de tudo e deus acima de todos', 
                  'pro-vida','e conhecerao a verdade, e a verdade os libertara','conservador', '2️⃣2️⃣' ) %>% 
  paste0(collapse = '|')

base_categorizada_completa %>% 
  filter(orientacao_categoria != 'outros') %>% 
  mutate(author_description_clean = str_to_lower(author_description) %>% rm_accent(),
         author_description_extrem = str_detect(author_description_clean, extrem_words),
         author_name_clean = str_to_lower(author_name) %>% rm_accent(),
         author_name_extrem = str_detect(author_name_clean, extrem_words)) |>
  View()

# ------

url_google_sheet <- 'https://docs.google.com/spreadsheets/d/1z3oNAxrIYHZkjwVirRxLKiGMQAyxGE8cAemfXvusUUY/edit#gid=462148650'

users_perfis_direita <- googlesheets4::read_sheet(url_google_sheet, 'perfis_bolsonaristas')

base_completa <- readr::read_rds("data/base_categorizada_completa.rds")

seguidores <- list.files(
  "data/author_following/", full.names = TRUE
) |> 
  unique() |> 
  purrr::map(readr::read_rds) |> 
  dplyr::bind_rows()


quem_segue_quem <- seguidores |> 
  tibble::as_tibble() |> 
  dplyr::select(
   author_seguidor = from_id,
   id, username
  ) |> 
  dplyr::mutate(
    eh_perfil_extrema_direita = dplyr::case_when(
      username %in% users_perfis_direita$author_username ~ TRUE,
      TRUE ~ FALSE
    )
  ) 

qnt_perfis_direita <- nrow(users_perfis_direita)

usuarios <- base_completa |>
  dplyr::select(
    author_seguidor = author_id, autor_username
  ) |> 
  dplyr::distinct()

quem_segue_perfis_minions <- quem_segue_quem |>
  dplyr::group_by(author_seguidor, eh_perfil_extrema_direita) |>
  dplyr::count() |>
  dplyr::filter(eh_perfil_extrema_direita == TRUE) |>
  dplyr::ungroup() |>
  dplyr::mutate(porc =  n / qnt_perfis_direita) |> 
  dplyr::arrange(desc(n)) |> 
  dplyr::left_join(usuarios) 

quem_segue_perfis_minions |> 
  readr::write_rds("data/seguidores-dos-ext-direita.rds")



