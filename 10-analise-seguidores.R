# DETECTANDO A orientacao --------------------------------------------
library(readr)
library(tidyverse)
library(abjutils)


base_categorizada_completa <- read_rds("data/base_categorizada_completa.rds")


# checar: manter esses perfis?
# História No Paint; 
# Haddad Debochado; 
# QuebrandoOTabu;
# Mídia NINJA; 	
# História & Cultura



# CUIDADO AQUI: FORA BOLSONARO ENTROU! Corrigir.
extrem_words <- c('patriota', 'bolsonaro', 'armamentista', 'anti-esquerda', 
                  'cristao de direita', 'cristao', 'deus, familia, patria','direita', 'anti comunas', 
                  'desesquerdizando', 'liberdade', 'bolsonarista', 'brasil acima de tudo e deus acima de todos', 
                  'pro-vida','e conhecerao a verdade, e a verdade os libertara','conservador', '2️⃣2️⃣' ) %>% 
  paste0(collapse = '|')

base_deteccao_por_palavras <- base_categorizada_completa %>% 
  filter(orientacao_categoria != 'outros', !cod_tweets_magalu %in% c("98", "99")) %>% 
  mutate(author_description_clean = str_to_lower(author_description) %>% rm_accent(),
         author_description_extrem = str_detect(author_description_clean, extrem_words),
         author_name_clean = str_to_lower(author_name) %>% rm_accent(),
         author_name_extrem = str_detect(author_name_clean, extrem_words)) 


# Quem segue quem? ------

url_google_sheet <- 'https://docs.google.com/spreadsheets/d/1z3oNAxrIYHZkjwVirRxLKiGMQAyxGE8cAemfXvusUUY/edit#gid=462148650'

users_perfis_direita <- googlesheets4::read_sheet(url_google_sheet, 'perfis_bolsonaristas')

users_perfis_esquerda <- googlesheets4::read_sheet(url_google_sheet, 'perfis_frente_ampla')

seguidores <- list.files(
  "data/busca_seguidores/", full.names = TRUE
) |> 
  unique() |> 
  purrr::map(readr::read_rds) |> 
  dplyr::bind_rows() |> 
  tidyr::drop_na(id) |> 
  tibble::as_tibble() 


quem_segue_quem <- seguidores |> 
  dplyr::select(
   author_seguidor = from_id,
   id, username
  ) |> 
  dplyr::mutate(
    eh_perfil_extrema_direita = dplyr::case_when(
      username %in% users_perfis_direita$author_username ~ TRUE,
      TRUE ~ FALSE
    ),
    eh_perfil_frente_ampla = dplyr::case_when(
      username %in% users_perfis_esquerda$author_username ~ TRUE,
      TRUE ~ FALSE
    ),
  ) 

usuarios <- base_categorizada_completa |>
  dplyr::select(
    author_seguidor = author_id, autor_username
  ) |> 
  dplyr::distinct()

# REVISAR ISSO! To achando os numeros baixoss
seguidores_por_alinhamento <- quem_segue_quem |>
  dplyr::select(author_seguidor, eh_perfil_extrema_direita, eh_perfil_frente_ampla) |> 
  dplyr::group_by(author_seguidor) |> 
  dplyr::summarise(soma_seguindo_direita = sum(eh_perfil_extrema_direita),
                   soma_seguindo_frente_ampla = sum(eh_perfil_frente_ampla)) |>
  dplyr::left_join(usuarios) 



# ---
seguidores_por_alinhamento_base_completa <- base_deteccao_por_palavras |> 
  dplyr::left_join(seguidores_por_alinhamento, by = "autor_username") |>
  dplyr::arrange(desc(soma_seguindo_direita))


seguidores_por_alinhamento_base_completa |> 
  readr::write_rds("data/seguidores-por-alinhamento.rds")


