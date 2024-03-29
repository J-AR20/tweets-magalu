# DETECTANDO A orientacao --------------------------------------------
library(readr)
library(tidyverse)
library(abjutils)


base_categorizada_completa <-
  read_rds("dados_output/06-explorando-categorias/base_categorizada_completa.rds")


extreme_words <-
  c(
    'patriota',
    'bolsonaro',
    'armamentista',
    'anti-esquerda',
    'cristao de direita',
    'cristao',
    'deus, familia, patria',
    'direita',
    'anti comunas',
    'desesquerdizando',
    'bolsonarista',
    'brasil acima de tudo e deus acima de todos',
    'pro-vida',
    'e conhecerao a verdade, e a verdade os libertara',
    'conservador',
    '2️⃣2️⃣',
    '2⃣2⃣',
    '🇧🇷🇮🇱',
    "fora lula",
    '100%jb'
  ) %>%
  paste0(collapse = '|')


funcao_detectar_extreme_words <- function(x) {
  dplyr::case_when(
    str_detect(
      x,
      "fora bolsonaro|forabolsonaro|anti-bolsonarista|lulapresidente|lula presidente"
    ) ~ FALSE,
    str_detect(x, extreme_words) ~ TRUE,
    TRUE ~ FALSE
  )
}

base_deteccao_por_palavras <- base_categorizada_completa %>%
  filter(orientacao_categoria != 'outros',
         !cod_tweets_magalu %in% c("98", "99")) %>%
  mutate(
    author_description_clean = str_to_lower(author_description) %>% rm_accent(),
    author_description_extrem = funcao_detectar_extreme_words(author_description_clean),
    author_name_clean = str_to_lower(author_name) %>% rm_accent(),
    author_name_extrem = funcao_detectar_extreme_words(author_name_clean)
  )


# Quem segue quem? ------

url_google_sheet <-
  'https://docs.google.com/spreadsheets/d/1z3oNAxrIYHZkjwVirRxLKiGMQAyxGE8cAemfXvusUUY/edit#gid=462148650'

users_perfis_direita <-
  googlesheets4::read_sheet(url_google_sheet, 'perfis_bolsonaristas')

users_perfis_frente_ampla <-
  googlesheets4::read_sheet(url_google_sheet, 'perfis_frente_ampla')

seguidores <-
  list.files("dados_brutos/07-buscar-quem-segue/busca_seguidores/",
             full.names = TRUE) |>
  unique() |>
  purrr::map(readr::read_rds) |>
  dplyr::bind_rows() |>
  tidyr::drop_na(id) |>
  tibble::as_tibble()


quem_segue_quem <- seguidores |>
  dplyr::select(author_seguidor = from_id,
                id, username) |>
  dplyr::mutate(
    eh_perfil_extrema_direita = dplyr::case_when(
      username %in% users_perfis_direita$author_username ~ TRUE,
      TRUE ~ FALSE
    ),
    eh_perfil_frente_ampla = dplyr::case_when(
      username %in% users_perfis_frente_ampla$author_username ~ TRUE,
      TRUE ~ FALSE
    ),
  )

usuarios <- base_categorizada_completa |>
  dplyr::select(author_seguidor = author_id, autor_username) |>
  dplyr::distinct()


seguidores_por_alinhamento <- quem_segue_quem |>
  dplyr::select(author_seguidor,
                eh_perfil_extrema_direita,
                eh_perfil_frente_ampla) |>
  dplyr::group_by(author_seguidor) |>
  dplyr::summarise(
    soma_seguindo_direita = sum(eh_perfil_extrema_direita),
    soma_seguindo_frente_ampla = sum(eh_perfil_frente_ampla)
  ) |>
  dplyr::left_join(usuarios)



# ---
seguidores_por_alinhamento_base_completa <-
  base_deteccao_por_palavras |>
  dplyr::left_join(seguidores_por_alinhamento, by = "autor_username") |>
  dplyr::mutate(
    diferenca_qnt_seguidores = soma_seguindo_direita - soma_seguindo_frente_ampla,
    eh_direita = dplyr::case_when(
      author_description_extrem == TRUE ~ TRUE,
      author_name_extrem == TRUE ~ TRUE,
      diferenca_qnt_seguidores >= 25 ~ TRUE,
      TRUE ~ NA
    )
  ) |>
  dplyr::arrange(desc(diferenca_qnt_seguidores))


# salvar as bases!
seguidores_por_alinhamento_base_completa |>
  writexl::write_xlsx("dados_output/08-analise-seguidores/seguidores-por-alinhamento.xlsx")


# exportar uma versão sumarizada.

seguidores_por_alinhamento_base_completa |>
  dplyr::count(eh_direita, orientacao_categoria, categoria) |>
  writexl::write_xlsx(
    "dados_output/08-analise-seguidores/seguidores-por-alinhamento_sumarizada.xlsx"
  )
