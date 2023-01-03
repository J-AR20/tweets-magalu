# library(academictwitteR)
library(readr)
# library(writexl)
# library(purrr)
# library(readxl)
library(dplyr)
# library(janitor)
library(ggplot2)
# install.packages('esquisse')
library(esquisse)

# lendo a base completa
base_completa <- read_rds('dados_brutos/04-unir-bases/base_tweets_completa.rds') %>%
  filter(!author_username %in% c('magalu', 'luizatrajano'))
# removi acima os tweets obviamente partidários da magalu e luizatrajano

linha_do_tempo <-
  googlesheets4::read_sheet(
    "https://docs.google.com/spreadsheets/d/1z3oNAxrIYHZkjwVirRxLKiGMQAyxGE8cAemfXvusUUY/edit#gid=1890916712",
    "linha_do_tempo"
  )


linha_do_tempo_alterada <- linha_do_tempo |>
  tidyr::separate(
    evento,
    sep = ": ",
    into = c("evento", "desc_evento"),
    remove = FALSE
  ) |>
  dplyr::mutate(data = as.Date(data))


# Gráfico de tweets ao longo do tempo -------------------------------------


# preparação da base
tweets_por_data <- base_completa %>%
  mutate(data = as.Date(created_at)) %>%  # ajustando a data
  dplyr::left_join(linha_do_tempo_alterada) |>
  count(data, evento, desc_evento, plot) # contando quantas linhas existe para cada data


tweets_por_data_com_evento <-
  tidyr::drop_na(tweets_por_data, evento) |>
  dplyr::mutate(legenda = dplyr::case_when(plot == "SIM" ~ stringr::str_wrap(paste0(
    evento, ": ", desc_evento
  ), 20),
  TRUE ~ evento))



# fazendo o gráfico de data através do pacote esquisse
grafico_linha_do_tempo <- ggplot(tweets_por_data) +
  aes(x = data, y = n) +
  geom_line(size = 0.5, colour = "#112446") +
  geom_point(data = tweets_por_data_com_evento, show.legend = FALSE) +
  ggrepel::geom_text_repel(
    data = tweets_por_data_com_evento,
    aes(label = legenda, color = plot),
    size = 3,
    show.legend = FALSE,
    nudge_y = 5000,
    nudge_x = 1.5,
    segment.color = "gray"
  ) +
  labs(#  caption = "\n Fonte: Dados extraídos da API do Twitter usando o pacote academictwitteR",
    y = "Quantidade de Tweets",
    x = "Data") +
  scale_x_date(
    date_labels = "%d/%m",
    limits = c(min(tweets_por_data$data), max(tweets_por_data$data)),
    date_breaks = "3 day"
  ) +
  theme_light() +
  scale_color_manual(values = c("gray50", "black"))

grafico_linha_do_tempo

ggsave(filename = "graficos/grafico_linha_do_tempo.png", plot = grafico_linha_do_tempo)




# grafico_simples_por_data
grafico_simples_por_data <- ggplot(tweets_por_data) +
  aes(x = data, y = n) +
  geom_line(size = 0.5, colour = "#112446") +
  labs(#  caption = "\n Fonte: Dados extraídos da API do Twitter usando o pacote academictwitteR",
    y = "Quantidade de Tweets",
    x = "Data") +
  scale_x_date(
    date_labels = "%d/%m",
    limits = c(min(tweets_por_data$data), max(tweets_por_data$data)),
    date_breaks = "3 day"
  ) +
  theme_light()

grafico_simples_por_data

ggsave(filename = "graficos/grafico_simples_por_data.png", plot = grafico_linha_do_tempo)
