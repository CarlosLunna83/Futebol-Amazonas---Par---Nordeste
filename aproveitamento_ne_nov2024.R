# Aproveitamento das equipes da Amazonas - Pará - Nordeste no mês de Novembro de 2024 (considerando só jogos oficiais)

# Objetivo:
# - Criar script para automatizar aproveitamento dos times da série C, mês a mês

# Vídeos auxiliares do gráfico https://www.youtube.com/watch?v=oY8ZzmRwqxs
#                              https://www.youtube.com/watch?v=_mvT1kxkx60
#                              https://www.youtube.com/watch?v=8h8-7Z31Wj0
# Post auxiliar do gráfico https://pt.stackoverflow.com/questions/407685/gr%C3%A1fico-de-barras-no-ggplot-no-r

library(tidyverse)
library(ggplot2)
library(readODS)
library(janitor)
library(scales)
library(installr)

times_am_pa_ne_geral_2024 <- read_ods('Futebol - Amazonas - Para - Nordeste 2024.ods') %>%
  mutate(qtd_unidade = as.numeric(qtd_unidade)) %>%
  mutate(pontos_ganhos = as.numeric(pontos_ganhos))

colnames(times_am_pa_ne_geral_2024)

class(times_am_pa_ne_geral_2024$qtd_unidade)
class(times_am_pa_ne_geral_2024$pontos_ganhos)

aproveitamento_times_am_pa_ne_nov2024 <- times_am_pa_ne_geral_2024 %>%
  select(time, pontos_ganhos, meses, qtd_unidade) %>%
  filter(str_detect(meses, "novembro")) %>%
  group_by(time) %>%
  summarise(pontos_ganhos_geral = sum(pontos_ganhos),
            qtd_pontos_disputados = sum(qtd_unidade)*3,
            ranking_aproveitamento = pontos_ganhos_geral/qtd_pontos_disputados)%>%
  arrange(desc(ranking_aproveitamento))

aproveitamento_times_am_pa_ne_nov2024

view(unique(times_am_pa_ne_geral_2024$time))

view(aproveitamento_times_am_pa_ne_nov2024)

# Gráfico:

aproveitamento_times_am_pa_ne_nov2024$time <- factor(aproveitamento_times_am_pa_ne_nov2024$time, 
                                                        levels = unique(aproveitamento_times_am_pa_ne_nov2024$time))

aproveitamento_times_am_pa_ne_nov2024 %>%
  ggplot(aes(reorder(x = time, ranking_aproveitamento), y = ranking_aproveitamento)) +
  geom_bar(stat = "identity", fill = "gray", color = "black", alpha = 0.5) +
  coord_flip() +  
  scale_y_continuous(labels = percent_format()) +
  geom_label(aes(label = percent(ranking_aproveitamento, accuracy = 0.1)),
             position = position_dodge(0.9), 
             vjust = 0.5, size = 1.8, hjust = 0.5) +
  labs(title = "Aproveitamento times AM PA e NE em Novembro 2024",
       x = "Times",
       y = "Aproveitamento")

# Jogos disputados por cada equipe em Novembro 2024:

jogos_disputados_am_pa_ne_nov2024 <- times_am_pa_ne_geral_2024 %>%
  select(time, meses, qtd_unidade) %>%
  filter(str_detect(meses, "novembro")) %>%
  group_by(time) %>%
  summarise(jogos_disputados = sum(qtd_unidade)) %>%
  arrange(desc(jogos_disputados))

jogos_disputados_am_pa_ne_nov2024
