library(tidyverse)

dados <- read.csv("Pokemon_full.csv")

df_summary <- dados %>% 
  mutate(total_atributos = hp + attack + defense + sp.atk + sp.def + speed) %>%
  group_by(type) %>% 
  summarise(
    media_total = mean(total_atributos),
    sd_total = sd(total_atributos)
  ) %>% 
  arrange(media_total) %>% 
  mutate(type = factor(type, levels = type))

ggplot(df_summary, aes(x = type, y = media_total, color = type)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = media_total - sd_total, ymax = media_total + sd_total), width = 0.2) +
  labs(
    x = "Tipo", 
    y = "Total de Atributos",
    title = "Comparação do Poder Geral por Tipo de Pokémon"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 11, face = "plain"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1.0, size = 11, face = "plain"),
    legend.position = "none"
  )

ggsave("grafico_pokemon.png", width = 6, height = 5)