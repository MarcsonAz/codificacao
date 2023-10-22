# GRAFICOS DE PERFIL DA POPULAÇÃO
# 
library(grid)
library(tidyverse)
library(shadowtext)
require(RColorBrewer)
require(wesanderson)
require(ggthemes)



nomes_profissoes = sort( unique(df3$profissao_tratado))
nomes_profissoes

prof = df3 %>% 
  group_by(profissao_tratado) %>% 
  summarise(count=n()) %>% 
  ungroup() %>% 
  mutate( profissao_tratado = factor(profissao_tratado, levels = nomes_profissoes)) %>% 
  arrange(desc(count))

prof2 = df3 %>% 
  filter(indicador_remoto_hibrido == 1) %>% 
  group_by(profissao_tratado) %>% 
  summarise(count=n()) %>% 
  ungroup() %>% 
  mutate( profissao_tratado = factor(profissao_tratado, levels = nomes_profissoes)) %>% 
  arrange(desc(count))

# profissao
# BASIC CHART
plt <- prof %>%
  filter(count > 2) %>% 
  mutate(names = profissao_tratado %>% forcats::fct_reorder(count, sum)) %>%
  ggplot(aes(x = names, y = count, fill = count)) + 
  geom_col() +
  coord_flip() 

# BASIC CHART - quem ja trabalhou hibrido ou remoto
plt2 <- prof2 %>%
  filter(count > 2) %>% 
  mutate(names = profissao_tratado %>% forcats::fct_reorder(count, sum)) %>%
  ggplot(aes(x = names, y = count, fill = count)) + 
  geom_col() +
  coord_flip() 

######################################################################

# SEXO
# FAZER DUAS VEZES - TOTAL E SOMENTE REMOTO 

df3 %>%
  filter(indicador_remoto_hibrido == 1) %>% 
  group_by(sexo) %>% 
  summarise(count=n()) %>% 
  ungroup() %>%
  mutate(prop = round((count/sum(count))*100)) %>% 
  mutate(names = sexo %>% forcats::fct_reorder(prop, sum)) %>%
  
  ggplot(aes(x = names, y = prop, fill = factor(prop))) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = paste(format(prop), "%")), hjust = 1.05, colour = "black") +
  scale_fill_brewer() +
  labs(title = 'Sexo',
       subtitle = 'Total - 414',
       #subtitle = 'Remoto - 353',
       x = '',
       y = '') +
  theme_fivethirtyeight() +
  theme(
    legend.position = 'none',
    
  )

ggsave("./graficos/plot_perfil_total_sexo.png")

######################################################################
# FAIXA ETARIA
# FAZER DUAS VEZES - TOTAL E SOMENTE REMOTO 

df3 %>%
  filter(indicador_remoto_hibrido == 1) %>% 
  group_by(faixa_etaria) %>% 
  summarise(count=n()) %>% 
  ungroup() %>%
  mutate(prop = round((count/sum(count))*100)) %>% 
  
  ggplot(aes(x = faixa_etaria, y = prop, fill = factor(prop))) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = paste(format(prop), "%")), hjust = 1.05, colour = "black") +
  scale_fill_brewer() +
  labs(title = 'Faixa etária',
       #subtitle = 'Total - 414',
       subtitle = 'Remoto - 353',
       x = '',
       y = 'Proporção') +
  theme_fivethirtyeight() +
  theme(
    legend.position = 'none',
    
  )

ggsave("./graficos/plot_perfil_total_faixa_etaria.png")
ggsave("./graficos/plot_perfil_remoto_faixa_etaria.png")


######################################################################

# TER FILHOS
# FAZER DUAS VEZES - TOTAL E SOMENTE REMOTO 

df3 %>%
  filter(indicador_remoto_hibrido == 1) %>% 
  group_by(ter_filhos) %>% 
  summarise(count=n()) %>% 
  ungroup() %>%
  mutate(prop = round((count/sum(count))*100)) %>% 
  
  ggplot(aes(x = ter_filhos, y = prop, fill = factor(prop))) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = paste(format(prop), "%")), hjust = 1.05, colour = "black") +
  scale_fill_brewer() +
  labs(title = 'Ter filhos',
       #subtitle = 'Total - 414',
       subtitle = 'Remoto - 353',
       x = '',
       y = 'Proporção') +
  theme_fivethirtyeight() +
  theme(
    legend.position = 'none',
    
  )

#ggsave("./graficos/plot_perfil_total_ter_filhos.png")
ggsave("./graficos/plot_perfil_remoto_ter_filhos.png")

######################################################################

# moradores
# FAZER DUAS VEZES - TOTAL E SOMENTE REMOTO 

df3 %>%
  #filter(indicador_remoto_hibrido == 1) %>% 
  group_by(local_residencia) %>% 
  summarise(count=n()) %>% 
  ungroup() %>%
  mutate(prop = round((count/sum(count))*100)) %>% 
  
  ggplot(aes(x = local_residencia, y = prop, fill = factor(prop))) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = paste(format(prop), "%")), hjust = 1.05, colour = "black") +
  scale_fill_brewer() +
  labs(title = 'Moradores',
       subtitle = 'Total - 414',
       #subtitle = 'Remoto - 353',
       x = '',
       y = 'Proporção') +
  theme_fivethirtyeight() +
  theme(
    legend.position = 'none',
    
  )

ggsave("./graficos/plot_perfil_total_local_residencia.png")
#ggsave("./graficos/plot_perfil_remoto_local_residencia.png")

######################################################################










# CUSTUMIZE LAYOUT
plt <- plt + 
  # scale_x_discrete(
  #   limits = c(0, 50),
  #   breaks = seq(0, 50, by = 5), 
  #   expand = c(0, 0), # The horizontal axis does not extend to either side
  #   position = "top"  # Labels are located on the top
  # ) +
  # # The vertical axis only extends upwards 
  # scale_y_discrete(expand = expansion(add = c(0, 0.5))) +
  theme(
    # Set background color to white
    panel.background = element_rect(fill = "white"),
    # Set the color and the width of the grid lines for the horizontal axis
    panel.grid.major.x = element_line(color = "#A8BAC4", linewidth = 0.3),
    # Remove tick marks by setting their length to 0
    axis.ticks.length = unit(0, "mm"),
    # Remove the title for both axes
    axis.title = element_blank(),
    # Only left line of the vertical axis is painted in black
    axis.line.y.left = element_line(color = "black")
  )

plt



