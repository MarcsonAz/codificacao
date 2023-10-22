
# apoio_graficos_principais

# GRAFICOS DAS VARIAVEIS PRINCIAPSI DO TRABALHO
# 
library(grid)
library(tidyverse)
#library(shadowtext)
require(RColorBrewer)
#require(wesanderson)
require(ggthemes)


######################################################################

# PREFERÊNCIA
# SOMENTE REMOTO 

df4 <- read_xlsx(nome_arquivo_tratado_perfil)

#total
df4 %>%
  filter(indicador_remoto_hibrido == 1) %>% 
  group_by(preferencia_pessoal) %>% 
  summarise(count=n()) %>% 
  mutate(prop = round((count/sum(count))*100)) %>% 
  ungroup() %>%
  #mutate(names = dias_trabalho %>% forcats::fct_reorder(prop, sum)) %>%
  
  ggplot(aes(x = preferencia_pessoal, y = prop, fill=preferencia_pessoal ),prop) +
  geom_bar(position="dodge", stat="identity",width = .5)  +
  geom_text(aes(label = paste(format(prop), "%"),fontface = "bold"),
            position = position_dodge(0.9),
            vjust = -1.01) +
  
  scale_fill_brewer() +
  labs(title = 'Prefere trabalhar',
       subtitle = 'Trabalhadores(as) que têm ou tiveram experiência remoto/híbrido (353)',
       x = '',
       y = '') +
  theme_fivethirtyeight() +
  theme(
    #legend.title = element_blank(),
    legend.position = 'none',
  )

ggsave("./graficos/plot_principais_preferencia_total2.png",
       width = 15, height = 18, units = "cm")

######################################################################

#setor
df4 %>%
  filter(indicador_remoto_hibrido == 1,
         setor %in% c('publico','privado')) %>% 
  group_by(setor,preferencia_pessoal) %>% 
  summarise(count=n()) %>% 
  mutate(prop = round((count/sum(count))*100)) %>% 
  ungroup() %>%
  #mutate(names = dias_trabalho %>% forcats::fct_reorder(prop, sum)) %>%
  
  ggplot(aes(x = setor, y = prop,fill = factor(preferencia_pessoal)),prop) +
  geom_bar(position="dodge", stat="identity")  +
  geom_text(aes(label = paste(format(prop), "%")),
            position = position_dodge(0.9),
            vjust = -1.01) +
  
  scale_fill_brewer() +
  labs(title = 'Preferência segundo setor',
       subtitle = 'Remoto - 311 - somente setor público ou privado',
       x = '',
       y = '') +
  theme_fivethirtyeight() +
  theme(
    legend.title = element_blank(),
    # legend.position = 'none',
  )

ggsave("./graficos/plot_principais_preferencia_setor.png",
       width = 20, height = 20, units = "cm")

######################################################################

######################################################################

#profissao

profissoes_mais_frequentes = df4 %>% 
  group_by(profissao_tratado) %>% 
  summarise(count=n()) %>% 
  ungroup() %>% 
  arrange(desc(count)) %>% 
  filter(count>=10) 

sum(profissoes_mais_frequentes$count) # 238
profissoes_mais_frequentes = profissoes_mais_frequentes$profissao_tratado

apoio_df4 <- tibble(
  profissao_tratado = rep(profissoes_mais_frequentes,each=3),
  preferencia_pessoal = rep(c('100% Presencial.','100% Remotamente.','Híbrido.'),
                            length(profissoes_mais_frequentes)))
left_join(apoio_df4, 
df4 %>%
  filter(indicador_remoto_hibrido == 1,
         profissao_tratado %in% profissoes_mais_frequentes) %>% 
         
  group_by(profissao_tratado,preferencia_pessoal) %>% 
  summarise(count=n()) %>% 
  ungroup()
) %>%
  mutate(count = ifelse(is.na(count),0,count)) %>% 
  group_by(profissao_tratado) %>% 
  mutate(prop = round((count/sum(count))*100)) %>% 
  ungroup() %>% 
  
  ggplot(aes(x = profissao_tratado, y = prop,fill = factor(preferencia_pessoal)),prop) +
  geom_bar(position="dodge", stat="identity")  +
  geom_text(aes(label = paste(format(prop), "%")),
            position = position_dodge(0.9),
            vjust = -1.01) +
  
  scale_fill_brewer() +
  labs(title = 'Preferência segundo profissão',
       subtitle = 'Profissões com frequência acima de 10 respostas - 238',
       x = '',
       y = '') +
  theme_fivethirtyeight() +
  theme(
    legend.title = element_blank(),
    # legend.position = 'none',
  )

ggsave("./graficos/plot_principais_preferencia_profissao.png",
       width = 30, height = 20, units = "cm")

######################################################################

#sexo
df4 %>%
  filter(indicador_remoto_hibrido == 1) %>% 
  group_by(sexo,preferencia_pessoal) %>% 
  summarise(count=n()) %>% 
  mutate(prop = round((count/sum(count))*100)) %>% 
  ungroup() %>%
  
  ggplot(aes(x = sexo, y = prop,fill = factor(preferencia_pessoal)),prop) +
  geom_bar(position="dodge", stat="identity")  +
  
  #facet_grid(cols = vars(sexo)) + 
  #coord_flip() +
  geom_text(aes(label = paste(format(prop), "%")),
            position = position_dodge(0.9),
            vjust = -1.01) +
  scale_y_continuous(limits = c(0,70)) +
  scale_fill_brewer() +
  #scale_fill_manual(values = c("#FF7171","#FF3333","#FF3333",'#71C1FF','#71C1FF',"#3377FF")) +
  labs(title = 'Preferência - Sexo',
       subtitle = 'Trabalhadores(as) que têm ou tiveram experiência remoto/híbrido (353)',
       x = '',
       y = '') +
  theme_fivethirtyeight() +
  theme(
    legend.title = element_blank(),
    # legend.position = 'none',
  )

ggsave("./graficos/plot_principais_preferencia_sexo2.png",
       width = 20, height = 20, units = "cm")

######################################################################


# fx etaria
rbind(df4 %>%
  filter(indicador_remoto_hibrido == 1) %>% 
  group_by(faixa_etaria,preferencia_pessoal) %>% 
  summarise(count=n()) %>% 
  mutate(prop = round((count/sum(count))*100)) %>% 
  ungroup(),
  
  tibble (
    faixa_etaria = c('60_mais','60_mais'),
    preferencia_pessoal = c('100% Presencial.','100% Remotamente.'),
    count = c(0,0),
    prop = c(0,0)
  )) %>%
  
  ggplot(aes(x = faixa_etaria, y = prop,fill = factor(preferencia_pessoal)),prop) +
  geom_bar(position="dodge", stat="identity")  +
  geom_text(aes(label = paste(format(prop), "%")),
            position = position_dodge(0.9),
            vjust = -1.01) +
  
  scale_fill_brewer() +
  labs(title = 'Preferência - Faixa etária',
       subtitle = 'Trabalhadores(as) que têm ou tiveram experiência remoto/híbrido (353)',
       x = '',
       y = '') +
  theme_fivethirtyeight() +
  theme(
    legend.title = element_blank(),
    # legend.position = 'none',
  )

ggsave("./graficos/plot_principais_preferencia_faixa_etaria2.png",
       width = 30, height = 20, units = "cm")

######################################################################


# ter filhos
df4 %>%
  filter(indicador_remoto_hibrido == 1) %>% 
  group_by(ter_filhos,preferencia_pessoal) %>% 
  summarise(count=n()) %>% 
  mutate(prop = round((count/sum(count))*100),
         ter_filhos2 = case_when(
           ter_filhos == 'nao' ~ 'Não',
           ter_filhos == 'sim_1' ~ 'Sim (1)',
           ter_filhos == 'sim_2_mais' ~ 'Sim (2+)'
         )) %>% 
  ungroup() %>%
  
  ggplot(aes(x = ter_filhos2, y = prop,fill = factor(preferencia_pessoal)),prop) +
  geom_bar(position="dodge", stat="identity")  +
  geom_text(aes(label = paste(format(prop), "%")),
            position = position_dodge(0.80),
            vjust = -1.01) +
  
  scale_fill_brewer() +
  labs(title = 'Preferência - Filhos',
       subtitle = 'Trabalhadores(as) que têm ou tiveram experiência remoto/híbrido (353)',
       x = '',
       y = '') +
  theme_fivethirtyeight() +
  theme(
    legend.title = element_blank(),
    # legend.position = 'none',
  )

ggsave("./graficos/plot_principais_preferencia_ter_filhos2.png",
       width = 25, height = 20, units = "cm")

######################################################################

# moradores
# 
rbind( df4 %>%
  filter(indicador_remoto_hibrido == 1) %>% 
  group_by(local_residencia,preferencia_pessoal) %>% 
  summarise(count=n()) %>% 
  mutate(prop = round((count/sum(count))*100)) %>% 
  ungroup(),
  tibble (
    local_residencia = 'companheiro_familia',
    preferencia_pessoal = '100% Remotamente.',
    count = 0,
    prop = 0)
  ) %>%
  
  mutate(local_residencia2 = case_when(
             local_residencia == 'filho' ~ 'com filho/a(os/as)',
             local_residencia == 'sozinho' ~ 'sozinho(a)',
             local_residencia == 'familia' ~ 'com familiares',
             local_residencia == 'companheiro' ~ 'companheiro(a)',
             local_residencia == 'companheiro_filho' ~ 'companheiro(a) + filho/a(os/as)',
             local_residencia == 'companheiro_familia' ~ 'companheiro(a) + familiares',
             local_residencia == 'colegas_quarto' ~ 'sem parentesco'
           )) %>% 
  
  ggplot(aes(x = local_residencia2, y = prop,fill = factor(preferencia_pessoal)),prop) +
  geom_bar(position="dodge", stat="identity")  +
  geom_text(aes(label = paste(format(prop), "%")),
            position = position_dodge(0.9),
            vjust = -1.01) +
  
  scale_fill_brewer() +
  labs(title = 'Preferência segundo pessoas no mesmo domicílio',
       subtitle = 'Remoto - 353',
       x = '',
       y = '') +
  theme_fivethirtyeight() +
  theme(
    legend.title = element_blank(),
    # legend.position = 'none',
  )

ggsave("./graficos/plot_principais_preferencia_local_residencia2.png",
       width = 35, height = 20, units = "cm")

######################################################################
######################################################################
######################################################################
######################################################################


# AVALIAÇÃO DE EXPERIÊNCIA REMOTA OU HIBRIDA
# SOMENTE REMOTO 

df5 <- df4 # APLICANDO O TRATAMENTO PRINCIPAIS

#total
df5 %>%
  filter(indicador_remoto_hibrido == 1) %>% 
  group_by(avaliacao_geral2) %>% 
  summarise(count=n()) %>% 
  mutate(prop = round((count/sum(count))*100)) %>% 
  ungroup() %>%
  mutate(avaliacao_geral2 = factor(
    avaliacao_geral2, levels= c("Muito ruim.","Ruim.","Regular.","Boa.","Muito boa."))) %>%
  
  ggplot(aes(x = avaliacao_geral2, y = prop, fill=factor(avaliacao_geral2) ),prop) +
  geom_bar(position="dodge", stat="identity")  +
  geom_text(aes(label = paste(format(prop), "%")),
            position = position_dodge(0.9),
            vjust = -1.01) +
  
  scale_fill_brewer(palette = 'RdYlGn') +
  labs(title = 'Avaliação da experiência no trabalho remoto',
       subtitle = 'Trabalhadores(as) que têm ou tiveram experiência remoto/híbrido (353)',
       x = '',
       y = '') +
  theme_fivethirtyeight() +
  theme(
    legend.title = element_blank(),
    # legend.position = 'none',
  )

ggsave("./graficos/plot_principais_avaliacao_geral_total.png",
       width = 25, height = 20, units = "cm")

######################################################################

# sexo 

  df5 %>%
    filter(indicador_remoto_hibrido == 1) %>% 
    group_by(sexo,avaliacao_geral2) %>% 
    summarise(count=n()) %>% 
    mutate(prop = round((count/sum(count))*100)) %>% 
    ungroup() %>%
  
  mutate(avaliacao_geral2 = factor(
    avaliacao_geral2, levels= c("Muito ruim.","Ruim.","Regular.","Boa.","Muito boa."))) %>% 
    #setor = factor(setor,levels = c('publico','privado'), labels = c('Público','Privado')))
  

  ggplot(aes(x = avaliacao_geral2, y = prop,fill = factor(sexo)),prop) +
  geom_bar(position="dodge", stat="identity")  +
  geom_text(aes(label = paste(format(prop), "%")),
            position = position_dodge(0.9),
            vjust = -1.01) +
  scale_y_continuous(limits = c(0,50)) +
  scale_fill_brewer(palette = 'Pastel1') +
  labs(title = 'Avaliação de experiência no trabalho remoto - Sexo',
       subtitle = 'Trabalhadores(as) que têm ou tiveram experiência remoto/híbrido (353)',
       x = '',
       y = '') +
  theme_fivethirtyeight() +
  theme(
    legend.title = element_blank(),
    # legend.position = 'none',
  )

ggsave("./graficos/plot_principais_avaliacao_geral2_sexo.png",
       width = 30, height = 20, units = "cm")

######################################################################

# filhos 

df4 %>%
  filter(indicador_remoto_hibrido == 1) %>% 
  group_by(ter_filhos,avaliacao_geral2) %>% 
  summarise(count=n()) %>% 
  mutate(prop = round((count/sum(count))*100)) %>% 
  ungroup() %>%
  mutate(prop = round((count/sum(count))*100),
         ter_filhos2 = case_when(
           ter_filhos == 'nao' ~ 'Não',
           ter_filhos == 'sim_1' ~ 'Sim (1)',
           ter_filhos == 'sim_2_mais' ~ 'Sim (2+)'
         )) %>%
  
  mutate(avaliacao_geral2 = factor(
    avaliacao_geral2, levels= c("Muito ruim.","Ruim.","Regular.","Boa.","Muito boa."))) %>% 
  #setor = factor(setor,levels = c('publico','privado'), labels = c('Público','Privado')))
  
  
  ggplot(aes(x = avaliacao_geral2, y = prop,fill = factor(ter_filhos2)),prop) +
  geom_bar(position="dodge", stat="identity")  +
  geom_text(aes(label = paste(format(prop), "%")),
            position = position_dodge(0.9),
            vjust = -1.01) +
  scale_y_continuous(limits = c(0,50)) +
  scale_fill_brewer(palette = 'Pastel1') +
  labs(title = 'Avaliação de experiência no trabalho remoto - Filhos',
       subtitle = 'Trabalhadores(as) que têm ou tiveram experiência remoto/híbrido (353)',
       x = '',
       y = '') +
  theme_fivethirtyeight() +
  theme(
    legend.title = element_blank(),
    # legend.position = 'none',
  )

ggsave("./graficos/plot_principais_avaliacao_geral2_ter_filhos.png",
       width = 30, height = 20, units = "cm")


######################################################################

# moradores 

df4 %>%
  filter(indicador_remoto_hibrido == 1) %>% 
  mutate(local_residencia2 = case_when(
    local_residencia == 'filho' ~ 'com filho/a(os/as)',
    local_residencia == 'sozinho' ~ 'sozinho(a)',
    local_residencia == 'familia' ~ 'com familiares',
    local_residencia == 'companheiro' ~ 'companheiro(a)',
    local_residencia == 'companheiro_filho' ~ 'companheiro(a) + filho/a(os/as)',
    local_residencia == 'companheiro_familia' ~ 'companheiro(a) + familiares',
    local_residencia == 'colegas_quarto' ~ 'sem parentesco'
  )) %>% 
  group_by(local_residencia2,avaliacao_geral2) %>% 
  summarise(count=n()) %>% 
  mutate(prop = round((count/sum(count))*100)) %>% 
  ungroup() %>%
  
  mutate(avaliacao_geral2 = factor(
    avaliacao_geral2, levels= c("Muito ruim.","Ruim.","Regular.","Boa.","Muito boa."))) %>% 
  #setor = factor(setor,levels = c('publico','privado'), labels = c('Público','Privado')))
  
  
  ggplot(aes(x = avaliacao_geral2, y = prop,fill = factor(local_residencia2)),prop) +
  geom_bar(position="dodge", stat="identity")  +
  geom_text(aes(label = paste(format(prop), "%")),
            position = position_dodge(0.9),
            vjust = -1.01) +
  scale_y_continuous(limits = c(0,76)) +
  scale_fill_brewer(palette = 'Pastel1') +
  labs(title = 'Avaliação de experiência no trabalho remoto - Mora com quem ...',
       subtitle = 'Trabalhadores(as) que têm ou tiveram experiência remoto/híbrido (353)',
       x = '',
       y = '') +
  theme_fivethirtyeight() +
  theme(
    legend.title = element_blank(),
    # legend.position = 'none',
  )

ggsave("./graficos/plot_principais_avaliacao_geral2_mora_com_quem.png",
       width = 30, height = 20, units = "cm")



######################################################################
######################################################################
######################################################################
######################################################################

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

######################################################################

# DIAS DE TRABALHO
# FAZER DUAS VEZES - TOTAL E SOMENTE REMOTO 
df4 <- read_xlsx(nome_arquivo_tratado_perfil)

df4 %>%
  filter(indicador_remoto_hibrido == 1) %>% 
  group_by(dias_trabalho) %>% 
  summarise(count=n()) %>% 
  ungroup() %>%
  mutate(prop = round((count/sum(count))*100)) %>% 
  #mutate(names = dias_trabalho %>% forcats::fct_reorder(prop, sum)) %>%
  
  ggplot(aes(x = dias_trabalho, y = prop, fill = factor(prop))) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = paste(format(prop), "%")), hjust = 1.05, colour = "black") +
  scale_fill_brewer() +
  labs(title = 'Dias trabalhados por semana',
       #subtitle = 'Total - 414',
       subtitle = 'Remoto - 353',
       x = '',
       y = '') +
  theme_fivethirtyeight() +
  theme(
    legend.position = 'none',
    
  )

ggsave("./graficos/plot_perfil_remoto_dias_trabalho.png")


#df4 %>% group_by(dias_presencial,dias_remoto, indicador_remoto_hibrido) %>% summarise(n=n())





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



