
# apoio_graficos_perfil

# GRAFICOS DE PERFIL DA POPULACAO
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
prof2 %>%
  filter(count > 2) %>% 
  mutate(names = profissao_tratado %>% forcats::fct_reorder(count, sum)) %>%
  ggplot(aes(x = names, y = count, fill = count)) + 
  geom_col() +
  coord_flip() +
  geom_text(aes(label=count), hjust = -0.25, colour = "black") +
  #scale_y_continuous(limits = c(0, 62)) +
  #scale_fill_brewer(palette= "Paired") +
  labs(title = 'Profissão',
       #subtitle = 'Total - 414',
       subtitle = 'Trabalhadores(as) que têm ou tiveram experiência remoto/híbrido (353)',
       x = '',
       y = '') +
  theme_fivethirtyeight() +
  theme(
    legend.position = 'none',
  )

ggsave("./graficos/plot_perfil_remoto_profissao.png",
       width = 25, height = 22, units = "cm")

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
  geom_text(aes(label = paste(format(prop), "%")), hjust = -0.25, colour = "black") +
  scale_y_continuous(limits = c(0, 62)) +
  scale_fill_brewer(palette= "Paired") +
  labs(title = 'Sexo',
       #subtitle = 'Total - 414',
       subtitle = 'Trabalhadores(as) que têm ou tiveram experiência remoto/híbrido (353)',
       x = '',
       y = '') +
  theme_fivethirtyeight() +
  theme(
    legend.position = 'none',
    
  )

ggsave("./graficos/plot_perfil_remoto_sexo.png")

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
  geom_text(aes(label = paste(format(prop), "%")), hjust = -0.25, colour = "black") +
  scale_fill_brewer() +
  scale_y_continuous(limits = c(0, 50)) +
  labs(title = 'Faixa etária',
       #subtitle = 'Total - 414',
       subtitle = 'Trabalhadores(as) que têm ou tiveram experiência remoto/híbrido (353)',
       x = '',
       y = 'Proporção') +
  theme_fivethirtyeight() +
  theme(
    legend.position = 'none',
    
  )

#ggsave("./graficos/plot_perfil_total_faixa_etaria.png")
ggsave("./graficos/plot_perfil_remoto_faixa_etaria.png",
       width = 20, height = 16, units = "cm")


######################################################################

# TER FILHOS
# FAZER DUAS VEZES - TOTAL E SOMENTE REMOTO 

df3 %>%
  filter(indicador_remoto_hibrido == 1) %>% 
  group_by(ter_filhos) %>% 
  summarise(count=n()) %>% 
  ungroup() %>%
  mutate(prop = round((count/sum(count))*100),
         ter_filhos2 = case_when(
           ter_filhos == 'nao' ~ 'Não',
           ter_filhos == 'sim_1' ~ 'Sim (1)',
           ter_filhos == 'sim_2_mais' ~ 'Sim (2+)'
         )) %>% 
# ter_filhos = factor(ter_filhos,levels = c('nao','sim_1','sim_2_mais',
#                              labels = c('Não','Sim (1)','Sim (2+)')))) %>% 
  
  ggplot(aes(x = ter_filhos2, y = prop, fill = factor(prop))) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = paste(format(prop), "%")), hjust = -0.25, colour = "black") +
  scale_y_continuous(limits = c(0,65)) +
  scale_fill_brewer() +
  labs(title = 'Filhos',
       #subtitle = 'Total - 414',
       subtitle = 'Trabalhadores(as) que têm ou tiveram experiência remoto/híbrido (353)',
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
  filter(indicador_remoto_hibrido == 1) %>% 
  group_by(local_residencia) %>% 
  summarise(count=n()) %>% 
  ungroup() %>%
  mutate(prop = round((count/sum(count))*100),
         local_residencia2 = case_when(
           local_residencia == 'filho' ~ 'com filho/a(os/as)',
           local_residencia == 'sozinho' ~ 'sozinho(a)',
           local_residencia == 'familia' ~ 'com familiares',
           local_residencia == 'companheiro' ~ 'companheiro(a)',
           local_residencia == 'companheiro_filho' ~ 'companheiro(a) + filho/a(os/as)',
           local_residencia == 'companheiro_familia' ~ 'companheiro(a) + familiares',
           local_residencia == 'colegas_quarto' ~ 'sem parentesco'
  )) %>% 
  
  ggplot(aes(x = local_residencia2, y = prop, fill = factor(prop))) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = paste(format(prop), "%")), hjust = -0.25, colour = "black") +
  scale_fill_brewer() +
  scale_y_continuous(limits = c(0,30)) +
  labs(title = 'Você mora com ...',
       #subtitle = 'Total - 414',
       subtitle = 'Trabalhadores(as) que têm ou tiveram experiência remoto/híbrido (353)',
       x = '',
       y = 'Proporção') +
  theme_fivethirtyeight() +
  theme(
    legend.position = 'none',
    
  )

#ggsave("./graficos/plot_perfil_total_local_residencia.png")
ggsave("./graficos/plot_perfil_remoto_local_residencia.png")


######################################################################
# dias de trabalho
# SOMENTE REMOTO 

df3 %>%
  filter(indicador_remoto_hibrido == 1) %>% 
  group_by(dias_trabalho) %>% 
  summarise(count=n()) %>% 
  ungroup() %>%
  mutate(prop = round((count/sum(count))*100),
         dias_trabalho2 = factor(dias_trabalho,levels = paste0(6:1))
         ) %>% 
  #        local_residencia2 = case_when(
  #          local_residencia == 'filho' ~ 'com filho/a(os/as)',
  #          local_residencia == 'sozinho' ~ 'sozinho(a)',
  #          local_residencia == 'familia' ~ 'com familiares',
  #          local_residencia == 'companheiro' ~ 'companheiro(a)',
  #          local_residencia == 'companheiro_filho' ~ 'companheiro(a) + filho/a(os/as)',
  #          local_residencia == 'companheiro_familia' ~ 'companheiro(a) + familiares',
  #          local_residencia == 'colegas_quarto' ~ 'sem parentesco'
  #        )) %>% 
  
  ggplot(aes(x = dias_trabalho2, y = prop, fill = factor(prop))) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = paste(format(prop), "%")), hjust = -0.25, colour = "black") +
  scale_fill_brewer() +
  scale_y_continuous(limits = c(0,55)) +
  labs(title = 'Dias de trabalho remoto',
       #subtitle = 'Total - 414',
       subtitle = 'Trabalhadores(as) que têm ou tiveram experiência remoto/híbrido (353)',
       x = '',
       y = 'Proporção') +
  theme_fivethirtyeight() +
  theme(
    legend.position = 'none',
    
  )

#ggsave("./graficos/plot_perfil_total_local_residencia.png")
ggsave("./graficos/plot_perfil_remoto_dias_trabalho2.png")

######################################################################
# meio transporte
# SOMENTE REMOTO 

df3 %>%
  filter(indicador_remoto_hibrido == 1,
         !is.na(deslocamento_transporte)) %>% 
  group_by(deslocamento_transporte) %>% 
  summarise(count=n()) %>% 
  ungroup() %>%
  mutate(prop = round((count/sum(count))*100) #,
         #dias_trabalho2 = factor(dias_trabalho,levels = paste0(6:1))
  ) %>% 
  #        local_residencia2 = case_when(
  #          local_residencia == 'filho' ~ 'com filho/a(os/as)',
  #          local_residencia == 'sozinho' ~ 'sozinho(a)',
  #          local_residencia == 'familia' ~ 'com familiares',
  #          local_residencia == 'companheiro' ~ 'companheiro(a)',
  #          local_residencia == 'companheiro_filho' ~ 'companheiro(a) + filho/a(os/as)',
  #          local_residencia == 'companheiro_familia' ~ 'companheiro(a) + familiares',
  #          local_residencia == 'colegas_quarto' ~ 'sem parentesco'
  #        )) %>% 
  
  ggplot(aes(x = deslocamento_transporte, y = prop, fill = factor(prop))) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = paste(format(prop), "%")), hjust = -0.25, colour = "black") +
  scale_fill_brewer() +
  scale_y_continuous(limits = c(0,55)) +
  labs(title = 'Meio de transporte (dias presenciais)',
       #subtitle = 'Total - 414',
       subtitle = 'Trabalhadores(as) que têm ou tiveram experiência remoto/híbrido (353)',
       x = '',
       y = 'Proporção') +
  theme_fivethirtyeight() +
  theme(
    legend.position = 'none',
    
  )

#ggsave("./graficos/plot_perfil_total_local_residencia.png")
ggsave("./graficos/plot_perfil_remoto_meio_transporte.png")
######################################################################

# setor
# SOMENTE REMOTO 

df3 %>%
  filter(indicador_remoto_hibrido == 1,
         !is.na(setor2)) %>% 
  group_by(setor2) %>% 
  summarise(count=n()) %>% 
  ungroup() %>%
  mutate(prop = round((count/sum(count))*100),
         setor2 = factor(setor2)
         #dias_trabalho2 = factor(dias_trabalho,levels = paste0(6:1))
  ) %>% 
  #        local_residencia2 = case_when(
  #          local_residencia == 'filho' ~ 'com filho/a(os/as)',
  #          local_residencia == 'sozinho' ~ 'sozinho(a)',
  #          local_residencia == 'familia' ~ 'com familiares',
  #          local_residencia == 'companheiro' ~ 'companheiro(a)',
  #          local_residencia == 'companheiro_filho' ~ 'companheiro(a) + filho/a(os/as)',
  #          local_residencia == 'companheiro_familia' ~ 'companheiro(a) + familiares',
  #          local_residencia == 'colegas_quarto' ~ 'sem parentesco'
  #        )) %>% 
  
  ggplot(aes(x = setor2, y = prop, fill = factor(prop))) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = paste(format(prop), "%")), hjust = -0.25, colour = "black") +
  scale_fill_brewer() +
  scale_y_continuous(limits = c(0,62)) +
  labs(title = 'Setor',
       #subtitle = 'Total - 414',
       subtitle = 'Trabalhadores(as) que têm ou tiveram experiência remoto/híbrido (350)',
       x = '',
       y = 'Proporção') +
  theme_fivethirtyeight() +
  theme(
    legend.position = 'none',
    
  )

#ggsave("./graficos/plot_perfil_total_local_residencia.png")
ggsave("./graficos/plot_perfil_remoto_setor.png")
######################################################################

# tempo gasto
# SOMENTE REMOTO 

df3 %>%
  filter(indicador_remoto_hibrido == 1) %>% 
         #!is.na(setor2)) %>% 
  group_by(deslocamento_tempo) %>% 
  summarise(count=n()) %>% 
  ungroup() %>%
  mutate(prop = round((count/sum(count))*100)) %>% 
         #setor2 = factor(setor2)
         #dias_trabalho2 = factor(dias_trabalho,levels = paste0(6:1))
  #) #%>% 
  #        local_residencia2 = case_when(
  #          local_residencia == 'filho' ~ 'com filho/a(os/as)',
  #          local_residencia == 'sozinho' ~ 'sozinho(a)',
  #          local_residencia == 'familia' ~ 'com familiares',
  #          local_residencia == 'companheiro' ~ 'companheiro(a)',
  #          local_residencia == 'companheiro_filho' ~ 'companheiro(a) + filho/a(os/as)',
  #          local_residencia == 'companheiro_familia' ~ 'companheiro(a) + familiares',
  #          local_residencia == 'colegas_quarto' ~ 'sem parentesco'
  #        )) %>% 
  
  ggplot(aes(x = deslocamento_tempo, y = prop, fill = factor(prop))) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = paste(format(prop), "%")), hjust = -0.25, colour = "black") +
  scale_fill_brewer() +
  scale_y_continuous(limits = c(0,77)) +
  labs(title = 'Tempo de deslocamento',
       #subtitle = 'Total - 414',
       subtitle = 'Trabalhadores(as) que têm ou tiveram experiência remoto/híbrido (353)',
       x = '',
       y = 'Proporção') +
  theme_fivethirtyeight() +
  theme(
    legend.position = 'none',
    
  )

#ggsave("./graficos/plot_perfil_total_local_residencia.png")
ggsave("./graficos/plot_perfil_remoto_tempo_gasto.png")


######################################################################

# horas_dia
# SOMENTE REMOTO 

df3 %>%
  filter(indicador_remoto_hibrido == 1) %>% 
  #!is.na(setor2)) %>% 
  group_by(horas_dedicacao) %>% 
  summarise(count=n()) %>% 
  ungroup() %>%
  mutate(prop = round((count/sum(count))*100)) %>% 
  #setor2 = factor(setor2)
  #dias_trabalho2 = factor(dias_trabalho,levels = paste0(6:1))
  #) #%>% 
  #        local_residencia2 = case_when(
  #          local_residencia == 'filho' ~ 'com filho/a(os/as)',
  #          local_residencia == 'sozinho' ~ 'sozinho(a)',
  #          local_residencia == 'familia' ~ 'com familiares',
  #          local_residencia == 'companheiro' ~ 'companheiro(a)',
  #          local_residencia == 'companheiro_filho' ~ 'companheiro(a) + filho/a(os/as)',
  #          local_residencia == 'companheiro_familia' ~ 'companheiro(a) + familiares',
  #          local_residencia == 'colegas_quarto' ~ 'sem parentesco'
#        )) %>% 

ggplot(aes(x = horas_dedicacao, y = prop, fill = factor(prop))) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = paste(format(prop), "%")), hjust = -0.25, colour = "black") +
  scale_fill_brewer() +
  scale_y_continuous(limits = c(0,73)) +
  labs(title = 'Horas diárias de dedicação',
       #subtitle = 'Total - 414',
       subtitle = 'Trabalhadores(as) que têm ou tiveram experiência remoto/híbrido (353)',
       x = '',
       y = 'Proporção') +
  theme_fivethirtyeight() +
  theme(
    legend.position = 'none',
    
  )

#ggsave("./graficos/plot_perfil_total_local_residencia.png")
ggsave("./graficos/plot_perfil_remoto_horas_dia.png")


#######################################################################################

# remoto_interrompido
# SOMENTE REMOTO 

df3 %>%
  filter(indicador_remoto_hibrido == 1) %>% 
  #!is.na(setor2)) %>% 
  group_by(remoto_interrompido) %>% 
  summarise(count=n()) %>% 
  ungroup() %>%
  mutate(prop = round((count/sum(count))*100)) %>% 
  #setor2 = factor(setor2)
  #dias_trabalho2 = factor(dias_trabalho,levels = paste0(6:1))
  #) #%>% 
  #        local_residencia2 = case_when(
  #          local_residencia == 'filho' ~ 'com filho/a(os/as)',
  #          local_residencia == 'sozinho' ~ 'sozinho(a)',
  #          local_residencia == 'familia' ~ 'com familiares',
  #          local_residencia == 'companheiro' ~ 'companheiro(a)',
  #          local_residencia == 'companheiro_filho' ~ 'companheiro(a) + filho/a(os/as)',
  #          local_residencia == 'companheiro_familia' ~ 'companheiro(a) + familiares',
  #          local_residencia == 'colegas_quarto' ~ 'sem parentesco'
#        )) %>% 

ggplot(aes(x = remoto_interrompido, y = prop, fill = factor(prop))) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = paste(format(prop), "%")), hjust = -0.25, colour = "black") +
  scale_fill_brewer() +
  scale_y_continuous(limits = c(0,70)) +
  labs(title = 'Interrupção por outra pessoa',
       subtitle = 'Trabalhadores(as) que têm ou tiveram experiência remoto/híbrido (353)',
       x = '',
       y = 'Proporção') +
  theme_fivethirtyeight() +
  theme(
    legend.position = 'none',
    
  )

ggsave("./graficos/plot_perfil_remoto_remoto_interrompido.png")

#######################################################################################

# remoto_atividade_paralela
# SOMENTE REMOTO 

df3 %>%
  filter(indicador_remoto_hibrido == 1) %>% 
  #!is.na(setor2)) %>% 
  group_by(remoto_atividade_paralela) %>% 
  summarise(count=n()) %>% 
  ungroup() %>%
  mutate(prop = round((count/sum(count))*100)) %>% 
  #setor2 = factor(setor2)
  #dias_trabalho2 = factor(dias_trabalho,levels = paste0(6:1))
  #) #%>% 
  #        local_residencia2 = case_when(
  #          local_residencia == 'filho' ~ 'com filho/a(os/as)',
  #          local_residencia == 'sozinho' ~ 'sozinho(a)',
  #          local_residencia == 'familia' ~ 'com familiares',
  #          local_residencia == 'companheiro' ~ 'companheiro(a)',
  #          local_residencia == 'companheiro_filho' ~ 'companheiro(a) + filho/a(os/as)',
  #          local_residencia == 'companheiro_familia' ~ 'companheiro(a) + familiares',
  #          local_residencia == 'colegas_quarto' ~ 'sem parentesco'
#        )) %>% 

ggplot(aes(x = remoto_atividade_paralela, y = prop, fill = factor(prop))) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = paste(format(prop), "%")), hjust = -0.25, colour = "black") +
  scale_fill_brewer() +
  scale_y_continuous(limits = c(0,70)) +
  labs(title = 'Interrupção por opção própria',
       subtitle = 'Trabalhadores(as) que têm ou tiveram experiência remoto/híbrido (353)',
       x = '',
       y = 'Proporção') +
  theme_fivethirtyeight() +
  theme(
    legend.position = 'none',
    
  )

ggsave("./graficos/plot_perfil_remoto_atividade_paralela.png")

#######################################################################################

# concilia tarefas
# SOMENTE REMOTO 

# versao 1
require(stringr)

df3 %>%
  filter(indicador_remoto_hibrido == 1) %>%
  
  mutate(
    ind_atividade_paralela = case_when(
      str_detect(remoto_atividade_paralela_descricao,
      'Não, eu não concilio atividades domésticas em horário de trabalho remoto.') ~ 'Não',
      .default = 'Sim'
    )
  ) %>% 
  
  
  #!is.na(setor2)) %>% 
  group_by(ind_atividade_paralela) %>% 
  summarise(count=n()) %>% 
  ungroup() %>%
  mutate(prop = round((count/sum(count))*100)) %>% 
  #setor2 = factor(setor2)
  #dias_trabalho2 = factor(dias_trabalho,levels = paste0(6:1))
  #) #%>% 
  #        local_residencia2 = case_when(
  #          local_residencia == 'filho' ~ 'com filho/a(os/as)',
  #          local_residencia == 'sozinho' ~ 'sozinho(a)',
  #          local_residencia == 'familia' ~ 'com familiares',
  #          local_residencia == 'companheiro' ~ 'companheiro(a)',
  #          local_residencia == 'companheiro_filho' ~ 'companheiro(a) + filho/a(os/as)',
  #          local_residencia == 'companheiro_familia' ~ 'companheiro(a) + familiares',
  #          local_residencia == 'colegas_quarto' ~ 'sem parentesco'
#        )) %>% 

ggplot(aes(x = ind_atividade_paralela, y = prop, fill = factor(prop))) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = paste(format(prop), "%")), hjust = -0.25, colour = "black") +
  scale_fill_brewer() +
  scale_y_continuous(limits = c(0,85)) +
  labs(title = 'Concliar tarefas domésticas em horário de trabalho remoto',
       subtitle = 'Trabalhadores(as) que têm ou tiveram experiência remoto/híbrido (353)',
       x = '',
       y = 'Proporção') +
  theme_fivethirtyeight() +
  theme(
    legend.position = 'none',
    
  )

# versao 2

# ranking das atividades

require(stringr)

df5 %>%
  filter(indicador_remoto_hibrido == 1,
         indicador_rapd == 1) %>%
  
  
  group_by() %>% 
  summarise(count=n()) %>% 
  ungroup() %>%
  mutate(prop = round((count/sum(count))*100)) %>% 
  #setor2 = factor(setor2)
  #dias_trabalho2 = factor(dias_trabalho,levels = paste0(6:1))
  #) #%>% 
  #        local_residencia2 = case_when(
  #          local_residencia == 'filho' ~ 'com filho/a(os/as)',
  #          local_residencia == 'sozinho' ~ 'sozinho(a)',
  #          local_residencia == 'familia' ~ 'com familiares',
  #          local_residencia == 'companheiro' ~ 'companheiro(a)',
  #          local_residencia == 'companheiro_filho' ~ 'companheiro(a) + filho/a(os/as)',
  #          local_residencia == 'companheiro_familia' ~ 'companheiro(a) + familiares',
  #          local_residencia == 'colegas_quarto' ~ 'sem parentesco'
#        )) %>% 

ggplot(aes(x = ind_atividade_paralela, y = prop, fill = factor(prop))) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = paste(format(prop), "%")), hjust = -0.25, colour = "black") +
  scale_fill_brewer() +
  scale_y_continuous(limits = c(0,85)) +
  labs(title = 'Concliar tarefas domésticas em horário de trabalho remoto',
       subtitle = 'Trabalhadores(as) que têm ou tiveram experiência remoto/híbrido (353)',
       x = '',
       y = 'Proporção') +
  theme_fivethirtyeight() +
  theme(
    legend.position = 'none',
    
  )

ggsave("./graficos/plot_perfil_remoto_conciliar_atividade_paralela.png")


#######################################################################################

# concilia tarefas - por sexo
# SOMENTE REMOTO 

require(stringr)

df3 %>%
  filter(indicador_remoto_hibrido == 1) %>%
  mutate(
    ind_atividade_paralela = case_when(
      str_detect(remoto_atividade_paralela_descricao,
                 'Não, eu não concilio atividades domésticas em horário de trabalho remoto.') ~ 'Não',
      .default = 'Sim'
    )
  ) %>% 
  
  group_by(sexo,ind_atividade_paralela) %>% 
  summarise(count=n()) %>% 
  #ungroup() %>%
  mutate(prop = round((count/sum(count))*100)) %>% 
  ungroup() %>% 

ggplot(aes(x = ind_atividade_paralela, y = prop, fill = factor(prop))) +
  geom_col() +
  coord_flip() +
  facet_grid(cols=vars(sexo)) +

  geom_text(aes(label = paste(format(prop), "%")), hjust = -0.25, colour = "black") +
  scale_fill_manual(values = c("#FF7171",'#71C1FF',"#3377FF","#FF3333")) +
  scale_y_continuous(limits = c(0,90)) +
  labs(title = 'Concliar tarefas domésticas em horário de trabalho remoto - Sexo',
       subtitle = 'Trabalhadores(as) que têm ou tiveram experiência remoto/híbrido (353)',
       x = '',
       y = 'Proporção') +
  theme_fivethirtyeight() +
  theme(
    legend.position = 'none',
    
  )

ggsave("./graficos/plot_perfil_remoto_conciliar_atividade_paralela_sexo.png")


# concilia tarefas - entre os sim
# SOMENTE REMOTO 

require(stringr)

df3 %>%
  filter(indicador_remoto_hibrido == 1) %>%
  
  mutate(
    ind_atividade_paralela = case_when(
      str_detect(remoto_atividade_paralela_descricao,
                 'Não, eu não concilio atividades domésticas em horário de trabalho remoto.') ~ 'Não',
      .default = 'Sim'
    )
  ) %>% 
  filter(ind_atividade_paralela == 'Sim') %>% 
  
  #!is.na(setor2)) %>% 
  group_by(remoto_atividade_paralela_descricao) %>% 
  summarise(count=n()) %>% 
  ungroup() %>%
  mutate(prop = round((count/sum(count))*100)) #%>% 
  #setor2 = factor(setor2)
  #dias_trabalho2 = factor(dias_trabalho,levels = paste0(6:1))
  #) #%>% 
  #        local_residencia2 = case_when(
  #          local_residencia == 'filho' ~ 'com filho/a(os/as)',
  #          local_residencia == 'sozinho' ~ 'sozinho(a)',
  #          local_residencia == 'familia' ~ 'com familiares',
  #          local_residencia == 'companheiro' ~ 'companheiro(a)',
  #          local_residencia == 'companheiro_filho' ~ 'companheiro(a) + filho/a(os/as)',
  #          local_residencia == 'companheiro_familia' ~ 'companheiro(a) + familiares',
  #          local_residencia == 'colegas_quarto' ~ 'sem parentesco'
#        )) %>% 

ggplot(aes(x = ind_atividade_paralela, y = prop, fill = factor(prop))) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = paste(format(prop), "%")), hjust = -0.25, colour = "black") +
  scale_fill_brewer() +
  scale_y_continuous(limits = c(0,85)) +
  labs(title = 'Concliar tarefas domésticas em horário de trabalho remoto',
       subtitle = 'Trabalhadores(as) que têm ou tiveram experiência remoto/híbrido (353)',
       x = '',
       y = 'Proporção') +
  theme_fivethirtyeight() +
  theme(
    legend.position = 'none',
    
  )

ggsave("./graficos/plot_perfil_remoto_conciliar_atividade_paralela.png")


#######################################################################################

# concilia tarefas - por sexo -  entre os sim
# SOMENTE REMOTO 

require(stringr)

df3 %>%
  filter(indicador_remoto_hibrido == 1) %>%
  mutate(
    ind_atividade_paralela = case_when(
      str_detect(remoto_atividade_paralela_descricao,
                 'Não, eu não concilio atividades domésticas em horário de trabalho remoto.') ~ 'Não',
      .default = 'Sim'
    )
  ) %>% 
  
  group_by(sexo,ind_atividade_paralela) %>% 
  summarise(count=n()) %>% 
  #ungroup() %>%
  mutate(prop = round((count/sum(count))*100)) %>% 
  ungroup() %>% 
  
  ggplot(aes(x = ind_atividade_paralela, y = prop, fill = factor(prop))) +
  geom_col() +
  coord_flip() +
  facet_grid(cols=vars(sexo)) +
  
  geom_text(aes(label = paste(format(prop), "%")), hjust = -0.25, colour = "black") +
  scale_fill_manual(values = c("#FF3333","#3377FF",'#71C1FF',"#FF7171")) +
  scale_y_continuous(limits = c(0,90)) +
  labs(title = 'Concliar tarefas domésticas em horário de trabalho remoto - Sexo',
       subtitle = 'Trabalhadores(as) que têm ou tiveram experiência remoto/híbrido (353)',
       x = '',
       y = 'Proporção') +
  theme_fivethirtyeight() +
  theme(
    legend.position = 'none',
    
  )

ggsave("./graficos/plot_perfil_remoto_conciliar_atividade_paralela_sexo.png")


######################################################################

######################################################################

######################################################################

######################################################################
######################################################################



# DIAS DE TRABALHO
# FAZER DUAS VEZES - TOTAL E SOMENTE REMOTO 

df4 %>%
  filter(indicador_remoto_hibrido == 1) %>% 
  group_by(dias_presencial) %>% 
  summarise(count=n()) %>% 
  ungroup() %>%
  mutate(prop = round((count/sum(count))*100)) %>% 
  mutate(names = dias_presencial %>% forcats::fct_reorder(prop, sum)) %>%
  
  ggplot(aes(x = names, y = prop, fill = factor(prop))) +
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

ggsave("./graficos/plot_perfil_total_dias_trabalho.png")


df4 %>% group_by(dias_presencial,dias_remoto, indicador_remoto_hibrido) %>% summarise(n=n())





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



