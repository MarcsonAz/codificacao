
# codificacao da coleta
# Pesquisa: Percepção sobre o Trabalho Remoto e/ou Híbrido
# Pesquisadores: Juliene Saback, Leonardo Lopes, Lucas Simões e Marcson Araújo

# APOIO

# apoio_construcao_graficos

  
require(tidyverse);require(ggthemes)

df_sexo = df2 %>% 
  filter(indicador_remoto_hibrido == 1) %>% 
  select(id, sexo, avaliacao_geral2) %>% 
  mutate(ag_categorica = factor(
    avaliacao_geral2,
    levels = c("Muito ruim.","Ruim.","Regular.","Boa.","Muito boa."))) %>%
  group_by(sexo,ag_categorica) %>% 
  summarise(value = n()) %>% 
  ungroup() 

df_sexo %>% 
  ggplot(aes(x = sexo, y=value , fill= ag_categorica)) + 
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_brewer(palette = "RdYlGn") +
  #coord_flip() +
  theme_fivethirtyeight() + 
  labs(title = "Percepção geral sobre o trabalho remoto - sexo",
       subtitle = "Proporção das avaliações da percepção geral sobre o trabalho remoto e/ou híbrido, \nsegundo sexo do respondente - 332 respondentes",
       fill="Percepção geral" )

df_setor = df2 %>% 
  filter(indicador_remoto_hibrido == 1,
         setor_trabalho %in% c("Setor Privado.","Setor Público.")) %>% 
  select(id, setor_trabalho, avaliacao_geral2) %>% 
  mutate(
    ag_categorica = factor(avaliacao_geral2,
                           levels = c("Muito ruim.","Ruim.","Regular.","Boa.","Muito boa.")),
    
    st_categorica = factor( setor_trabalho,
                            levels = c("Setor Privado.","Setor Público."))
  ) %>%
  group_by(st_categorica,ag_categorica) %>% 
  summarise(value = n()) %>% 
  ungroup()   

# adicionar o muito ruim de privado como zero!
df_setor = rbind(df_setor, tibble(
  st_categorica = "Setor Privado.",
  ag_categorica = "Muito ruim.",
  value = 0))

df_setor %>% 
  ggplot(aes(x = st_categorica, y=value , fill= ag_categorica)) + 
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_brewer(palette = "RdYlGn") +
  
  theme_fivethirtyeight() + 
  labs(title = "Percepção geral sobre o trabalho remoto - setor",
       subtitle = "Proporção das avaliações da percepção geral sobre o trabalho remoto e/ou híbrido, \nsegundo setor de ocupação do respondente - 290 respondentes",
       fill="Percepção geral" )


df_casa = df2 %>% 
  filter(indicador_remoto_hibrido == 1) %>% 
  select(id, local_residencia, avaliacao_geral2) %>% 
  mutate(
    ag_categorica = factor(avaliacao_geral2,
                           levels = c("Muito ruim.","Ruim.","Regular.","Boa.","Muito boa.")),
    
    local = ifelse(
      local_residencia == "Com companheiro(a), em algum tipo de união (namoro/casamento/união estável e outros).",
      "Com companheiro(a)", local_residencia)
  ) %>%
  #mutate(local = factor(local)) %>% 
  group_by(local,ag_categorica) %>% 
  summarise(value = n()) %>% 
  ungroup()   

# # adicionar o muito ruim de privado como zero!
# df_setor = rbind(df_setor, tibble(
#   st_categorica = "Setor Privado.",
#   ag_categorica = "Muito ruim.",
#   value = 0))

df_casa %>% 
  ggplot(aes(x = local, y=value , fill= ag_categorica)) + 
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_brewer(palette = "RdYlGn") +
  
  theme_fivethirtyeight() + 
  labs(title = "Percepção geral sobre o trabalho remoto - local de residência",
       subtitle = "Proporção das avaliações da percepção geral sobre o trabalho remoto e/ou híbrido, \nsegundo local de residência do respondente - 332 respondentes",
       fill="Percepção geral" ) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))



df_sexo_ativ_paralela = df2 %>% 
  filter(indicador_remoto_hibrido == 1) %>% 
  select(id, sexo, remoto_atividade_paralela) %>% 
  mutate(
    rap_categorica = factor(remoto_atividade_paralela,
                            levels = c("Muitas vezes.","Às vezes.","Nunca.")),
    sexo = factor(sexo)
  ) %>%
  group_by(sexo,rap_categorica) %>% 
  summarise(value = n()) %>% 
  ungroup()   

df_sexo_ativ_paralela %>% 
  ggplot(aes(x = sexo, y=value , fill= rap_categorica)) + 
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_brewer(palette = "RdYlGn") +
  
  theme_fivethirtyeight() + 
  labs(title = "Tarefas domésticas no trabalho remoto - sexo",
       subtitle = "Frequência em que costuma interromper o trabalho remoto e/ou híbrido para fazer tarefas domésticas, \nsegundo sexo do respondente - 332 respondentes",
       fill="Frequência de interrumpção" ) #+ 
#theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


"Durante as horas que se dedica ao trabalho remoto e/ou híbrido, você costuma/costumava interromper o 
trabalho para realizar tarefas domésticas  (lavar louça, limpar casa, lavar roupas, etc.)?"

"Frequência de interrupção do trabalho remoto para fazer tarefas domésticas - sexo"

