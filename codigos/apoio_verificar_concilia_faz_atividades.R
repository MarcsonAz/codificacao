# apoio para testar o concilia as atividades
#
#

# concilia atividades domésticas com o trabalho remoto
# teste

df5 = df2 %>% mutate(rapd = remoto_atividade_paralela_descricao)
df5 = df5 %>% 
  filter(indicador_remoto_hibrido == 1) %>% 
  select(id,indicador_remoto_hibrido,remoto_interrompido,remoto_atividade_paralela,
         remoto_atividade_paralela_descricao,rapd)


# tratamento das atividades domesticas que concilia


df5 = df5 %>% mutate(
  indicador_rapd = case_when(
    str_detect(rapd,'(Não, eu não concilio atividades domésticas em horário de trabalho remoto)') ~ 0L,
    .default = 1L
  ))

# criar uma mova coluna para cada atividade
df5 = df5 %>% mutate(
  at1 = case_when(
    str_detect(rapd,'(Preparar ou servir alimentos, arrumar a mesa ou lavar louça)') ~ 1L,
    .default = 0L
  ),
  at2 = case_when(
    str_detect(rapd,'(Cuidar da limpeza ou manutenção de roupas e sapatos)') ~ 1L,
    .default = 0L
  ),
  at3 = case_when(
    str_detect(rapd,'(Fazer pequenos reparos ou manutenção do domicílio, do automóvel, de eletrodomésticos)') ~ 1L,
    .default = 0L
  ),
  at4 = case_when(
    str_detect(rapd,'(Limpar ou arrumar o domicílio, a garagem, o quintal ou o jardim)') ~ 1L,
    .default = 0L
  ),
  at5 = case_when(
    str_detect(rapd,'(Cuidar da organização do domicílio)') ~ 1L,
    .default = 0L
  ),
  at6 = case_when(
    str_detect(rapd,'(Fazer compras ou pesquisar preços de bens para o domicílio)') ~ 1L,
    .default = 0L
  ),
  at7 = case_when(
    str_detect(rapd,'(Cuidar dos animais domésticos)') ~ 1L,
    .default = 0L
  ),
  at8 = case_when(
    str_detect(rapd,'(Cuidados com outros moradores)') ~ 1L,
    .default = 0L
  )
)

#Verificacao
df6 = df5 %>% mutate(
  soma_at = at1 + at2 + at3 + at4 + at5 + at6 + at7 + at8)

df6 = df6 %>% mutate(
  nunca_sem_at = case_when(
    remoto_atividade_paralela == 'Nunca.' & soma_at == 0 ~ 1L,
    .default = 0L
  ),
  nunca_com_at = case_when(
    remoto_atividade_paralela == 'Nunca.' & soma_at > 0 ~ 1L,
    .default = 0L
  ),
  concilia_sem_at = case_when(
    remoto_atividade_paralela %in% c('Às vezes.','Muitas vezes.') & soma_at == 0 ~ 1L,
    .default = 0L
  ),
  concilia_com_at = case_when(
    remoto_atividade_paralela %in% c('Às vezes.','Muitas vezes.') & soma_at > 0 ~ 1L,
    .default = 0L
  )
  
)

# problemas para averiguar e decidir
# nunca com atividades e concilia sem atividades
# 18 casos -> 5%
df7 = df6 %>% filter(nunca_com_at == 1 | concilia_sem_at == 1)
# removidos a principio - 30/09/2023 - decidir com o leo
