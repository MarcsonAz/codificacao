
require(wordcloud2)

wordcloud2::wordcloud2(prof2)

wordcloud2(prof2, minRotation = 0, maxRotation = 0)

df3 = df3 %>% mutate(
  setor2 = case_when(
    str_detect(setor_trabalho,'Não se aplica.') ~ NA_character_,
    is.na(setor_trabalho) ~ NA_character_, 
    setor_trabalho == "Setor Privado., Setor Público." ~ 'ambos',
    setor_trabalho == 'Setor Privado.' ~ 'privado',
    setor_trabalho == 'Setor Público.' ~ 'publico',
    str_detect(setor_trabalho,regex('Outros.')) ~ 'outros',
    
    .default = NA_character_
  )
)

df4 = df3

# NUVEM DE FRASES DE PONTOS POSITIVOS

df_nuvem = df_opiniao_positivo %>%
  select(word2) %>% 
  group_by(word2) %>%
  summarise(freq = n()) %>% 
  ungroup() %>% 
  na.omit()

wordcloud2(df_nuvem, minRotation = 0, maxRotation = 0)
