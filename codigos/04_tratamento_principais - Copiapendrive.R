
# codificacao da coleta
# Pesquisa: Percepção sobre o Trabalho Remoto e/ou Híbrido
# Pesquisadores: Juliene Saback, Leonardo Lopes, Lucas Simões e Marcson Araújo

# ./codigos/tratamento_principais.R
# TRATAMENTO PERFIL

#require(tidyverse);require(readxl);require(tidytext)

tratamento_principais <- function(dir, nome_arquivo_entrada){
  
  df <- read_xlsx(nome_arquivo_entrada)
  
  # criar tempo deslocamento, dias no remoto
  df2 = df %>% 
    mutate(
      deslocamento_tempo_num = case_when(
        deslocamento_tempo == 'Até 1 hora de deslocamento.' ~ 1,
        deslocamento_tempo == 'Até 2 horas de deslocamento.' ~ 2,
        deslocamento_tempo == '2 horas ou mais de deslocamento.' ~ 3,
        deslocamento_tempo == 'Não se aplica.' ~ 0,
        .default = NA_integer_
      ),
      descricao_avaliacao_geral2 = avaliacao_geral2,
      
      avaliacao_geral2 = factor(
        avaliacao_geral2, levels= c("Muito ruim.","Ruim.","Regular.","Boa.","Muito boa."))
      
      
    )
  
  # concilia atividades domesticas com o trabalho remoto
  
  df2 = df2 %>% mutate(rapd = remoto_atividade_paralela_descricao)
  
  # tratamento das atividades domesticas que concilia
  
  df3 = df2 %>% mutate(
    indicador_rapd = case_when(
      str_detect(rapd,'(Não, eu não concilio atividades domésticas em horário de trabalho remoto)') ~ 0L,
      .default = 1L
    ))
  
  # criar uma mova coluna para cada atividade
  df3 = df3 %>% mutate(
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
  df3 = df3 %>% mutate(
    soma_at = at1 + at2 + at3 + at4 + at5 + at6 + at7 + at8)
  
  df3 = df3 %>% mutate(
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
  
  ############ IMPORTANTE ############
  
  # problemas para averiguar e decidir
  # nunca mas com atividades ou concilia mas sem atividades
  # 18 casos -> 5%
  # df4 = df3 %>% filter(!(nunca_com_at == 1 | concilia_sem_at == 1))
  # removidos a principio - 30/09/2023 - decidir com o leo
  
  ############ IMPORTANTE ############
  
  df_concilia_at = df3 %>% group_by(sexo) %>% summarise(
    n_at1 = sum(at1),
    n_at2 = sum(at2),
    n_at3 = sum(at3),
    n_at4 = sum(at4),
    n_at5 = sum(at5),
    n_at6 = sum(at6),
    n_at7 = sum(at7),
    n_at8 = sum(at8)
  )
  
  
  
  # fazer por sexo e AT
  # salvar base separada
  
  df4 <- df3
  # PONTOS POSITIVOS - divisores . e ; esperar acabar a versão do código.
  
  df4$opiniao_positivo_1 <- NA
  df4$opiniao_positivo_2 <- NA
  df4$opiniao_positivo_3 <- NA
  
  for (i in 1:nrow(df4)) {
    response <- df4$opiniao_positivo[i]
    
    # Define a custom regex pattern to split on various delimiters, including parentheses, listed numbers, and hyphens
    #custom_pattern <- "[,;\\.\\d/]+|[()]|[0-9]+-[ ]?"
    custom_pattern <- "[;\\.]"
    
    # Tokenize the response using str_split
    tokens <- str_split(response, custom_pattern)
    
    # Extract up to three phrases, remove leading/trailing spaces, and filter out empty phrases
    phrases <- tokens %>% unlist() %>% str_trim() %>% .[. != ""]
    num_phrases <- min(length(phrases), 3)
    
    # Assign phrases to respective columns
    df4$opiniao_positivo_1[i] <- ifelse(num_phrases >= 1, phrases[1], NA)
    df4$opiniao_positivo_2[i] <- ifelse(num_phrases >= 2, phrases[2], NA)
    df4$opiniao_positivo_3[i] <- ifelse(num_phrases >= 3, phrases[3], NA)
  }
  
  
  df_opiniao_positivo = rbind( 
    tibble::tibble(word = df4$opiniao_positivo_1),
    tibble::tibble(word = df4$opiniao_positivo_2),
    tibble::tibble(word = df4$opiniao_positivo_3)
  ) %>% na.omit()
  
  df_opiniao_positivo = df_opiniao_positivo %>% 
    mutate(
      flexbilidade = str_detect(word,'(lexibili)'),
      conforto = str_detect(word,'(onfort)'),
      nao_deslocamento = str_detect(word,'(eslocamento)|(eslocar)|(ESLOCAMENTO)'),
      qualidade = str_detect(word,'(ualidade)'),
      autonomia = str_detect(word,'(utonomia)'),
      comodidade = str_detect(word,'(omodidade)'),
      produtividade = str_detect(word,'(rodutividade)|(rodutiv)'),
      praticidade = str_detect(word,'(raticidade)|(prátic)|(Prátic)'),
      liberdade = str_detect(word,'(liberdade)|(Liberdade)|(livre)|(Livre)'),
      concentracao = str_detect(word,'(oncentra)'),
      economia = str_detect(word,'(conomia)|(conomiza)'),
      foco = str_detect(word,'(foco)|(foca)|(Foco)|(Foca)|(focar)|(Focar)|(focaliza)|(Focaliza)'),
      tempo = str_detect(word,'(tempo)|(Tempo)')
  ) %>% 
    mutate(
      word2 = case_when(
        flexbilidade ~ 'Flexibilidade',
        conforto ~ 'Conforto',
        nao_deslocamento ~ 'Deslocamento',
        qualidade ~ 'Qualidade',
        autonomia ~ 'Autonomia',
        comodidade ~ 'Comodidade',
        produtividade ~ 'Produtividade',
        praticidade ~ 'Praticidade',
        liberdade ~ 'Liberdade',
        concentracao ~ 'Concentração',
        economia ~ 'Economia',
        foco ~ 'Foco',
        tempo ~ 'Tempo',
        .default = word
      )
    )
  
  
  
  ##
  a =1
  ##
  
  
  ##
  a =1
  ##
  
  ##
  a =1
  ##
  
  ##
  a =1
  ##
  
  ##
  a =1
  ##
  
  nome_arquivo = criar_nome_arquivo(dir,'tratado_principais')
  
  write.xlsx(df4, file = nome_arquivo)
  
  if(fs::file_exists(nome_arquivo)) cat("Dados tratados atualizados!\n")
  
  return(nome_arquivo)
  
}



df5 = df5 %>% mutate(
  descricao_avaliacao_geral2 = avaliacao_geral2,
  
  avaliacao_geral2 = factor(
    avaliacao_geral2, levels= c("Muito ruim.","Ruim.","Regular.","Boa.","Muito boa."))
)


df_pos <- df4 %>% select(id,opiniao_positivo) %>% clean_at(opiniao_positivo)

###############################################################################
###############################################################################
###############################################################################
###############################################################################

# variaveis de quais categorias

require(tidyr)

novo <- df4 %>% 
  filter(indicador_remoto_hibrido==1) %>% 
  select(remoto_atividade_paralela_descricao) %>% 
  separate_rows(remoto_atividade_paralela_descricao, sep=',') %>% 
  count(remoto_atividade_paralela_descricao, sort=TRUE)
  




















# variaveis tratadas - grupos de respostas abertas

positivo_words2 <- df4 %>% 
  select(opiniao_positivo) %>% 
  unnest_sentences(word, opiniao_positivo, strip_punct = T)

positivo_words2 <- positivo_words2 %>% count(word, sort=TRUE)

positivo_words3 <- positivo_words2 %>% mutate(
  grupo1 = 
)

# negativo

negativo_words2 <- df4 %>% 
  select(opiniao_negativo) %>% 
  unnest_sentences(word, opiniao_negativo, strip_punct = T)

negativo_words2 <- negativo_words2 %>% count(word, sort=TRUE)

