# codificacao da coleta
# Pesquisa: Percepção sobre o Trabalho Remoto e/ou Híbrido
# Pesquisadores: Juliene Saback, Leonardo Lopes, Lucas Simões e Marcson Araújo

# ./codigos/tratamento_inicial.R
# versao 24/09/2023
# TRATAMENTO INICIAL

# RÓTULOAS DE VARIAVEIS, TIRAR IDENTIFICADORES E INDICADOR DE TRABALHO REMOTO 

tratamento_inicial <- function(dir, nome_arquivo_entrada){
  
  #require(tidyverse);require(readxl)
  
  df <- read_xlsx(nome_arquivo_entrada)
  df2 = df
  
  variaveis <- names(df)
  novas_variaveis <- c(
    "timestamp","email","aceite","faixa_etaria","sexo","profissao","ter_filhos",
    "local_residencia", "condicao_trabalho","setor_trabalho","deslocamento_tempo",
    "deslocamento_transporte","condicao_trabalho2","dias_trabalho","horas_dedicacao",
    "avaliacao_geral","preferencia_pessoal","remoto_interrompido",
    "remoto_atividade_paralela","remoto_atividade_paralela_descricao","opiniao_positivo",
    "opiniao_negativo","avaliacao_geral2"
  )
  
  names(df2) <- novas_variaveis
  
  # tirar grupos de identificação que não alteram as analises
  
  df2$id = criar_codigo(1:dim(df2)[1])
  
  if(unique(df2$aceite) != "Aceito participar."){
    cat("Olhar aceite!")
    table(df2$aceite)
  }else{
    cat("Todos deram o aceite -",dim(df2)[1],"participantes!")
    
    df_de_chaves <- df2 %>% select(id,timestamp,email,aceite)
  }
  
  df2 <- df2 %>% select(-timestamp,-email,-aceite)
  
  missing_ct <- sum( is.na( df2$condicao_trabalho))
  missing_ag <- sum( is.na( df2$avaliacao_geral))  
  
  if(dim(df2)[1] == missing_ct) df2$condicao_trabalho = NULL
  if(dim(df2)[1] == missing_ag) df2$avaliacao_geral = NULL
  
  # criar dummy para identificar se ja trabalhou ou trabalha remoto
  
  df2 = df2 %>% mutate(
    indicador_remoto_hibrido = case_when(
      condicao_trabalho2 == 'Não. Sempre trabalhei ou ainda trabalho presencialmente.' ~ 0,
      condicao_trabalho2 == 'Sim. Trabalhei ou trabalho no modelo híbrido e/ou remoto.' ~ 1,
      .default = NA_integer_
    )
  )
  
  df2 = df2 %>% select(id,setor_trabalho,profissao,sexo,faixa_etaria,ter_filhos,
                       local_residencia,indicador_remoto_hibrido, everything())
  
#openxlsx::write.xlsx(tab,'./dados/tabela_profissoes.xlsx')
  
  nome_arquivo = criar_nome_arquivo(dir,'tratado_inicial')
  
  write.xlsx(df2, file = nome_arquivo)
  
  if(fs::file_exists(nome_arquivo)) cat("Dados tratados atualizados!\n")

  return(nome_arquivo)
  
}


#numeros = seq(1,1500,by =95)

criar_codigo <- function(numeros){
  
  require(stringr)
  
  maior_numero = max(numeros)
  
  algarismos_maior_numero = length( str_split_1(paste(maior_numero),pattern = ""))
  
  if(algarismos_maior_numero > 4){
    cat('rever codigo!')
    return(paste0(numeros))
  }
  
  novos_numeros = paste0(numeros)
  if(algarismos_maior_numero <= 4){
    for(i in 1:length(numeros)){
      
      if(numeros[i] < 10){
        novos_numeros[i] = paste0('000',novos_numeros[i])
        next
      }
      
      if(numeros[i] < 100){
        novos_numeros[i] = paste0('00',novos_numeros[i])
        next
      }
      
      if(numeros[i] < 1000){
        novos_numeros[i] = paste0('0',novos_numeros[i])
        next
      }
    }
    
    return(novos_numeros)
  }
  
  cat('rever codigo2!')
  return(paste0(numeros))
}
#criar_codigo(numeros)








