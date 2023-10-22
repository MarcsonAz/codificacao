# codificacao da coleta
# Pesquisa: Percepção sobre o Trabalho Remoto e/ou Híbrido
# Pesquisadores: Juliene Saback, Leonardo Lopes, Lucas Simões e Marcson Araújo

# ./codigos/01_TRATAMENTO.R
# TRATAMENTO DE DADOS GERAL

dir <- 'C:/ence/mpl/codificacao/'
dir <- 'D:/ence/mpl/codificacao/'

# carregar funcoes

if(!require(pacman)) install.packages('pacman')

require(pacman)

pacman::p_load( char = c(
  'googlesheets4',
  'stringr',
  'fs',
  'openxlsx',
  'tidyverse',
  'readxl',
  'tidytext',
  'janitor'
))



arquivos <- c(
  #paste0(dir,'codigos/tratamento_inicial.R'),
  #paste0(dir,'codigos/tratamento_perfil.R'),
  #paste0(dir,'codigos/tratamento_principais.R')
  paste0(dir,'codigos/acessar_baixar_atualizar.R')
)

for(arquivo in arquivos){ source(arquivo) }



nome1 = acessar_baixar_atualizar(dir)

nome2 = 







