
# codificacao da coleta
# Pesquisa: Percepção sobre o Trabalho Remoto
# Pesquisadores: Juliene Saback, Leonardo Lopes, Lucas Simoes e Marcson Araujo
# 
# main.R
# PROGRAMA PRINCIPAL


# 1 - onde esta a versao principal do codigo
dir <- "D:/ence/mpl/codificacao/" # pendrive
dir <- "C:/ence/mpl/codificacao/"  # pc

# 2 - carregar funcoes

if(!require(pacman)) install.packages('pacman')

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

for(codigos in fs::dir_ls(paste0(dir,"/codigos/"))){
  # os codigos de apoio nao tem funcoes
  if(!stringr::str_detect(codigos,"apoio")) {
    source(codigos)
    print(codigos)
  }
}

# 3 - conectar e atualizar dados

# PADRAO EH PEGAR PLANILHA EDITADA - PEGAR DO FORMULARIO: principal = FALSE
# ver codigo: ./codigos/01_acessar_baixar_atualizar.R
nome_arquivo_dado_atualizado = acessar_baixar_atualizar(dir) 

# 4 - tratamentos iniciais

# PEGAR ARQUIVO BRUTO RECENTE
# limpeza inicial
nome_arquivo_tratado_inicial <- tratamento_inicial(
  dir, nome_arquivo_dado_atualizado)


# codificacao

nome_arquivo_tratado_perfil <- tratamento_perfil(
  dir, nome_arquivo_tratado_inicial)

nome_arquivo_tratado_principais <- tratamento_principais(
  dir, nome_arquivo_tratado_perfil)


# atualizar versao final
# gerar tabelas
# gerar gráficos

