
# codificacao da coleta
# Pesquisa: Percepção sobre o Trabalho Remoto e/ou Híbrido
# Pesquisadores: Juliene Saback, Leonardo Lopes, Lucas Simões e Marcson Araújo

# ./codigos/acessar_baixar_atualizar.R
# versao 24/09/2023
# versao 26/10/2023 - funcao sem download
# ACESSAR DADOS 

#require(fs); require(googlesheets4); require(stringr); require(openxlsx)

criar_nome_arquivo <- function(dir,tipo = c(
  "bruto","tratado",'tratado_inicial','tratado_perfil','tratado_principais',"final")){
  # criar arquivo data e hora
  if(length(tipo) != 1){
    cat("definir um tipo: \nbruto  OU  tratado_inicial   OU  tratado_principais  OU  tratado_perfil  OU  tratado  OU  final\n")
    return("vazio")
  }
  
  
  
  if(tipo %in% c(
    "bruto","tratado",'tratado_inicial','tratado_perfil','tratado_principais',"final")){
    
    t = Sys.time()
    t1 <- str_replace_all( str_sub(t, 0,10), '-', '_' )
    t2 <- str_replace_all( str_sub(t,12,19), ':', '_' )
    data_e_hora <- paste0(t1,"_",t2)
    
    if(stringr::str_ends(dir,'/')){
      nome_arquivo = paste0(dir,'dados/base_formulario_',tipo,'_',data_e_hora,'.xlsx')
    }else{
      nome_arquivo = paste0(dir,'/dados/base_formulario_',tipo,'_',data_e_hora,'.xlsx')
    } 
    
    return(nome_arquivo)
    
  }else{
    cat("definir um tipo: \nbruto  OU  tratado_inicial   OU  tratado_principais  OU  tratado_perfil  OU  tratado  OU  final\n")
    return("vazio")
  }
}


acessar_baixar_atualizar <- function(dir, novo_download = FALSE){
  
  if(!novo_download){
    #nome_arquivo = paste0(dir,'/dados/base_formulario_bruto_principal.xlsx')
    nome_arquivo = paste0(dir,'/dados/base_formulario_bruto_2023_09_24_14_20_44_ajuste_manual.xlsx')
  }else{
    #url <- "https://docs.google.com/spreadsheets/d/1U6Cf_qEOhiR9AZqTqS3mbMF3zt2db48ZP5v3rkrAEJY/edit#gid=780868077"
    
    # fazer a conexão
    nossa_url <- "https://docs.google.com/spreadsheets/d/1ESYfrCiG_cQdRDxE7hhhr8HJGqmiYSORQdXEFvR-TpA/edit?usp=sharing"
    
    respostas_formulario <- read_sheet(nossa_url)
    
    #sheet_properties(url)
    
    # atualizar arquivo
    nome_arquivo = criar_nome_arquivo(dir,"bruto")
    
    write.xlsx(respostas_formulario, file = nome_arquivo)
    
    if(fs::file_exists(nome_arquivo)) cat("Dados brutos atualizados!\n")
  }

  return(nome_arquivo)
}