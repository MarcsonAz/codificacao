
# codificacao da coleta
# Pesquisa: Percepção sobre o Trabalho Remoto e/ou Híbrido
# Pesquisadores: Juliene Saback, Leonardo Lopes, Lucas Simões e Marcson Araújo


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


# TRATAMENTO INICIAL

tratamento_inicial <- function(nome_arquivo_bruto){
  
  require(tidyverse);require(readxl)
  
  df <- read_xlsx(nome_arquivo_bruto)
  df2 = df
  
  variaveis <- names(df)
  novas_variaveis <- c(
    "timestamp","email","aceite","faixa_etaria","sexo","profissao","ter_filhos",
    "local_residencia", "condicao_trabalho","setor_trabalho","deslocamento_tempo",
    "deslocamento_transporte","condicao_trabalho2","dias_presencial","horas_dedicacao",
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
    cat("Todos deram o aceite -",dim(df2)[1]," participantes!")
    
    df_de_chaves <- df2 %>% select(id,timestamp,email,aceite)
  }
  
  df2 <- df2 %>% select(-timestamp,-email,-aceite)
  
  missing_ct <- sum( is.na( df2$condicao_trabalho))
  missing_ag <- sum( is.na( df2$avaliacao_geral))  
  
  if(dim(df2)[1] == missing_ct) df2$condicao_trabalho = NULL
  if(dim(df2)[1] == missing_ag) df2$avaliacao_geral = NULL
  
  df2 = df2 %>% mutate(
    indicador_remoto_hibrido = case_when(
      condicao_trabalho2 == 'Não. Sempre trabalhei ou ainda trabalho presencialmente.' ~ 0,
      condicao_trabalho2 == 'Sim. Trabalhei ou trabalho no modelo híbrido e/ou remoto.' ~ 1,
      .default = NA_integer_
    )
  )
  
  # setor, grupos de ocupacao, sexo, faixa etaria, ter filhos, moradores
  
  df3 = df2 %>% mutate(
    
    descricao_setor = setor_trabalho,
    
    setor = case_when(
      str_detect(setor_trabalho,'Não se aplica.') ~ NA_character_,
      
      setor_trabalho == 'Setor Privado.' ~ 'privado',
      setor_trabalho == 'Setor Público.' ~ 'publico',
      str_detect(setor_trabalho,
                 regex('Outros.|,')) ~ 'outros_ambos',
      
      .default = NA_character_
    ),
    profissao_tratado = case_when(
      
      str_detect(profissao,regex(
        '(Coordenadora de projetos)|(Gestora de projetos)')) ~ 'gestao_projetos',
      
      str_detect(profissao,regex('(Diretor)|(diretor)|(Gerente)|(Gestor)')) ~ 'gestor',
      
      str_detect(profissao,regex('(Supervisora de pesquisa)')) ~ 'supervisor_pesquisa',
      
      str_detect(profissao,regex('(dministra)|(RH)|(Rh)')) ~ 'administrativo',
      
      str_detect(profissao,regex('(dvogad)')) ~ 'advogado',
      
      str_detect(profissao,regex('(Bancária)')) ~ 'bancario',
      
      str_detect(profissao,regex('(Empregada de empresa pública)')) ~ 'empregada_publica', 
      
      str_detect(profissao,regex('(Saúde)|(Enferm)|(nferimeir)')) ~ 'agente_saude',
      
      str_detect(profissao,regex(
        '(Autônomo)|(Comerciante)|(mpresári)|(mpresari)|(Micro empreendora)')) ~ 'empresario',
      
      str_detect(profissao,regex('(Agrônomo)|(Agrimensura)|(Rh)')) ~ 'agronomo',
      
      str_detect(profissao,regex(
        '(analista de dados)|(Analista de dados)|(Analista de Dados)')) ~ 'analista_dados',
      
      str_detect(profissao,regex(
        '(Cientista de dados)|(Cientista de Dados)')) ~ 'cientista_dados',
      
      str_detect(profissao,regex('(processamento de dados)')) ~ 'processamento_dados',
      
      str_detect(profissao,regex('(Antropólogo)')) ~ 'antropologo',
 
      str_detect(profissao,regex('(Analista Acadêmico)|(esquisador)')) ~ 'pesquisador',
      # PESQUISADOR AQUI
      
      str_detect(profissao,regex(
      '(comercio exterior)|(comércio exterior)|(Comercio Exterior)|(Comércio Exterior)')) ~ 'comercio_exterior',
      
      str_detect(profissao,regex(
        '(Analista de comercio)|(Analista de Comércio)|(Representante comercial)|(Técnico comercial)|(Vendedor)|(Assistente comercial)')
        ) ~ 'representante_comercial',
      
      str_detect(profissao,regex(
        '(judici)|(Jurídic)|(Analista Jurídico)')) ~ 'analista_judiciario',
      
      str_detect(profissao,regex(
        '(Analista Técnico Ambiental)|(Cientista Ambiental)|(Tec de meio ambiente)')) ~ 'analista_ambiental',
      
      str_detect(profissao,regex('(Contabil)|(CONTADOR)|(ontador)')) ~ 'contador',
      
      str_detect(profissao,regex('(Analista de comunicação e marketing)')) ~ 'analista_marketing',
      
      str_detect(profissao,regex('(Analista de Desenvolvimento Social)')) ~ 'analista_social',
      
      str_detect(profissao,regex('(Analista de Sistemas)')) ~ 'analista_sistemas',
      
      str_detect(profissao,regex(
        '(Analista Educacional)|(Especialista em Educação Básica)')) ~ 'analista_educacional',
      
      #'analista_negocios', 'analista_qualidade', 'analista_relacionamento','analista_sac','analista_tecnica'
      str_detect(profissao,regex('(Analista)|(analista)')) ~ 'analista',
      
      str_detect(profissao,regex('(Arquitet)|(arquitet)')) ~ 'arquiteto',
      
      str_detect(profissao,regex('(Artista)|(Artista plástica)')) ~ 'artista',
      
      str_detect(profissao,regex('(Assessor Parlamentar)')) ~ 'assessor_parlamentar',
      
      str_detect(profissao,regex('(Assistente Social)')) ~ 'assistente_social',
      
      str_detect(profissao,regex('(Atuári)|(Atuari)|(atuári)|(atuari)')) ~ 'atuario',
      
      str_detect(profissao,regex('(Auditor)|(auditor)')) ~ 'auditor',
      
      str_detect(profissao,regex('(utônomo)|(utonomo)')) ~ 'autonomo',
      
      str_detect(profissao,regex('(Bibliotec)|(bibliotec)|(Auxiliar de Bilbioteca)')) ~ 'bibliotecario',
      
      str_detect(profissao,regex('(Bióloga)|(Biólogo)')) ~ 'biologo',
      
      str_detect(profissao,regex('(Cientista da Computação)')) ~ 'cientista_computacao',
      
      str_detect(profissao,regex('(dentista)')) ~ 'dentista',
      
      str_detect(profissao,regex('(Compliance Analyst)')) ~ 'compliance_analyst',
      
      str_detect(profissao,regex('(Comprador)|(comprador)')) ~ 'comprador',
      
      str_detect(profissao,regex('(Coordenadora Pedagógica)')) ~ 'coordenador_pedagogico',
      
      str_detect(profissao,regex('(Desempregad)|(desempregad)')) ~ 'desempregado',
      
      str_detect(profissao,regex('(esenvolvedor)|(rogramador)')) ~ 'desenvolvedor', 
      
      str_detect(profissao,regex('(Designer)|(designer)')) ~ 'designer',
      
      str_detect(profissao,regex('(Dj)')) ~ 'dj',
      
      str_detect(profissao,regex('(Do lar)')) ~ 'do_lar',
      
      str_detect(profissao,regex('(Economista)')) ~ 'economista',
      
      str_detect(profissao,regex('(Empresari)|(empresari)')) ~ 'empresario',
      
      str_detect(profissao,regex('(Epidemiologista)')) ~ 'epidemiologista',
      
      str_detect(profissao,regex('(statístic)|(statistic)')) ~ 'estatistico',
      
      str_detect(profissao,regex('(steticist)')) ~ 'esteticista',
      
      str_detect(profissao,regex('(Fiscal de Obras)')) ~ 'fiscal_obra',
      
      str_detect(profissao,regex('(Fisioterapeuta)')) ~ 'fisioterapeuta',
      
      str_detect(profissao,regex('(otograf)|(otógraf)')) ~ 'fotografo',
      
      str_detect(profissao,regex('(Geograf)|(Geógraf)|(geograf)|(geógraf)')) ~ 'geografo',
      
      str_detect(profissao,regex('(Guia de Turismo)')) ~ 'guia_turismo',
      
      str_detect(profissao,regex('(Turismóloga)')) ~ 'turismologo',
      
      str_detect(profissao,regex('(Informático)')) ~ 'informatico',
      
      str_detect(profissao,regex('(Internacionalista)')) ~ 'internacionalista',
      
      str_detect(profissao,regex(
        '(interprete de Libras)|(Intérprete de Libras)')) ~ 'interprete_libras',
      
      str_detect(profissao,regex('(jornalista)|(Jornalista)')) ~ 'jornalista',
      
      str_detect(profissao,regex('(Maquiadora)')) ~ 'maquiadora',
      
      str_detect(profissao,regex('(Médica)|(Médico)')) ~ 'medico',
      
      str_detect(profissao,regex('(Membro de Ordem Religiosa)')) ~ 'membro_religiao',
      
      str_detect(profissao,regex('(Militar)|(militar)')) ~ 'militar',
      
      str_detect(profissao,regex('(utricionista)')) ~ 'nutricionista',
      
      str_detect(profissao,regex('(Oceanógrafa)')) ~ 'oceanografa',
      
      str_detect(profissao,regex('(Produtor cultural)|(Produtora Cultural)')) ~ 'producao_cultural',
      
      str_detect(profissao,regex('(Produtor)')) ~ 'produtor',
       
      str_detect(profissao,regex('(sicólog)|(sicolog)')) ~ 'psicologia',
      
      str_detect(profissao,regex('(Publicitario)|(Publicitário)')) ~ 'publicitario',
      
      str_detect(profissao,regex('(Revisor de textos)')) ~ 'revisor_texto',
      
      str_detect(profissao,regex('(Secretária Executiva)')) ~ 'secretaria_executiva',
      
      str_detect(profissao,regex('(federal)|(Federal)')) ~ 'servidor_publico_federal',
     
      str_detect(profissao,regex('(servidor)|(Servidor)|(Servudor)')) ~ 'servidor_publico',
      
      str_detect(profissao,regex('(Supervisor de compras)')) ~ 'supervisor_compras',
      
      str_detect(profissao,regex('(Supervisor de compras)')) ~ 'supervisor_compras',
      
      str_detect(profissao,regex('(Consultor)')) ~ 'consultor',
      
      str_detect(profissao,regex('(Eng. Agrônomo - Professor EBTT)|(Engenharia)|(Engenheir)')) ~ 'engenheiro',
      
      str_detect(profissao,regex('(studante)|(Formanda em Pedagogia)')) ~ 'estudante',
      
      str_detect(profissao,regex('(Pedagoga)|(Pedagogo)|(Pedagogia)')) ~ 'pedagogo',
      
      str_detect(profissao,regex(
        '(Professor do Ensino Superior)|(Professora do Magistério Superior)|(Professora ensino superior)|(Professora universitária)')) ~ 'professor_superior',
      
      str_detect(profissao,regex(
        '(Docente)|(rofessor)|(Peofessora)|(Professsor)|(Profwssora)|(PRROFESSOR)')) ~ 'professor',
      
      str_detect(profissao,regex('(Técnico)')) ~ 'tecnico',
      
      .default = NA_character_
    ),
    
    descricao_sexo = sexo,
    sexo = factor(sexo, levels = c('Feminino','Masculino')),
    
    descricao_faixa_etaria = faixa_etaria,
    
    faixa_etaria = case_when(
      faixa_etaria == '18 - 29 anos' ~ '18_29',
      faixa_etaria == '30 - 39 anos' ~ '30_39',
      faixa_etaria == '40 - 49 anos' ~ '40_49',
      faixa_etaria == '50 - 59 anos' ~ '50_59',
      faixa_etaria == '60 anos ou mais' ~ '60_mais',
      .default = NA_character_
    ),
    
    descricao_ter_filhos = ter_filhos,
    
    ter_filhos = case_when(
      ter_filhos == 'Não.' ~ 'nao',
      ter_filhos == 'Sim. Tenho 1 filho(a).' ~ 'sim_1',
      ter_filhos == 'Sim. Tenho 2 filhos(as) ou mais.' ~ 'sim_2_mais',
      .default = NA_character_
    ),
    
    descricao_local_residencia = local_residencia,
    
    local_residencia = case_when(
      local_residencia == 'Com companheiro(a) e filho(s)' ~ 'companheiro_filho',
      local_residencia == 'Com companheiro(a) e outros familiares.' ~ 'companheiro_familia',
      local_residencia == 'Com companheiro(a), em algum tipo de união (namoro/casamento/união estável e outros).' ~ 'companheiro',
      local_residencia == 'Com familiares.' ~ 'familia',
      local_residencia == 'Com pessoas sem parentesco.' ~ 'colegas_quarto',
      local_residencia == 'Somente com filho(s).' ~ 'filho',
      local_residencia == 'Sozinho.' ~ 'sozinho',
      
      .default = NA_character_
    )
)
    
  # colocar faixa_etaria, terfilhos, local_residencia as factor
  
df3 = df3 %>% mutate(
  faixa_etaria = factor(faixa_etaria, levels = c('18_29','30_39','40_49','50_59','60_mais')),
  ter_filhos= factor(ter_filhos, levels = c('nao','sim_1','sim_2_mais')),
  local_residencia= factor(local_residencia)
)  
  
# criar o descricao profissoes

tab_new <- read_csv("dados/tabela_profissoes.xlsx - descrição profissões saída.csv")

df4 = df3 %>% left_join(tab_new)

# criar tempo deslocamento, dias no remoto
df4 = df4 %>% 
  mutate(
    deslocamento_tempo_num = case_when(
      deslocamento_tempo == 'Até 1 hora de deslocamento.' ~ 1,
      deslocamento_tempo == 'Até 2 horas de deslocamento.' ~ 2,
      deslocamento_tempo == '2 horas ou mais de deslocamento.' ~ 3,
      deslocamento_tempo == 'Não se aplica.' ~ 0,
      .default = NA_integer_
    ),
    dias_remoto = case_when(
      dias_presencial %in% c('5 dias.','6 dias.') ~ 0,
      dias_presencial == '4 dias.' ~ 1,
      dias_presencial == '4 dias.' ~ 1,
      dias_presencial == '4 dias.' ~ 1,
      dias_presencial == '4 dias.' ~ 1,
    )
  )
  

  
  
  
  
  
  
  
  
  
  
  
  
  
  #df3 %>% group_by(profissao_tratado) %>% summarise(n=n()) %>% arrange(desc(n)) %>% View()


#tab = df3 %>% group_by(profissao_tratado,profissao) %>% summarise(n=n()) %>% arrange(desc(n))

  #nomes_profissoes['dvogad' %in% nomes_profissoes]

#prof = df2 %>% group_by(profissao) %>% summarise(n=n())
  
#openxlsx::write.xlsx(tab,'./dados/tabela_profissoes.xlsx')
  
  
  
  
  
  
  
  
  nome_arquivo_tratado = criar_nome_arquivo(dir,'tratado')
  
  write.xlsx(df2, file = nome_arquivo_tratado)
  
  if(fs::file_exists(nome_arquivo_tratado)) cat("Dados tratados atualizados!\n")

  return(nome_arquivo_tratado)
  
}


