
# codificacao da coleta
# Pesquisa: Percepção sobre o Trabalho Remoto e/ou Híbrido
# Pesquisadores: Juliene Saback, Leonardo Lopes, Lucas Simões e Marcson Araújo

# ./codigos/tratamento_perfil.R
# TRATAMENTO PERFIL

# ajuste de variáveis de informações dos usuários
# setor, profissão, sexo, faixa etária, ter filhos, local de residência,
# dias de trabalho,

tratamento_perfil <- function(dir,nome_arquivo_entrada){
  
  #require(tidyverse);require(readxl);require(janitor)
  
  df <- read_xlsx(nome_arquivo_entrada)
  #dir <- "C:/ence/mpl/codificacao"
  
  # setor, grupos de profissao, sexo, faixa etaria, 
  # ter filhos, moradores, dias_trabalho
  
  df3 = df %>% mutate(
    
    descricao_setor = setor_trabalho,
    
    setor = case_when(
      str_detect(setor_trabalho,'Não se aplica.') ~ NA_character_,
      
      setor_trabalho == 'Setor Privado.' ~ 'privado',
      setor_trabalho == 'Setor Público.' ~ 'publico',
      str_detect(setor_trabalho,
                 regex('Outros.|,')) ~ 'outros_ambos',
      
      .default = NA_character_
    ),
    
    setor2 = case_when(
      str_detect(setor_trabalho,'Não se aplica.') ~ NA_character_,
      is.na(setor_trabalho) ~ NA_character_, 
      setor_trabalho == "Setor Privado., Setor Público." ~ 'ambos',
      setor_trabalho == 'Setor Privado.' ~ 'privado',
      setor_trabalho == 'Setor Público.' ~ 'publico',
      str_detect(setor_trabalho,regex('Outros.')) ~ 'outros',
      
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
      
      #str_detect(profissao,regex(
      #  '(analista de dados)|(Analista de dados)|(Analista de Dados)')) ~ 'analista_dados',
      
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
      
      #str_detect(profissao,regex(
      #  '(judici)|(Jurídic)|(Analista Jurídico)')) ~ 'analista_judiciario',
      
      #str_detect(profissao,regex(
      #  '(Analista Técnico Ambiental)|(Cientista Ambiental)|(Tec de meio ambiente')) ~ 'analista_ambiental',
      
      str_detect(profissao,regex('(Contabil)|(CONTADOR)|(ontador)')) ~ 'contador',
      
      #str_detect(profissao,regex('(Analista de comunicação e marketing)')) ~ 'analista_marketing',
      
      #str_detect(profissao,regex('(Analista de Desenvolvimento Social)')) ~ 'analista_social',
      
      #str_detect(profissao,regex('(Analista de Sistemas)')) ~ 'analista_sistemas',
      
      #str_detect(profissao,regex(
       # '(Analista Educacional)|(Especialista em Educação Básica)')) ~ 'analista_educacional',
      
      #'analista_negocios', 'analista_qualidade', 'analista_relacionamento','analista_sac','analista_tecnica'
      str_detect(profissao,regex('(Analista)|(analista)|(Especialista em Educação Básica)|(Residente Jurídica)')) ~ 'analista',
      
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
      
      #str_detect(profissao,regex('(federal)|(Federal)')) ~ 'servidor_publico_federal',
      
      str_detect(profissao,regex('(servidor)|(Servidor)|(Servudor)')) ~ 'servidor_publico',
      
      str_detect(profissao,regex('(Supervisor de compras)')) ~ 'supervisor_compras',
      
      str_detect(profissao,regex('(Supervisor de compras)')) ~ 'supervisor_compras',
      
      str_detect(profissao,regex('(Consultor)')) ~ 'consultor',
      
      str_detect(profissao,regex('(Eng. Agrônomo - Professor EBTT)|(Engenharia)|(Engenheir)')) ~ 'engenheiro',
      
      str_detect(profissao,regex('(studante)|(Formanda em Pedagogia)')) ~ 'estudante',
      
      str_detect(profissao,regex('(Pedagoga)|(Pedagogo)|(Pedagogia)')) ~ 'pedagogo',
      
     # str_detect(profissao,regex(
       # '(Professor do Ensino Superior)|(Professora do Magistério Superior)|(Professora ensino superior)|(Professora universitária)')) ~ 'professor_superior',
      
      str_detect(profissao,regex(
        '(Docente)|(rofessor)|(Peofessora)|(Professsor)|(Profwssora)|(PRROFESSOR)')) ~ 'professor',
      
      str_detect(profissao,regex('(Técnico)|(Cientista Ambiental)|(Tec)|(técnico)')) ~ 'tecnico',
      
      .default = NA_character_
    ),
    
  
    descricao_sexo = sexo,
    sexo = factor(sexo, levels = c('Feminino','Masculino')),
    # levels = c('Mulher','Homem')) ???
     
    
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
    ),
    
    
    descricao_dias_trabalho = dias_trabalho,
    dias_trabalho = case_when(
      dias_trabalho == '1 dia.' ~ '1',
      dias_trabalho == '2 dias.' ~ '2',
      dias_trabalho == '3 dias.' ~ '3',
      dias_trabalho == '4 dias.' ~ '4',
      dias_trabalho == '5 dias.' ~ '5',
      dias_trabalho == '6 dias.' ~ '6',
      .default = NA_character_
    )
    
    
  )
  
  # apoio = df3 %>% group_by(profissao_tratado) %>% summarise(n=n()) %>% arrange(desc(n))    
  # apoio3 = df3 %>% group_by(profissao_tratado,profissao) %>% summarise(n=n())
  
  
  # colocar faixa_etaria, terfilhos, local_residencia e dias_trabalho as factor
  
  df3 = df3 %>% mutate(
    faixa_etaria = factor(faixa_etaria, levels = c('18_29','30_39','40_49','50_59','60_mais')),
    ter_filhos = factor(ter_filhos, levels = c('nao','sim_1','sim_2_mais')),
    local_residencia = factor(local_residencia),
    dias_trabalho = factor(dias_trabalho, levels = c("1","2","3","4","5","6"))
  )  
  
  # criar o descricao profissoes
  # IMPORTANTE
  # EDITAR ESSE ARQUIVO DE ENTRADA COM OS AJUSTES DE ANALISTA, PROFESSOR, SERVIDOR E TÉCNICO###############################
  
  arquivo_profissoes = paste0(
    dir,'/dados/tabela_profissoes.xlsx - descrição profissões saída.csv')
  tab_new <- read_csv(arquivo_profissoes)
  
  tab_new <- janitor::clean_names(tab_new)
  
  df4 = df3 %>% left_join(tab_new)
  

    nome_arquivo = criar_nome_arquivo(dir,'tratado_perfil')
  
  write.xlsx(df4, file = nome_arquivo)
  
  if(fs::file_exists(nome_arquivo)) cat("Dados tratados atualizados!\n")
  
  return(nome_arquivo)
  
}







