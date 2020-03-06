#============================
#
#============================
#install.packages("pdftables")
#library(pdftables)
# install.packages("pdftools")
library(pdftools)

library(dplyr)
#============================
#  funç?es
#============================
#  
preservar_palavras_chave <-  function(Tabela)
{
  Tabela <- gsub( "Rio de Janeiro"   , "Rio_de_Janeiro", Tabela )
  Tabela <- gsub( "São Paulo"        , "Sao_Paulo", Tabela )
  Tabela <- gsub( "Mês anterior"     , "Mes_anterior"  , Tabela )
  Tabela <- gsub( "Ano anterior"     , "Ano_anterior"  , Tabela )
  Tabela <- gsub( "Preço médio"      , "Preco_medio   ", Tabela )
  Tabela <- gsub( "São Luís"         , "Sao_Luis", Tabela )
  Tabela <- gsub( "Rio Grande"       , "Rio_Grande", Tabela )
  Tabela <- gsub( "São Sebastião"    , "Sao_Sebastiao ", Tabela )
  Tabela <- gsub( "Outras Naftas"    , "Outras_Naftas ", Tabela )
  Tabela <- gsub( "Porto Alegre"     , "Porto_Alegre", Tabela )
  
  Tabela <- gsub( ","                , "."             , Tabela )
  return( Tabela )
}
#
delimitar_colunas_na_Tabela <- function(Tabela, delimitador_inicio, delimitador_fim )
{        
  #=========================
  #
  #=========================
  tabela_cortada <- sapply(
    1:length(Tabela) ,
    function(x)
      substring( Tabela[x]  , 
                 delimitador_inicio , 
                 delimitador_fim
      )
  )
  return( tabela_cortada )
}

#============================
# 
#============================
gerar_tabelas_das_paginas_de_relatório_pdf <- function(
  numero_da_pagina,
  inicio_da_tabela_desejada,
  fim_da_tabela_desejada,
  texto_do_pdf_cortado
)
{
  texto_do_pdf_cortado_sem_r   <- gsub("\\r","", texto_do_pdf_cortado[[numero_da_pagina]])
  
  if( any( grepl(inicio_da_tabela_desejada , texto_do_pdf_cortado_sem_r) ) )
  {
    inicio_da_tabela                 <- which( grepl(inicio_da_tabela_desejada , texto_do_pdf_cortado_sem_r) )
    fim_da_tabela                    <- which( grepl(fim_da_tabela_desejada    , texto_do_pdf_cortado_sem_r) ) 
    fim_da_tabela                    <- fim_da_tabela[ which(fim_da_tabela > inicio_da_tabela[1] )]
    quantidade_de_tabelas_na_pagina  <- length( inicio_da_tabela )
    lista_tabela001                  <- list()    
    indice_tabela                    <- 1
    for( indice_tabela in 1:quantidade_de_tabelas_na_pagina ){
      Tabela_delimitada_horizontal <- texto_do_pdf_cortado_sem_r[ 
        (inicio_da_tabela[indice_tabela]+1):( fim_da_tabela[indice_tabela]-1) 
        ]
      #============================
      #  converter Palavras_chave
      #============================
      Tabela_delimitada_horizontal <- preservar_palavras_chave(Tabela_delimitada_horizontal)
      #============================
      # Ajuste de posição de cortes
      #============================
      #largura_total         <- nchar( Tabela_delimitada_horizontal[[1]] )
      #nomes_das_colunas     <- strsplit( Tabela_delimitada_horizontal[[1]], split = " " )
      #nomes_das_colunas     <- nomes_das_colunas[[1]][ nomes_das_colunas[[1]]!="" ]
      #quantidade_de_colunas <- length( nomes_das_colunas )            
      #delimitador_inicio    <- seq( 0, largura_total , largura_total/quantidade_de_colunas )
      #delimitador_inicio    <- delimitador_inicio[ -length(delimitador_inicio) ]
      #delimitador_fim       <- seq( delimitador_inicio[2], largura_total, largura_total/quantidade_de_colunas )            
      #delimitador_inicio
      #delimitador_fim
      largura_total <- nchar( Tabela_delimitada_horizontal[[1]] )
      nomes_das_colunas     <- strsplit( Tabela_delimitada_horizontal[[1]], split = " " )
      nomes_das_colunas     <- nomes_das_colunas[[1]][ nomes_das_colunas[[1]]!="" ]
      quantidade_de_colunas <- length( nomes_das_colunas )
      delimitador_inicio <- numeric(0)
      delimitador_fim    <- numeric(0)
      if(quantidade_de_colunas == 8 ){
        if(indice_de_relatorio == 1)
        {
          delimitador_inicio <- c( 10                , largura_total-110, largura_total-90 , largura_total-70 , largura_total-50 , largura_total-40  , largura_total-20 , largura_total  - 10  )
          delimitador_fim    <- c( largura_total-110 , largura_total-90 , largura_total-70 , largura_total-50 , largura_total-40 , largura_total-20  , largura_total-10 , largura_total        ) 
        }
        if(indice_de_relatorio == 2){
          delimitador_inicio <- c( 10                , largura_total-155, largura_total-126 , largura_total-95 , largura_total-70 , largura_total-45  , largura_total-21 , largura_total - 10  )
          delimitador_fim    <- c( largura_total-160 , largura_total-125 , largura_total-96 , largura_total-71 , largura_total-46 , largura_total-22  , largura_total-10 , largura_total       ) 
        }
        if(indice_de_relatorio == 3){
          delimitador_inicio <- c( 10                , largura_total-158, largura_total-140 , largura_total-95 , largura_total-70 , largura_total-45  , largura_total-21 , largura_total - 10  )
          delimitador_fim    <- c( largura_total-160 , largura_total-140 , largura_total-96 , largura_total-71 , largura_total-46 , largura_total-22  , largura_total-10 , largura_total       ) 
        }
        if(indice_de_relatorio == 4){
          delimitador_inicio <- c( 10                , largura_total-170, largura_total-158 , largura_total-110 , largura_total-94 , largura_total-61  , largura_total-21 , largura_total - 10 )
          delimitador_fim    <- c( largura_total-170 , largura_total-158 , largura_total-110 , largura_total-94 , largura_total-61 , largura_total-22  , largura_total-10 , largura_total      ) 
        }
        if(indice_de_relatorio == 5){
          delimitador_inicio <- c( 10                , largura_total-170, largura_total-158 , largura_total-110 , largura_total-94 , largura_total-61  , largura_total-21 , largura_total-  10 )
          delimitador_fim    <- c( largura_total-170 , largura_total-158 , largura_total-110 , largura_total-94 , largura_total-61 , largura_total-22  , largura_total-10 , largura_total      )
        }
        if(indice_de_relatorio == 6){
          delimitador_inicio <- c( 10                , largura_total-170, largura_total-158 , largura_total-110 , largura_total-94 , largura_total-61  , largura_total-21 , largura_total - 10 )
          delimitador_fim    <- c( largura_total-170 , largura_total-158 , largura_total-110 , largura_total-94 , largura_total-61 , largura_total-22  , largura_total-10 , largura_total      ) 
        }
        if(indice_de_relatorio == 7){
          delimitador_inicio <- c( 10                , largura_total-170, largura_total-158 , largura_total-110 , largura_total-93 , largura_total-61  , largura_total-21 , largura_total - 10 )
          delimitador_fim    <- c( largura_total-170 , largura_total-158 , largura_total-110 , largura_total-94 , largura_total-61 , largura_total-22  , largura_total-10 , largura_total      ) 
        }
        if(indice_de_relatorio == 8){
          delimitador_inicio <- c( 10                , largura_total-170, largura_total-150 , largura_total-110 , largura_total-90 , largura_total-59  , largura_total-25 , largura_total - 10 )
          delimitador_fim    <- c( largura_total-170 , largura_total-150 , largura_total-110 , largura_total-92 , largura_total-59 , largura_total-25  , largura_total-10 , largura_total + 5  ) 
        }
        if(indice_de_relatorio == 9){
          delimitador_inicio <- c( 10                , largura_total-170, largura_total-150 , largura_total-110 , largura_total-90 , largura_total-59  , largura_total-25 , largura_total - 10 )
          delimitador_fim    <- c( largura_total-170 , largura_total-150 , largura_total-110 , largura_total-92 , largura_total-59 , largura_total-25  , largura_total-10 , largura_total + 5  ) 
        }
      }else{
        if(quantidade_de_colunas == 7 ){
          stop("sete!")
          #delimitador_inicio <- c( 10              , largura_total-30 , largura_total-25 , largura_total-20 , largura_total-15  , largura_total-14, largura_total-7)
          #delimitador_fim    <- c( largura_total-30, largura_total-25 , largura_total-20 , largura_total-15 , largura_total-10 , largura_total-7  , largura_total  ) 
        }
      }
      #============================
      # cortes de planilha
      #============================
      tabela_cortada               <- delimitar_colunas_na_Tabela( 
        Tabela_delimitada_horizontal, 
        delimitador_inicio, 
        delimitador_fim )
      tabela_cortada
      
      #=========================
      #
      #=========================
      tabela_cortada_lapidada <- trimws( tabela_cortada )
      tabela_crua             <- t( tabela_cortada_lapidada )
      tabela_header           <- tabela_crua[ 1,]
      tabela_corpo            <- tabela_crua[-1,]
      dataframe_tabela        <- data.frame( tabela_corpo, stringsAsFactors = TRUE )
      names(dataframe_tabela) <- tabela_header
      #=========================
      #
      #=========================
      print( dataframe_tabela )
      lista_tabela001[[indice_tabela]] <- dataframe_tabela
      
    }
    return(lista_tabela001)
  }else{
    return("A tabela desejada não foi encontrada nessa página")
  }
}





#============================
#
#============================
vetor_lista_de_relatorios <- c(
  "http://www.anp.gov.br/images/Importacao_Exportacao/Relatorios/Comercio_Exterior/Relatorio_de_Comercio_Exterior_n09.pdf",
  "http://www.anp.gov.br/images/Importacao_Exportacao/Relatorios/Comercio_Exterior/Relatorio_de_Comercio_Exterior_n08.pdf",
  "http://www.anp.gov.br/images/Importacao_Exportacao/Relatorios/Comercio_Exterior/Relatorio_de_Comercio_Exterior_n07.pdf",
  "http://www.anp.gov.br/images/Importacao_Exportacao/Relatorios/Comercio_Exterior/Relatorio_de_Comercio_Exterior_n06.pdf",
  "http://www.anp.gov.br/images/Importacao_Exportacao/Relatorios/Comercio_Exterior/Relatorio_de_Comercio_Exterior_n05.pdf",
  "http://www.anp.gov.br/images/Importacao_Exportacao/Relatorios/Comercio_Exterior/Relatorio_de_Comercio_Exterior_n04.pdf",
  "http://www.anp.gov.br/images/Importacao_Exportacao/Relatorios/Comercio_Exterior/Relatorio_de_Comercio_Exterior_n03.pdf",
  "http://www.anp.gov.br/images/Importacao_Exportacao/Relatorios/Comercio_Exterior/Relatorio_de_Comercio_Exterior_n02.pdf"#,
  #"http://www.anp.gov.br/images/Importacao_Exportacao/Relatorios/Comercio_Exterior/Relatorio_de_Comercio_Exterior_n01.pdf" Não possui preço
)
resultado_relatorios <- list()
#============================
#
#============================
indice_de_relatorio <- 2
for( indice_de_relatorio in c(1,2,5) )
{
  texto_do_pdf_cortado <- strsplit( pdf_text( vetor_lista_de_relatorios[ indice_de_relatorio ] )  , split = "\n" )
  
  numero_da_pagina             <- 5
  inicio_da_tabela_desejada    <- "DA GASOLINA POR PORTO"
  fim_da_tabela_desejada       <- "Evolução"
  
  resultado_relatorio <- list()
  
  for( numero_da_pagina in 1:length(texto_do_pdf_cortado) ) 
  {
    resultado_relatorio[[numero_da_pagina]] <- gerar_tabelas_das_paginas_de_relatório_pdf(
      numero_da_pagina,
      inicio_da_tabela_desejada,
      fim_da_tabela_desejada,
      texto_do_pdf_cortado
    )
  }
  filtro_para_encontrados <- which( !sapply(resultado_relatorio, function(x) x == "A tabela desejada não foi encontrada nessa página") == TRUE )
  #which( !sapply( sapply(resultado_relatorio, function(x) x == "A tabela desejada não foi encontrada nessa página"), function(x) length(x)==1 ) )
  resultado_relatorio_final <- list()
  indice_de_lista <- 1
  for( indice_tabela in filtro_para_encontrados)
  {
    resultado_relatorio_final[[indice_de_lista]] <- list( "NomeRelatorio" = vetor_lista_de_relatorios[ indice_de_relatorio ],
                                                          "Titulo_Tabela" = inicio_da_tabela_desejada,
                                                          "Tabela"        = resultado_relatorio[[indice_tabela]]
    )
    indice_de_lista                              <- indice_de_lista+1
  }
  resultado_relatorios[[indice_de_relatorio]] <-  resultado_relatorio_final
}

#as.matrix( resultado_relatorios[[1]][[1]]$Tabela[[1]] )
#as.matrix( resultado_relatorios[[2]][[1]]$Tabela[[1]] )
#as.matrix( resultado_relatorios[[3]][[1]]$Tabela[[1]] )
#as.matrix( resultado_relatorios[[indice_de_relatorio]][[1]]$Tabela[[1]] )
#str( as.matrix( resultado_relatorios[[indice_de_relatorio]][[1]]$Tabela[[1]] ) )
#resultado_relatorios[[5]]
# 2015 até 2035








resultado_relatorios[[1]][[1]]$Tabela[[1]]
resultado_relatorios[[2]][[1]]$Tabela[[1]]


dataframe_gasolina  <- data.frame( "indice_df" = 1, resultado_relatorios[[1]][[1]]$Tabela[[1]] )
indice_de_relatorio <- 2
for(indice_de_relatorio in c(2,5) ){
  dataframe_gasolina  <- full_join( dataframe_gasolina, data.frame( "indice_df" = indice_de_relatorio, resultado_relatorios[[indice_de_relatorio]][[1]]$Tabela[[1]] ) )
  
}




dataframe_gasolina$Período <- factor( 
  dataframe_gasolina$Período, 
  levels = c(
    "jan/15", "fev/15",
    "mar/15", "abr/15",
    "mai/15", "jun/15",
    "jul/15", "ago/15",
    "set/15", "out/15",
    "nov/15", "dez/15",
    "jan/16", "fev/16",
    "mar/16", "abr/16",
    "mai/16", "jun/16",
    "jul/16", "ago/16",
    "set/16", "out/16",
    "nov/16", "dez/16",
    "jan/17", "fev/17",
    "mar/17", "abr/17",
    "mai/17", "jun/17",
    "jul/17", "ago/17",
    "set/17", "out/17",
    "nov/17", "dez/17",
    "jan/18", "fev/18",
    "mar/18", "abr/18",
    "mai/18", "jun/18",
    "jul/18", "ago/18",
    "set/18", "out/18",
    "nov/18", "dez/18",
    "jan/19", "fev/19",
    "mar/19", "abr/19",
    "mai/19", "jun/19",
    "jul/19", "ago/19",
    "set/19", "out/19",
    "nov/19", "dez/19" 
  )
);





dataframe_gasolina           <- dataframe_gasolina[ order(dataframe_gasolina$Período), ]
dataframe_gasolina_ordenado  <- dataframe_gasolina[order(dataframe_gasolina$Período),]
dataframe_gasolina_final     <- dataframe_gasolina_ordenado[ -which( duplicated(dataframe_gasolina_ordenado$Período) ), ]




#paste( dataframe_gasolina$indice_df ,dataframe_gasolina$Período )






