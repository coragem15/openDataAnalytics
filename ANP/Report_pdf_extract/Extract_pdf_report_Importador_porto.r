#     [02/03/2020 13:22]  Wagner Christe:  
#     http://www.anp.gov.br/importacao-e-exportacao/autorizacoes-para-exercicio-da-atividade 
#      
#     [02/03/2020 13:24]  Wagner Christe:  
#     http://www.anp.gov.br/importacao-e-exportacao/relatorios  
#     http://www.mdic.gov.br/index.php/comercio-exterior/estatisticas-de-comercio-exterior/base-de-dados-do-comercio-exterior-brasileiro-arquivos-para-download 
#     http://www.anp.gov.br/importacao-e-exportacao/autorizacoes-para-exercicio-da-atividade 





# "http://www.anp.gov.br/arquivos/importacao_exportacao/relatorios/2020/202001-0131-lis-deferidas.pdf "




substrRight <- function(x, n){
  substr( x, nchar(x)-n+1, nchar(x) )
}

substrLeft <- function(x, n){
  substr( x , 0 , n )
}




#============================
#
#============================
#install.packages("pdftables")
#library(pdftables)
# install.packages("pdftools")
library(pdftools)
library(dplyr)


#====================================================
# LISTA DE LINKS DOS RALATORIOS QUE SERÃO EXTRAIDOS
#====================================================
vetor_lista_de_relatorios <- c(
  "http://www.anp.gov.br/arquivos/importacao_exportacao/relatorios/2020/202001-0131-lis-deferidas.pdf ",                         # 1
  "http://www.anp.gov.br/arquivos/importacao_exportacao/relatorios/2020/202002-0129-lis-deferidas.pdf" ,                         # 2
  "http://www.anp.gov.br/images/Importacao_Exportacao/Relatorios/2019/LIs-Deferidas_Janeiro-2019.pdf"  ,                         # 3
  "http://www.anp.gov.br/images/Importacao_Exportacao/Relatorios/2019/LIs-Deferidas_Fevereiro-2019.pdf",                         # 4
  "http://www.anp.gov.br/images/Importacao_Exportacao/Relatorios/2019/LIs-Deferidas_Marco-2019.pdf"    ,                         # 5
  "http://www.anp.gov.br/images/Importacao_Exportacao/Relatorios/2019/LIs-Deferidas_Abril-2019.pdf"    ,                         # 6
  "http://www.anp.gov.br/images/Importacao_Exportacao/Relatorios/2019/LIs-Deferidas_Abril-2019-2.pdf"  ,                         # 7
  "http://www.anp.gov.br/arquivos/importacao_exportacao/relatorios/201905-0115-lis-deferidas.pdf"      ,                         # 8
  "http://www.anp.gov.br/arquivos/importacao_exportacao/relatorios/201905-1631-lis-deferidas.pdf"      ,                         # 9
  "http://www.anp.gov.br/arquivos/importacao_exportacao/relatorios/201906-0130-lis-deferidas.pdf"      ,                         # 10
  "http://www.anp.gov.br/arquivos/importacao_exportacao/relatorios/201907-0131-lis-deferidas.pdf"      ,                         # 11
  "http://www.anp.gov.br/arquivos/importacao_exportacao/relatorios/201908-0131-lis-deferidas.pdf"      ,                         # 12
  "http://www.anp.gov.br/arquivos/importacao_exportacao/relatorios/201909-0130-lis-deferidas.pdf"      ,                         # 13
  "http://www.anp.gov.br/arquivos/importacao_exportacao/relatorios/201910-0131-lis-deferidas.pdf"      ,                         # 14
  "http://www.anp.gov.br/arquivos/importacao_exportacao/relatorios/201911-0130-lis-deferidas.pdf"      ,                         # 15
  "http://www.anp.gov.br/arquivos/importacao_exportacao/relatorios/201912-0131-lis-deferidas.pdf"      ,                         # 16                                                           
  "http://www.anp.gov.br/images/Importacao_Exportacao/Relatorios/2018/Relatorio_Importacao_2018_01_01_a_01_15_Consolidado.pdf" , # 17
  "http://www.anp.gov.br/images/Importacao_Exportacao/Relatorios/2018/Relatorio_Importacao_2018_01_16_a_01_31_Consolidado.pdf" , # 18
  "http://www.anp.gov.br/images/Importacao_Exportacao/Relatorios/2018/Relatorio_Importacao_2018_02_01_a 02_15_Consolidado.pdf" , # 19
  "http://www.anp.gov.br/images/Importacao_Exportacao/Relatorios/2018/Relatorio_Importacao_2018_02_16_a 02_28_Consolidado.pdf" , # 20
  "http://www.anp.gov.br/images/Importacao_Exportacao/Relatorios/2018/Relatorio_Importacao_2018_03_01_a_03_15_Consolidado.pdf" , # 21
  "http://www.anp.gov.br/images/Importacao_Exportacao/Relatorios/2018/Relatorio_Importacao_2018_03_16_a_03_31_Consolidado.pdf" , # 22
  "http://www.anp.gov.br/images/Importacao_Exportacao/Relatorios/2018/Relatorio_Importacao_2018_04_01_a_04_15_Consolidado.pdf" , # 23
  "http://www.anp.gov.br/images/Importacao_Exportacao/Relatorios/2018/Relatorio_Importacao_2018_04_16_a_04_30_Consolidado.pdf" , # 24
  "http://www.anp.gov.br/images/Importacao_Exportacao/Relatorios/2018/Relatorio_Importacao_2018_05_01_a_05_15_Consolidado.pdf" , # 25
  "http://www.anp.gov.br/images/Importacao_Exportacao/Relatorios/2018/Relatorio_Importacao_2018_05_16_a_05_31_Consolidado.pdf" , # 26
  "http://www.anp.gov.br/images/Importacao_Exportacao/Relatorios/2018/Relatorio_Importacao_2018_06_01_a_06_15_Consolidado.pdf" , # 27
  "http://www.anp.gov.br/images/Importacao_Exportacao/Relatorios/2018/Relatorio_Importacao_2018_06_16_a_06_30_Consolidado.pdf" , # 28
  "http://www.anp.gov.br/images/Importacao_Exportacao/Relatorios/2018/Relatorio_Importacao_2018_07_01_a_07_15_Consolidado.pdf" , # 29
  "http://www.anp.gov.br/images/Importacao_Exportacao/Relatorios/2018/Relatorio_Importacao_2018_07_16_a_07_31_Consolidado.pdf" , # 30
  "http://www.anp.gov.br/images/Importacao_Exportacao/Relatorios/2018/Relatorio_Importacao_2018_08_01_a_08_15_Consolidado.pdf" , # 31
  "http://www.anp.gov.br/images/Importacao_Exportacao/Relatorios/2018/Relatorio_Importacao_2018_08_16_a_08_31_Consolidado.pdf" , # 32
  "http://www.anp.gov.br/images/Importacao_Exportacao/Relatorios/2018/Relatorio_Importacao_2018_09_01_a_09_15_Consolidado.pdf" , # 33
  "http://www.anp.gov.br/images/Importacao_Exportacao/Relatorios/2018/Relatorio_Importacao_2018_09_16_a_09_30_Consolidado.pdf" , # 34
  "http://www.anp.gov.br/images/Importacao_Exportacao/Relatorios/2018/Relatorio_Importacao_2018_10_01_a_10_15_Consolidado.pdf" , # 35
  "http://www.anp.gov.br/images/Importacao_Exportacao/Relatorios/2018/Relatorio_Importacao_2018_10_16_a_10_31_Consolidado.pdf" , # 36
  "http://www.anp.gov.br/images/Importacao_Exportacao/Relatorios/2018/Relatorio_Importacao_2018_11_01_a_11_15_Consolidado.pdf" , # 37
  "http://www.anp.gov.br/images/Importacao_Exportacao/Relatorios/2018/Relatorio_Importacao_2018_11_16_a_11_30_Consolidado.pdf" , # 38
  "http://www.anp.gov.br/images/Importacao_Exportacao/Relatorios/2018/Relatorio_Importacao_2018_12_01_a_12_15_Consolidado.pdf" , # 39
  "http://www.anp.gov.br/arquivos/importacao_exportacao/relatorios/2018/relatorio_importacao_2018_12_16_a_12_31_consolidado.pdf" # 40                                
)



indice_de_relatorio              <- 3
lista_relatorio_importador       <- list()

for( indice_de_relatorio in 14)#length(vetor_lista_de_relatorios) )
{
  #====================================================
  #    TRANSFORMAR PDF EM UMA LISTA DE VETORES
  #    CADA ITEM DO VETOR É UMA LINHA 
  #    DA PAGINA DO DOCUMENTO
  #    CADA ITEM DA LISTA É UMA PAGINA DO DOCUMENTO
  #====================================================
  texto_do_pdf_cortado             <- strsplit( pdf_text( vetor_lista_de_relatorios[ indice_de_relatorio ] )  , split = "\n" )             
  #====================================================
  #
  #  ARMAZENA-SE O PERÍODO QUE SE ENCONTRA NA 
  #  TERCEIRA LINHA DA PRIMEIRA PAGINA
  #  - PRIMEIRO ITEM DO VETOR 
  #    DO PRIMEIRO ITEM DA LISTA.
  #====================================================
  numero_da_pagina                 <- 1
  texto_do_pdf_cortado_sem_r       <- gsub("\\r","", texto_do_pdf_cortado[[numero_da_pagina]])
  periodo_do_relatorio             <- strsplit( texto_do_pdf_cortado_sem_r[[3]], split = " " )
  periodo_do_relatorio             <- periodo_do_relatorio[[1]][which(periodo_do_relatorio[[1]] != "" )]
  periodo_do_relatorio             <- paste(periodo_do_relatorio, collapse = "")
  periodo_do_relatorio
  #====================================================
  # CORTA-SE AS DUAS ULTIMAS LINHAS DE CADA ITEM
  # DA LISTA DE PAGINAS
  #====================================================
  #====================================================
  # CORTA-SE AS DUAS PRIMEIRAS LINHAS A PARTIR DA
  # SEGUNDA PÁGINA 
  #====================================================
  #====================================================
  # CONCATENA-SE TODAS ESSAS LINHAS NUM UNICO VETOR
  #====================================================
  
  lista_de_paginas <- ""
  numero_da_pagina <- 1
  for(numero_da_pagina in 1:length(texto_do_pdf_cortado) )
  {
    texto_do_pdf_cortado_sem_r             <- gsub("\\r","", texto_do_pdf_cortado[[numero_da_pagina]])
    if( numero_da_pagina == 1 ){
      filtro_retira_duas_ultimas_linhas  <- c( length(texto_do_pdf_cortado_sem_r),length(texto_do_pdf_cortado_sem_r)-1 )
    }else{
      filtro_retira_duas_ultimas_linhas  <- c(1,2, length(texto_do_pdf_cortado_sem_r),length(texto_do_pdf_cortado_sem_r)-1 )
    }
    lista_de_paginas                       <- c( lista_de_paginas, texto_do_pdf_cortado_sem_r[ -filtro_retira_duas_ultimas_linhas ] )
  }
  amostra_de_relatorio_concatenado <- lista_de_paginas 
  amostra_de_relatorio_concatenado
  
  
  
  indice_de_linha      <- 7
  is_importador        <- function(x) substrLeft( x, 1  ) != " "
  #====================================================
  # ARMAZENAR A POSIÇÃO DE TODOS OS IMPORTADORES
  # CONTIDOS NO VETOR DO RELATÓRIO SELECIONADO
  #====================================================
  filtro_de_importador <- which( is_importador( amostra_de_relatorio_concatenado ) )
  #====================================================
  # CORREÇÃO DE NOMES
  #====================================================
  linhas_para_retirar  <- numeric(0)
  for( indice_do_filtro in 1:( length(filtro_de_importador) -1) )
  {
    if( filtro_de_importador[indice_do_filtro]+1 == filtro_de_importador[indice_do_filtro+1] ){            
      cat( filtro_de_importador[indice_do_filtro]-filtro_de_importador[indice_do_filtro+1], "\n")
      linhas_para_retirar  <- c( linhas_para_retirar, filtro_de_importador[indice_do_filtro+1] )
      indice_do_filtro     <- indice_do_filtro+1
      
    }else{
      
    }
  }
  if(length(linhas_para_retirar)>0)
    amostra_de_relatorio_concatenado <- amostra_de_relatorio_concatenado[-linhas_para_retirar]    
  #====================================================
  # ARMAZENAR A POSIÇÃO DE TODOS OS IMPORTADORES
  # CONTIDOS NO VETOR DO RELATÓRIO SELECIONADO
  # (CORRIGIDO)
  #====================================================    
  filtro_de_importador <- which( is_importador( amostra_de_relatorio_concatenado ) )
  
  #====================================================
  #  A PARTIR DO TERCEIRO INDICE SE ENCONTRAM 
  #  AS PPOSIÇÕES DOS NOMES DOS IMPORTADORES
  #====================================================
  k<-7 # SELECIONA O IMPORTADOR
  lista_de_importadores         <- list()
  lista_de_produtos_final_final <- list()
  
  
  for(k in 7:( length(filtro_de_importador) ) )
  {
    if( k == length(filtro_de_importador) )
      intervalo_do_produto               <- (filtro_de_importador[k]+1):length(amostra_de_relatorio_concatenado)
    if( k != length(filtro_de_importador) )
      intervalo_do_produto               <- (filtro_de_importador[k]+1):(filtro_de_importador[k+1]-1)        
    
    
    produtos_do_importador_selecionado <- amostra_de_relatorio_concatenado[intervalo_do_produto]    
    produtos_do_importador_selecionado
    
    
    nome_produto   <- numeric(0)
    volume_produto <- numeric(0)
    
    if(indice_de_relatorio %in% c(1,2,12,13,14) ){
      filtro_de_produto              <- grep( "[[:alpha:]]", substrLeft( produtos_do_importador_selecionado, 48 )  )    
      nome_produto    <- gsub( "[[0-9]"       , "" , produtos_do_importador_selecionado[filtro_de_produto] )        
      nome_produto    <- substrLeft( nome_produto,90 )
      nome_produto    <- trimws(nome_produto)
      volume_produto  <- gsub( "[[:alpha:]]"  , "", produtos_do_importador_selecionado[filtro_de_produto] )
      volume_produto  <- substrRight( volume_produto, 30 )
      volume_produto  <- trimws( volume_produto )
      
    }else{
      if(indice_de_relatorio %in% c(3) ){
        filtro_de_produto              <- grep( "[[:alpha:]]", substrLeft( produtos_do_importador_selecionado, 46 )  )    
        nome_produto    <- gsub( "[[0-9]"       , "" , produtos_do_importador_selecionado[filtro_de_produto] )        
        nome_produto    <- substrLeft( nome_produto,100 )
        nome_produto    <- trimws(nome_produto)
        volume_produto  <- gsub( "[[:alpha:]]"  , "", produtos_do_importador_selecionado[filtro_de_produto] )
        volume_produto  <- substrRight( volume_produto, 30 )
        volume_produto  <- trimws( volume_produto )
      }else{ 
        if(indice_de_relatorio %in% c(10,11) ){
          filtro_de_produto              <- grep( "[[:alpha:]]", substrLeft( produtos_do_importador_selecionado, 45 )  )    
          nome_produto    <- gsub( "[[0-9]"       , "" , produtos_do_importador_selecionado[filtro_de_produto] )        
          nome_produto    <- substrLeft( nome_produto,90 )
          nome_produto    <- trimws(nome_produto)
          volume_produto  <- gsub( "[[:alpha:]]"  , "", produtos_do_importador_selecionado[filtro_de_produto] )
          volume_produto  <- substrRight( volume_produto, 30 )
          volume_produto  <- trimws( volume_produto )
        }else{
          filtro_de_produto              <- grep( "[[:alpha:]]", substrLeft( produtos_do_importador_selecionado, 44 )  )    
          nome_produto    <- gsub( "[[0-9]"       , "" , produtos_do_importador_selecionado[filtro_de_produto] )        
          nome_produto    <- substrLeft( nome_produto,100 )
          nome_produto    <- trimws(nome_produto)
          volume_produto  <- gsub( "[[:alpha:]]"  , "", produtos_do_importador_selecionado[filtro_de_produto] )
          volume_produto  <- substrRight( volume_produto, 20 )
          volume_produto  <- trimws( volume_produto )
        }
      }
    }
    
    lista_de_produtos <- list()
    ki                    <- 1
    if( length(filtro_de_produto)!=0 ){
      for( ki in 1:( length(filtro_de_produto) ) )
      {
        if( ki == length(filtro_de_produto) )
          intervalo_porto       <- (filtro_de_produto[ki]+1):length(produtos_do_importador_selecionado)
        if( ki != length(filtro_de_produto) )
          intervalo_porto       <- (filtro_de_produto[ki]+1):(filtro_de_produto[ki+1]-1)
        porto_do_produto          <- produtos_do_importador_selecionado[intervalo_porto]
        porto_do_produto
        
        nome_porto           <- numeric(0)
        volume_produto_porto <- numeric(0)
        if(indice_de_relatorio %in% c(10,11,13,2) ){
          nome_porto            <- gsub( "[[0-9]"       , "" , porto_do_produto )        
          nome_porto            <- substrLeft( nome_porto,90 )
          nome_porto            <- trimws(nome_porto)
          volume_produto_porto  <- gsub( "[[:alpha:]]"  , "" , porto_do_produto )
          volume_produto_porto  <- gsub( "\\/"  , "" , porto_do_produto )
          volume_produto_porto  <- substrRight( volume_produto_porto, 29 )
          volume_produto_porto  <- trimws( volume_produto_porto )
        }else{
          nome_porto            <- gsub( "[[0-9]"       , "" , porto_do_produto )        
          nome_porto            <- substrLeft( nome_porto,100 )
          nome_porto            <- trimws(nome_porto)
          volume_produto_porto  <- gsub( "[[:alpha:]]"  , "" , porto_do_produto )
          volume_produto_porto  <- gsub( "\\/"  , "" , porto_do_produto )
          volume_produto_porto  <- substrRight( volume_produto_porto, 29 )
          volume_produto_porto  <- trimws( volume_produto_porto )
        }
        lista_de_produtos[[ki]] <- data.frame(
          "Periodo"    = periodo_do_relatorio, 
          "Importador" = amostra_de_relatorio_concatenado[ filtro_de_importador[k] ], 
          "Produto"    = nome_produto[ ki ],
          "Porto"      = nome_porto,
          "Volume"     = volume_produto_porto
        )
      }
    }else{
      lista_de_produtos[[ki]] <- data.frame(
        "Periodo"    = periodo_do_relatorio, 
        "Importador" = amostra_de_relatorio_concatenado[ filtro_de_importador[k] ], 
        "Produto"    = "Fora do Padrão",
        "Porto"      = "Fora do Padrão",
        "Volume"     = "Fora do Padrão"
      )
    }
    
    
    lista_de_produtos_final <- lista_de_produtos[[1]]
    if( length(lista_de_produtos) >=2 )
      for( indice_lista in 2:length(lista_de_produtos) )
        lista_de_produtos_final <- rbind( lista_de_produtos_final, lista_de_produtos[[indice_lista]] )
    lista_de_produtos_final_final[[k]] <- lista_de_produtos_final
  }
  lista_de_importadores <- lista_de_produtos_final_final[[1]]
  for( indice_lista in 2:length(lista_de_produtos_final_final) )
    lista_de_importadores <- rbind( lista_de_importadores, lista_de_produtos_final_final[[indice_lista]] )
  
  lista_relatorio_importador[[indice_de_relatorio]] <- lista_de_importadores
}


#unique( lista_relatorio_importador[[indice_de_relatorio]]$Porto   )
#unique( lista_relatorio_importador[[indice_de_relatorio]]$Produto )

table(lista_relatorio_importador[[indice_de_relatorio]]$Porto)
table(lista_relatorio_importador[[indice_de_relatorio]]$Produto)


#lista_relatorio_importador[[5]][ which(is.na(lista_relatorio_importador[[5]]$Volume)), ]


