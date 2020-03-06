


source( "C:\\Users\\GM793SZ\\Desktop\\Projetos\\P004_Marathon\\pdf_to_tables\\pdf_to_tables_gasolina_000.r", encoding = "utf-8")
source( "C:\\Users\\GM793SZ\\Desktop\\Projetos\\P004_Marathon\\pdf_to_tables\\pdf_to_tables_etanol_000.r", encoding = "utf-8")
source( "C:\\Users\\GM793SZ\\Desktop\\Projetos\\P004_Marathon\\pdf_to_tables\\pdf_to_tables_diesel_000.r", encoding = "utf-8")


dataframe_gasolina_final_1 <- data.frame( "Combustivel" = "Gasolina" , dataframe_gasolina_final )
dataframe_etanol_final_1   <- data.frame( "Combustivel" = "Etanol"   , dataframe_etanol_final   )
dataframe_diesel_final_1   <- data.frame( "Combustivel" = "Diesel"   , dataframe_diesel_final   )
dataframe_qav_final_1      <- data.frame( "Combustivel" = "QAV"      , dataframe_qav_final      )




dataframe_fuel <- full_join( dataframe_gasolina_final_1, dataframe_etanol_final_1  )
dataframe_fuel <- full_join( dataframe_fuel, dataframe_diesel_final_1 )
dataframe_fuel <- full_join( dataframe_fuel, dataframe_qav_final_1 )




dataframe_fuel[,-c(ncol(dataframe_fuel),ncol(dataframe_fuel)-1) ]



write.csv2( dataframe_fuel,"C:\\Users\\GM793SZ\\Desktop\\Projetos\\P004_Marathon\\pdf_to_tables\\resultado.csv" )

