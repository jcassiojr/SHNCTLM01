#' OBJETIVO:
#' gerar arquivos tidy a partir dos meses anteriores lidos diretamente
#' da planilha TDB Produção gerada para o mês anterior
########################
f_LeTDB <- function(mm_aaaa)
{
  fileout1 <- paste("./tidydata", mm_aaaa, "tidyCarProd.csv", sep = "/")
  fileout2 <- paste("./tidydata", mm_aaaa, "tidyCarteProd.csv", sep = "/")
  fileout3 <- paste("./tidydata", mm_aaaa, "tidyCarteSeguros.csv", sep = "/")
  fileout4 <- paste("./tidydata", mm_aaaa, "tidyCarteCartoes.csv", sep = "/")
  fileout5 <- paste("./tidydata", mm_aaaa, "tidyCarteAmort.csv", sep = "/")
  
  aaaamm <- paste0(substr(mm_aaaa,4,7), substr(mm_aaaa,1,2))
  
  filein <- paste0("./tidydata/TDB-Producao-", aaaamm,".xlsx")

  # libraries
  # verifica e prepara ambiente de libraries necessárias
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("openxlsx needed for this function to work. Please install it.",
         call. = FALSE)
  } else {
    if(!require(openxlsx)){install.packages("openxlsx")}
  }  
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("dplyr needed for this function to work. Please install it.",
       call. = FALSE)
  } else {
    if(!require(dplyr)){install.packages("dplyr")}
  }  
  
  ################################
  # aba <mês>", DADOS DE PRODUÇÃO CREDIÁRIO
  ################################
  creprodData_ano <- read.xlsx(filein, sheet = "Base-CAR-Prod", colNames = TRUE)
  # elimina colunas de formulas
  creprodData_ano <-
    creprodData_ano %>%
    select(-(N), -(X2))
    # changing column name and forcing uppercase names
    names(creprodData_ano) <- toupper(names(creprodData_ano))
  ###################
  # aba <mês>, DADOS DE PRODUÇÃO CARTÃO
  ###################
  carprodData_ano <- read.xlsx(filein, sheet = "CARPROD-Compras", colNames = TRUE)
  # elimina coluna DATAPROC.FMT
  carprodData_ano <-
    carprodData_ano %>%
    select(-(DATAPROC.FMT))
  # changing column name and forcing uppercase names
  names(carprodData_ano) <- toupper(names(carprodData_ano))
  
  ####################
  # aba <mês>, DADOS DE ADESÃO E BASE DE SEGUROS
  ##################
  segData_ano <- read.xlsx(filein, sheet = "baseCARTESEGUROS", colNames = TRUE)
  # changing column name and forcing uppercase names an eliminating columns
  segData_ano <-
    segData_ano %>%
    select(-(X1), -(DATAPROC.FMT))
  
  # changing column name and forcing uppercase names
  names(segData_ano) <- toupper(names(segData_ano))
  
  ###################
  # aba <mês>, DADOS DE CARTÃO
  ########################
  cardData_ano <- read.xlsx(filein, sheet = "baseCARTECARTOES", colNames = TRUE)
  # changing column name and forcing uppercase names an eliminating columns
  cardData_ano <-
    cardData_ano %>%
    select(-(DATAPROC.FMT))
  
  # changing column name and forcing uppercase names
  names(cardData_ano) <- toupper(names(cardData_ano))

  ####################
  # aba DadosAmort, DADOS DE AMORTIZACAO
  #####################
  amortData <- read.xlsx(filein, sheet = "baseAMORT", colNames = TRUE)
  # changing column name and forcing uppercase names an eliminating columns

  # changing column name and forcing uppercase names
  names(amortData) <- toupper(names(amortData))
  
# ALTERNATIVA
# gravar cada tabela gerada em aba diferente da planilha de relatório
l <- list("CREPROD" = creprodData_ano, "CARPROD" = carprodData_ano, "CARTSEG" = segData_ano,
          "CARTCARD" = cardData_ano, "AMORT" = amortData)
#return(l)
#write.xlsx(l, file = fileout, colNames = TRUE, borders = "columns")
# grava arquivo csv
write.table(l$CREPROD, file = fileout1, quote = FALSE, sep = ";", row.names = FALSE)
write.table(l$CARPROD, file = fileout2, quote = FALSE, sep = ";", row.names = FALSE)
write.table(l$CARTSEG, file = fileout3, quote = FALSE, sep = ";", row.names = FALSE)
write.table(l$CARTCARD, file = fileout4, quote = FALSE, sep = ";", row.names = FALSE)
write.table(l$AMORT, file = fileout5, quote = FALSE, sep = ";", row.names = FALSE)
}
