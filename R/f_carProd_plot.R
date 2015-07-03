########################
# Criação de plot para valor de número de contratos de RelCTL01, aba REL-CarProd
# 1. cria plots para variáveis mensais da aba REL-CarProd
# 2. Alternativamente, grava arquivo csv para inmportação em CLIC-DATA com os mesmos dados
#######################
f_carProd_plot <- function(in_df)
{
# libraries
    if(!require(openxlsx)){install.packages("openxlsx")}
    if(!require(dplyr)){install.packages("dplyr")}
    if(!require(ggplot2)){install.packages("ggplot2")}
    if(!require(gridExtra)){install.packages("gridExtra")}

# constants
#in_prodalp <- "CREN"
#filein_ctlm <- "./RelCTL01/dashboard/dshRelCTL01-CarProd.xlsx"
#filein <- paste0("tidydata/dshRelCTL01.xlsx") 

############################
# RELATORIO RELCTL01-TDB PRODUÇÃO
############################

# reading rawCARPROD csv file
############################
#df_relCreSys <- read.xlsx(filein_ctlm, sheet = "tidyCredSys", colNames = TRUE)
#df_relCreSys <- read.xlsx(filein, sheet = in_prodalp, colNames = TRUE)

#####################################################################################
# AGRUPAMENTO 1 de TOTAL CREDIARIO e SYS: Para toda a Cetelem
#####################################################################################

######################################################################################
# plotting the result
######################################################################################
# Plotting Número de Contratos
if (all(is.na(in_df$NBFI))) { # força 0 se coluna toda NA
    in_df$NBFI <- 0
}
    g <- ggplot(in_df, aes(DATAPROC, NBFI, group = 1)) 
    g1 <- g + geom_line() 
    g2 <- g1 + geom_smooth(method = "lm")
    g3 <- g2 + labs(title = "Quantidade de Contratos")
    g4 <- g3 + labs(y = "Quantidade de Contratos")
    g_nContr <- g4 + labs(x = "M\u0207s")
#    g_nContr

# Plotting Montante de Contratos
if (all(is.na(in_df$MTFI))) { # força 0 se coluna toda NA
    in_df$MTFI <- 0
}
    g <- ggplot(in_df, aes(DATAPROC, MTFI, group = 1)) 
    g1 <- g + geom_line() 
    g2 <- g1 + geom_smooth(method = "lm")
    g3 <- g2 + labs(title = "Montante de Contratos (R$)")
    g4 <- g3 + labs(y = "Montante de Contratos (R$)")
    g_mtFin <- g4 + labs(x = "M\u0207s")
#    g_mtFin

# Plotting Prazo Médio (meses)
if (all(is.na(in_df$DURFIN))) { # força 0 se coluna toda NA
    in_df$DURFIN <- 0
}
    g <- ggplot(in_df, aes(DATAPROC, DURFIN, group = 1)) 
    g1 <- g + geom_line() 
    g2 <- g1 + geom_smooth(method = "lm")
    g3 <- g2 + labs(title = "Prazo M\u00E9dio (meses)")
    g4 <- g3 + labs(y = "Prazo M\u00E9dio (meses)")
    g_przmed <- g4 + labs(x = "M\u0207s")
#    g_przmed

# Plotting Ticket Médio (R$)
if (all(is.na(in_df$TKTMED))) { # força 0 se coluna toda NA
    in_df$TKTMED <- 0
}
    g <- ggplot(in_df, aes(DATAPROC, TKTMED, group = 1)) 
    g1 <- g + geom_line() 
    g2 <- g1 + geom_smooth(method = "lm")
    g3 <- g2 + labs(title = "Ticket M\u00E9dio (R$)")
    g4 <- g3 + labs(y = "Ticket M\u00E9dio (R$)")
    g_tktmed <- g4 + labs(x = "M\u0207s")
#    g_tktmed

# Plotting TAC Média por contrato (R$)
if (all(is.na(in_df$TAC))) { # força 0 se coluna toda NA
    in_df$TAC <- 0
}
    g <- ggplot(in_df, aes(DATAPROC, TAC, group = 1)) 
    g1 <- g + geom_line() 
    g2 <- g1 + geom_smooth(method = "lm")
    g3 <- g2 + labs(title = "TAC M\u00E9dia por Contrato (R$)")
    g4 <- g3 + labs(y = "TAC M\u00E9dia por Contrato (R$)")
    g_tac <- g4 + labs(x = "M\u0207s")
#    g_tac


# Plotting % produção com seguro (R$)
if (all(is.na(in_df$SEGURO))) { # força 0 se coluna toda NA
    in_df$SEGURO <- 0
}
    g <- ggplot(in_df, aes(DATAPROC, SEGURO, group = 1)) 
    g1 <- g + geom_line() 
    g2 <- g1 + geom_smooth(method = "lm")
    g3 <- g2 + labs(title = "Produ\u00E7\u00E3o com Seguro (%)")
    g4 <- g3 + labs(y = "Produ\u00E7\u00E3o com Seguro (%)")
    g_seg <- g4 + labs(x = "M\u0207s")
#    g_seg

# aberturas com seguro (em números)
if (all(is.na(in_df$ABRSEG))) { # força 0 se coluna toda NA
    in_df$ABRSEG <- 0
}
    g <- ggplot(in_df, aes(DATAPROC, ABRSEG, group = 1))     
    g1 <- g + geom_line() 
    g2 <- g1 + geom_smooth(method = "lm")
    g3 <- g2 + labs(title = "Aberturas com Seguro (%)")
    g4 <- g3 + labs(y = "Aberturas com Seguro (%)")
    g_abrseg <- g4 + labs(x = "M\u0207s")
#    g_abrseg

#####################################################################################
# AGRUPAMENTO 2 por TAXAS MÉDIAS PRODALP: (crediário), S (SYS) e diversos MODPAY
#####################################################################################

# Plotting Taxa Média Crediário (%)
if (all(is.na(in_df$TXMEDCRED))) { # força 0 se coluna toda NA
    in_df$TXMEDCRED <- 0
}
    g <- ggplot(in_df, aes(DATAPROC, TXMEDCRED, group = 1))
    g1 <- g + geom_line() 
    g2 <- g1 + geom_smooth(method = "lm")
    g3 <- g2 + labs(title = "Taxa M\u00E9dia Credi\u00E1rio (%)")
    g4 <- g3 + labs(y = "Taxa M\u00E9dia Credi\u00E1rio (%)")
    g_txmedcre <- g4 + labs(x = "M\u0207s")
#    g_txmedcre

# Plotting Taxa Média Cliente SYS (%)
if (all(is.na(in_df$TXMEDCLISYS))) { # força 0 se coluna toda NA
    in_df$TXMEDCLISYS <- 0
}
    g <- ggplot(in_df, aes(DATAPROC, TXMEDCLISYS, group = 1)) 
    #g1 <- g + geom_bar(stat="identity") 
    g1 <- g + geom_line() 
    g2 <- g1 + geom_smooth(method = "lm")
    g3 <- g2 + labs(title = "Taxa M\u00E9dia Cliente SYS (%)")
    g4 <- g3 + labs(y = "Taxa M\u00E9dia Cliente SYS (%)")
    g_txmedclisys <- g4 + labs(x = "M\u0207s")
#    g_txmedclisys


# taxa média no MODO PGTO A
if (all(is.na(in_df$TXMEDMODOA))) { # força 0 se coluna toda NA
    in_df$TXMEDMODOA <- 0
}
    g <- ggplot(in_df, aes(DATAPROC, TXMEDMODOA, group = 1)) 
    #g1 <- g + geom_bar(stat="identity") 
    g1 <- g + geom_line() 
    g2 <- g1 + geom_smooth(method = "lm")
    g3 <- g2 + labs(title = "Taxa M\u00E9dia de Modo Pagto A (%)")
    g4 <- g3 + labs(y = "Taxa M\u00E9dia de Modo Pagto A (%)")
    g_txmedmodoa <- g4 + labs(x = "M\u0207s")
#    g_txmedmodoa


# taxa média no cheque
if (all(is.na(in_df$TXMEDCHQ))) { # força 0 se coluna toda NA
    in_df$TXMEDCHQ <- 0
}
    g <- ggplot(in_df, aes(DATAPROC, TXMEDCHQ, group = 1)) 
    #g1 <- g + geom_bar(stat="identity") 
    g1 <- g + geom_line() 
    g2 <- g1 + geom_smooth(method = "lm")
    g3 <- g2 + labs(title = "Taxa M\u00E9dia no Cheque (%)")
    g4 <- g3 + labs(y = "Taxa M\u00E9dia no Cheque (%)")
    g_txmedchq <- g4 + labs(x = "M\u0207s")
#    g_txmedchq


# taxa média no DCC
if (all(is.na(in_df$TXMEDDCC))) { # força 0 se coluna toda NA
    in_df$TXMEDDCC <- 0
}
    g <- ggplot(in_df, aes(DATAPROC, TXMEDDCC, group = 1)) 
    #g1 <- g + geom_bar(stat="identity") 
    g1 <- g + geom_line() 
    g2 <- g1 + geom_smooth(method = "lm")
    g3 <- g2 + labs(title = "Taxa M\u00E9dia no DCC (%)")
    g4 <- g3 + labs(y = "Taxa M\u00E9dia no DCC (%)")
    g_txmeddcc <- g4 + labs(x = "M\u0207s")
#    g_txmeddcc

# taxa média no carnê
if (all(is.na(in_df$TXMEDCARNE))) { # força 0 se coluna toda NA
    in_df$TXMEDCARNE <- 0
}
    g <- ggplot(in_df, aes(DATAPROC, TXMEDCARNE, group = 1)) 
    #g1 <- g + geom_bar(stat="identity") 
    g1 <- g + geom_line() 
    g2 <- g1 + geom_smooth(method = "lm")
    g3 <- g2 + labs(title = "Taxa M\u00E9dia no Carn\u0207 (%)")
    g4 <- g3 + labs(y = "Taxa M\u00E9dia no Carn\u0207 (%)")
    g_txmedcarne <- g4 + labs(x = "M\u0207s")
#    g_txmedcarne


# taxa média no MODO PGTO G
if (all(is.na(in_df$TXMEDMODOG))) { # força 0 se coluna toda NA
    in_df$TXMEDMODOG <- 0
}
    g <- ggplot(in_df, aes(DATAPROC, TXMEDMODOG, group = 1)) 
    #g1 <- g + geom_bar(stat="identity") 
    g1 <- g + geom_line() 
    g2 <- g1 + geom_smooth(method = "lm")
    g3 <- g2 + labs(title = "Taxa M\u00E9dia de Modo Pagto G (%)")
    g4 <- g3 + labs(y = "Taxa M\u00E9dia de Modo Pagto G (%)")
    g_txmedmodog <- g4 + labs(x = "M\u0207s")
#    g_txmedmodog

#sprintf('5\u03BCg'))
#####################################################################################
# AGRUPAMENTO 3 por MONTANTE DE PRODUÇÃO,  diversos MODPAY
#####################################################################################

# % producao montante no MODO PGTO A  (toda a CETELEM)
if (all(is.na(in_df$MTFI_MODOA))) { # força 0 se coluna toda NA
    in_df$MTFI_MODOA <- 0
}
    g <- ggplot(in_df, aes(DATAPROC, MTFI_MODOA, group = 1)) 
    g1 <- g + geom_line() 
    g2 <- g1 + geom_smooth(method = "lm")
    g3 <- g2 + labs(title = "Montante de Produ\u00E7\u00E3o Modo A (%)")
    g4 <- g3 + labs(y = "Montante de Produ\u00E7\u00E3o Modo A (%)")
    g_mtmodoa <- g4 + labs(x = "M\u0207s")
#    g_mtmodoa


# % producao montante em cheque
if (all(is.na(in_df$MTFI_CHQ))) { # força 0 se coluna toda NA
    in_df$MTFI_CHQ <- 0
}
    g <- ggplot(in_df, aes(DATAPROC, MTFI_CHQ, group = 1))  
    g1 <- g + geom_line() 
    g2 <- g1 + geom_smooth(method = "lm")
    g3 <- g2 + labs(title = "Montante de Produ\u00E7\u00E3o em Cheque (%)")
    g4 <- g3 + labs(y = "Montante de Produ\u00E7\u00E3o em Cheque (%)")
    g_mtchq <- g4 + labs(x = "M\u0207s")
#    g_mtchq


# % producao montante em DCC
if (all(is.na(in_df$MTFI_DCC))) { # força 0 se coluna toda NA
    in_df$MTFI_DCC <- 0
}
    g <- ggplot(in_df, aes(DATAPROC, MTFI_DCC, group = 1)) 
    g1 <- g + geom_line() 
    g2 <- g1 + geom_smooth(method = "lm")
    g3 <- g2 + labs(title = "Montante de Produ\u00E7\u00E3o em DCC (%)")
    g4 <- g3 + labs(y = "Montante de Produ\u00E7\u00E3o em DCC (%)")
    g_mtdcc <- g4 + labs(x = "M\u0207s")
#    g_mtdcc


# % producao montante em carnê
if (all(is.na(in_df$MTFI_CARNE))) { # força 0 se coluna toda NA
    in_df$MTFI_CARNE <- 0
}
    g <- ggplot(in_df, aes(DATAPROC, MTFI_CARNE, group = 1)) 
    #g1 <- g + geom_bar(stat="identity") 
    g1 <- g + geom_line() 
    g2 <- g1 + geom_smooth(method = "lm")
    g3 <- g2 + labs(title = "Montante de Produ\u00E7\u00E3o em Carn\u0207 (%)")
    g4 <- g3 + labs(y = "Montante de Produ\u00E7\u00E3o em Carn\u0207 (%)")
    g_mtcarne <- g4 + labs(x = "M\u0207s")
#    g_mtcarne


# % producao montante em MODO PGTO G
if (all(is.na(in_df$MTFI_MODOG))) { # força 0 se coluna toda NA
    in_df$MTFI_MODOG <- 0
}
    g <- ggplot(in_df, aes(DATAPROC, MTFI_MODOG, group = 1)) 
    #g1 <- g + geom_bar(stat="identity") 
    g1 <- g + geom_line() 
    g2 <- g1 + geom_smooth(method = "lm")
    g3 <- g2 + labs(title = "Montante de Produ\u00E7\u00E3o em Carn\u0207 (%)")
    g4 <- g3 + labs(y = "Montante de Produ\u00E7\u00E3o em Carn\u0207 (%)")
    g_mtmodog <- g4 + labs(x = "M\u0207s")
#    g_mtmodog

#retorna lista de plots
l <- list("g_nContr" = g_nContr, "g_mtFin" = g_mtFin, "g_przmed" = g_przmed, "g_tktmed" = g_tktmed,
                    "g_tac" = g_tac, "g_seg" = g_seg, "g_abrseg" = g_abrseg,
                    "g_txmedcre" = g_txmedcre, "g_txmedclisys" = g_txmedclisys, "g_txmedmodoa" = g_txmedmodoa,
                    "g_txmedchq" = g_txmedcre, "g_txmeddcc" = g_txmeddcc, "g_txmedcarne" = g_txmedcarne,
                    "g_txmedmodog" = g_txmedmodog, "g_mtmodoa" = g_mtmodoa, "g_mtchq" = g_mtchq,
                    "g_mtdcc" = g_mtdcc, "g_mtcarne" = g_mtcarne, "g_mtmodog" = g_mtmodog)
return(l)         
}