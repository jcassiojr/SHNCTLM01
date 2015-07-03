# DEBUG
# rodar shinyapps::showLogs() do diretório da aplicação shiny para ver o log da execução local
# obs. Escolhendo simplificada/sem para taxa média deu tela cinza
# caso possa ser problema de memória, try: shinyapps::configureApp("CTL01dashb", size="xlarge")

# grupos selecionados por sociedade: 
# 1. Total crediário e Sys
# 2. Crediário Normal sem entrada (CREN)
# 3. Crediário Normal com entrada (CRET)
# 4. Crediário Normal sem entrada e Pgto no Fluxo (SYSN)
# 5. Crediário Normal com entrada e Pgto no Fluxo (SYSE)
# 6. Total Crediário Normal: (SYSE+CREN+CRET+SYSN+SYSE)
# 7. Crediário Simplificado sem entrada (CRES)
# 8. Crediário Simplificado com entrada (CRST)
# 9. Crediário Simplificado sem entrada e Pgto no Fluxo (SYSS)
# 10. Crediário Simplificado com entrada e Pgto no Fluxo (SYSC)
# 11. Total Crediário Simplificado: (CRES+CRST+SYSS+SYSC)
# RADIO 1: tipo de crediário (Normal,Simplificado)
# RADIO 1: Entrada (com, sem)
# RADIO 3: Pgto no Fluxo (sim,não)
# RADIO 4: Total Normal, Total Simplificado (obs. desconsidra os combos acima)
# - A combinação acima gera 9 dashboards de acordo com as tabelas da aba CarProd
# - criar duas abas: uma para mostrar o dashboard selecionado e outra para mostrar o dataframe completo
# - para ler o arquivo e criar os plots, colocar o fonte RelCTL01_tidy.R no dir do server.R,
#   chamar source("RelCTL01_tidy.R") e executar sua chamada em Server.R
# seta o diretorio de trabalho
#setwd("/Users/jcassiojr/Documents/MyGit/MIS/CETELEM/RelCTL01/R")
#setwd("RelCTL01/R")
# libraries
if(!require(gridExtra)){install.packages("gridExtra")}
if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(dplyr)){install.packages("dplyr")}
# ano e mês de processamento
# mudar este valor abaixo para cada mês a processar
mes_ano <- "06.2015"

# carrega source da função que gera a tabela tidy de acordo com a seleção
source("R/f_carProd_tidy.R")
# carrega source da função que plota os gráficos de acordo com a seleção
source("R/f_carProd_plot.R")
# carrega source da função que lê dados do ano da planilha TBD Produção
source("R/f_LeTDB.R")

#f_LeTDB(mes_ano)

normal <- "1"
simplificado <- "2"
comEntrada <- "1"
semEntrada <- "2"
comSYS <- "1"
semSYS <- "2"

shinyServer(function(input, output) {
    # seleciona pelo PRODALP de acordo com escolha nos widgets que gráficos gerar
    # acertando a variável que passará a funcao que gera o plot
    # CREN: Crediário = Normal(1), Entrada = Sem(2), Pagamento no Fluxo = Não (2)
    # CRET: Crediário = Normal(1), Entrada = Com(1), Pagamento no Fluxo = Não (2)
    # SYSN: Crediário = Normal(1), Entrada = Sem(2), Pagamento no Fluxo = Sim (1)
    # SYSE: Crediário = Normal(1), Entrada = Com(1), Pagamento no Fluxo = Sim (1)
    # CRE S: Crediário = Simplificado(2), Entrada = Sem(2), Pagamento no Fluxo = Não (2)
    # CRST: Crediário = Simplificado(2), Entrada = Com(1), Pagamento no Fluxo = Não (2)
    # SYSS: Crediário = Simplificado(2), Entrada = Sem(2), Pagamento no Fluxo = Sim (1)
    # SYSC: Crediário = Simplificado(2), Entrada = Com(1), Pagamento no Fluxo = Sim (1)
    
   
       
    
    # Expression that generates a histogram. The expression is
    # wrapped in a call to renderPlot to indicate that:
    #
    #  1) It is "reactive" and therefore should re-execute automatically
    #     when inputs change
    #  2) Its output type is a plot
    # alimenta caixa de texto
    #output$value <- renderPrint({ input$radioTpCred })
    #output$value <- renderPrint({ input$radioEntr })
    #output$logo <- renderImage(img(src="./CetelemAtual.png", height = 100, width = 100))
    #output$mytable1 = renderDataTable({
    #    if(input$radioTpCred == normal & input$radioEntr == semEntrada & input$radioSys == semSYS) {
    #        df_tabDados <- f_carProd_tidy("CREN")
    #    }
        #if(input$radioTpCred == normal & input$radioEntr == comEntrada & input$radioSys == semSYS) {
        #    df_tabDados <- f_carProd_tidy("CRET")
        #}
        #if(input$radioTpCred == normal & input$radioEntr == semEntrada & input$radioSys == comSYS) {
        #    df_tabDados <- f_carProd_tidy("SYSN")
        #}
        #if(input$radioTpCred == normal & input$radioEntr == comEntrada & input$radioSys == comSYS) {
        #    df_tabDados <- f_carProd_tidy("SYSE")
        #}
        #if(input$radioTpCred == simplificado & input$radioEntr == semEntrada & input$radioSys == semSYS) {
        #    df_tabDados <- f_carProd_tidy("CRES")
        #}
        #if(input$radioTpCred == simplificado & input$radioEntr == comEntrada & input$radioSys == semSYS) {
        #    df_tabDados <- f_carProd_tidy("CRST")
        #}
        #if(input$radioTpCred == simplificado & input$radioEntr == semEntrada & input$radioSys == comSYS) {
        #    df_tabDados <- f_carProd_tidy("SYSS")
        #}
        #if(input$radioTpCred == simplificado & input$radioEntr == comEntrada & input$radioSys == comSYS) {
        #    df_tabDados <- f_carProd_tidy("SYSC")
        #}

        #prepara tabela para mostrar
        #df_tabDados_final <-
        #   df_tabDados %>%
        #    rename(Número.de.Contratos = NBFI) %>%
        #    rename(Montante.Financiado.kR = MTFI) %>%
        #    rename(Prazo.Médio.meses = DURFIN) %>%
        #    rename(Ticket.Médio = TKTMED) %>%
        #    rename(TAC.Média.por.Contrato = TAC) %>%
        #    rename(Produção.com.Seguro = SEGURO) %>%
        #    rename(Aberturas.com.Seguro = ABRSEG) %>%
        #    rename(Taxa.Média.Crediário = TXMEDCRED) %>%
        #    rename(Taxa.Média.Cliente.SYS = TXMEDCLISYS) %>%
        #    rename(Taxa.Média.no.Modo.A = TXMEDMODOA) %>%
        #    rename(Taxa.Média.no.Cheque = TXMEDCHQ) %>%
        #    rename(Taxa.Média.no.DCC = TXMEDDCC) %>%
        #    rename(Taxa.Média.no.carne = TXMEDCARNE) %>%
        #    rename(Taxa.Média.no.Modo.G = TXMEDMODOG) %>%
        #    rename(Produção.em.Montante.no.Modo.A = MTFI_MODOA) %>%
        #    rename(Produção.em.Montante.no.Cheque = MTFI_CHQ) %>%
        #    rename(Produção.em.Montante.no.DCC = MTFI_DCC) %>%
        #    rename(Produção.em.Montante.no.Carne = MTFI_CARNE) %>%
        #    rename(Produção.em.Montante.no.Modo.G = MTFI_MODOG) 
        
        # transpoe dados do dataframe e coloca nomes mais amigáveis
        #c <- df_tabDados_final$DATAPROC
        # transpoe o dataframe
        #df_tabDados_final <- as.data.frame(t(df_tabDados_final))
        #colnames(df_tabDados_final) <- df_tabDados_final[1, ]
        # muda nomes das colunas usando DATAPROC
        #colnames(df_tabDados_final) <- c
        #df_tabDados_final <- df_tabDados_final[-1, ]
        # cria columa para mostrar os nomes das linhas
        #df_tabDados_final$NOME <- row.names(df_tabDados_final)
        # colocando NOME na primeira coluna
        #df_tabDados_final <- df_tabDados_final[c(13,1,2,3,4,5,6,7,8,9,10,11,12)]

    #}, options = list(orderClasses = TRUE,paging=FALSE))
    
    output$distPlot1 = renderPlot({
        # selecionar pelo tipo MODPAY
        if(input$radioTpCred == normal & input$radioEntr == semEntrada & input$radioSys == semSYS) {
          df_tabDados <- f_carProd_tidy(mes_ano,"CREN",as.numeric(input$selectChn))
          #df_tabDados <- f_carProd_tidy(mes_ano,"CREN",1)
          l_plots <- f_carProd_plot(df_tabDados)
          # FALTA: mudar para todos abaixo
        }
        if(input$radioTpCred == normal & input$radioEntr == comEntrada & input$radioSys == semSYS) {
          df_tabDados <- f_carProd_tidy(mes_ano,"CRET",as.numeric(input$selectChn)) 
          l_plots <- f_carProd_plot(df_tabDados)
        }
        if(input$radioTpCred == normal & input$radioEntr == semEntrada & input$radioSys == comSYS) {
          df_tabDados <- f_carProd_tidy(mes_ano,"SYSN",as.numeric(input$selectChn))  
          l_plots <- f_carProd_plot(df_tabDados)
        }
        if(input$radioTpCred == normal & input$radioEntr == comEntrada & input$radioSys == comSYS) {
          df_tabDados <- f_carProd_tidy(mes_ano,"SYSE",as.numeric(input$selectChn))  
          l_plots <- f_carProd_plot(df_tabDados)
        }
        if(input$radioTpCred == simplificado & input$radioEntr == semEntrada & input$radioSys == semSYS) {
          df_tabDados <- f_carProd_tidy(mes_ano,"CRES",as.numeric(input$selectChn))  
          l_plots <- f_carProd_plot(df_tabDados)
        }
        if(input$radioTpCred == simplificado & input$radioEntr == comEntrada & input$radioSys == semSYS) {
          df_tabDados <- f_carProd_tidy(mes_ano,"CRST",as.numeric(input$selectChn))  
          l_plots <- f_carProd_plot(df_tabDados) 
        }
        if(input$radioTpCred == simplificado & input$radioEntr == semEntrada & input$radioSys == comSYS) {
          df_tabDados <- f_carProd_tidy(mes_ano,"SYSS",as.numeric(input$selectChn))  
          l_plots <- f_carProd_plot(df_tabDados)
        }
        if(input$radioTpCred == simplificado & input$radioEntr == comEntrada & input$radioSys == comSYS) {
          df_tabDados <- f_carProd_tidy(mes_ano,"SYSC",as.numeric(input$selectChn))  
          l_plots <- f_carProd_plot(df_tabDados)
        }
        
        # grid 1: Taxas Médias
        grid.arrange(main = "PRODUÇÃO - CONTRATOS", 
                     #g_nContr,g_mtFin,g_przmed, g_tktmed, g_tac, g_seg, g_abrseg,
                     l_plots[[1]],l_plots[[2]],l_plots[[3]],l_plots[[4]],l_plots[[5]],l_plots[[6]],l_plots[[7]],
                     nrow=4, ncol=2,
                     as.table=TRUE,
                     heights=c(1,1))
    })
    output$distPlot2 = renderPlot({
      # selecionar pelo tipo MODPAY
      if(input$radioTpCred == normal & input$radioEntr == semEntrada & input$radioSys == semSYS) {
        df_tabDados <- f_carProd_tidy(mes_ano,"CREN",as.numeric(input$selectChn))
        l_plots <- f_carProd_plot(df_tabDados)
      }
      if(input$radioTpCred == normal & input$radioEntr == comEntrada & input$radioSys == semSYS) {
        df_tabDados <- f_carProd_tidy(mes_ano,"CRET",as.numeric(input$selectChn)) 
        l_plots <- f_carProd_plot(df_tabDados)
      }
      if(input$radioTpCred == normal & input$radioEntr == semEntrada & input$radioSys == comSYS) {
        df_tabDados <- f_carProd_tidy(mes_ano,"SYSN",as.numeric(input$selectChn))  
        l_plots <- f_carProd_plot(df_tabDados)
      }
      if(input$radioTpCred == normal & input$radioEntr == comEntrada & input$radioSys == comSYS) {
        df_tabDados <- f_carProd_tidy(mes_ano,"SYSE",as.numeric(input$selectChn))  
        l_plots <- f_carProd_plot(df_tabDados)
      }
      if(input$radioTpCred == simplificado & input$radioEntr == semEntrada & input$radioSys == semSYS) {
        df_tabDados <- f_carProd_tidy(mes_ano,"CRES",as.numeric(input$selectChn))  
        l_plots <- f_carProd_plot(df_tabDados)
      }
      if(input$radioTpCred == simplificado & input$radioEntr == comEntrada & input$radioSys == semSYS) {
        df_tabDados <- f_carProd_tidy(mes_ano,"CRST",as.numeric(input$selectChn))  
        l_plots <- f_carProd_plot(df_tabDados) 
      }
      if(input$radioTpCred == simplificado & input$radioEntr == semEntrada & input$radioSys == comSYS) {
        df_tabDados <- f_carProd_tidy(mes_ano,"SYSS",as.numeric(input$selectChn))  
        l_plots <- f_carProd_plot(df_tabDados)
      }
      if(input$radioTpCred == simplificado & input$radioEntr == comEntrada & input$radioSys == comSYS) {
        df_tabDados <- f_carProd_tidy(mes_ano,"SYSC",as.numeric(input$selectChn))  
        l_plots <- f_carProd_plot(df_tabDados)
      }
        # grid 1: Taxas Médias
        grid.arrange(main = "PRODUÇÃO - % DE TAXAS MÉDIAS POR MODO DE PAGAMENTO", 
                     #g_txmedcre,g_txmedclisys,g_txmedmodoa,g_txmedchq,g_txmeddcc,g_txmedcarne,g_txmedmodog,
                     l_plots[[8]],l_plots[[9]],l_plots[[10]],l_plots[[11]],l_plots[[12]],l_plots[[13]],l_plots[[14]],
                     nrow=3, ncol=2,
                     as.table=TRUE,
                     heights=c(1,1))
    })
    output$distPlot3 = renderPlot({
      # selecionar pelo tipo MODPAY
      if(input$radioTpCred == normal & input$radioEntr == semEntrada & input$radioSys == semSYS) {
        df_tabDados <- f_carProd_tidy(mes_ano,"CREN",as.numeric(input$selectChn))
        l_plots <- f_carProd_plot(df_tabDados)
      }
      if(input$radioTpCred == normal & input$radioEntr == comEntrada & input$radioSys == semSYS) {
        df_tabDados <- f_carProd_tidy(mes_ano,"CRET",as.numeric(input$selectChn)) 
        l_plots <- f_carProd_plot(df_tabDados)
      }
      if(input$radioTpCred == normal & input$radioEntr == semEntrada & input$radioSys == comSYS) {
        df_tabDados <- f_carProd_tidy(mes_ano,"SYSN",as.numeric(input$selectChn))  
        l_plots <- f_carProd_plot(df_tabDados)
      }
      if(input$radioTpCred == normal & input$radioEntr == comEntrada & input$radioSys == comSYS) {
        df_tabDados <- f_carProd_tidy(mes_ano,"SYSE",as.numeric(input$selectChn))  
        l_plots <- f_carProd_plot(df_tabDados)
      }
      if(input$radioTpCred == simplificado & input$radioEntr == semEntrada & input$radioSys == semSYS) {
        df_tabDados <- f_carProd_tidy(mes_ano,"CRES",as.numeric(input$selectChn))  
        l_plots <- f_carProd_plot(df_tabDados)
      }
      if(input$radioTpCred == simplificado & input$radioEntr == comEntrada & input$radioSys == semSYS) {
        df_tabDados <- f_carProd_tidy(mes_ano,"CRST",as.numeric(input$selectChn))  
        l_plots <- f_carProd_plot(df_tabDados) 
      }
      if(input$radioTpCred == simplificado & input$radioEntr == semEntrada & input$radioSys == comSYS) {
        df_tabDados <- f_carProd_tidy(mes_ano,"SYSS",as.numeric(input$selectChn))  
        l_plots <- f_carProd_plot(df_tabDados)
      }
      if(input$radioTpCred == simplificado & input$radioEntr == comEntrada & input$radioSys == comSYS) {
        df_tabDados <- f_carProd_tidy(mes_ano,"SYSC",as.numeric(input$selectChn))  
        l_plots <- f_carProd_plot(df_tabDados)
      }
    #    # grid 1: Taxas Médias
        grid.arrange(main = "PRODUÇÃO - % MONTANTE DE PRODUÇÃO POR MODO DE PAGAMENTO", 
                     #g_mtmodoa, g_mtchq,g_mtdcc,g_mtcarne,g_mtmodog,
                     l_plots[[15]],l_plots[[16]],l_plots[[17]],l_plots[[18]],l_plots[[19]],
                     nrow=3, ncol=2,
                     as.table=TRUE,
                     heights=c(1,1))
    })
})