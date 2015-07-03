########################
# ATENCAO: USAR ESTE FONTE PARA TODOS EXCETO A TOTAL CETELEM
# in_prodalp pode ser:
# "CREN", "CRET", "SYSN", "SYSE", "CRES", "CRST", "SYSS", "SYSC"
##########################################################
# MUDAR: ft_chn e ft_soci para ser carregado com retorno de combobox em ui.R
# mas deve ter amarração entre os dois. criar dataframe com SOCIEDADE e CHAINEORI, com indice 
# pego do selectbox shiny
f_carProd_tidy <- function(mm_aaaa,in_prodalp, in_indcombo)
{
    # lendo os dados do ano a partir da planilha gerada e gerando os .csv
    f_LeTDB(mm_aaaa)
    # criando vetor de sociedade
  v_soci <- rep("CTLM", 375)
  v_soci[1] <- "??L?"
  v_soci[28] <- "CONS"
  v_soci[257] <- "CONS"
  v_soci[317] <- "CONS"
  v_soci[332] <- "SUBA"
  v_soci[354] <- "TELN"
  # criando vetor de canais de origem
  v_chn <- 
    c(".......",
      "9124884",
      "9124892",
      "9105891",
      "9115734",
      "9123357",
      "9107533",
      "9122615",
      "9115049",
      "9123118",
      "9100249",
      "9126202",
      "9121336",
      "9126475",
      "9125741",
      "9101486",
      "9122458",
      "9101296",
      "9100199",
      "9101007",
      "9107525",
      "9125774",
      "9105875",
      "9105867",
      "9117060",
      "9122128",
      "9124371",
      "9122060",
      "9101080",
      "9100579",
      "9124033",
      "9102666",
      "9121856",
      "9118845",
      "9108002",
      "9119405",
      "9100363",
      "9101163",
      "9124694",
      "9119322",
      "9126392",
      "9113903",
      "9126400",
      "9121104",
      "9126020",
      "9121161",
      "9116385",
      "9122557",
      "9126152",
      "9116690",
      "9108689",
      "9118548",
      "9112475",
      "9113945",
      "9103003",
      "9121153",
      "9107582",
      "9109968",
      "9100256",
      "9126046",
      "9107111",
      "9123209",
      "9102708",
      "9105792",
      "9121807",
      "9125139",
      "9124025",
      "9123449",
      "9101494",
      "9108895",
      "9121617",
      "9101346",
      "9121799",
      "9106550",
      "9121732",
      "9111170",
      "9102583",
      "9106519",
      "9110321",
      "9108481",
      "9100397",
      "9111154",
      "9101171",
      "9117250",
      "9117243",
      "9119637",
      "9122417",
      "9120031",
      "9106139",
      "9122250",
      "9101833",
      "9101197",
      "9109299",
      "9119959",
      "9113580",
      "9122979",
      "9118340",
      "9106089",
      "9103763",
      "9123290",
      "9100793",
      "9117078",
      "9100975",
      "9108440",
      "9121831",
      "9102047",
      "9121013",
      "9105925",
      "9123068",
      "9119504",
      "9123621",
      "9121484",
      "9107301",
      "9107160",
      "9122227",
      "9123100",
      "9107947",
      "9100900",
      "9100413",
      "9123019",
      "9103615",
      "9105479",
      "9108317",
      "9105487",
      "9121948",
      "9109018",
      "9121435",
      "9100611",
      "9112657",
      "9125386",
      "9113671",
      "9117912",
      "9123472",
      "9116336",
      "9123159",
      "9100272",
      "9125345",
      "9126038",
      "9113135",
      "9122268",
      "9101312",
      "9122029",
      "9100280",
      "9122953",
      "9104886",
      "9100371",
      "9106931",
      "9105727",
      "9107004",
      "9119538",
      "9107012",
      "9107228",
      "9107632",
      "9106956",
      "9122136",
      "9114695",
      "9119330",
      "9101510",
      "9121658",
      "9124082",
      "9122987",
      "9124801",
      "9122797",
      "9103854",
      "9110057",
      "9119397",
      "9121427",
      "9122698",
      "9118050",
      "9113739",
      "9109984",
      "9108408",
      "9107350",
      "9106857",
      "9118068",
      "9105529",
      "9125667",
      "9100223",
      "9118217",
      "9100215",
      "9109067",
      "9100330",
      "9106618",
      "9122912",
      "9106246",
      "9122300",
      "9103425",
      "9106758",
      "9101361",
      "9119744",
      "9119686",
      "9107400",
      "9100967",
      "9102807",
      "9122276",
      "9101056",
      "9123480",
      "9126079",
      "9126111",
      "9126186",
      "9126103",
      "9126129",
      "9126145",
      "9126095",
      "9126087",
      "9126137",
      "9125451",
      "9121211",
      "9106030",
      "9100850",
      "9109257",
      "9111295",
      "9122755",
      "9103821",
      "9122748",
      "9121229",
      "9123365",
      "9123126",
      "9114083",
      "9110107",
      "9126160",
      "9110024",
      "9110610",
      "9125956",
      "9107251",
      "9121047",
      "9108812",
      "9106592",
      "9120353",
      "9102351",
      "9121385",
      "9121682",
      "9107624",
      "9100892",
      "9107046",
      "9113994",
      "9100561",
      "9124066",
      "9101403",
      "9111329",
      "9105743",
      "9108929",
      "9120056",
      "9117698",
      "9117862",
      "9121468",
      "9117755",
      "9114869",
      "9121666",
      "9119280",
      "9118910",
      "9101072",
      "9105842",
      "9100603",
      "9123787",
      "9125972",
      "9110388",
      "9108465",
      "9108879",
      "9121559",
      "9101288",
      "9100348",
      "9125907",
      "9100389",
      "9114745",
      "9125899",
      "9116500",
      "9107343",
      "9105420",
      "9105909",
      "9104126",
      "9122466",
      "9106402",
      "9121443",
      "9107921",
      "9108770",
      "9118076",
      "9118704",
      "9107939",
      "9101205",
      "9102328",
      "9119314",
      "9101775",
      "9103839",
      "9106329",
      "9122086",
      "9100785",
      "9103771",
      "9122342",
      "9126293",
      "9126350",
      "9126301",
      "9126251",
      "9126244",
      "9126236",
      "9126277",
      "9126269",
      "9126285",
      "9126368",
      "9126335",
      "9126327",
      "9126228",
      "9126343",
      "9101049",
      "9101882",
      "9101270",
      "9119967",
      "9105990",
      "9123084",
      "9100264",
      "9106022",
      "9113960",
      "9105735",
      "9108473",
      "9122581",
      "0",
      "9105818",
      "9101320",
      "9105883",
      "9100306",
      "9101064",
      "9102237",
      "9126467",
      "9109414",
      "9119678",
      "9119769",
      "9100405",
      "9122037",
      "9124314",
      "9104480",
      "9101247",
      "9106782",
      "9100736",
      "9108267",
      "9103565",
      "9108622",
      "9125063",
      "9125642",
      "9113655",
      "9123324",
      "9125394",
      "9125733",
      "9113846",
      "9125725",
      "9125790",
      "9100835",
      "9125865",
      "9118688",
      "9102914",
      "9107285",
      "9999999",
      "9102674",
      "9122649",
      "9106220",
      "9125782",
      "9124702",
      "9104795",
      "9102369",
      "9103375",
      "9106105",
      "9125923",
      "9123811",
      "9126210",
      "9103789",
      "9118043",
      "9119421",
      "9108341",
      "9124710",
      "9108150",
      "9122201",
      "9125766",
      "9103797",
      "9119173",
      "9105859",
      "9124512")
  
  ft_chn <- v_chn[in_indcombo]
  ft_soci <- v_soci[in_indcombo]
  # aqui receber o indice do combo e criar vetor com os canais e sociedades associados.
  # obter os textos aqui e nao precisa mudar nada depois
  # Criação de aba de TIDY DATA a partir da leitura dos arquivos raw necessários
# localizados na aba padrão "rawdata"do projeto CETELEM
# seguindo a formatação na planilha CodeBook-REL01-V1.0.xls
    # seta o diretorio de trabalho
    # setwd("/Users/jcassiojr/Documents/MyGit/MIS/CETELEM/RelCTL01/R")
    
    
    # libraries
    #if(!require(openxlsx)){install.packages("openxlsx")}
    #if(!require(dplyr)){install.packages("dplyr")}

# constants

# realiza execução especial para Tabela TOTAL CREDIÁRIO E SYS, recebendo in_prodalp = "TOTAL"
if (in_prodalp == "TCTL") {
    in_prodalp <- "TCTL"
}
# fileout <- paste0("../CTL01dashb/tidydata/dshRelCTL01-CarProd-", in_prodalp,".xlsx")
# idem
#filein1 <- "../CTL01dashb/rawdata/rawCarprod.csv"
filein1 <- paste0("./tidydata/", mm_aaaa,"/tidyCarprod.csv")


######################################################################################
# reading all raw data csv files needed to create the excel report or graph indicator
######################################################################################
df_carprod_raw <- read.csv(filein1, sep = ";")

######################################################################################
# cleaning and manipulating the raw data
######################################################################################
# INSERIR CHECAGEM DE LAYOUT: colunas presentes e com nome certo, etc.

# forcing uppercase names
names(df_carprod_raw) <- toupper(names(df_carprod_raw))

######################################################################################
# filtering data
######################################################################################
# Filtro
# S: SOCIEDADE, C: CHAINEORI, P:PRODALP, M:MODPAY, TIPOTNC
# mapa da chave: SSSSCCCCCCCPPPPMT

#####################################################################################
# AGRUPAMENTO 1 de TOTAL CREDIARIO e SYS: Para toda a Cetelem
# Linhas da planilha original tratadas:
# Número de contratos, Montante financiado, Prazo médio em meses, Ticket Médio, 
# TAC média por contrato, % produção com seguro, aberturas com seguro (em números)
#####################################################################################
#ft_soci = "..L." # CETELEM
#ft_chn = "......."
if (in_prodalp == "TCTL") {
    in_prodalp <- "...."
}
#in_prodalp = "CREN" # CREDIARIO
ft_mpay = "."
ft_tptnc = "."

#f_chkVazia(df_carprod_raw,)

df_aux <- filter(df_carprod_raw,grepl(ft_soci,df_carprod_raw$SOCIEDADE), 
                 grepl(ft_chn,df_carprod_raw$CHAINEORI),
                 grepl(in_prodalp,df_carprod_raw$PRODALP), 
                 grepl(ft_mpay,df_carprod_raw$MODPAY), 
                 grepl(ft_tptnc,df_carprod_raw$TIPOTNC))

if (nrow(df_aux)) {
    df_filtro1 <-
        df_carprod_raw %>%
        filter(grepl(ft_soci,df_carprod_raw$SOCIEDADE), # Filtro por SOCIEDADE
               grepl(ft_chn,df_carprod_raw$CHAINEORI), # Filtro por CHAINEORI
               grepl(in_prodalp,df_carprod_raw$PRODALP), # Filtro por PRODALP
               grepl(ft_mpay,df_carprod_raw$MODPAY),  # Filtro por MODPAY
               grepl(ft_tptnc,df_carprod_raw$TIPOTNC)) %>% # Filtro por TIPOTNC
        select(DATAPROC, NBFI, MTFI, DURFIN, TAC, SEGURO) %>%  # seleciona somente colunas necessárias
        mutate(MTFI = MTFI / 100000) %>%  # acerta valor monetário de Montante
        mutate(DURFIN = DURFIN / 100000) %>%  # acerta valor monetário de Duração Finaciamento (???)
        group_by(DATAPROC) %>%  # agrupa por coluna DATAPROC           
        summarise(NBFI = sum(NBFI), # Número de contratos
                  MTFI = round(sum(MTFI),2), # Montante financiado
                  DURFIN = round(sum(DURFIN)/MTFI,1), # Prazo médio em meses
                  TKTMED = round(sum(MTFI)/sum(NBFI)*1000,2), # Ticket Médio
                  TAC = round(sum(TAC)/sum(NBFI)/100,2),  # TAC média por contrato
                  SEGURO = round(sum(SEGURO)/sum(NBFI)*100,2), # % produção com seguro
                  ABRSEG = NBFI*SEGURO/100)  # aberturas com seguro (em números)
} else { # caso seleção seja vazia, gera tabela com valores 0
    df_filtro1 <-
        df_carprod_raw %>%
        select(DATAPROC) %>%  
        group_by(DATAPROC) %>%  
        summarise(NBFI = 0,MTFI = 0, DURFIN = 0, TKTMED = 0, TAC = 0, SEGURO = 0, ABRSEG = 0)    
}

#####################################################################################
# AGRUPAMENTO 2 por TAXAS MÉDIAS PRODALP: (crediário), S (SYS) e diversos MODPAY
# Linhas da planilha original tratadas:
# Taxa Média Crediário, Taxa Média cliente SYS, taxa média no MODO PGTO A,
# taxa média no cheque (MODOPAY B ou D), taxa média no DCC (MODOPAY I ou P),
# taxa média no carnê (MODOPAY M ou N), taxa média no MODO PGTO G
#####################################################################################
# Taxa Média Crediário
#ft_soci = "..L." # CETELEM
#ft_chn = "......."
if (in_prodalp == "TCTL") {
    in_prodalp <- "C..."
}
#in_prodalp = "CREN" # CREDIARIO
ft_mpay = "."
ft_tptnc = "."
df_aux <- filter(df_carprod_raw,grepl(ft_soci,df_carprod_raw$SOCIEDADE), 
                 grepl(ft_chn,df_carprod_raw$CHAINEORI),
                 grepl(in_prodalp,df_carprod_raw$PRODALP), 
                 grepl(ft_mpay,df_carprod_raw$MODPAY), 
                 grepl(ft_tptnc,df_carprod_raw$TIPOTNC))
if (nrow(df_aux)) {
    df_filtro2 <-
        df_carprod_raw %>%
        filter(grepl(ft_soci,df_carprod_raw$SOCIEDADE), # Filtro por SOCIEDADE
               grepl(ft_chn,df_carprod_raw$CHAINEORI), # Filtro por CHAINEORI
               grepl(in_prodalp,df_carprod_raw$PRODALP), # Filtro por PRODALP
               grepl(ft_mpay,df_carprod_raw$MODPAY),  # Filtro por MODPAY
               grepl(ft_tptnc,df_carprod_raw$TIPOTNC)) %>% # Filtro por TIPOTNC
        select(DATAPROC, DURFIN, TXCLM, TXVDRFINAL) %>%  # seleciona somente colunas necessárias
        #mutate(DURFIN = DURFIN / 100000) %>%  # acerta valor monetário de Duração Finaciamento (???)
        group_by(DATAPROC) %>%  # agrupa por coluna DATAPROC           
        summarise(TXMEDCRED = round(((sum(TXCLM)+sum(TXVDRFINAL))/sum(DURFIN)/1000000),2))  # sumariza por taxa média crediário
} else { # caso seleção seja vazia, gera tabela com valores NA
    df_filtro2 <-
        df_carprod_raw %>%
        select(DATAPROC) %>%  
        group_by(DATAPROC) %>%  
        summarise(TXMEDCRED = 0)    
}

# Taxa Média cliente SYS
# para CREN: forçar NA
#ft_soci = "..L." # CETELEM
#ft_chn = "......."
if (in_prodalp == "TCTL") {
    in_prodalp <- "S..."
}
#in_prodalp = "CREN" 
ft_mpay = "."
ft_tptnc = "."
df_aux <- filter(df_carprod_raw,grepl(ft_soci,df_carprod_raw$SOCIEDADE), 
                 grepl(ft_chn,df_carprod_raw$CHAINEORI),
                 grepl(in_prodalp,df_carprod_raw$PRODALP), 
                 grepl(ft_mpay,df_carprod_raw$MODPAY), 
                 grepl(ft_tptnc,df_carprod_raw$TIPOTNC))
SOMENTE_CETELEM <- 0 # por enquanto/ para não fazer neste caso de teste para CREN
if (nrow(df_aux) & SOMENTE_CETELEM) { # obs: se proalp inicia com S
    df_filtro3 <-
        df_carprod_raw %>%
        filter(grepl(ft_soci,df_carprod_raw$SOCIEDADE), # Filtro por SOCIEDADE
               grepl(ft_chn,df_carprod_raw$CHAINEORI), # Filtro por CHAINEORI
               grepl(in_prodalp,df_carprod_raw$PRODALP), # Filtro por PRODALP
               grepl(ft_mpay,df_carprod_raw$MODPAY),  # Filtro por MODPAY
               grepl(ft_tptnc,df_carprod_raw$TIPOTNC)) %>% # Filtro por TIPOTNC
        select(DATAPROC, DURFIN, TXCLM) %>%  # seleciona somente colunas necessárias
        group_by(DATAPROC) %>%  # agrupa por coluna DATAPROC           
        summarise(TXMEDCLISYS = round(sum(TXCLM)/sum(DURFIN)/1000000,2))  # sumariza por taxa média cliente SYS
} else { # caso seleção seja vazia, gera tabela com valores NA
    df_filtro3 <-
        df_carprod_raw %>%
        select(DATAPROC) %>%  
        group_by(DATAPROC) %>%  
        summarise(TXMEDCLISYS = 0)    
}

# taxa média no MODO PGTO A
#ft_soci = "..L." # CETELEM
#ft_chn = "......."
if (in_prodalp == "TCTL") {
    in_prodalp <- "C..."
}
#in_prodalp = "CREN" # SYS
ft_mpay = "A"
ft_tptnc = "."
df_aux <- filter(df_carprod_raw,grepl(ft_soci,df_carprod_raw$SOCIEDADE), 
                 grepl(ft_chn,df_carprod_raw$CHAINEORI),
                 grepl(in_prodalp,df_carprod_raw$PRODALP), 
                 grepl(ft_mpay,df_carprod_raw$MODPAY), 
                 grepl(ft_tptnc,df_carprod_raw$TIPOTNC))
if (nrow(df_aux)) {
    df_filtro4 <-
        df_carprod_raw %>%
        filter(grepl(ft_soci,df_carprod_raw$SOCIEDADE), # Filtro por SOCIEDADE
               grepl(ft_chn,df_carprod_raw$CHAINEORI), # Filtro por CHAINEORI
               grepl(in_prodalp,df_carprod_raw$PRODALP), # Filtro por PRODALP
               grepl(ft_mpay,df_carprod_raw$MODPAY),  # Filtro por MODPAY
               grepl(ft_tptnc,df_carprod_raw$TIPOTNC)) %>% # Filtro por TIPOTNC
        select(DATAPROC, DURFIN, TXCLM, TXVDRFINAL) %>%  # seleciona somente colunas necessárias
        group_by(DATAPROC) %>%  # agrupa por coluna DATAPROC           
        summarise(TXMEDMODOA = round((sum(TXCLM)+sum(TXVDRFINAL))/sum(DURFIN)/1000000,2))  # sumariza por taxa media no modo pagto A
} else { # caso seleção seja vazia, gera tabela com valores NA
    df_filtro4 <-
        df_carprod_raw %>%
        select(DATAPROC) %>%  
        group_by(DATAPROC) %>%  
        summarise(TXMEDMODOA = 0)    
}

# taxa média no cheque (MODOPAY B ou D)
#ft_soci = "..L." # CETELEM
#ft_chn = "......."
#in_prodalp = "CREN" # SYS
if (in_prodalp == "TCTL") {
    in_prodalp <- "C..."
}
ft_mpay = "[B|D]"
ft_tptnc = "."
df_aux <- filter(df_carprod_raw,grepl(ft_soci,df_carprod_raw$SOCIEDADE), 
                 grepl(ft_chn,df_carprod_raw$CHAINEORI),
                 grepl(in_prodalp,df_carprod_raw$PRODALP), 
                 grepl(ft_mpay,df_carprod_raw$MODPAY), 
                 grepl(ft_tptnc,df_carprod_raw$TIPOTNC))
if (nrow(df_aux)) {
    df_filtro5 <-
        df_carprod_raw %>%
        filter(grepl(ft_soci,df_carprod_raw$SOCIEDADE), # Filtro por SOCIEDADE
               grepl(ft_chn,df_carprod_raw$CHAINEORI), # Filtro por CHAINEORI
               grepl(in_prodalp,df_carprod_raw$PRODALP), # Filtro por PRODALP
               grepl(ft_mpay,df_carprod_raw$MODPAY),  # Filtro por MODPAY
               grepl(ft_tptnc,df_carprod_raw$TIPOTNC)) %>% # Filtro por TIPOTNC
        select(DATAPROC, DURFIN, TXCLM, TXVDRFINAL) %>%  
        group_by(DATAPROC) %>%  # agrupa por coluna DATAPROC           
        summarise(TXMEDCHQ = round((sum(TXCLM)+sum(TXVDRFINAL))/sum(DURFIN)/1000000,2))  # Sumariza por taxa média no cheque
} else { # caso seleção seja vazia, gera tabela com valores NA
    df_filtro5 <-
        df_carprod_raw %>%
        select(DATAPROC) %>%  
        group_by(DATAPROC) %>%  
        summarise(TXMEDCHQ = 0)    
}

# taxa média no DCC (MODOPAY I ou P)
#ft_soci = "..L." # CETELEM
#ft_chn = "......."
#in_prodalp = "CREN" # SYS
if (in_prodalp == "TCTL") {
    in_prodalp <- "C..."
}
ft_mpay = "[I|P]"
ft_tptnc = "."
df_aux <- filter(df_carprod_raw,grepl(ft_soci,df_carprod_raw$SOCIEDADE), 
                 grepl(ft_chn,df_carprod_raw$CHAINEORI),
                 grepl(in_prodalp,df_carprod_raw$PRODALP), 
                 grepl(ft_mpay,df_carprod_raw$MODPAY), 
                 grepl(ft_tptnc,df_carprod_raw$TIPOTNC))
if (nrow(df_aux)) {
    df_filtro6 <-
        df_carprod_raw %>%
        filter(grepl(ft_soci,df_carprod_raw$SOCIEDADE), # Filtro por SOCIEDADE
               grepl(ft_chn,df_carprod_raw$CHAINEORI), # Filtro por CHAINEORI
               grepl(in_prodalp,df_carprod_raw$PRODALP), # Filtro por PRODALP
               grepl(ft_mpay,df_carprod_raw$MODPAY),  # Filtro por MODPAY
               grepl(ft_tptnc,df_carprod_raw$TIPOTNC)) %>% # Filtro por TIPOTNC
        select(DATAPROC, DURFIN, TXCLM, TXVDRFINAL) %>%  # seleciona somente colunas necessárias
        group_by(DATAPROC) %>%  # agrupa por coluna DATAPROC           
        summarise(TXMEDDCC = round((sum(TXCLM)+sum(TXVDRFINAL))/sum(DURFIN)/1000000,2))  # Sumariza por taxa media no DCC
} else { # caso seleção seja vazia, gera tabela com valores NA
    df_filtro6 <-
        df_carprod_raw %>%
        select(DATAPROC) %>%  
        group_by(DATAPROC) %>%  
        summarise(TXMEDDCC = 0)    
}

# taxa média no carnê (MODOPAY M ou N)
#ft_soci = "..L." # CETELEM
#ft_chn = "......."
#in_prodalp = "CREN" # SYS
if (in_prodalp == "TCTL") {
    in_prodalp <- "C..."
}
ft_mpay = "[M|N]"
ft_tptnc = "."
df_aux <- filter(df_carprod_raw,grepl(ft_soci,df_carprod_raw$SOCIEDADE), 
                 grepl(ft_chn,df_carprod_raw$CHAINEORI),
                 grepl(in_prodalp,df_carprod_raw$PRODALP), 
                 grepl(ft_mpay,df_carprod_raw$MODPAY), 
                 grepl(ft_tptnc,df_carprod_raw$TIPOTNC))
if (nrow(df_aux)) {
    df_filtro7 <-
        df_carprod_raw %>%
        filter(grepl(ft_soci,df_carprod_raw$SOCIEDADE), # Filtro por SOCIEDADE
               grepl(ft_chn,df_carprod_raw$CHAINEORI), # Filtro por CHAINEORI
               grepl(in_prodalp,df_carprod_raw$PRODALP), # Filtro por PRODALP
               grepl(ft_mpay,df_carprod_raw$MODPAY),  # Filtro por MODPAY
               grepl(ft_tptnc,df_carprod_raw$TIPOTNC)) %>% # Filtro por TIPOTNC
        select(DATAPROC, DURFIN, TXCLM, TXVDRFINAL) %>%  # seleciona somente colunas necessárias
        group_by(DATAPROC) %>%  # agrupa por coluna DATAPROC           
        summarise(TXMEDCARNE = round((sum(TXCLM)+sum(TXVDRFINAL))/sum(DURFIN)/1000000,2))  # Sumariza por taxa media no carnê
} else { # caso seleção seja vazia, gera tabela com valores NA
    df_filtro7 <-
        df_carprod_raw %>%
        select(DATAPROC) %>%  
        group_by(DATAPROC) %>%  
        summarise(TXMEDCARNE = 0)    
}

# taxa média no MODO PGTO G
#ft_soci = "..L." # CETELEM
#ft_chn = "......."
#in_prodalp = "CREN" # SYS
if (in_prodalp == "TCTL") {
    in_prodalp <- "C..."
}
ft_mpay = "G"
ft_tptnc = "."
df_aux <- filter(df_carprod_raw,grepl(ft_soci,df_carprod_raw$SOCIEDADE), 
                 grepl(ft_chn,df_carprod_raw$CHAINEORI),
                 grepl(in_prodalp,df_carprod_raw$PRODALP), 
                 grepl(ft_mpay,df_carprod_raw$MODPAY), 
                 grepl(ft_tptnc,df_carprod_raw$TIPOTNC))
if (nrow(df_aux)) {
    df_filtro8 <-
        df_carprod_raw %>%
        filter(grepl(ft_soci,df_carprod_raw$SOCIEDADE), # Filtro por SOCIEDADE
               grepl(ft_chn,df_carprod_raw$CHAINEORI), # Filtro por CHAINEORI
               grepl(in_prodalp,df_carprod_raw$PRODALP), # Filtro por PRODALP
               grepl(ft_mpay,df_carprod_raw$MODPAY),  # Filtro por MODPAY
               grepl(ft_tptnc,df_carprod_raw$TIPOTNC)) %>% # Filtro por TIPOTNC
        select(DATAPROC, DURFIN, TXCLM, TXVDRFINAL) %>%  # seleciona somente colunas necessárias
        group_by(DATAPROC) %>%  # agrupa por coluna DATAPROC           
        summarise(TXMEDMODOG = round((sum(TXCLM)+sum(TXVDRFINAL))/sum(DURFIN)/1000000,2))  # Sumariza por taxa media no MODO G
} else { # caso seleção seja vazia, gera tabela com valores NA
    df_filtro8 <-
        df_carprod_raw %>%
        select(DATAPROC) %>%  
        group_by(DATAPROC) %>%  
        summarise(TXMEDMODOG = 0)    
}

#####################################################################################
# AGRUPAMENTO 3 por MONTANTE DE PRODUÇÃO,  diversos MODPAY
# Linhas da planilha original tratadas:
# % producao montante em cheque  (TODA CELETELM, MODPAY B ou D),
#
#####################################################################################
# % producao montante no MODO PGTO A  (toda a CETELEM)
#ft_soci = "..L." # CETELEM
#ft_chn = "......."
#in_prodalp = "CREN" 
if (in_prodalp == "TCTL") {
    in_prodalp <- "...."
}
ft_mpay = "A"
ft_tptnc = "."
df_aux <- filter(df_carprod_raw,grepl(ft_soci,df_carprod_raw$SOCIEDADE), 
                 grepl(ft_chn,df_carprod_raw$CHAINEORI),
                 grepl(in_prodalp,df_carprod_raw$PRODALP), 
                 grepl(ft_mpay,df_carprod_raw$MODPAY), 
                 grepl(ft_tptnc,df_carprod_raw$TIPOTNC))
if (nrow(df_aux)) {
    # passo1: criar dataframe somente com MTFI para MODPAY B ou D
    df_filtro_aux <-
        df_carprod_raw %>%
        filter(grepl(ft_soci,df_carprod_raw$SOCIEDADE), # Filtro por SOCIEDADE
               grepl(ft_chn,df_carprod_raw$CHAINEORI), # Filtro por CHAINEORI
               grepl(in_prodalp,df_carprod_raw$PRODALP), # Filtro por PRODALP
               grepl(ft_mpay,df_carprod_raw$MODPAY),  # Filtro por MODPAY
               grepl(ft_tptnc,df_carprod_raw$TIPOTNC)) %>% # Filtro por TIPOTNC
        select(DATAPROC, MTFI) %>%  # seleciona somente colunas necessárias
        group_by(DATAPROC) %>%  # agrupa por coluna DATAPROC           
        summarise(MTFI_modoA = sum(MTFI))  # Sumariza por DATAPROC                  
        # passo2: merge via coluna DATAPROC das duas tabelas
        df_aux2 <- merge(df_filtro1,df_filtro_aux, by = "DATAPROC", all = TRUE)
        # passo3: sumarizar na formula
        df_filtro9 <-
            df_aux2 %>%
            select(DATAPROC, MTFI, MTFI_modoA) %>%  # seleciona somente colunas necessárias
            mutate(MTFI_MODOA = round(MTFI_modoA/MTFI/1000,2)) %>% # Sumariza por producao montante pg tipo A
            select(DATAPROC, MTFI_MODOA) # seleciona somente colunas necessárias
} else { # caso seleção seja vazia, gera tabela com valores NA
    df_filtro9 <-
        df_carprod_raw %>%
        select(DATAPROC) %>%  
        group_by(DATAPROC) %>%  
        summarise(MTFI_MODOA = 0)    
}

# % producao montante em cheque  (TODA CELETELM, MODPAY B ou D)
#ft_soci = "..L." # CETELEM
#ft_chn = "......."
#in_prodalp = "CREN" 
if (in_prodalp == "TCTL") {
    in_prodalp <- "...."
}
ft_mpay = "[B|D]"
ft_tptnc = "."
df_aux <- filter(df_carprod_raw,grepl(ft_soci,df_carprod_raw$SOCIEDADE), 
                 grepl(ft_chn,df_carprod_raw$CHAINEORI),
                 grepl(in_prodalp,df_carprod_raw$PRODALP), 
                 grepl(ft_mpay,df_carprod_raw$MODPAY), 
                 grepl(ft_tptnc,df_carprod_raw$TIPOTNC))

if (nrow(df_aux)) {
    # passo1: criar dataframe somente com MTFI para MODPAY B ou D
    df_filtro_aux <-
        df_carprod_raw %>%
        filter(grepl(ft_soci,df_carprod_raw$SOCIEDADE), # Filtro por SOCIEDADE
               grepl(ft_chn,df_carprod_raw$CHAINEORI), # Filtro por CHAINEORI
               grepl(in_prodalp,df_carprod_raw$PRODALP), # Filtro por PRODALP
               grepl(ft_mpay,df_carprod_raw$MODPAY),  # Filtro por MODPAY
               grepl(ft_tptnc,df_carprod_raw$TIPOTNC)) %>% # Filtro por TIPOTNC
        select(DATAPROC, MTFI) %>%  # seleciona somente colunas necessárias
        group_by(DATAPROC) %>%  # agrupa por coluna DATAPROC           
        summarise(MTFI_chq = sum(MTFI))  # Sumariza por producao montante cheque              
    # passo2: merge via coluna DATAPROC da tabela acima com a para cetelem total (df_filtro1)
    df_aux2 <- merge(df_filtro1,df_filtro_aux, by = "DATAPROC", all = TRUE)
    # passo3: sumarizar na formula
    df_filtro10 <-
        df_aux2 %>%
        select(DATAPROC,MTFI, MTFI_chq) %>%  # seleciona somente colunas necessárias
        mutate(MTFI_CHQ = round(MTFI_chq/MTFI/1000,2)) %>% # Sumariza por DATAPROC 
        select(DATAPROC, MTFI_CHQ)
} else { # caso seleção seja vazia, gera tabela com valores NA
    df_filtro10 <-
        df_carprod_raw %>%
        select(DATAPROC) %>%  
        group_by(DATAPROC) %>%  
        summarise(MTFI_CHQ = 0)    
}

# % producao montante no DCC  (TODA CELETELM, MODPAY I ou P)
# passo1: criar dataframe somente com MTFI para MODPAY I ou P
#ft_soci = "..L." # CETELEM
#ft_chn = "......."
#in_prodalp = "CREN"
if (in_prodalp == "TCTL") {
    in_prodalp <- "...."
}
ft_mpay = "[I|P]"
ft_tptnc = "."
df_aux <- filter(df_carprod_raw,grepl(ft_soci,df_carprod_raw$SOCIEDADE), 
                 grepl(ft_chn,df_carprod_raw$CHAINEORI),
                 grepl(in_prodalp,df_carprod_raw$PRODALP), 
                 grepl(ft_mpay,df_carprod_raw$MODPAY), 
                 grepl(ft_tptnc,df_carprod_raw$TIPOTNC))
if (nrow(df_aux)) {
    # passo1: criar dataframe somente com MTFI para MODPAY B ou D
    df_filtro_aux <-
        df_carprod_raw %>%
        filter(grepl(ft_soci,df_carprod_raw$SOCIEDADE), # Filtro por SOCIEDADE
               grepl(ft_chn,df_carprod_raw$CHAINEORI), # Filtro por CHAINEORI
               grepl(in_prodalp,df_carprod_raw$PRODALP), # Filtro por PRODALP
               grepl(ft_mpay,df_carprod_raw$MODPAY),  # Filtro por MODPAY
               grepl(ft_tptnc,df_carprod_raw$TIPOTNC)) %>% # Filtro por TIPOTNC
        select(DATAPROC, MTFI) %>%  # seleciona somente colunas necessárias
        group_by(DATAPROC) %>%  # agrupa por coluna DATAPROC           
        summarise(MTFI_DCC = sum(MTFI))  # Sumariza por DATAPROC              
    
    # passo2: merge via coluna DATAPROC das duas tabelas
    df_aux2 <- merge(df_filtro1,df_filtro_aux, by = "DATAPROC", all = TRUE)
    # passo3: sumarizar na formula
    df_filtro11 <-
        df_aux2 %>%
        select(DATAPROC,MTFI, MTFI_DCC) %>%  # seleciona somente colunas necessárias
        mutate(MTFI_DCC = round(MTFI_DCC/MTFI/1000,2)) %>% # Sumariza por producao montante no DCC
        select(DATAPROC, MTFI_DCC)
} else { # caso seleção seja vazia, gera tabela com valores NA
    df_filtro11 <-
        df_carprod_raw %>%
        select(DATAPROC) %>%  
        group_by(DATAPROC) %>%  
        summarise(MTFI_DCC = 0)    
}

# % producao montante no carne  (TODA CELETELM, MODPAY M ou N)
# passo1: criar dataframe somente com MTFI para MODPAY M ou N
#ft_soci = "..L." # CETELEM
#ft_chn = "......."
#in_prodalp = "CREN" 
if (in_prodalp == "TCTL") {
    in_prodalp <- "...."
}
ft_mpay = "[M|N]"
ft_tptnc = "."
df_aux <- filter(df_carprod_raw,grepl(ft_soci,df_carprod_raw$SOCIEDADE), 
                 grepl(ft_chn,df_carprod_raw$CHAINEORI),
                 grepl(in_prodalp,df_carprod_raw$PRODALP), 
                 grepl(ft_mpay,df_carprod_raw$MODPAY), 
                 grepl(ft_tptnc,df_carprod_raw$TIPOTNC))
if (nrow(df_aux)) {
    # passo1: criar dataframe somente com MTFI para MODPAY B ou D
    df_filtro_aux <-
        df_carprod_raw %>%
        filter(grepl(ft_soci,df_carprod_raw$SOCIEDADE), # Filtro por SOCIEDADE
               grepl(ft_chn,df_carprod_raw$CHAINEORI), # Filtro por CHAINEORI
               grepl(in_prodalp,df_carprod_raw$PRODALP), # Filtro por PRODALP
               grepl(ft_mpay,df_carprod_raw$MODPAY),  # Filtro por MODPAY
               grepl(ft_tptnc,df_carprod_raw$TIPOTNC)) %>% # Filtro por TIPOTNC
        select(DATAPROC, MTFI) %>%  # seleciona somente colunas necessárias
        group_by(DATAPROC) %>%  # agrupa por coluna DATAPROC           
        summarise(MTFI_carne = sum(MTFI))  # Sumariza por DATAPROC              
    
    # passo2: merge via coluna DATAPROC das duas tabelas
    df_aux2 <- merge(df_filtro1,df_filtro_aux, by = "DATAPROC", all = TRUE)
    # passo3: sumarizar na formula
    df_filtro12 <-
        df_aux2 %>%
        select(DATAPROC,MTFI, MTFI_carne) %>%  # seleciona somente colunas necessárias
        mutate(MTFI_CARNE = round(MTFI_carne/MTFI/1000,2)) %>% # producao montante no carne
        select(DATAPROC, MTFI_CARNE)
} else { # caso seleção seja vazia, gera tabela com valores NA
    df_filtro12 <-
        df_carprod_raw %>%
        select(DATAPROC) %>%  
        group_by(DATAPROC) %>%  
        summarise(MTFI_CARNE = 0)    
}

# % producao montante no MODO PGTO G  (toda a CETELEM)
#ft_soci = "..L." # CETELEM
#ft_chn = "......."
#in_prodalp = "CREN" 
if (in_prodalp == "TCTL") {
    in_prodalp <- "...."
}
ft_mpay = "G"
ft_tptnc = "."
df_aux <- filter(df_carprod_raw,grepl(ft_soci,df_carprod_raw$SOCIEDADE), 
                 grepl(ft_chn,df_carprod_raw$CHAINEORI),
                 grepl(in_prodalp,df_carprod_raw$PRODALP), 
                 grepl(ft_mpay,df_carprod_raw$MODPAY), 
                 grepl(ft_tptnc,df_carprod_raw$TIPOTNC))
if (nrow(df_aux)) {
    # passo1: criar dataframe somente com MTFI para MODPAY B ou D
    df_filtro_aux <-
        df_carprod_raw %>%
        filter(grepl(ft_soci,df_carprod_raw$SOCIEDADE), # Filtro por SOCIEDADE
               grepl(ft_chn,df_carprod_raw$CHAINEORI), # Filtro por CHAINEORI
               grepl(in_prodalp,df_carprod_raw$PRODALP), # Filtro por PRODALP
               grepl(ft_mpay,df_carprod_raw$MODPAY),  # Filtro por MODPAY
               grepl(ft_tptnc,df_carprod_raw$TIPOTNC)) %>% # Filtro por TIPOTNC
        select(DATAPROC, MTFI) %>%  # seleciona somente colunas necessárias
        group_by(DATAPROC) %>%  # agrupa por coluna DATAPROC              
        summarise(MTFI_modoG = sum(MTFI))  # Sumariza por DATAPROC              
    
    # passo2: merge via coluna DATAPROC das duas tabelas
    df_aux2 <- merge(df_filtro1,df_filtro_aux, by = "DATAPROC", all = TRUE)
    # passo3: sumarizar na formula
    df_filtro13 <-
        df_aux2 %>%
        select(DATAPROC,MTFI, MTFI_modoG) %>%  # seleciona somente colunas necessárias
        mutate(MTFI_MODOG = round(MTFI_modoG/MTFI/1000,2)) %>% # Sumariza por DATAPROC
        select(DATAPROC, MTFI_MODOG)
} else { # caso seleção seja vazia, gera tabela com valores NA
    df_filtro13 <-
        df_carprod_raw %>%
        select(DATAPROC) %>%  
        group_by(DATAPROC) %>%  
        summarise(MTFI_MODOG = 0)    
}

##### FALTA ENTENDER O QUE A COLUNA B DE aba <mês> É!!!!!!! 
# seleciona somente linhas de acordo com filtro original (somente CETELEM, PRODALP iniciando com "S")
# Prazo médio em meses
#df_carprod3_credsys <-
#    df_carprod %>%
#    filter(grepl("^..L........S.....",df_carprod$chave)) %>% # filtra para toda a CETELEM
#    select(DATAPROC, DURFIN, TXCLM, TXVDRFINAL) %>%  # seleciona somente colunas necessárias
#    #mutate(DURFIN = DURFIN / 100000) %>%  # acerta valor monetário de Duração Finaciamento (???)
#    group_by(DATAPROC) %>%  # agrupa por coluna DATAPROC           
#    summarise(txMedCred = ((sum(TXCLM)+sum(TXVDRFINAL))/sum(DURFIN)/1000000))  # Sumariza por DATAPROC

#########################
# creating and writing in xls the complete tidy data
########################
# OBS: depois usar lista de data.frames ao invés de data.frames isolados
# merging os dataframes para gerar um único arquivo tidy
df_aux1 <- merge(df_filtro1,df_filtro2, by = "DATAPROC", all = TRUE)
df_aux2 <- merge(df_aux1,df_filtro3, by = "DATAPROC", all = TRUE)
df_aux3 <- merge(df_aux2,df_filtro4, by = "DATAPROC", all = TRUE)
df_aux4 <- merge(df_aux3,df_filtro5, by = "DATAPROC", all = TRUE)
df_aux5 <- merge(df_aux4,df_filtro6, by = "DATAPROC", all = TRUE)
df_aux6 <- merge(df_aux5,df_filtro7, by = "DATAPROC", all = TRUE)
df_aux7 <- merge(df_aux6,df_filtro8, by = "DATAPROC", all = TRUE)
df_aux8 <- merge(df_aux7,df_filtro9, by = "DATAPROC", all = TRUE)
df_aux9 <- merge(df_aux8,df_filtro10, by = "DATAPROC", all = TRUE)
df_aux10 <- merge(df_aux9,df_filtro11, by = "DATAPROC", all = TRUE)
df_aux11 <- merge(df_aux10,df_filtro12, by = "DATAPROC", all = TRUE)
df_carProd <- merge(df_aux11,df_filtro13, by = "DATAPROC", all = TRUE)

######################################################################################
# saving for import into CLIC-DATA and Excel reports (INIBIDO: GRAVACAO DOS ARQUIVOS NO CHAMADOR)
######################################################################################
# wrinting the csv file
# write.csv(df_carprod_credsys, file = fileout, row.names = TRUE)
# write.xlsx(df_relCreSys, file = fileout, sheetName = in_prodalp , colNames = TRUE, borders = "columns")

return(df_carProd)
}

######################################################################################
# writing all dataframes from raw data into diferent tidy data sheets into the excel report
######################################################################################
#l <- list("baseCREPROD" = df_CarProd, "baseRETSYS" = df_CarProdRetSys, "baseCARPROD" = df_CarteProd, "baseCARTSEG" = df_CarteSeguros,
#          "baseCARTCARD" = df_CarteCartoes, "baseAMORT" = df_CarteAmort, "baseRISCO" = df_RiscoPerdaExp)
#write.xlsx(l, file = fileout, colNames = TRUE, borders = "columns")