library(kableExtra)
library(dplyr)
library(htmltools)
gerar_tabela <- function(gerar ='tab1',
                         arquivo = "C:/FP_Generator/dados/Carnes 2.csv",
                         beneficioME=T,
                         casasdecimais = 2){
  #setwd(pasta)
  brutal <- read.csv(file =arquivo ,header = F,sep = ',',encoding = 'UTF-8')
  
  quanti <- brutal[1:4,] # 1 - item, 2 - cod. item, 3 - quantitativo, 4 - unidade de medida
  brutos <- brutal[-c(1:4),]
  nlinhas <- nrow(brutos)
  nitens <- 1:ncol(brutos)
  itens <- paste0(nitens,' - ',brutos[1,])
  precos_idx <- seq(2,nrow(brutos),by = 2)
  precos <- brutos[precos_idx,]
  lista_itens <- rep('',8)
  precos_estimados <- rep('',length(nitens))
  for(i in 1:length(itens)){
    #i=3
    
    precos[,i] <- gsub(pattern = '\\.',replacement = '',x = precos[,i])
    precos[,i] <- gsub(pattern = ',',replacement = '.',x = precos[,i])
    nas <- is.na(as.numeric(precos[,i]))
    numeros <- as.numeric(precos[!nas,i])
    n <- length(numeros)
    desv_padrao1 <- sqrt(var(numeros)*(n-1)/n)
    media1 <- mean(numeros)
    mediana1 <- median(numeros)
    cv1 <- desv_padrao1/media1*100
    descritivo1 <- paste0('M = ',format(round(media1,digits = casasdecimais), nsmall = casasdecimais, decimal.mark = ","),
                          '\nMd = ',format(round(mediana1,digits = casasdecimais), nsmall = casasdecimais, decimal.mark = ","),
                          '\nDP = ',format(round(desv_padrao1,digits = casasdecimais), nsmall = casasdecimais, decimal.mark = ","),
                          '\nCV = ',format(round(cv1,digits = casasdecimais), nsmall = casasdecimais, decimal.mark = ",")%>% paste0("%"))
    aux1 <- 1:n
    for(j in 1:n){
      #j=1
      aux1[j] <- mean(numeros[-j])
    }
    temp2 <- numeros/aux1 *100
    
    avalic <- ifelse(temp2 > 125 ,yes = "Excessivamente\nelevado",no = "Válido")
    numeros_validos <- numeros[temp2 < 125]
    desv_padrao <- sqrt(var(numeros_validos)*(n-1)/n)
    media <- mean(numeros_validos)
    cv <- desv_padrao/media*100
    mediana <- median(numeros_validos)
    atualizados <- paste0('M\' = ',format(round(media,digits = casasdecimais), nsmall = casasdecimais, decimal.mark = ","),
                          '\nMd\' = ',format(round(mediana,digits = casasdecimais), nsmall = casasdecimais, decimal.mark = ","),
                          '\nDP\' = ',format(round(desv_padrao,digits = casasdecimais), nsmall = casasdecimais, decimal.mark = ","),
                          '\nCV\' = ',format(round(cv,digits = casasdecimais), nsmall = casasdecimais, decimal.mark = ",")%>% paste0("%"))
    
    if(cv < 25){
      metodo <- paste0("Média\nCV\' < 25%\n\n","Preço Estimado: ",format(round(media,digits = casasdecimais), nsmall = casasdecimais, decimal.mark = ","))
      precos_estimados[i] <- round(media,digits = casasdecimais)
    }else{
      metodo <- paste0("Mediana\nCV\' > 25%\n\nPreço Estimado: ",format(round(mediana,digits = casasdecimais), nsmall = casasdecimais, decimal.mark = ","))
      precos_estimados[i] <- round(mediana,digits = casasdecimais)}
    
    
    lista_itens <- rbind(lista_itens, cbind(rep(itens[[i]],n), #1
                                            format(round(numeros,digits = casasdecimais), nsmall = casasdecimais, decimal.mark = ","),#2
                                            rep(descritivo1,n),#3
                                            format(round(aux1,digits = casasdecimais), nsmall = casasdecimais, decimal.mark = ","), #4
                                            format(round(temp2,digits = casasdecimais), nsmall = casasdecimais, decimal.mark = ","), #5
                                            avalic, #6
                                            rep(atualizados,n),#7
                                            rep(metodo,n))) #8
  }
  df_itens <- data.frame(lista_itens[-1,],row.names = NULL,stringsAsFactors = FALSE)
  quantitativos <- gsub(pattern = '\\.',replacement = '',x = quanti[3,])
  quantitativos <- gsub(pattern = ',',replacement = '.',x = quantitativos)
  
  maxprec<- simplify2array(round(as.numeric(precos_estimados)*as.numeric(quantitativos),digits = casasdecimais))
  total <- sum(maxprec)
  itens_df <- data.frame('n' = as.numeric(quantitativos),
                         'preco_unitario' = simplify2array(round(as.numeric(precos_estimados),digits = casasdecimais)))
  
  formatar_moeda <- function(valor) {
    paste0("R\\$ ", formatC(valor, format = "f", big.mark = ".", decimal.mark = ",", digits = casasdecimais))
  }
  
  tab1 <- df_itens
  tab1_formatada <- tab1 %>%
    mutate(
      across(
        where(is.character),
        ~ gsub("\n", "<br>", .x)
      ),
      across(
        c(2, 4, 5, 6),
        ~ cell_spec(
          .x,
          format = "html",
          color = case_when(
            avalic == "Excessivamente<br>elevado" ~ "red",
            avalic == "Válido" ~ "green",
            TRUE ~ "black"
          ),
          escape = FALSE
        )
      ),
      avalic = cell_spec(
        avalic,
        format = "html",
        escape = FALSE
      )
    )
  
  # Definir larguras em PIXELS (exemplo total = 1200px)
  larguras_px <- c("420px", "60px", "120px", "60px", "60px", "120px", "180px", "180px")
  
  # Criar tabela com CSS inline para fixar tamanhos
  if(gerar=='tab1'){
    
  tabela <- tab1_formatada %>%
    kable(
      format = "html",
      escape = FALSE,
      align = "c",
      table.attr = 'style="border-collapse: collapse; width: 1200px; table-layout: fixed; margin: 0 auto;"',
      col.names = c("Item", "Preços (R\\$)", "Resumo Inicial", "Média dos Demais Valores (R\\$)",
                    "Razão (%)", "Avaliação", "Resumo Final", "Método de Avaliação")
    ) %>%
    kable_styling(
      full_width = FALSE,  # Fundamental para tamanhos fixos!
      bootstrap_options = "striped",html_font = 'calibri'
    ) %>%
    # Aplicar larguras pixel a pixel
    column_spec(
      1:8,
      width = larguras_px,
      extra_css = "border: 1px solid black !important; padding: 5px !important;"
    ) %>%
    row_spec(
      0:nrow(tab1_formatada),
      extra_css = "border: 1px solid black !important;"
    ) %>%
    collapse_rows(
      columns = c(1, 3, 7, 8),
      valign = "middle"
    ) %>%
    add_footnote(
      "Nota: M = Média, Md = Mediana, DP = Desvio Padrão, CV = Coeficiente de Variação",
      notation = "none",
      escape = FALSE,
    )
  tabelinha <- tabela
  return(tabelinha)
  }
  
  ## Tabela do valor maximo
  
  if(beneficioME){
    maximos <- itens_df$n*itens_df$preco_unitario
    idx_ME <- maximos > 80000 #fracionar entre Ampla conc e ME
    sequencia_itens <- 1:(sum(idx_ME) + length(quanti[1,]))
    AC <- itens_df[idx_ME,]
    ME <- itens_df[idx_ME,]
    for(i in 1:nrow(AC)){      #i=1
      AC[i,1] <- ifelse(floor(AC[i,1]/4)  * AC[i,2] <= 80000,
                        AC[i,1] <- ceiling(itens_df[idx_ME,][i,1]*.75),
                        AC[i,1] <- AC[i,1] - floor(80000 / AC[i,2]))
    }
    
    ME[,1]<- itens_df[idx_ME,1]-AC[,1]
    
    larguras_px2 <- paste0(c(.05,.1,.4,.10,.05,.15,.15)*1200,'px')
    
    
  
    ampla_concorrencia <- data.frame('item' = sequencia_itens[1:sum(idx_ME)],
                                     'codigo' = simplify2array((unname(quanti[2,idx_ME]))),
                                     'descricao' = simplify2array((unname(brutos[1,idx_ME]))),
                                     'quantitativo' = AC[,1],#
                                     'unidade' = simplify2array((unname(quanti[4,idx_ME]))),
                                     'precoestimado' = formatar_moeda(itens_df[idx_ME,2]),
                                     'precomaximo' = formatar_moeda(AC[,1] * AC[,2]))
    
    
    beneficio <- data.frame('item' = sequencia_itens[(sum(idx_ME)+1):((2*sum(idx_ME)))],
                            'codigo' = simplify2array((unname(quanti[2,idx_ME]))),
                            'descricao' = simplify2array((unname(brutos[1,idx_ME]))),
                            'quantitativo' = ME[,1],#
                            'unidade' = simplify2array((unname(quanti[4,idx_ME]))),
                            'precoestimado' = formatar_moeda(ME[,2]),
                            'precomaximo' = formatar_moeda(ME[,1] * ME[,2]))
    
    
    if(sum(!idx_ME) !=0){
    beneficio2 <- data.frame('item' = sequencia_itens[(2*sum(idx_ME)+1):(length(idx_ME)+sum(idx_ME))],
                            'codigo' = simplify2array((unname(quanti[2,!idx_ME]))),
                            'descricao' = simplify2array((unname(brutos[1,!idx_ME]))),
                            'quantitativo' = itens_df[!idx_ME,1],
                            'unidade' = simplify2array((unname(quanti[4,!idx_ME]))),
                            'precoestimado' = formatar_moeda(itens_df[!idx_ME,2]),
                            'precomaximo' = formatar_moeda(itens_df[!idx_ME,1] * itens_df[!idx_ME,2]))
    
    
    } else beneficio2 <- rep("",7)
    total_AC <- formatar_moeda(sum(AC[,1] * AC[,2]))
    total_ME <- formatar_moeda(sum(ME[,1]*ME[,2]) + sum(itens_df[!idx_ME,1] * itens_df[!idx_ME,2]))
    Totalgeral <- formatar_moeda(sum(AC[,1] * AC[,2]) + sum(ME[,1]*ME[,2]) + sum(itens_df[!idx_ME,1] * itens_df[!idx_ME,2]))
    tabelao <- rbind(rep(c('','AMPLA CONCORRÊNCIA'),c(6,1)),
                     ampla_concorrencia,
                     rep(c('','Total',total_AC),c(5,1,1)),
                     rep(c('','EXCLUSIVO ME'),c(6,1)),
                     beneficio,
                     beneficio2,
                     rep(c('','Total',total_ME),c(5,1,1)),
                     rep(c('','Total Geral',Totalgeral),c(5,1,1)))
    
    
    tab_max <- tabelao %>% kable(
      format = "html",
      escape = FALSE,
      align = "c",
      valign = "middle",
      table.attr = 'style="border-collapse: collapse; width: 1200px; table-layout: fixed; margin: 0 auto;"',
      col.names = c("Item", 
                    "Código<br>Elotech", 
                    "Descrição", 
                    "Quantitativo",
                    "Unidade<br>de<br>Medida", 
                    "Preço<br>Estimado", 
                    "Total")
    ) %>% kable_styling(
      full_width = FALSE,  # Fundamental para tamanhos fixos!
      bootstrap_options = "striped"
    ) %>% column_spec(
      1:7,
      width = larguras_px2,
      extra_css = "border: 1px solid black !important; padding: 5px !important;"
    ) %>%
      row_spec(
        0:nrow(tabelao),
        extra_css = "border: 1px solid black !important;")
    tabelinha <- tab_max
    
  }else{
    df_quantitativo <- data.frame('item' = simplify2array(unname(quanti[1,])),
                                  'codigo' = simplify2array((unname(quanti[2,]))),
                                  'descricao' = simplify2array((unname(brutos[1,]))),
                                  'quantitativo' = simplify2array((unname(quanti[3,]))),
                                  'unidade' = simplify2array((unname(quanti[4,]))),
                                  'precoestimado' = formatar_moeda(simplify2array((unname(as.numeric(precos_estimados))))),
                                  'precomaximo' = formatar_moeda(maxprec))
    
    
    larguras_px2 <- paste0(c(.05,.1,.4,.10,.05,.15,.15)*1200,'px')
    df_quantitativo<- rbind(df_quantitativo,rep(c('','Total',formatar_moeda(round(total,digits = casasdecimais))),c(5,1,1)))
    
    tab_max <- df_quantitativo %>% kable(
      format = "html",
      escape = FALSE,
      align = "c",
      valign = "middle",
      table.attr = 'style="border-collapse: collapse; width: 1200px; table-layout: fixed; margin: 0 auto;"',
      col.names = c("Item", 
                    "Código<br>Elotech", 
                    "Descrição", 
                    "Quantitativo",
                    "Unidade<br>de<br>Medida", 
                    "Preço<br>Estimado", 
                    "Total")
    ) %>% kable_styling(
      full_width = FALSE,  # Fundamental para tamanhos fixos!
      bootstrap_options = "striped"
    ) %>% column_spec(
      1:7,
      width = larguras_px2,
      extra_css = "border: 1px solid black !important; padding: 5px !important;"
    ) %>%
      row_spec(
        0:nrow(df_quantitativo),
        extra_css = "border: 1px solid black !important;"
      )
    tabelinha <- tab_max
  }
  return(tabelinha)
}
