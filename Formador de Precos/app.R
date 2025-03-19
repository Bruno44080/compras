organizar <- function(arquivo1, arquivo2) {
  
  # Leitura e tratamento dos CSVs
  brutal1 <- read.csv(arquivo1, header = FALSE, encoding = 'UTF-8', 
                      col.names = c("Fonte", "Descricao_fonte"))
  
  brutal2 <- read.csv(arquivo2, header = FALSE, encoding = 'UTF-8', 
                      stringsAsFactors = FALSE)
  
  # Estruturação dos dados
  definicoes <- as.data.frame(t(brutal2[1:6, ]), stringsAsFactors = FALSE)
  colnames(definicoes) <- c("Item", "Codigo", "Catmat", "Quantitativo", "Unidade","Descricao")
  
  brutos <- brutal2[-c(1:6), ]
  lista_itens <- list()
  
  # Processamento para cada coluna/item
  for(i in seq_along(brutos)) {
    # i=1
    item <- list()
    
    # Metadados do item
    item$codigo <- definicoes$Codigo[i]
    item$descricao <- paste0(definicoes$Item[i]," - ",definicoes$Descricao[i])
    item$unidade <- definicoes$Unidade[i]
    
    # Preços e fontes
    
    precos <- brutos[seq(1, nrow(brutos), by = 2), i]
    origens <- brutos[seq(2, nrow(brutos), by = 2), i]
    
    # Limpeza numérica rigorosa
    precos_clean <- gsub("\\.", "", precos)
    precos_clean <- gsub(",", ".", precos_clean)
    
    # Conversão e validação
    precos_numeric <- suppressWarnings(as.numeric(precos_clean))
    validos <- !is.na(precos_numeric) & 
      grepl("^\\d{1,3}(\\.\\d{3})*,\\d{2}$", precos)
    
    # Atribuição final
    item$precos <- setNames(precos_numeric[validos], origens[validos])
    item$fontes <- merge(data.frame(Fonte = origens[validos]), brutal1, by = "Fonte",sort = F)
    item$catmat <- definicoes$Catmat[i]
    item$quantidade <- as.numeric(gsub(",", ".", gsub("\\.", "", definicoes$Quantitativo[i])))
    lista_itens[[i]] <- item
  }
  
  names(lista_itens) <- paste0("Item_", definicoes$Item)
  return(lista_itens)
}

library(shiny)
library(shinyjs)
library(dplyr)
library(kableExtra)
library(htmltools)
require(rmarkdown)
ui <- fluidPage(
  useShinyjs(),
  titlePanel("Sistema de Formação de Preços"),
  
  conditionalPanel(
    condition = "input.processar == 0",
    wellPanel(
      h3("Etapa 1 - Carregar Arquivos"),
      fileInput("csv1", "Arquivo .CSV com as fontes de pesquisa:", accept = ".csv"),
      fileInput("csv2", "Arquivo .CSV com os preços pesquisados:", accept = ".csv"),
      actionButton("processar", "Processar Arquivos", class = "btn-primary")
    )
  ),
  
  conditionalPanel(
    condition = "input.processar > 0",
    wellPanel(
      h3("Etapa 2 - Configurações"),
      fluidRow(
        column(12, align = "center",
               actionButton("gerar_relatorio", "Gerar Tabelas", 
                            class = "btn-success", icon = icon("file-export"))
        )
      ),
      hr(),
      fluidRow(
        column(4, numericInput("casas_decimais", "Casas Decimais:", 2, min = 2, max = 4)),
        column(4,
               checkboxInput("todas_inex", "Realizar análise de Inexequibilidade em todos os itens."),
               checkboxInput("todas_me", "Aplicar Benefício de ME em todos os Itens.")
        )
      ),
      hr(),
      
      uiOutput("config_itens"),
      actionButton("gerar_relatorio", "Gerar Tabelas", class = "btn-success")
    )
  )
)

# Server --------------------------------------------------------------------------
server <- function(input, output, session){
  
  dados <- reactiveValues(
    lista_itens = NULL,
    parametros = list()
  )
  
  observeEvent(input$processar, {
    req(input$csv1, input$csv2)
    
    tryCatch({
      dados$lista_itens <- organizar(input$csv1$datapath, input$csv2$datapath)
    }, error = function(e) {
      showNotification(paste("Erro:", e$message), type = "error")
    })
  })
  
  output$config_itens <- renderUI({
    req(dados$lista_itens)
    
    tagList(
      lapply(dados$lista_itens, function(item) {
        wellPanel(
          h4(item$descricao),
          
          fluidRow(
            column(4,
                   checkboxInput(
                     paste0("inex_", item$codigo),
                     "Inexequibilidade",
                     value = FALSE
                   )
            ),
            column(4,
                   checkboxInput(
                     paste0("me_", item$codigo),
                     "Benefício ME",
                     value = FALSE
                   )
            ),
            column(4,
                   numericInput(
                     paste0("manual_", item$codigo),
                     "Valor Manual",
                     value = NA,
                     min = 0
                   )
            )
          ),
          
          hr(),
          
          uiOutput(paste0("tabela_", item$codigo)) , # Alterado para uiOutput
        )
      })
    )
  })
  
  observeEvent(input$todas_inex, {
    req(dados$lista_itens)
    lapply(dados$lista_itens, function(item) {
      updateCheckboxInput(session, paste0("inex_", item$codigo), value = input$todas_inex)
    })
  })
  
  observeEvent(input$todas_me, {
    req(dados$lista_itens)
    lapply(dados$lista_itens, function(item) {
      updateCheckboxInput(session, paste0("me_", item$codigo), value = input$todas_me)
    })
  })
  
  # Dentro do observer que renderiza as tabelas (~linha 130)
  observe({
    req(dados$lista_itens)
    
    lapply(dados$lista_itens, function(item) {
      output[[paste0("tabela_", item$codigo)]] <- renderUI({
        config <<- list(
          inexequibilidade = input[[paste0("inex_", item$codigo)]],
          me = input[[paste0("me_", item$codigo)]],
          manual = input[[paste0("manual_", item$codigo)]],
          casasdecimais = input$casas_decimais
        )
        
        # Cálculos base
        precos_originais <- item$precos
        n <- length(precos_originais)
        
        # Aplicar filtro de inexequibilidade
        if(config$inexequibilidade) {
          media_demais <- sapply(1:n, \(j) mean(precos_originais[-j]))
          razao <- precos_originais/media_demais            
          elevados <- razao > 1.25
          precos_filtrados <- precos_originais[!elevados]
          
          # Recalcular para os remanescentes
          n_filtrado <- length(precos_filtrados)
          media_demais_filtrado <- sapply(1:n_filtrado, \(j) mean(precos_filtrados[-j]))
          razao_filtrado <- precos_filtrados/media_demais_filtrado
          inexequiveis1 <- razao_filtrado < 0.75
          idx_inexeq <- names(which(inexequiveis1))
          inexequiveis <- rep(FALSE, n)
          names(inexequiveis) <- names(precos_originais)
          inexequiveis[idx_inexeq] <- TRUE
          if(!is.na(inexequiveis["A"]))  inexequiveis["A"]<- FALSE
          if(!is.na(inexequiveis["C"]))  inexequiveis["C"]<- FALSE
          razao[idx_inexeq] <- razao_filtrado[idx_inexeq]
        } else {
          media_demais <- sapply(1:n, \(j) mean(precos_originais[-j]))
          razao <- precos_originais/media_demais
          elevados <- razao > 1.25
          inexequiveis <- rep(FALSE, n)
        }
        
        # Cálculo das novas colunas
        avaliacao <- ifelse(elevados, "Excessivamente<br>Elevado",
                            ifelse(inexequiveis, "Inexequível", "Válido"))
        
        # Método de avaliação
        validos <- precos_originais[!elevados & !inexequiveis]
        nv <- length(validos)
           
        if(length(validos) > 1) {
          media <- mean(validos)
          mediana <- median(validos)
          desvio_padrao <- sqrt((nv-1)/nv*var(validos))
          cv <- desvio_padrao/media
          resumo <- paste0("M = ", format(round(media, config$casasdecimais),nsmall = config$casasdecimais, decimal.mark = ","),
                           "<br>Md = ", format(round(mediana, config$casasdecimais),nsmall = config$casasdecimais, decimal.mark = ","),
                           "<br>DP = ", format(round(desvio_padrao, config$casasdecimais),nsmall = config$casasdecimais, decimal.mark = ","),
                           "<br>CV = ", format(round(cv*100, config$casasdecimais),nsmall = config$casasdecimais, decimal.mark = ","), "%")
          if(cv < 0.25){
            metodo <- paste0("Média<br>CV < 25%<br>Preço Estimado: ", format(round(mean(validos), config$casasdecimais),
                                                                             nsmall = config$casasdecimais, decimal.mark = ","))
            preco_estimado <<- round(mean(validos), config$casasdecimais)
            }else{
              metodo <- paste0("Mediana<br>CV ≥ 25%<br>Preço Estimado: ",
                               format(round(median(validos), config$casasdecimais),
                                      nsmall = config$casasdecimais, decimal.mark = ","))
              preco_estimado <<- round(mean(validos), config$casasdecimais)
              }
          } else {
            resumo <- "Dados insuficientes"
            preco_estimado <<- round(mean(validos), config$casasdecimais)
            }
        
        
        if(!is.na(config$manual)) {
          metodo <- paste0("Personalizado<br>Preço: ",
                           format(round(config$manual, config$casasdecimais),
                                  nsmall = config$casasdecimais, decimal.mark = ","))
          preco_estimado <<- round(config$manual, config$casasdecimais)
        }
        
        
        # Dataframe completo
        df <- data.frame(
          Item = rep(item$descricao,n),
          Preços = paste0(
            format(round(unname(precos_originais), config$casasdecimais),
                   nsmall = config$casasdecimais, decimal.mark = ","),
            "<br><small>", names(precos_originais), "</small>"
          ),
          `Média dos Demais` = format(round(unname(media_demais), config$casasdecimais),
                                      nsmall = config$casasdecimais, decimal.mark = ","),
          Razão = paste0(
            format(round(unname(razao)*100, config$casasdecimais),
                   nsmall = config$casasdecimais, decimal.mark = ","), "%"),
          Avaliação = unname(avaliacao),
          Resumo = resumo,
          `Método de Avaliação` = unname(metodo)
        )
        
        df_formatado <- df %>%
          mutate(
            across(
              c(2, 3, 4, 5),
              ~ cell_spec(
                .x,
                format = "html",
                color = case_when(
                  Avaliação == "Excessivamente<br>Elevado" ~ "red",
                  Avaliação == "Válido" ~ "green",
                  Avaliação == "Inexequível" ~ "blue",
                  TRUE ~ "black"
                ),
                escape = FALSE
              )
            ),
            Avaliação = cell_spec(
              Avaliação,
              format = "html",
              escape = FALSE
            )
          )
        
        #inicio df com preco maximo total
        
        total_estimado <- item$quantidade * preco_estimado
        
        if(config$me){
          if(total_estimado <= 80000){#ok
            df_resumo <- data.frame(
              Item = item$descricao,
              Item2 = item$codigo,
              CATMAT = item$catmat,
              Quantidade = format(item$quantidade, big.mark = ".", decimal.mark = ","),
              Unidade = item$unidade,
              `Preço Estimado` = paste0("R$ ",format(round(preco_estimado, config$casasdecimais),nsmall = config$casasdecimais, big.mark = ".", decimal.mark = ",")),
              `Total Estimado` = paste0("R$ ",format(round(total_estimado, config$casasdecimais),nsmall = config$casasdecimais, big.mark = ".", decimal.mark = ",")),stringsAsFactors = F)
            df_resumo <- rbind(rep(c("","Exclusivo ME"),c(6,1)),df_resumo)
          } else{
            qtd_me <- ifelse(test = (floor(item$quantidade*.25) * preco_estimado) <= 80000,
                             yes = floor(item$quantidade * .25),
                             no = floor(80000/preco_estimado))
            qtd_ac <- item$quantidade - qtd_me

              ME <- data.frame(
                Item = item$descricao,
                Item2 = item$codigo,
                CATMAT = item$catmat,
                Quantidade = format(qtd_me, big.mark = ".",decimal.mark = ","),
                Unidade = item$unidade,
                `Preço Estimado` = paste0("R$ ",format(round(preco_estimado, config$casasdecimais),nsmall = config$casasdecimais, big.mark = ".", decimal.mark = ",")),
                `Total Estimado` = paste0("R$ ",format(round(qtd_me * preco_estimado, config$casasdecimais),nsmall = config$casasdecimais, big.mark = ".", decimal.mark = ",")),
                stringsAsFactors = F)
              
              AC <- data.frame(
                Item = item$descricao,
                Item2 = item$codigo,
                CATMAT = item$catmat,
                Quantidade = format(qtd_ac, big.mark = ".",decimal.mark = ","),
                Unidade = item$unidade,
                `Preço Estimado` = paste0("R$ ",
                                          format(round(preco_estimado, 
                                                       config$casasdecimais),nsmall = config$casasdecimais,
                                                 big.mark = ".", 
                                                 decimal.mark = ",")),
                `Total Estimado` = paste0("R$ ",
                                          format(
                                            round( qtd_ac * preco_estimado, config$casasdecimais),nsmall = config$casasdecimais, 
                                            big.mark = ".", decimal.mark = ",")),
                stringsAsFactors = F)
              df_resumo <- rbind(rep(c("","Ampla Concorrência"),c(6,1)),AC,
                                 rep(c("","Exclusivo ME"),c(6,1)),ME)
                
            
          }
        } else{#ok
          df_resumo <- data.frame(
            Item = item$descricao,
            Item2 = item$codigo,
            CATMAT = item$catmat,
            Quantidade = format(item$quantidade, big.mark = ".", decimal.mark = ","),       
            Unidade = item$unidade,
            `Preço Estimado` = paste0("R$ ",format(round(preco_estimado, config$casasdecimais), nsmall = config$casasdecimais, big.mark = ".", decimal.mark = ",")),
            `Total Estimado` = paste0("R$ ",format(round(total_estimado, config$casasdecimais), nsmall = config$casasdecimais, big.mark = ".", decimal.mark = ",")))
          }
        
        # Renderização
        HTML(
          paste(
            kable(df_formatado, "html", 
                  escape = FALSE, 
                  align = c("l", "c", "c", "c", "l"),
                  col.names = c("Item", "Preços (R$)", "Média dos demais<br>Valores (R$)",
                                "Razão (%)", "Avaliação", "Resumo Descritivo", "Método de<br>Avaliação")) %>%
              kable_styling(bootstrap_options = "striped",
                            html_font = 'calibri', 
                            full_width = FALSE) %>%
              column_spec(2:6, 
                          extra_css = "border: 1px solid black !important; padding: 5px !important;") %>%
              collapse_rows(
                columns = c(1, 6, 7),
                valign = "middle"
              ) %>% 
              row_spec(
                0:nrow(df_formatado),
                extra_css = "border: 1px solid black !important;"
              ) %>%  
              add_footnote(
                "Nota: M = Média, Md = Mediana, DP = Desvio Padrão, CV = Coeficiente de Variação",
                notation = "none",
                escape = FALSE,
              ) %>% as.character(),
            
            kable(df_resumo, "html", 
                  escape = FALSE, 
                  align = 'c',
                  col.names = c("Descrição", 
                                "Código<br>Elotech", 
                                "CATMAT",
                                "Quantitativo",
                                "Unidade<br>de<br>Medida", 
                                "Preço<br>Estimado", 
                                "Total")) 
            %>%
              kable_styling(bootstrap_options = c("striped"), full_width = FALSE) %>%
              column_spec(1:7,
                          extra_css = "border: 1px solid black !important; padding: 5px !important;") %>%
              row_spec(
                0:nrow(df_resumo),
                extra_css = "border: 1px solid black !important;"
              )%>%
              as.character(),
            sep = "\n"
          )
        )
      })
    })
    
    observeEvent(input$gerar_relatorio, {
      req(dados$lista_itens)
      
      # Criar lista de configurações
      configs_geral <- lapply(dados$lista_itens, function(item) {
        list(
          codigo = item$codigo,
          inexequibilidade = input[[paste0("inex_", item$codigo)]],
          me = input[[paste0("me_", item$codigo)]],
          manual = input[[paste0("manual_", item$codigo)]]
        )
      })
      
      
      # Processar dados para relatório
      dados_relatorio <- lapply(dados$lista_itens, function(item) {
        config <<- list(
          inexequibilidade = input[[paste0("inex_", item$codigo)]],
          me = input[[paste0("me_", item$codigo)]],
          manual = input[[paste0("manual_", item$codigo)]],
          casasdecimais = input$casas_decimais
        )
        
        # Cálculos base
        precos_originais <- item$precos
        n <- length(precos_originais)
        
        # Aplicar filtro de inexequibilidade
        if(config$inexequibilidade) {
          media_demais <- sapply(1:n, \(j) mean(precos_originais[-j]))
          razao <- precos_originais/media_demais            
          elevados <- razao > 1.25
          precos_filtrados <- precos_originais[!elevados]
          
          # Recalcular para os remanescentes
          n_filtrado <- length(precos_filtrados)
          media_demais_filtrado <- sapply(1:n_filtrado, \(j) mean(precos_filtrados[-j]))
          razao_filtrado <- precos_filtrados/media_demais_filtrado
          inexequiveis1 <- razao_filtrado < 0.75
          idx_inexeq <- names(which(inexequiveis1))
          inexequiveis <- rep(FALSE, n)
          names(inexequiveis) <- names(precos_originais)
          inexequiveis[idx_inexeq] <- TRUE
          razao[idx_inexeq] <- razao_filtrado[idx_inexeq]
        } else {
          media_demais <- sapply(1:n, \(j) mean(precos_originais[-j]))
          razao <- precos_originais/media_demais
          elevados <- razao > 1.25
          inexequiveis <- rep(FALSE, n)
        }
        
        # Cálculo das novas colunas
        avaliacao <- ifelse(elevados, "Excessivamente<br>Elevado",
                            ifelse(inexequiveis, "Inexequível", "Válido"))
        
        # Método de avaliação
        validos <- precos_originais[!elevados & !inexequiveis]
        nv <- length(validos)
        if(length(validos) > 1) {
          media <- mean(validos)
          mediana <- median(validos)
          desvio_padrao <- sqrt((nv-1)/nv*var(validos))
          cv <- desvio_padrao/media
          
          resumo <- paste0("M = ", format(round(media, config$casasdecimais),nsmall = config$casasdecimais, decimal.mark = ","),
                           "<br>Md = ", format(round(mediana, config$casasdecimais),nsmall = config$casasdecimais, decimal.mark = ","),
                           "<br>DP = ", format(round(desvio_padrao, config$casasdecimais),nsmall = config$casasdecimais, decimal.mark = ","),
                           "<br>CV = ", format(round(cv*100, config$casasdecimais),nsmall = config$casasdecimais, decimal.mark = ","), "%")
          if(cv < 0.25){
            metodo <- paste0("Média<br>CV < 25%<br>Preço Estimado: ", format(round(mean(validos), config$casasdecimais),
                                                                             nsmall = config$casasdecimais, decimal.mark = ","))
            preco_estimado <<- round(mean(validos), config$casasdecimais)
          }else{
            metodo <- paste0("Mediana<br>CV ≥ 25%<br>Preço Estimado: ",
                             format(round(median(validos), config$casasdecimais),
                                    nsmall = config$casasdecimais, decimal.mark = ","))
            preco_estimado <<- round(mean(validos), config$casasdecimais)
          }
        } else {
          resumo <- "Dados insuficientes"
        }
        
        if(!is.na(config$manual)) {
          metodo <- paste0("Personalizado<br>Preço: ",
                           format(round(config$manual, config$casasdecimais),
                                  nsmall = config$casasdecimais, decimal.mark = ","))
          preco_estimado <<- round(config$manual, config$casasdecimais)
        } 
        
        
        # Dataframe completo
        df <- data.frame(
          Item = rep(item$descricao,n),
          Preços = paste0(
            format(round(unname(precos_originais), config$casasdecimais),
                   nsmall = config$casasdecimais, decimal.mark = ","),
            "<br><small>", names(precos_originais), "</small>"
          ),
          `Média dos Demais` = format(round(unname(media_demais), config$casasdecimais),
                                      nsmall = config$casasdecimais, decimal.mark = ","),
          Razão = paste0(
            format(round(unname(razao)*100, config$casasdecimais),
                   nsmall = config$casasdecimais, decimal.mark = ","), "%"),
          Avaliação = unname(avaliacao),
          Resumo = resumo,
          `Método de Avaliação` = unname(metodo)
        )
        
        df_formatado <- df %>%
          mutate(
            across(
              c(2, 3, 4, 5),
              ~ cell_spec(
                .x,
                format = "html",
                color = case_when(
                  Avaliação == "Excessivamente<br>Elevado" ~ "red",
                  Avaliação == "Válido" ~ "green",
                  Avaliação == "Inexequível" ~ "blue",
                  TRUE ~ "black"
                ),
                escape = FALSE
              )
            ),
            Avaliação = cell_spec(
              Avaliação,
              format = "html",
              escape = FALSE
            )
          ) 
        return(df_formatado)
      }) %>% bind_rows()
      dados_relatorio2 <- lapply(dados$lista_itens, function(item) {
        config <<- list(
          inexequibilidade = input[[paste0("inex_", item$codigo)]],
          me = input[[paste0("me_", item$codigo)]],
          manual = input[[paste0("manual_", item$codigo)]],
          casasdecimais = input$casas_decimais
        )
        
        # Cálculos base
        precos_originais <- item$precos
        n <- length(precos_originais)
        
        # Aplicar filtro de inexequibilidade
        if(config$inexequibilidade) {
          media_demais <- sapply(1:n, \(j) mean(precos_originais[-j]))
          razao <- precos_originais/media_demais            
          elevados <- razao > 1.25
          precos_filtrados <- precos_originais[!elevados]
          
          # Recalcular para os remanescentes
          n_filtrado <- length(precos_filtrados)
          media_demais_filtrado <- sapply(1:n_filtrado, \(j) mean(precos_filtrados[-j]))
          razao_filtrado <- precos_filtrados/media_demais_filtrado
          inexequiveis1 <- razao_filtrado < 0.75
          idx_inexeq <- names(which(inexequiveis1))
          inexequiveis <- rep(FALSE, n)
          names(inexequiveis) <- names(precos_originais)
          inexequiveis[idx_inexeq] <- TRUE
          if(!is.na(inexequiveis["A"]))  inexequiveis["A"]<- FALSE
          if(!is.na(inexequiveis["C"]))  inexequiveis["C"]<- FALSE
          razao[idx_inexeq] <- razao_filtrado[idx_inexeq]
        } else {
          media_demais <- sapply(1:n, \(j) mean(precos_originais[-j]))
          razao <- precos_originais/media_demais
          elevados <- razao > 1.25
          inexequiveis <- rep(FALSE, n)
        }
        
        # Cálculo das novas colunas
        avaliacao <- ifelse(elevados, "Excessivamente<br>Elevado",
                            ifelse(inexequiveis, "Inexequível", "Válido"))
        
        # Método de avaliação
        validos <- precos_originais[!elevados & !inexequiveis]
        nv <- length(validos)
        
        if(length(validos) > 1) {
          media <- mean(validos)
          mediana <- median(validos)
          desvio_padrao <- sqrt((nv-1)/nv*var(validos))
          cv <- desvio_padrao/media
          resumo <- paste0("M = ", format(round(media, config$casasdecimais),nsmall = config$casasdecimais, decimal.mark = ","),
                           "<br>Md = ", format(round(mediana, config$casasdecimais),nsmall = config$casasdecimais, decimal.mark = ","),
                           "<br>DP = ", format(round(desvio_padrao, config$casasdecimais),nsmall = config$casasdecimais, decimal.mark = ","),
                           "<br>CV = ", format(round(cv*100, config$casasdecimais),nsmall = config$casasdecimais, decimal.mark = ","), "%")
          if(cv < 0.25){
            metodo <- paste0("Média<br>CV < 25%<br>Preço Estimado: ", format(round(mean(validos), config$casasdecimais),
                                                                             nsmall = config$casasdecimais, decimal.mark = ","))
            preco_estimado <<- round(mean(validos), config$casasdecimais)
          }else{
            metodo <- paste0("Mediana<br>CV ≥ 25%<br>Preço Estimado: ",
                             format(round(median(validos), config$casasdecimais),
                                    nsmall = config$casasdecimais, decimal.mark = ","))
            preco_estimado <<- round(mean(validos), config$casasdecimais)
          }
        } else {
          resumo <- "Dados insuficientes"
          preco_estimado <<- round(mean(validos), config$casasdecimais)
        }
        
        
        if(!is.na(config$manual)) {
          metodo <- paste0("Personalizado<br>Preço: ",
                           format(round(config$manual, config$casasdecimais),
                                  nsmall = config$casasdecimais, decimal.mark = ","))
          preco_estimado <<- round(config$manual, config$casasdecimais)
        }
        
        
        total_estimado <- item$quantidade * preco_estimado
        
        if(config$me){
          if(total_estimado <= 80000){#ok
            df_resumo <- data.frame(
              Item = item$descricao,
              Item2 = item$codigo,
              CATMAT = item$catmat,
              Quantidade = format(item$quantidade, big.mark = ".", decimal.mark = ","),
              Unidade = item$unidade,
              `Preço Estimado` = paste0("R$ ",format(round(preco_estimado, config$casasdecimais),nsmall = config$casasdecimais, big.mark = ".", decimal.mark = ",")),
              `Total Estimado` = paste0("R$ ",format(round(total_estimado, config$casasdecimais),nsmall = config$casasdecimais, big.mark = ".", decimal.mark = ",")),
              beneficio = 80,
              preco = preco_estimado,
              stringsAsFactors = F)
          } else{
            qtd_me <- ifelse(test = (floor(item$quantidade*.25) * preco_estimado) <= 80000,
                             yes = floor(item$quantidade * .25),
                             no = floor(80000/preco_estimado))
            qtd_ac <- item$quantidade - qtd_me
            
            ME <- data.frame(
              Item = item$descricao,
              Item2 = item$codigo,
              CATMAT = item$catmat,
              Quantidade = format(qtd_me, big.mark = ".",decimal.mark = ","),
              Unidade = item$unidade,
              `Preço Estimado` = paste0("R$ ",format(round(preco_estimado, config$casasdecimais),nsmall = config$casasdecimais, big.mark = ".", decimal.mark = ",")),
              `Total Estimado` = paste0("R$ ",format(round(qtd_me * preco_estimado, config$casasdecimais),nsmall = config$casasdecimais, big.mark = ".", decimal.mark = ",")),
              beneficio = 25,
              preco = preco_estimado,
              stringsAsFactors = F)
            
            AC <- data.frame(
              Item = item$descricao,
              Item2 = item$codigo,
              CATMAT = item$catmat,
              Quantidade = format(qtd_ac, big.mark = ".",decimal.mark = ","),
              Unidade = item$unidade,
              `Preço Estimado` = paste0("R$ ",
                                        format(round(preco_estimado, 
                                                     config$casasdecimais), nsmall = config$casasdecimais,
                                               big.mark = ".", 
                                               decimal.mark = ",")),
              `Total Estimado` = paste0("R$ ",
                                        format(
                                          round( qtd_ac * preco_estimado, config$casasdecimais), nsmall = config$casasdecimais,
                                          big.mark = ".", decimal.mark = ",")),
              beneficio = 0,
              preco = preco_estimado,
              stringsAsFactors = F)
            df_resumo <- rbind(AC,ME)
          }
        } else{#ok
          df_resumo <- data.frame(
            Item = item$descricao,
            Item2 = item$codigo,
            CATMAT = item$catmat,
            Quantidade = format(item$quantidade, big.mark = ".", decimal.mark = ","),       
            Unidade = item$unidade,
            `Preço Estimado` = paste0("R$ ",format(round(preco_estimado, config$casasdecimais),nsmall = config$casasdecimais, big.mark = ".", decimal.mark = ",")),
            `Total Estimado` = paste0("R$ ",format(round(total_estimado, config$casasdecimais),nsmall = config$casasdecimais ,big.mark = ".", decimal.mark = ",")),
            beneficio = 0,
            preco = preco_estimado)
        }
        return(df_resumo)
      }) %>% bind_rows()

      
      temp_file <- tempfile(fileext = ".RDS")
      saveRDS(dados_relatorio, temp_file)
      temp_file2 <- tempfile(fileext = ".RDS")
      saveRDS(dados_relatorio2, temp_file2)
      
      # Renderizar relatório
      output_dir <- choose.dir(default = "C:/Formador de Precos/resultados/") # Ou usar shinyFiles
      render(
        input = "C:\\Formador de Precos\\Template_T1.Rmd",
        output_file = paste0("Tabela_1_", input$csv1$name, ".html"),
        output_dir = output_dir,
        params = list(
          dados = temp_file,
          casas_decimais = input$casas_decimais
        ))

      render(
        input = "C:\\Formador de Precos\\Template_T2.Rmd",
        output_file = paste0("Tabela_2_", input$csv2$name, ".html"),
        output_dir = output_dir,
        params = list(
          dados = temp_file2,
          casas_decimais = input$casas_decimais
        )
      )
      
      showNotification("Relatório gerado com sucesso!", type = "message")
    })
    
  })
}
# Fecha server

shinyApp(ui, server)
