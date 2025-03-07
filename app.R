library(shiny)
library(shinyFiles)
library(rmarkdown)
library(dplyr)
library(httr)

ui <- fluidPage(
  titlePanel("Gerador de Relatórios de Formação de Preço"),
  sidebarLayout(
    sidebarPanel(
      h4("Configurações:"),
      shinyFilesButton("arquivo", "Selecione o CSV", "Selecione o arquivo CSV", multiple = FALSE),
      tags$br(),
      tags$small("Caminho do arquivo:"),
      verbatimTextOutput("caminho_arquivo", placeholder = TRUE),
      
      checkboxGroupInput(
        "tabelas", 
        "Tabelas a gerar:",
        choices = c("Tabela 1 - Crítica do Preço" = "tab1", 
                    "Tabela 2 - Preço Máximo" = "tab2")
      ),
      
      numericInput("casasdecimais", "Casas decimais:", value = 2, min = 1, max = 4),
      checkboxInput("beneficioME", "Aplicar benefício a microempresas?", value = FALSE),
      
      actionButton("gerar", "Gerar Relatórios", class = "btn-primary"),
      actionButton("reset", "Limpar Campos", class = "btn-danger")
    ),
    mainPanel(
      h4("Status:"),
      verbatimTextOutput("status"),
      h4("Links para Download:"),
      uiOutput("links")
    )
  )
)

server <- function(input, output, session) {
  volumes <- c(Home = "C:\\FP_Generator", "Disco C" = "C:/", getVolumes()())
  shinyFileChoose(input, "arquivo", roots = volumes, filetypes = c("csv"))
  
  valores <- reactiveValues(
    caminho_csv = NULL,
    html_gerados = NULL
  )
  
  # URLs dos arquivos no GitHub (RAW)
  github_template <- "https://raw.githubusercontent.com/Bruno44080/compras/main/template_unico.Rmd"
  github_funcoes <- "https://raw.githubusercontent.com/Bruno44080/compras/main/funcoes_tabelas.R"
  
  observeEvent(input$arquivo, {
    if (!is.integer(input$arquivo)) {
      selected_file <- parseFilePaths(volumes, input$arquivo)
      if (nrow(selected_file) > 0) {
        valores$caminho_csv <- as.character(selected_file$datapath)
      }
    }
  })
  
  output$caminho_arquivo <- renderText({
    if (is.null(valores$caminho_csv)) "Nenhum arquivo selecionado" else valores$caminho_csv
  })
  
  observeEvent(input$gerar, {
    tryCatch({
      req(valores$caminho_csv)
      if (!file.exists(valores$caminho_csv)) stop("Arquivo CSV não encontrado!")
      
      withProgress(message = 'Processando relatórios...', value = 0, {
        # Configurar ambiente temporário
        temp_dir <- tempdir()
        addResourcePath("relatorios", temp_dir)
        
        html_files <- c()
        total_tabelas <- length(input$tabelas)
        
        for (i in seq_along(input$tabelas)) {
          tabela <- input$tabelas[i]
          incProgress(1/total_tabelas, detail = paste("Gerando", tabela))
          
          # Baixar template
          temp_rmd <- tempfile(fileext = ".Rmd")
          download.file(github_template, temp_rmd, quiet = TRUE, mode = "wb")
          
          # Nome do arquivo de saída
          output_file <- paste0(
            "Tabela", gsub("tab", "", tabela), "_",
            tools::file_path_sans_ext(basename(valores$caminho_csv)), 
            ".html"
          )
          
          # Renderizar com parâmetros
          render(
            input = temp_rmd,
            output_file = output_file,
            output_dir = temp_dir,
            params = list(
              dados = valores$caminho_csv,
              casas = input$casasdecimais,
              beneficio = input$beneficioME,
              tabela = tabela
            ),
            envir = new.env(),
            quiet = TRUE
          )
          
          html_files <- c(html_files, file.path("relatorios", output_file))
        }
        
        valores$html_gerados <- html_files
      })
      
      output$status <- renderText("Relatórios gerados com sucesso!")
      
    }, error = function(e) {
      output$status <- renderText(paste("ERRO:", e$message)) ### exibir parametros
    })
  })
  
  observeEvent(input$reset, {
    valores$caminho_csv <- NULL
    valores$html_gerados <- NULL
    updateCheckboxGroupInput(session, "tabelas", selected = character(0))
    updateNumericInput(session, "casasdecimais", value = 2)
    updateCheckboxInput(session, "beneficioME", value = FALSE)
    output$status <- renderText("")
    output$links <- renderUI({})
  })
  
  output$links <- renderUI({
    req(valores$html_gerados)
    tagList(
      lapply(valores$html_gerados, function(f) {
        tags$a(
          basename(f), 
          href = f, 
          download = basename(f), 
          class = "btn btn-primary",
          style = "margin: 5px;",
          target = "_blank"
        )
      })
    )
  })
}

shinyApp(ui, server)