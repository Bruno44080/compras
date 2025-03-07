install_and_load <- function(packages) {
  for(pkg in packages) {
    if(!require(pkg, character.only = TRUE, quietly = TRUE)) {
      message("Instalando pacote: ", pkg)
      install.packages(pkg, dependencies = TRUE)
      library(pkg, character.only = TRUE)
    }
  }
}

# Lista de pacotes necessários
required_packages <- c(
  "shiny", "shinyFiles", "rmarkdown", "dplyr", "httr", 
  "kableExtra", "knitr", "htmltools", "fs", "jsonlite"
)

# Instala/atualiza pacotes
install_and_load(required_packages)

# Verificação crítica do Pandoc
if(!rmarkdown::pandoc_available()) {
  message("\nInstalando Pandoc... (requer privilégios de administrador)")
  tryCatch({
    rmarkdown::install_pandoc()
    Sys.setenv(RSTUDIO_PANDOC = rmarkdown::pandoc_exec())
  }, error = function(e) {
    stop("\nFalha na instalação automática do Pandoc.\n",
         "1. Baixe manualmente em: https://github.com/jgm/pandoc/releases\n",
         "2. Execute como administrador e marque 'Add to PATH'")
  })
}

# Configurações
temp_dir <- tempfile(pattern = "shinyapp_")
dir.create(temp_dir)
message("\nDiretório temporário criado: ", temp_dir)

# Lista de arquivos necessários
github_repo <- "https://raw.githubusercontent.com/Bruno44080/compras/main/"
files_needed <- c(
  "app.R",
  "funcoes_tabelas.R",
  "template_unico.Rmd"
)

# Download automático dos arquivos
message("\nBaixando arquivos do GitHub...")
tryCatch({
  for(file in files_needed) {
    file_url <- paste0(github_repo, file)
    dest_file <- file.path(temp_dir, file)
    
    if(!file.exists(dirname(dest_file))) {
      dir.create(dirname(dest_file), recursive = TRUE)
    }
    
    response <- GET(
      file_url,
      write_disk(dest_file, overwrite = TRUE),
      add_headers("User-Agent" = "R"),
      timeout(15)
    
    if(http_error(response)) {
      stop("Falha ao baixar: ", file)
    }
    message("Sucesso: ", file)
  }
}, error = function(e) {
  unlink(temp_dir, recursive = TRUE)
  stop("Erro crítico:\n", e$message, "\nDiretório limpo: ", temp_dir)
})

# Execução do aplicativo
message("\nIniciando aplicativo Shiny...")
tryCatch({
  shiny::runApp(
    appDir = temp_dir,
    launch.browser = TRUE,
    quiet = TRUE,
    display.mode = "normal"
  )
}, finally = {
  message("\nLimpando diretório temporário...")
  unlink(temp_dir, recursive = TRUE)
})
