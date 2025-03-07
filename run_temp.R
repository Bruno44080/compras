library(httr)

# Configurações
repo_url <- "https://github.com/Bruno44080/compras"
branch <- "main"
files_needed <- c("app.R", "funcoes_tabelas.R", "template_unico.Rmd")

# Criar diretório temporário
temp_dir <- tempfile()
dir.create(temp_dir)

# Download dos arquivos
for (file in files_needed) {
  raw_url <- paste0("https://raw.githubusercontent.com/Bruno44080/compras/", branch, "/", file)
  dest_file <- file.path(temp_dir, file)
  
  tryCatch({
    GET(raw_url, write_disk(dest_file, overwrite = TRUE))
    message("Arquivo baixado: ", file)
  }, error = function(e) {
    stop("Falha ao baixar: ", file, "\nErro: ", e$message)
  })
}

# Executar app
shiny::runApp(temp_dir, launch.browser = TRUE)
