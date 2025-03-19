# executar_app.R
# 1. Configuração precisa do Pandoc ---------------------------------------------
caminho_personalizado <- "C:/RStudio/resources/app/bin/quarto/bin/tools"

# Função de verificação aprimorada
set_pandoc_path <- function() {
  # Verifica se o arquivo EXE existe no caminho personalizado
  if (file.exists(file.path(caminho_personalizado, "pandoc.exe"))) {
    Sys.setenv(RSTUDIO_PANDOC = caminho_personalizado)
    return(TRUE)
  }
  return(FALSE)
}

set_pandoc_path()

# 2. Verificação detalhada ------------------------------------------------------
if (!set_pandoc_path()) {
  cat("\nERRO: Pandoc não encontrado!\n")
  cat("Verifique se o arquivo existe neste local exato:\n")
  cat("-> ", file.path(caminho_personalizado, "pandoc.exe"), "\n")
  
  # Diagnóstico interativo
  cat("\nTeste manual no R:\n")
  cat('file.exists("', file.path(caminho_personalizado, 'pandoc.exe")','\n', sep = ""))
      stop("Execute o teste acima no console do R para verificar o caminho")
}

# 3. Execução do aplicativo -----------------------------------------------------
cat(paste0("Pandoc encontrado com sucesso! Versão:", rmarkdown::pandoc_version(), "\n"))
shiny::runApp(
  appDir = "C:/Formador de Precos/",
  launch.browser = TRUE,
  port = 4321
)
  