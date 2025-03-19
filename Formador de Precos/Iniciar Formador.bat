@echo off
cd /d "C:\Formador de Precos" || (
    echo Erro: Diretório "C:\Formador de Precos" não encontrado.
    pause
    exit /b 1
)

setlocal enabledelayedexpansion

:: --------------------------------------------
:: Passo 1: Procurar a versão mais recente do R
:: --------------------------------------------

:: Verifica em dois locais comuns de instalação (usuário e sistema)
set "r_dirs=%LOCALAPPDATA%\Programs\R;C:\Program Files\R"

for %%d in (%r_dirs%) do (
    if exist "%%d\" (
        :: Lista diretórios R-* em ordem decrescente (mais recente primeiro)
        for /f "delims=" %%i in ('dir /b /ad /o-n "%%d\R-*" 2^>nul') do (
            set "r_path=%%d\%%i\bin\Rscript.exe"
            if exist "!r_path!" (
                echo R encontrado: "!r_path!"
                :: Executa o Rscript
                "!r_path!" executar_app.R
                pause
                exit /b 0
            )
        )
    )
)

:: Se nenhum R foi encontrado
echo Erro: Rscript.exe não foi encontrado.
echo Verifique se o R está instalado nos locais:
echo - %LOCALAPPDATA%\Programs\R
echo - C:\Program Files\R
pause
exit /b 1