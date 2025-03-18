# FP Generator2000 - Documento de Formalização de Preços

## 📋 Pré-requisitos
- Conta Google (para acesso ao Google Drive)

## 🚀 Como Utilizar

### 📁 Trabalhando no Google Drive
1. **Crie uma pasta de trabalho** na raiz do seu Google Drive
2. **Acesse a planilha modelo** e faça uma cópia:
   - `Arquivo > Fazer uma cópia`
   - Selecione a pasta criada
   - Clique em `Fazer uma cópia`

### ⚙️ Instalação do Ambiente
1. **Instale o R** ([Download Windows](https://cran.r-project.org/bin/windows/base/))
   - Execute o instalador com configurações padrão

2. **Instale o RStudio** ([Download](https://posit.co/download/rstudio-desktop/)):
   - Na seção *Zip/Tarballs*, baixe `RStudio-2024.12.1-563.zip` ou versão superior
   - Extraia o conteúdo para `C:\`

3. **Configure o FP Generator2000**:
- Baixe o repositório (Botão `<Code> > Download ZIP`)
- Extraia o conteúdo para `C:\`
Se certifique se exista seguinte estrutura de pastas

   ```bash
   C:/
   └── FP_Generator2000/
       ├── dados/
       └── resultados/

## 📊 Preparação de Dados

### 📄 Primeiro Arquivo (Fontes)
1. Na planilha de pesquisa de preços:
- Selecione o conteúdo das colunas **Código da Fonte** e **Descrição** (sem o cabeçalho)
- Copie (`Ctrl + C`)
2. Crie nova planilha:
- Cole com `Colar Especial > Só os valores` (`Ctrl + Shift + V`)
- Exporte como CSV: `Arquivo > Baixar > Valores Separados por Vírgulas`
- Salve em `C:\FP_Generator2000\dados` com nome `fontes.csv`

### 📄 Segundo Arquivo (Itens)
**ATENÇÃO: Mantenha esta ordem de colunas:**
| Item nº | Código Elotech | CATMAT/CATSER | Quantidade | Unidade | Especificação | Preço 1 | Fonte 1 | ... |
|---------|----------------|---------------|------------|---------|---------------|---------|---------|-----|

**Regras de Formatação**:
- ❌ Não incluir "R$" ou "*"
- 🔢 Formato numérico:
- Separador de milhar: `.` (ex: 1.234)
- Separador decimal: `,` (ex: 1.234,56)
- ⏳ Para preços históricos (>1 ano):  
`1.234,56 - Pregão 123/2022`

**Exportação**:
1. Selecione dados **sem cabeçalho**
2. Copie (`Ctrl + C`)
3. Crie nova planilha:
- Cole com `Colar Especial > Dados transpostos`
- Limpe formatação: `Formatar > Limpar Formatação` (`Ctrl + \`)
- Exporte como CSV para `C:\FP_Generator2000\dados` (ex: `itens.csv`)

## ▶️ Execução
1. Abra o RStudio
2. Configure o diretório de trabalho:
```r
setwd("C:/FP_Generator2000")
📌 Notas Importantes
Verifique os arquivos CSV gerados antes da execução

Resultados serão salvos em C:\FP_Generator2000\resultados

Para atualizações, refaça o download do repositório

✉️ Suporte: contato@empresa.com.br | (41) 99999-9999
