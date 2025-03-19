# Formador de Precos

## 📋 Pré-requisitos
- Conta Google (para acesso ao Google Drive)

## 🚀 Como Utilizar

### 📁 Trabalhando no Google Drive
1. **Crie uma pasta de trabalho**, igual fazia na pasta compartilhada
2. **Acesse a  [Planilha Modelo](https://docs.google.com/spreadsheets/d/1Qu5qovPixLVfbQDbVl9Vsqqh1DiX1DtdwEygoPVsxWc/edit?usp=sharing)** e faça uma cópia:
   - `Arquivo > Fazer uma cópia`
   - Selecione a pasta criada
   - Clique em `Fazer uma cópia`

#### Pasta de trabalho já existente na pasta compartilhada
   - Arraste a pasta em seu Google Drive

**Regras de Formatação e Preenchimento**:
- ❌ Não incluir "R$" ou "*"
- 🔢 Formato numérico:
   - Separador de milhar: `.` (ex: 1.234)
   - Separador decimal: `,` (ex: 1.234,56)
   - Preços da Administração:
      - ⏳ Para preços históricos (>1 ano):  `1.234,56 - Pregão 123/2022`
      - Sempre preencher com o valor realinhado.


### ⚙️ Instalação do Ambiente
1. **Instale o R** ([Download Windows](https://cran.r-project.org/bin/windows/base/))
   - Execute o instalador com configurações padrão

2. **Instale o RStudio** ([Download](https://posit.co/download/rstudio-desktop/)):
   - Na seção *Zip/Tarballs*, baixe `RStudio-2024.12.1-563.zip` ou versão superior
   - Extraia o conteúdo para `C:\`

3. **Configure o Formador de Preços**:
- Baixe o repositório (Botão `<Code> > Download ZIP`)
- Extraia o a pasta `Formador de Preços` para `C:\`
Se certifique se exista seguinte estrutura de pastas

   ```bash
   C:/
   └── Formador de Precos/
       ├── dados/
       └── resultados/

## 📊 Preparação de Dados

### 📄 Primeiro Arquivo (Fontes)
1. Na planilha de pesquisa de preços:
- Selecione o conteúdo das colunas **Código da Fonte** e **Descrição** (sem o cabeçalho)
- Copie (`Ctrl + C`)

**Exemplo:**

| Código |Fontes do Mercado Pesquisadas |
| :---: | :---: |
| A | Empresa 1 |
| B | Empresa 2 |

Copiar:


| A | Empresa 1 |
| :---: | :---: |
| B | Empresa 2 |


2. Crie nova planilha:
- Cole com `Colar Especial > Só os valores` (`Ctrl + Shift + V`)
- Exporte como CSV: `Arquivo > Baixar > Valores Separados por Vírgulas`
- Salve em `C:\FP_Generator2000\dados` com nome `fontes.csv`

### 📄 Segundo Arquivo (Itens)
**ATENÇÃO: Mantenha esta ordem de colunas:**

| Item nº | Código Elotech | CATMAT/CATSER | Quantidade | Unidade | Especificação | Preço 1 | Fonte 1 | ... |
|:---------:|:----------------:|:---------------:|:------------:|:---------:|:---------------:|:---------:|:---------:|:-----:|
| 1 | 123 | 321 | 50 | KG | Goiaba vermelha | 9,62 | A | ... |
| 2 | 124 | 322 | 50 | KG | Goiaba branca | 162,1 | B | ... |

**Exportação**:
1. Selecione dados **sem cabeçalho**
2. Copie (`Ctrl + C`)

**Exemplo:**

| 1 | 123 | 321 | 50 | KG | Goiaba vermelha | 9,62 | A | ... |
|:---------:|:----------------:|:---------------:|:------------:|:---------:|:---------------:|:---------:|:---------:|:-----:|
| 2 | 124 | 322 | 50 | KG | Goiaba branca | 162,1 | B | ... |

3. Crie nova planilha:
- Cole com `Colar Especial > Dados transpostos`
- Limpe formatação: `Formatar > Limpar Formatação` (`Ctrl + \`)
- Exporte como CSV para `C:\Formador de Precos\dados` (ex: `itens.csv`)

## ▶️ Execução
1. Execute o arquivo `Iniciar Formador.bat`

✉️ Suporte: (44) 3127-7072
