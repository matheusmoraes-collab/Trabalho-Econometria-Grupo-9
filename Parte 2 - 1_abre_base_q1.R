# ==============================================================================
# LEITURA DE BASES (Versão Simplificada)
# ==============================================================================

# --- 1. Carregar Pacotes ---
library(dplyr)
library(readr)
library(geobr) # Pacote para baixar os mapas e fronteiras do Brasil

# --- 2. Caminhos dos Arquivos ---
caminho_censo <- "C:/Users/matheus.moraes/Downloads/econometria_entrega1_gini_censo1872/econometria/input/censo_1872.csv"
caminho_gini <- "C:/Users/matheus.moraes/Downloads/econometria_entrega1_gini_censo1872/econometria/input/ginibr.csv"

# --- 3. Lendo a Base do Gini ---
# Lendo o CSV do Gini que você já baixou manualmente (ignorando as 2 primeiras linhas)
dados_gini <- read_csv2(caminho_gini, skip = 2, locale = locale(encoding = "latin1"))

# --- 4. Lendo o Censo 1872 ---
# Lendo direto com ponto e vírgula e consertando os acentos
dados_1872 <- read_csv2(caminho_censo, locale = locale(encoding = "latin1"))

# --- 5. Baixando a Geometria (Mapa) dos Municípios ---
# A função read_municipality vai à internet trazer o formato das cidades em 2010
mapa_municipios <- read_municipality(year = 2010, showProgress = FALSE)

# (Opcional) Ajustando o código do município no mapa para ter 7 dígitos e cruzar depois
mapa_municipios <- mapa_municipios %>%
  mutate(id_municipio = as.character(code_muni))

# Verificando se deu certo
head(mapa_municipios)
