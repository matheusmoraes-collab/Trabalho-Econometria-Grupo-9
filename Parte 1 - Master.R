# ==============================================================================
# Trabalho de Econometria I - 2026.1
# Questão 1 e 2
# ==============================================================================

# --- 1. Carregar Pacotes ---
# (Se der erro de pacote não encontrado, rode install.packages("nome_do_pacote") no console)
library(dplyr)
library(readr)
library(stringr)
library(ggplot2)
library(stargazer) # Para as tabelas de regressão
library(car)       # Para testes de hipóteses

# ==============================================================================
# QUESTÃO 1: Legados Históricos (Corte Transversal)
# ==============================================================================

# --- 2. Caminhos dos Ficheiros ---
# Coloquei o caminho direto do seu computador, como se faz normalmente
caminho_censo <- "C:/Users/matheus.moraes/Downloads/econometria_entrega1_gini_censo1872/econometria/input/censo_1872.csv"
caminho_depara <- "C:/Users/matheus.moraes/Downloads/econometria_entrega1_gini_censo1872/econometria/input/de_para_censo1872_municipios_COMPLETO.csv" # Lembre-se de usar o completo!
caminho_gini <- "C:/Users/matheus.moraes/Downloads/econometria_entrega1_gini_censo1872/econometria/input/ginibr.csv"

# --- 3. Abrir as Bases de Dados ---
# Usando o locale latin1 para evitar erros com acentos (como vimos antes)
dados_1872 <- read_csv2(caminho_censo, locale = locale(encoding = "latin1"))
depara <- read_csv(caminho_depara, locale = locale(encoding = "latin1"))
dados_gini <- read_csv2(caminho_gini, skip = 2, locale = locale(encoding = "latin1"))

# --- 4. Limpeza e Cruzamento ---
# Junta o Censo de 1872 com os nomes atuais (De-Para)
hist_com_id <- left_join(dados_1872, depara, by = c("PrimeiroDeMunicipio" = "nome_censo"))

# Limpa a coluna do Gini para tirar os números do início do nome da cidade
dados_gini <- dados_gini %>%
  mutate(nome_limpo = str_remove(Município, "^[0-9]+\\s+"))

# Junta tudo na base final da Questão 1
base_q1 <- left_join(hist_com_id, dados_gini, by = c("nome_atual" = "nome_limpo"))

# Tira os NAs (municípios que não cruzaram) para a regressão rodar limpa
base_q1 <- base_q1 %>% filter(!is.na(nome_atual))

# --- 5. Análise da Questão 1 (Gráficos e Regressões) ---

# (Aqui você vai colocar o seu código do ggplot para o gráfico de dispersão)
# Exemplo: 
# plot(base_q1$Total_Almas, base_q1$`2010`) 

# (Aqui você vai colocar o seu código de regressão lm() e os testes)
# modelo1 <- lm(`2010` ~ Total_Almas, data = base_q1)
# summary(modelo1)


# ==============================================================================
# QUESTÃO 2: Curva de Phillips (Séries Temporais)
# ==============================================================================

# (Quando você baixar os dados de inflação e desemprego, é só continuar o script aqui para baixo)

# caminho_inflacao <- "C:/Users/matheus.moraes/Downloads/..."
# inflacao <- read_csv(caminho_inflacao)

# modelo_phillips <- lm(inflacao ~ desemprego, data = base_q2)
# summary(modelo_phillips)
