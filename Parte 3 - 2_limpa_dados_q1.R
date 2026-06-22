# ==============================================================================
# TRABALHO DE ECONOMETRIA - QUESTÃO 1 (CÓDIGO COMPLETO)
# ==============================================================================

# --- 1. Carregar Pacotes ---
library(dplyr)
library(readr)
library(stringr)
library(sf)
library(geobr) # Para o mapa

# --- 2. Caminhos dos Ficheiros ---
caminho_censo <- "C:/Users/matheus.moraes/Downloads/econometria_entrega1_gini_censo1872/econometria/input/censo_1872.csv"
caminho_gini <- "C:/Users/matheus.moraes/Downloads/econometria_entrega1_gini_censo1872/econometria/input/ginibr.csv"
# ATENÇÃO: Aqui está o nome correto que existe no seu computador (EXEMPLO)
caminho_depara <- "C:/Users/matheus.moraes/Downloads/econometria_entrega1_gini_censo1872/econometria/input/de_para_censo1872_municipios_EXEMPLO.csv"

# --- 3. Lendo as Bases de Dados ---
dados_1872 <- read_csv2(caminho_censo, locale = locale(encoding = "latin1"))
dados_gini <- read_csv2(caminho_gini, skip = 2, locale = locale(encoding = "latin1"))
depara <- read_csv(caminho_depara, locale = locale(encoding = "latin1"))

# --- 4. Preparar o Censo 1872 ---
censo_agrupado <- dados_1872 %>%
  group_by(PrimeiroDeProvincia, PrimeiroDeMunicipio) %>%
  summarise(
    pop_total = sum(Total_Almas, na.rm = TRUE),
    pop_escrava = sum(Total_Almas[grepl("Escrav", Sexo_Condicao, ignore.case = TRUE)], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(share_escravos = pop_escrava / pop_total)

# --- 5. Preparar o Gini ---
gini_limpo <- dados_gini %>%
  mutate(
    id_municipio = str_extract(Município, "^[0-9]+"),
    nome_limpo = str_remove(Município, "^[0-9]+\\s+")
  )

# --- 6. Distância até a Linha de Tordesilhas e Geometria ---
mapa_municipios <- read_municipality(year = 2010, showProgress = FALSE)
linha_tordesilhas <- st_sfc(st_linestring(rbind(c(-46.62, -34), c(-46.62, 6))), crs = 4326)
centroides <- st_centroid(mapa_municipios)
linha_tordesilhas <- st_transform(linha_tordesilhas, crs = st_crs(centroides))
centroides$dist_tordesilhas_km <- as.numeric(st_distance(centroides, linha_tordesilhas)) / 1000

dados_geo <- centroides %>% 
  st_drop_geometry() %>% 
  select(code_muni, abbrev_state, dist_tordesilhas_km) %>%
  mutate(id_municipio = as.character(code_muni))

# --- 7. Criar Dummies Regionais ---
dados_geo <- dados_geo %>%
  mutate(
    regiao = case_when(
      abbrev_state %in% c("AC", "AM", "AP", "PA", "RO", "RR", "TO") ~ "Norte",
      abbrev_state %in% c("AL", "BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE") ~ "Nordeste",
      abbrev_state %in% c("DF", "GO", "MS", "MT") ~ "Centro-Oeste",
      abbrev_state %in% c("PR", "RS", "SC") ~ "Sul",
      abbrev_state %in% c("ES", "MG", "RJ", "SP") ~ "Sudeste"
    ),
    d_norte = ifelse(regiao == "Norte", 1, 0),
    d_nordeste = ifelse(regiao == "Nordeste", 1, 0),
    d_sul = ifelse(regiao == "Sul", 1, 0),
    d_centro = ifelse(regiao == "Centro-Oeste", 1, 0)
  )

# --- 8. O GRANDE MERGE FINAL (Versão à Prova de Falhas) ---

# Passo A: Censo + De-Para
# Criamos uma "chave" de nome para o cruzamento: tudo minúsculo e sem espaços sobrando
censo_atual <- left_join(censo_agrupado, depara, by = c("PrimeiroDeMunicipio" = "nome_censo")) %>%
  mutate(chave_nome = str_to_lower(str_trim(nome_atual)))

# Passo B: Limpar a chave de nome no Gini também
gini_limpo <- gini_limpo %>%
  mutate(chave_nome = str_to_lower(str_trim(nome_limpo)))

# Juntar Censo com Gini usando a nossa chave limpa
base_q1 <- left_join(censo_atual, gini_limpo, by = "chave_nome")

# Passo C: Para cruzar com o mapa, usamos APENAS os 6 primeiros dígitos do código IBGE
base_q1 <- base_q1 %>% mutate(id_6 = str_sub(id_municipio, 1, 6))
dados_geo <- dados_geo %>% mutate(id_6 = str_sub(id_municipio, 1, 6))

# Juntar tudo com o mapa e as regiões
base_final <- left_join(base_q1, dados_geo, by = "id_6")

# --- 9. Base Final Pronta para a Regressão ---
base_final_completa <- base_final %>%
  filter(!is.na(`2010`), !is.na(share_escravos), !is.na(dist_tordesilhas_km)) %>%
  mutate(
    gini_2010 = as.numeric(`2010`), # Força o Gini a ser número por garantia
    share_escravos2 = share_escravos^2,
    escr_x_nordeste = share_escravos * d_nordeste
  )

# Imprime na tela quantas cidades sobreviveram!
cat("Número de cidades que sobreviveram na base final:", nrow(base_final_completa), "\n")
