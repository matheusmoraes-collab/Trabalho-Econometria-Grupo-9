# ==============================================================================
# 5. SÉRIES TEMPORAIS Q2: Download, Organização e Visualização
# ==============================================================================

# --- 1. Carregar pacotes necessários ---
# Se algum destes der erro, rode no console: install.packages(c("tidyverse", "lubridate", "rbcb"))
library(tidyverse)
library(lubridate)
library(rbcb)

cat(">> [5] Baixando séries da Q2 (Curva de Phillips)...\n")

# Período de análise
DATA_INI <- "2012-01-01"
DATA_FIM  <- "2024-12-31"

# ---- 5.1 Função para baixar via rbcb (SGS do Banco Central) ---------------

baixar_serie <- function(codigo, nome_arquivo_fallback, nome_var) {
  # Tenta baixar direto da internet via rbcb
  resultado <- tryCatch({
    s <- rbcb::get_series(codigo,
                          start_date = DATA_INI,
                          end_date   = DATA_FIM)
    names(s) <- c("data", nome_var)
    cat(sprintf("  [OK via Banco Central] %s: %d observações\n", nome_var, nrow(s)))
    s
  }, error = function(e) {
    # Se falhar (ex: sem internet), tenta ler arquivo local na pasta atual
    cat(sprintf("  [Falha na internet] Lendo arquivo local %s...\n", nome_arquivo_fallback))
    if (!file.exists(nome_arquivo_fallback)) {
      stop(sprintf(
        "Arquivo %s não encontrado na sua pasta!\nVerifique sua conexão ou baixe do SGS (código %d).",
        nome_arquivo_fallback, codigo))
    }
    s <- read_csv(nome_arquivo_fallback, col_types = cols())
    if (!nome_var %in% names(s)) {
      if (ncol(s) == 2) names(s) <- c("data", nome_var)
    }
    s$data <- as.Date(s$data)
    s
  })
  resultado
}

# SGS 433   = IPCA variação mensal (%)
# SGS 24369 = Desemprego PNAD Contínua trimestral (%)
# SGS 13521 = Focus: mediana expectativa IPCA 12 meses (%)

ipca_mensal   <- baixar_serie(433,   "ipca_mensal.csv",        "ipca_mensal")
desemprego_t  <- baixar_serie(24369, "desemprego_trimestral.csv", "desemprego")
focus_mensal  <- baixar_serie(13521, "focus_trimestral.csv",    "expect_inflacao")

# ---- 5.2 Agregar IPCA e Focus para Trimestral -----------------------------

# IPCA: acumulado no trimestre
ipca_trim <- ipca_mensal %>%
  mutate(
    ano       = year(data),
    trimestre = quarter(data)
  ) %>%
  group_by(ano, trimestre) %>%
  # Acumulado trimestral pelo método de capitalização composta
  summarise(
    ipca = prod(1 + ipca_mensal / 100) * 100 - 100,
    .groups = "drop"
  ) %>%
  mutate(data = yq(paste0(ano, ":Q", trimestre)))

# Focus: média do trimestre
focus_trim <- focus_mensal %>%
  mutate(
    ano       = year(data),
    trimestre = quarter(data)
  ) %>%
  group_by(ano, trimestre) %>%
  summarise(
    expect_inflacao = mean(expect_inflacao, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(data = yq(paste0(ano, ":Q", trimestre)))

# ---- 5.3 Merge das séries trimestrais --------------------------------------

# Desemprego já é trimestral
if (!"data" %in% names(desemprego_t)) {
  desemprego_t <- desemprego_t %>%
    mutate(data = as.Date(data))
}

desemprego_trim <- desemprego_t %>%
  mutate(
    ano       = year(data),
    trimestre = quarter(data)
  ) %>%
  group_by(ano, trimestre) %>%
  summarise(desemprego = mean(desemprego, na.rm=TRUE), .groups="drop") %>%
  mutate(data = yq(paste0(ano, ":Q", trimestre)))

# Juntando tudo num Painel (Base Q2)
base_q2 <- ipca_trim %>%
  left_join(desemprego_trim, by = c("ano","trimestre","data")) %>%
  left_join(focus_trim,      by = c("ano","trimestre","data")) %>%
  arrange(data) %>%
  # Criando defasagens (lags)
  mutate(
    ipca_lag1 = lag(ipca, 1),
    ipca_lag2 = lag(ipca, 2)
  ) %>%
  filter(!is.na(ipca), !is.na(desemprego))

cat(sprintf("\nBase Q2 final: %d trimestres (%s a %s)\n",
            nrow(base_q2),
            format(min(base_q2$data), "%Y-T%q"),
            format(max(base_q2$data), "%Y-T%q")))

# ---- 5.4 Gráfico das séries (Salvo direto no computador) -------------------

# Prepara os dados para o gráfico usando explicitamente o recode do dplyr
base_longa <- base_q2 %>%
  select(data, ipca, desemprego, expect_inflacao) %>%
  pivot_longer(-data, names_to = "serie", values_to = "valor") %>%
  mutate(serie = dplyr::recode(serie,
                               "ipca"            = "IPCA (% acum. trim.)",
                               "desemprego"      = "Desemprego (%)",
                               "expect_inflacao" = "Expectativa inflação 12m (Focus, %)"
  ))

p_series <- ggplot(base_longa, aes(x = data, y = valor, color = serie)) +
  geom_line(linewidth = 0.8) +
  facet_wrap(~serie, scales = "free_y", ncol = 1) +
  scale_color_manual(values = c("#1f77b4","#d62728","#2ca02c")) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(
    title    = "Séries Macroeconômicas Trimestrais — Brasil",
    subtitle = paste0("Período: ", format(min(base_q2$data),"%Y"), " a ",
                      format(max(base_q2$data),"%Y")),
    x        = NULL,
    y        = "Valor (%)",
    caption  = "Fontes: IPCA e Focus — SGS/Banco Central; Desemprego — PNAD Contínua."
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "none",
        strip.text = element_text(face = "bold"))

# Salva na pasta atual
ggsave("fig_series_q2.pdf", plot = p_series, width = 9, height = 7)
ggsave("fig_series_q2.png", plot = p_series, width = 9, height = 7, dpi = 150)
saveRDS(base_q2, "base_q2.rds")

cat("\nGráficos salvos com sucesso na sua pasta (fig_series_q2.pdf e .png)\n")
cat(">> [5] Concluído.\n\n")
