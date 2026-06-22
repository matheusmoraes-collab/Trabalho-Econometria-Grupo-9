# ==============================================================================
# 3. ESTATÍSTICAS, GRÁFICOS E REGRESSÕES (Q1)
# ==============================================================================

# --- 1. Carregar Pacotes Necessários ---
# Se o R reclamar que não tem algum, rode install.packages("stargazer") ou ("ggplot2") no console
library(ggplot2)
library(stargazer)

cat(">> [3] Estimando regressões da Q1...\n")

# --- 2. Usar a base do passo anterior ---
# Em vez de ler de um ficheiro, puxamos a base que acabámos de criar:
base <- base_final_completa

# ---- 3.1 Estatísticas descritivas -------------------------------------------
vars_desc <- base %>%
  select(gini_2010, share_escravos, dist_tordesilhas_km,
         d_norte, d_nordeste, d_sul, d_centro) %>%
  as.data.frame()

# Tabela descritiva
stargazer::stargazer(
  vars_desc,
  type    = "text", # Deixei "text" para ler no console. Para usar no Word/LaTeX, ele salva o ficheiro abaixo.
  title   = "Estatísticas Descritivas --- Questão 1",
  digits  = 3,
  summary.stat = c("n", "mean", "sd", "min", "p25", "median", "p75", "max"),
  covariate.labels = c(
    "Índice de Gini (2010)",
    "Share escravizados 1872",
    "Dist. Tordesilhas (km)",
    "Dummy Norte",
    "Dummy Nordeste",
    "Dummy Sul",
    "Dummy Centro-Oeste"
  ),
  out = "tab_descritivas_q1.tex" # Salva direto na sua pasta atual
)
cat("Tabela descritiva salva no seu computador como tab_descritivas_q1.tex\n")

# ---- 3.2 Scatterplot --------------------------------------------------------
p_scatter <- ggplot(base, aes(x = share_escravos, y = gini_2010)) +
  geom_point(aes(color = regiao), alpha = 0.4, size = 1.2) +
  geom_smooth(method = "lm", se = TRUE, color = "black", linewidth = 0.8) +
  scale_color_brewer(palette = "Set1", name = "Região") +
  labs(
    title    = "Índice de Gini vs. Share de Escravizados em 1872",
    subtitle = "Cada ponto é um município brasileiro. Linha: MQO simples.",
    x        = "Share de escravizados na população total (1872)",
    y        = "Índice de Gini (2010)",
    caption  = "Fontes: ONU-ADH/Base dos Dados; Recenseamento 1872/UFMG."
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

# Imprime o gráfico na aba 'Plots' do RStudio para você ver
print(p_scatter)

# Salva o gráfico no seu computador (sem depender de pastas específicas)
ggsave("fig_scatter_q1.pdf", plot = p_scatter, width = 8, height = 5)
ggsave("fig_scatter_q1.png", plot = p_scatter, width = 8, height = 5, dpi = 150)
cat("Scatterplot salvo no seu computador como fig_scatter_q1.pdf / .png\n")

# ---- 3.3 Regressões ---------------------------------------------------------

# Modelo 1: simples (só a variável histórica)
m1 <- lm(gini_2010 ~ share_escravos, data = base)

# Modelo 2: com termo quadrático e controle geográfico
m2 <- lm(gini_2010 ~ share_escravos + share_escravos2 + dist_tordesilhas_km, data = base)

# Modelo 3: com dummies regionais
m3 <- lm(gini_2010 ~ share_escravos + share_escravos2 + dist_tordesilhas_km +
           d_norte + d_nordeste + d_sul + d_centro, data = base)

# Modelo 4 (principal): com interação escravos × Nordeste
m4 <- lm(gini_2010 ~ share_escravos + share_escravos2 + dist_tordesilhas_km +
           d_norte + d_nordeste + d_sul + d_centro + escr_x_nordeste, data = base)

# Ponto de inflexão do termo quadrático (modelo 4)
b1 <- coef(m4)["share_escravos"]
b2 <- coef(m4)["share_escravos2"]
inflexao <- -b1 / (2 * b2)
cat(sprintf("\nPonto de inflexão (share_escravos): %.4f\n", inflexao))
cat(sprintf("Média amostral (share_escravos): %.4f\n\n", mean(base$share_escravos, na.rm = TRUE)))

# Tabela de regressões
stargazer::stargazer(
  m1, m2, m3, m4,
  type  = "text", # Mostra o resultado diretamente no RStudio
  title = "Legados Históricos e Desigualdade Municipal: Estimativas por MQO",
  dep.var.labels  = "Índice de Gini (2010)",
  column.labels   = c("Simples", "+ Quadrático", "+ Regiões", "+ Interação"),
  covariate.labels = c(
    "Share escravizados (1872)",
    "Share escravizados² (1872)",
    "Dist. Tordesilhas (km)",
    "Dummy Norte",
    "Dummy Nordeste",
    "Dummy Sul",
    "Dummy Centro-Oeste",
    "Escravizados × Nordeste"
  ),
  omit.stat = c("f", "ser"),
  add.lines = list(
    c("Dummies regionais", "Não", "Não", "Sim", "Sim"),
    c("Interação hist. × região", "Não", "Não", "Não", "Sim"),
    c(sprintf("Ponto de inflexão (M4): %.4f", inflexao), rep("", 3), "")
  ),
  digits  = 4,
  notes   = "Notas: Estimativas por MQO. Categoria-base: Sudeste.",
  notes.align = "l",
  out = "tab_regressoes_q1.tex"
)
cat("Tabela de regressões salva no seu computador como tab_regressoes_q1.tex\n")

cat(">> [3] Concluído.\n\n")
