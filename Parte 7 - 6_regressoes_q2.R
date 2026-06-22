# ==============================================================================
# 6. REGRESSÕES E TESTES DA CURVA DE PHILLIPS (Q2)
# ==============================================================================

# --- 1. Carregar pacotes necessários ---
# (Se der erro, rode install.packages(c("lmtest", "sandwich", "stargazer")) no console)
library(lmtest)
library(sandwich)
library(stargazer)
library(ggplot2) # Já carregado antes, mas garantindo

cat(">> [6] Estimando Curva de Phillips (Q2)...\n")

# Puxa a base gerada no passo anterior. Se não estiver na memória, tenta ler o arquivo local.
if (!exists("base_q2")) {
  base_q2 <- readRDS("base_q2.rds")
}
base <- base_q2

# ---- 6.1 Curva de Phillips estática ----------------------------------------
# pi_t = alfa + beta*u_t + erro_t
cp_estatica <- lm(ipca ~ desemprego, data = base)

cat("\n--- Modelo 1: Curva de Phillips estática ---\n")
print(summary(cp_estatica))

sinal_beta <- ifelse(coef(cp_estatica)["desemprego"] < 0,
                     "NEGATIVO (compatível com trade-off inflação-desemprego)",
                     "POSITIVO (inconsistente com a teoria; pode indicar problema de especificação)")
cat(sprintf("\nSinal de beta: %s\n", sinal_beta))

# ---- 6.2 Testes de autocorrelação (modelo estático) ------------------------
cat("\n--- Testes de autocorrelação (modelo estático) ---\n")

# Durbin-Watson
cat("\nH0 (DW): sem autocorrelação de 1ª ordem nos resíduos\n")
cat("H1 (DW): autocorrelação de 1ª ordem positiva\n")
dw_stat <- lmtest::dwtest(cp_estatica)
print(dw_stat)

# Breusch-Godfrey (até 4 defasagens para dados trimestrais)
cat("\nH0 (BG): sem autocorrelação até ordem p=4\n")
cat("H1 (BG): existe autocorrelação em alguma ordem até 4\n")
bg_stat <- lmtest::bgtest(cp_estatica, order = 4)
print(bg_stat)

# Ljung-Box nos resíduos
cat("\nH0 (Ljung-Box): resíduos são ruído branco (sem autocorrelação)\n")
cat("H1 (Ljung-Box): existe autocorrelação\n")
lb_stat <- Box.test(residuals(cp_estatica), lag = 4, type = "Ljung-Box")
print(lb_stat)

# --- Gráfico dos resíduos ---
resid_df <- data.frame(
  data    = base$data,
  residuo = residuals(cp_estatica),
  predito = fitted(cp_estatica)
)

p_res <- ggplot(resid_df, aes(x = data, y = residuo)) +
  geom_line(color = "#1f77b4", linewidth = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 0.8) +
  labs(title = "Resíduos — Curva de Phillips Estática",
       x = NULL, y = "Resíduo") +
  theme_minimal(base_size = 11)

ggsave("fig_residuos_q2.pdf", plot = p_res, width = 8, height = 3.5)

# --- Correlograma (ACF) ---
pdf("fig_acf_q2.pdf", width = 8, height = 4)
acf(residuals(cp_estatica), main = "ACF dos Resíduos — Curva de Phillips Estática")
dev.off()

cat("\nGráficos de resíduos salvos com sucesso na pasta atual.\n")

# ---- 6.3 Modelo dinâmico ---------------------------------------------------
# pi_t = alfa + beta*u_t + gamma*pi_{t-1} + delta*E_t[pi_{t+1}] + erro_t

# Verifica se expectativa de inflação está disponível na base
tem_focus <- !all(is.na(base$expect_inflacao))

if (tem_focus) {
  cp_dinamica <- lm(ipca ~ desemprego + ipca_lag1 + expect_inflacao, data = base)
} else {
  warning("Série Focus não disponível. Estimando modelo dinâmico sem expectativas.")
  cp_dinamica <- lm(ipca ~ desemprego + ipca_lag1, data = base)
}

cat("\n--- Modelo 2: Curva de Phillips dinâmica ---\n")
print(summary(cp_dinamica))

# Efeito de longo prazo do desemprego sobre a inflação
beta_u <- coef(cp_dinamica)["desemprego"]
gamma  <- coef(cp_dinamica)["ipca_lag1"]
elp    <- beta_u / (1 - gamma)

cat(sprintf("\nEfeito de curto prazo (beta):  %.4f\n", beta_u))
cat(sprintf("Coef. inércia inflacionária (gamma): %.4f\n", gamma))
cat(sprintf("Efeito de LONGO PRAZO (beta/(1-gamma)): %.4f\n", elp))

# Reavalia autocorrelação no modelo dinâmico
cat("\nAutocorrelação no modelo dinâmico:\n")
bg_din <- lmtest::bgtest(cp_dinamica, order = 4)
print(bg_din)

# ---- 6.4 Erros Robustos HAC (Newey-West) -----------------------------------
cat("\n--- Erros HAC (Newey-West) ---\n")

vcov_hac_est <- sandwich::NeweyWest(cp_estatica)
vcov_hac_din <- sandwich::NeweyWest(cp_dinamica)

cat("\nModelo Estático — MQO usual:\n")
print(lmtest::coeftest(cp_estatica))
cat("\nModelo Estático — Erros HAC (Newey-West):\n")
print(lmtest::coeftest(cp_estatica, vcov = vcov_hac_est))

cat("\nModelo Dinâmico — MQO usual:\n")
print(lmtest::coeftest(cp_dinamica))
cat("\nModelo Dinâmico — Erros HAC (Newey-West):\n")
print(lmtest::coeftest(cp_dinamica, vcov = vcov_hac_din))

# ---- 6.5 Tabela LaTeX (Salva no computador) --------------------------------

# Prepara Erros-padrão para o stargazer
se_est_ols <- sqrt(diag(vcov(cp_estatica)))
se_est_hac <- sqrt(diag(vcov_hac_est))
se_din_ols <- sqrt(diag(vcov(cp_dinamica)))
se_din_hac <- sqrt(diag(vcov_hac_din))

stargazer(
  cp_estatica, cp_estatica, cp_dinamica, cp_dinamica,
  type  = "latex",
  title = "Curva de Phillips Brasileira (Trimestral)",
  label = "tab:phillips",
  dep.var.labels  = "IPCA acumulado trimestral (\\%)",
  column.labels   = c("Estática (OLS)", "Estática (HAC)",
                      "Dinâmica (OLS)", "Dinâmica (HAC)"),
  se = list(se_est_ols, se_est_hac, se_din_ols, se_din_hac),
  covariate.labels = c(
    "Desemprego (\\%)",
    "IPCA defasado ($\\pi_{t-1}$)",
    if (tem_focus) "Expectativa inflação 12m (Focus)" else NULL,
    "Constante"
  ),
  omit.stat = c("f","ser"),
  add.lines = list(
    c("Erros-padrão", "OLS", "HAC", "OLS", "HAC"),
    c("ELP desemprego [$\\beta/(1-\\gamma)$]", "—", "—",
      sprintf("%.4f", elp), sprintf("%.4f", elp))
  ),
  digits = 4,
  notes = paste0(
    "Notas: Erros HAC estimados pelo método de Newey-West. ",
    "ELP = efeito de longo prazo. ",
    "Período: ", format(min(base$data, na.rm=TRUE), "%Y:Q%q"), " a ",
    format(max(base$data, na.rm=TRUE), "%Y:Q%q"), "."
  ),
  notes.align = "l",
  out = "tab_phillips_q2.tex"
)

cat("\nTabela da Curva de Phillips salva na pasta atual como tab_phillips_q2.tex\n")

# Salva os modelos para caso você precise carregar no futuro
saveRDS(list(
  cp_estatica  = cp_estatica,
  cp_dinamica  = cp_dinamica,
  elp          = elp,
  beta_u       = beta_u,
  gamma        = gamma,
  dw           = dw_stat,
  bg_estatica  = bg_stat,
  bg_dinamica  = bg_din,
  lb           = lb_stat
), "modelos_q2.rds")

cat(">> [6] Concluído com sucesso.\n\n")
