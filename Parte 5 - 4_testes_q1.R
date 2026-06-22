# ==============================================================================
# 4. TESTES DE HIPÓTESES F (QUESTÃO 1)
# ==============================================================================

# --- 1. Carregar pacote necessário ---
# (Se der erro, rode install.packages("car") no console primeiro)
library(car)

cat(">> [4] Executando Testes F da Q1...\n")

# Garantir que a média de escravizados está guardada para o Teste 3
media_escravos <- mean(base_final_completa$share_escravos, na.rm = TRUE)

# ==============================================================================
# TRAVA DE SEGURANÇA: Verificar se há cidades suficientes para os testes
# ==============================================================================
graus_liberdade <- df.residual(m4)

if (graus_liberdade <= 4) {
  
  cat("\n========================================================================\n")
  cat("⚠️ ALERTA DE DADOS INSUFICIENTES (residual df = 0) ⚠️\n")
  cat("O código parou com segurança para evitar erros vermelhos!\n\n")
  cat("Motivo: A sua base atual tem municípios a menos do que variáveis no modelo.\n")
  cat("Solução: Vá ao Passo 2 (no início do seu script), mude o nome do ficheiro\n")
  cat("de 'EXEMPLO' para 'COMPLETO', e corra o script inteiro desde o início.\n")
  cat("========================================================================\n\n")
  
  # Cria valores nulos para a tabela final não quebrar
  F1 <- F2 <- F3 <- NA
  pv1 <- pv2 <- pv3 <- NA
  
} else {
  
  # ============================================================
  # TESTE F 1 — Significância conjunta das dummies regionais
  # ============================================================
  
  cat("\n--- TESTE F 1: Significância conjunta das dummies regionais ---\n")
  cat("H0: d_norte = d_nordeste = d_sul = d_centro = 0\n")
  cat("H1: pelo menos uma dummy regional != 0\n\n")
  
  tf1 <- car::linearHypothesis(
    m4,
    c("d_norte = 0",
      "d_nordeste = 0",
      "d_sul = 0",
      "d_centro = 0")
  )
  print(tf1)
  
  F1  <- tf1$F[2]
  pv1 <- tf1$`Pr(>F)`[2]
  cat(sprintf("\nF calculado: %.4f | p-valor: %.6f\n", F1, pv1))
  cat(ifelse(pv1 < 0.05,
             "Conclusão: Rejeitamos H0 ao nível de 5%. As dummies regionais são conjuntamente significativas.\n",
             "Conclusão: Não rejeitamos H0 ao nível de 5%. Dummies regionais não são conjuntamente significativas.\n"))
  
  # ============================================================
  # TESTE F 2 — Igualdade entre regiões (duas restrições simultâneas)
  # ============================================================
  
  cat("\n--- TESTE F 2: Igualdade entre pares de regiões ---\n")
  cat("H0: d_norte = d_nordeste  E  d_sul = d_centro\n")
  cat("H1: pelo menos uma das igualdades não se sustenta\n\n")
  
  tf2 <- car::linearHypothesis(
    m4,
    c("d_norte - d_nordeste = 0",
      "d_sul - d_centro = 0")
  )
  print(tf2)
  
  F2  <- tf2$F[2]
  pv2 <- tf2$`Pr(>F)`[2]
  cat(sprintf("\nF calculado: %.4f | p-valor: %.6f\n", F2, pv2))
  cat(ifelse(pv2 < 0.05,
             "Conclusão: Rejeitamos H0. Norte != Nordeste ou Sul != Centro-Oeste.\n",
             "Conclusão: Não rejeitamos H0. Pares de regiões são estatisticamente iguais.\n"))
  
  # ============================================================
  # TESTE F 3 — Forma funcional e interação (três restrições)
  # ============================================================
  
  cat("\n--- TESTE F 3: Forma funcional e interação ---\n")
  cat("H0:\n")
  cat(sprintf("  (i)  share_escravos + 2*%.4f*share_escravos2 = 0  (derivada na média)\n", media_escravos))
  cat("  (ii) escr_x_nordeste = 0\n")
  cat("  (iii)share_escravos2 = 0\n")
  cat("H1: pelo menos uma das restrições é violada\n\n")
  
  restricao_i <- sprintf("share_escravos + %f * share_escravos2 = 0",
                         2 * media_escravos)
  
  tf3 <- car::linearHypothesis(
    m4,
    c(restricao_i,
      "escr_x_nordeste = 0",
      "share_escravos2 = 0")
  )
  print(tf3)
  
  F3  <- tf3$F[2]
  pv3 <- tf3$`Pr(>F)`[2]
  cat(sprintf("\nF calculado: %.4f | p-valor: %.6f\n", F3, pv3))
  cat(ifelse(pv3 < 0.05,
             "Conclusão: Rejeitamos H0. A forma funcional quadrática e/ou a interação são relevantes.\n",
             "Conclusão: Não rejeitamos H0. O modelo linear sem interação não seria rejeitado.\n"))
}

# ---- 2. Resumo em Tabela LaTeX (Salva no computador) ------------------------
resultados_testes <- data.frame(
  Teste = c(
    "F1: Dummies regionais conjuntas",
    "F2: Norte=Nordeste e Sul=C.Oeste",
    "F3: Forma funcional + interação"
  ),
  Restricoes = c(4, 2, 3),
  H0 = c(
    "d\\_norte=d\\_nordeste=d\\_sul=d\\_centro=0",
    "d\\_norte=d\\_nordeste; d\\_sul=d\\_centro",
    "Derivada na média=0; escr×NE=0; quad=0"
  ),
  F_calculado = round(c(F1, F2, F3), 4),
  p_valor     = format(c(pv1, pv2, pv3), digits=4, scientific=TRUE),
  Conclusao   = c(
    ifelse(!is.na(pv1) && pv1<0.05, "Rejeita H0", ifelse(is.na(pv1), "Sem Dados", "Não rejeita H0")),
    ifelse(!is.na(pv2) && pv2<0.05, "Rejeita H0", ifelse(is.na(pv2), "Sem Dados", "Não rejeita H0")),
    ifelse(!is.na(pv3) && pv3<0.05, "Rejeita H0", ifelse(is.na(pv3), "Sem Dados", "Não rejeita H0"))
  )
)

# Cria e salva o arquivo LaTeX na pasta atual
sink("tab_testes_q1.tex")
cat("\\begin{table}[htbp]\n")
cat("\\centering\n")
cat("\\caption{Testes de Hipótese --- Questão 1}\n")
cat("\\label{tab:testes_q1}\n")
cat("\\small\n")
cat("\\begin{tabular}{lcccc}\n")
cat("\\hline\\hline\n")
cat("Teste & Restrições & F calculado & p-valor & Conclusão \\\\\n")
cat("\\hline\n")
for (i in 1:nrow(resultados_testes)) {
  cat(sprintf("%s & %d & %s & %s & %s \\\\\n",
              resultados_testes$Teste[i],
              resultados_testes$Restricoes[i],
              ifelse(is.na(resultados_testes$F_calculado[i]), "NA", resultados_testes$F_calculado[i]),
              ifelse(is.na(resultados_testes$p_valor[i]), "NA", resultados_testes$p_valor[i]),
              resultados_testes$Conclusao[i]))
}
cat("\\hline\\hline\n")
cat("\\multicolumn{5}{l}{\\footnotesize Nível de significância: 5\\%.}\\\\\n")
cat("\\end{tabular}\n")
cat("\\end{table}\n")
sink()

cat("\nTabela de testes salva no seu computador como tab_testes_q1.tex\n")
cat(">> [4] Concluído.\n\n")
