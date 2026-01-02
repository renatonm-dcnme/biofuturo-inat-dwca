araras_counts <- users_by_city_period %>%
  filter(
    municipio_tag == "Araras",
    period_en %in% c("Before", "After")
  ) %>%
  arrange(period_en)

cat("### Araras – usuários únicos Before/After (observado) ###\n")
print(araras_counts)

obs <- araras_counts$n_users

mean_obs <- mean(obs)
exp_counts <- rep(mean_obs, 2)

araras_table <- tibble::tibble(
  period   = araras_counts$period_en,
  observed = obs,
  expected = exp_counts
)

cat("\n### Araras – tabela observados vs esperados (média) ###\n")
print(araras_table)

exp_prob <- exp_counts / sum(exp_counts)

chisq_araras <- chisq.test(x = obs, p = exp_prob)

cat("\n### Resultado do qui-quadrado (Araras, Before/After vs média) ###\n")
print(chisq_araras)

cat("\n### Valores esperados usados pelo qui-quadrado ###\n")
print(chisq_araras$expected)
