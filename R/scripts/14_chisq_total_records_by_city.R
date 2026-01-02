suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tibble)
})

mestre <- readr::read_csv(
  "data/processed/02_mestre_filtrada_bioblitz.csv",
  show_col_types = FALSE
) %>%
  mutate(
    observed_on   = as.Date(observed_on),
    municipio_tag = as.character(municipio_tag),
    periodo       = as.character(periodo)
  )

cat("### Dimensão da base mestre ###\n")
print(dim(mestre))

records_by_city_period <- mestre %>%
  mutate(

    period_en = dplyr::recode(
      periodo,
      "Antes"   = "Before",
      "Durante" = "During",
      "Depois"  = "After"
    ),
    period_en = factor(period_en, levels = c("Before", "During", "After")),
    municipio_tag = factor(
      municipio_tag,
      levels = c("Araras",
                 "Campina do Monte Alegre",
                 "São Carlos",
                 "Sorocaba")
    )
  ) %>%
  group_by(municipio_tag, period_en) %>%
  summarise(
    n_records = n(),
    .groups   = "drop"
  )

cat("\n### Total de registros por município × período (tabela base do gráfico) ###\n")
print(records_by_city_period)

records_ba <- records_by_city_period %>%
  filter(period_en %in% c("Before", "After")) %>%
  arrange(municipio_tag, period_en)

cat("\n### Total de registros (Before/After) por município – base para o qui-quadrado ###\n")
print(records_ba)

cities <- levels(records_ba$municipio_tag)

results_list <- list()

for (city in cities) {
  dat_city <- records_ba %>%
    filter(municipio_tag == city) %>%
    arrange(period_en)

  if (nrow(dat_city) != 2L) {
    warning("Cidade ", city, " não tem exatamente 2 linhas (Before e After). Pulando.")
    next
  }

  obs <- dat_city$n_records
  names(obs) <- as.character(dat_city$period_en)

  total <- sum(obs)
  mean_obs <- mean(obs)
  exp_counts <- rep(mean_obs, 2)

  exp_prob <- exp_counts / sum(exp_counts)

  city_table <- tibble(
    municipio_tag = city,
    period        = dat_city$period_en,
    observed      = obs,
    expected      = exp_counts
  )

  cat("\n============================================================\n")
  cat("### Município:", city, "###\n")
  cat("Tabela de total de registros observados vs esperados (média de Before e After)\n")
  print(city_table)

  chisq_city <- chisq.test(x = obs, p = exp_prob)

  cat("\nResultado do teste qui-quadrado (", city, ") ###\n", sep = "")
  print(chisq_city)

  cat("\nValores esperados usados pelo qui-quadrado (", city, ") ###\n", sep = "")
  print(chisq_city$expected)

  results_list[[city]] <- tibble(
    municipio_tag = city,
    before        = obs[["Before"]],
    after         = obs[["After"]],
    expected      = mean_obs,
    chisq_stat    = unname(chisq_city$statistic),
    df            = unname(chisq_city$parameter),
    p_value       = chisq_city$p.value
  )
}

chisq_summary <- dplyr::bind_rows(results_list)

cat("\n============================================================\n")
cat("### Resumo dos qui-quadrado (total de registros, Before vs After) por município ###\n")
print(chisq_summary)

out_dir <- "outputs"
fs::dir_create(out_dir)

readr::write_csv(
  chisq_summary,
  file.path(out_dir, "14_chisq_total_records_by_city_summary.csv")
)

cat("\nArquivo de resumo salvo em: outputs/14_chisq_total_records_by_city_summary.csv\n")
