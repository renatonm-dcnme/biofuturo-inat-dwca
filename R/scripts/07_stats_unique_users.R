suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(fs)
})

mestre <- readr::read_csv(
  "data/processed/02_mestre_filtrada_bioblitz.csv",
  show_col_types = FALSE
)

mestre <- mestre %>%
  mutate(
    observed_on   = as.Date(observed_on),
    municipio_tag = as.character(municipio_tag),
    periodo       = as.character(periodo)
  )

cat("### Dimensão da base mestre ###\n")
print(dim(mestre))

users_by_city_period <- mestre %>%
  filter(!is.na(user.id)) %>%
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
    n_users = dplyr::n_distinct(user.id),
    .groups = "drop"
  )

cat("\n### Usuários únicos por município × período (tabela base) ###\n")
print(users_by_city_period)

tab_chisq <- users_by_city_period %>%
  filter(period_en %in% c("Before", "After")) %>%
  select(municipio_tag, period_en, n_users) %>%
  tidyr::pivot_wider(
    names_from  = period_en,
    values_from = n_users
  )

cat("\n### Tabela de usuários únicos (Before / After) por município ###\n")
print(tab_chisq)

mat_chisq <- as.matrix(tab_chisq[, c("Before", "After")])
rownames(mat_chisq) <- tab_chisq$municipio_tag

chisq_res <- chisq.test(mat_chisq)

cat("\n### Resultado do teste do qui-quadrado (Before vs After, agregando municípios) ###\n")
print(chisq_res)

cat("\n### Valores esperados do qui-quadrado ###\n")
print(chisq_res$expected)

daily_users <- mestre %>%
  filter(!is.na(user.id)) %>%
  mutate(
    period_en = dplyr::recode(
      periodo,
      "Antes"   = "Before",
      "Durante" = "During",
      "Depois"  = "After"
    ),
    period_en = factor(period_en, levels = c("Before", "During", "After"))
  ) %>%
  group_by(municipio_tag, period_en, observed_on) %>%
  summarise(
    n_users = dplyr::n_distinct(user.id),
    .groups = "drop"
  )

cat("\n### Primeiras linhas da base diária de usuários únicos ###\n")
print(head(daily_users, 15))

daily_summary <- daily_users %>%
  filter(period_en %in% c("Before", "After")) %>%
  group_by(municipio_tag, period_en) %>%
  summarise(
    mean_n   = mean(n_users),
    median_n = median(n_users),
    sd_n     = sd(n_users),
    n_days   = dplyr::n(),
    .groups  = "drop"
  )

cat("\n### Estatísticas diárias (Before vs After) por município ###\n")
print(daily_summary)

kw_global <- daily_users %>%
  filter(period_en %in% c("Before", "After")) %>%
  kruskal.test(n_users ~ period_en, data = .)

cat("\n### Kruskal-Wallis global (Before vs After, todos municípios) ###\n")
print(kw_global)

cat("\n### Kruskal-Wallis por município (Before vs After) ###\n")

kw_by_city <- daily_users %>%
  filter(period_en %in% c("Before", "After")) %>%
  group_by(municipio_tag) %>%
  group_modify(~ {
    test <- kruskal.test(n_users ~ period_en, data = .x)
    tibble(
      statistic = unname(test$statistic),
      p_value   = test$p.value
    )
  })

print(kw_by_city)
