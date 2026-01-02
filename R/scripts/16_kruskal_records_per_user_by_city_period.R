suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(purrr)
  library(tibble)
  library(fs)
})

mestre <- readr::read_csv(
  "data/processed/02_mestre_filtrada_bioblitz.csv",
  show_col_types = FALSE
) %>%
  mutate(
    observed_on   = as.Date(observed_on),
    municipio_tag = as.character(municipio_tag),
    periodo       = as.character(periodo),

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
  )

cat("### Dimensão da base mestre ###\n")
print(dim(mestre))

user_contrib <- mestre %>%
  filter(!is.na(user.id)) %>%
  group_by(municipio_tag, period_en, user.id) %>%
  summarise(
    n_records = n(),
    .groups   = "drop"
  )

cat("\n### Dimensão da base user_contrib (registros por usuário × município × período) ###\n")
print(dim(user_contrib))

cat("\n### Amostra de user_contrib ###\n")
print(head(user_contrib, 10))

out_dir <- "outputs"
fs::dir_create(out_dir)

normality_results <- user_contrib %>%
  group_by(municipio_tag, period_en) %>%
  group_modify(~{
    x <- .x$n_records
    n <- length(x)
    tibble(
      n_users   = n,
      shapiro_p = if (n >= 3) shapiro.test(x)$p.value else NA_real_
    )
  }) %>%
  ungroup()

cat("\n============================================================\n")
cat("### Shapiro-Wilk por município × período (número de registros por usuário) ###\n")
cat("Obs.: variável é contagem; Shapiro serve aqui só como diagnóstico, não como pré-requisito rígido.\n")
print(normality_results)

readr::write_csv(
  normality_results,
  file.path(out_dir, "16_shapiro_records_per_user_by_city_period.csv")
)

kw_results <- user_contrib %>%
  group_by(municipio_tag) %>%
  group_modify(~{
    x <- .x

    if (length(unique(x$period_en)) < 2) {
      return(tibble(
        n_users_total = nrow(x),
        kw_statistic  = NA_real_,
        df            = NA_real_,
        p_value       = NA_real_
      ))
    }
    test <- kruskal.test(n_records ~ period_en, data = x)
    tibble(
      n_users_total = nrow(x),
      kw_statistic  = unname(test$statistic),
      df            = unname(test$parameter),
      p_value       = test$p.value
    )
  }) %>%
  ungroup()

cat("\n============================================================\n")
cat("### Kruskal–Wallis por município (registros por usuário ~ período) ###\n")
print(kw_results)

readr::write_csv(
  kw_results,
  file.path(out_dir, "16_kruskal_records_per_user_by_city_period.csv")
)

period_levels <- c("Before", "During", "After")
period_pairs  <- t(combn(period_levels, 2))

pairwise_results <- user_contrib %>%
  group_by(municipio_tag) %>%
  group_modify(~{
    x <- .x

    res_list <- apply(period_pairs, 1, function(pr) {
      g1 <- pr[1]
      g2 <- pr[2]
      x1 <- x$n_records[x$period_en == g1]
      x2 <- x$n_records[x$period_en == g2]

      if (length(x1) == 0L || length(x2) == 0L) {
        return(NULL)
      }

      wt <- wilcox.test(x1, x2, exact = FALSE)

      tibble(
        group1 = g1,
        group2 = g2,
        n1     = length(x1),
        n2     = length(x2),
        p_raw  = wt$p.value
      )
    })

    res <- bind_rows(res_list)
    if (nrow(res) == 0L) {
      return(tibble(
        group1 = character(),
        group2 = character(),
        n1     = integer(),
        n2     = integer(),
        p_raw  = numeric(),
        p_adj  = numeric()
      ))
    }

    res %>%
      mutate(
        p_adj = p.adjust(p_raw, method = "holm")
      )
  }) %>%
  ungroup()

cat("\n============================================================\n")
cat("### Testes de Wilcoxon (Mann–Whitney) por município (comparações pareadas de períodos) ###\n")
cat("Comparações: Before vs During, Before vs After, During vs After;\n")
cat("p_adj = p-values com correção de Holm dentro de cada município.\n\n")
print(pairwise_results)

readr::write_csv(
  pairwise_results,
  file.path(out_dir, "16_pairwise_wilcox_records_per_user_by_city_period.csv")
)

cat("\nArquivo salvo em: outputs/16_pairwise_wilcox_records_per_user_by_city_period.csv\n")
