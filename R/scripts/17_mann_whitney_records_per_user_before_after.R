suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
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

ba_contrib <- user_contrib %>%
  filter(period_en %in% c("Before", "After")) %>%
  droplevels()

cat("\n### Dimensão da base ba_contrib (apenas Before e After) ###\n")
print(dim(ba_contrib))

mw_summary <- ba_contrib %>%
  group_by(municipio_tag) %>%
  group_modify(~{
    x <- .x

    x_before <- x$n_records[x$period_en == "Before"]
    x_after  <- x$n_records[x$period_en == "After"]

    n_before <- length(x_before)
    n_after  <- length(x_after)
    n_total  <- n_before + n_after

    shapiro_before_p <- if (n_before >= 3) shapiro.test(x_before)$p.value else NA_real_
    shapiro_after_p  <- if (n_after  >= 3) shapiro.test(x_after)$p.value  else NA_real_

    mean_before   <- if (n_before > 0) mean(x_before) else NA_real_
    mean_after    <- if (n_after  > 0) mean(x_after)  else NA_real_
    median_before <- if (n_before > 0) median(x_before) else NA_real_
    median_after  <- if (n_after  > 0) median(x_after)  else NA_real_

    if (n_before >= 3 && n_after >= 3) {
      wt <- wilcox.test(x_before, x_after, exact = FALSE)
      W_stat  <- unname(wt$statistic)
      p_value <- wt$p.value
    } else {
      W_stat  <- NA_real_
      p_value <- NA_real_
    }

    tibble(
      municipio_tag    = unique(x$municipio_tag),
      n_before         = n_before,
      n_after          = n_after,
      n_total          = n_total,
      mean_before      = mean_before,
      mean_after       = mean_after,
      median_before    = median_before,
      median_after     = median_after,
      shapiro_p_before = shapiro_before_p,
      shapiro_p_after  = shapiro_after_p,
      W_stat           = W_stat,
      p_value          = p_value
    )
  }) %>%
  ungroup()

cat("\n============================================================\n")
cat("### Resumo Shapiro–Wilk + Mann–Whitney (Before vs After, registros por usuário) ###\n")
cat("Obs.: Campina do Monte Alegre tem n_total baixo; o teste é pouco informativo.\n\n")
print(mw_summary)

out_dir <- "outputs"
fs::dir_create(out_dir)

readr::write_csv(
  mw_summary,
  file.path(out_dir, "17_mann_whitney_records_per_user_before_after_summary.csv")
)

cat("\nArquivo salvo em: outputs/17_mann_whitney_records_per_user_before_after_summary.csv\n")
