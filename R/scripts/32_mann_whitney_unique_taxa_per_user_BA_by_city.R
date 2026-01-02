library(dplyr)

user_taxa_BA <- user_taxa %>%
  filter(period_en %in% c("Before", "After"))

run_mw <- function(df) {
  x <- df$n_unique_taxa
  g <- df$period_en

  wtest <- wilcox.test(x ~ g, exact = FALSE)

  tibble(
    n_before      = sum(g == "Before"),
    n_after       = sum(g == "After"),
    median_before = median(x[g == "Before"]),
    median_after  = median(x[g == "After"]),
    method        = wtest$method,
    W_stat        = unname(wtest$statistic),
    p_value       = wtest$p.value
  )
}

mw_results <- user_taxa_BA %>%
  group_by(municipio_tag) %>%
  group_modify(~ run_mw(.x)) %>%
  ungroup()

cat("\n### Mann–Whitney (Before vs After) para número de táxons únicos por usuário ###\n")
print(mw_results)

readr::write_csv(
  mw_results,
  "outputs/32_mann_whitney_unique_taxa_per_user_BA_by_city.csv"
)
