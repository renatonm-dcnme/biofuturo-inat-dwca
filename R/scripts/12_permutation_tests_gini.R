suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(fs)
  library(purrr)
})

rot_csv <- "data/processed/02_mestre_filtrada_bioblitz.csv"
stopifnot(fs::file_exists(rot_csv))

out_dir <- "outputs"
fs::dir_create(out_dir)

mestre <- readr::read_csv(
  rot_csv,
  show_col_types = FALSE
)

mestre <- mestre %>%
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
    period_en = factor(
      period_en,
      levels = c("Before", "During", "After")
    ),
    municipio_tag = factor(
      municipio_tag,
      levels = c("Araras",
                 "Campina do Monte Alegre",
                 "São Carlos",
                 "Sorocaba")
    )
  )

user_contrib <- mestre %>%
  filter(!is.na(user.id)) %>%
  group_by(municipio_tag, period_en, user.id) %>%
  summarise(
    n_records = n(),
    .groups = "drop"
  )

gini_coef <- function(x) {
  x <- x[!is.na(x) & x >= 0]
  n <- length(x)
  if (n == 0) return(NA_real_)
  if (all(x == 0)) return(0)
  x <- sort(x)
  G <- sum((2 * seq_along(x) - n - 1) * x)
  G / (n * sum(x))
}

perm_test_gini_diff <- function(x1, x2, B = 9999L) {
  x1 <- x1[!is.na(x1) & x1 >= 0]
  x2 <- x2[!is.na(x2) & x2 >= 0]

  n1 <- length(x1)
  n2 <- length(x2)

  if (n1 < 2 || n2 < 2) {
    return(tibble(
      gini1       = NA_real_,
      gini2       = NA_real_,
      diff_obs    = NA_real_,
      B           = B,
      p_two_sided = NA_real_,
      p_greater   = NA_real_,
      p_less      = NA_real_
    ))
  }

  g1_obs <- gini_coef(x1)
  g2_obs <- gini_coef(x2)
  diff_obs <- g2_obs - g1_obs

  x_all <- c(x1, x2)
  n_all <- length(x_all)

  perm_diffs <- numeric(B)

  for (b in seq_len(B)) {
    idx <- sample.int(n_all)
    x1_star <- x_all[idx[1:n1]]
    x2_star <- x_all[idx[(n1 + 1):n_all]]
    perm_diffs[b] <- gini_coef(x2_star) - gini_coef(x1_star)
  }

  p_two <- (sum(abs(perm_diffs) >= abs(diff_obs)) + 1) / (B + 1)
  p_g   <- (sum(perm_diffs >= diff_obs)            + 1) / (B + 1)
  p_l   <- (sum(perm_diffs <= diff_obs)            + 1) / (B + 1)

  tibble(
    gini1       = g1_obs,
    gini2       = g2_obs,
    diff_obs    = diff_obs,
    B           = B,
    p_two_sided = p_two,
    p_greater   = p_g,
    p_less      = p_l
  )
}

comparisons <- tibble::tribble(
  ~period1,  ~period2,      ~contrast_label,
  "Before",  "After",       "After - Before",
  "Before",  "During",      "During - Before",
  "During",  "After",       "After - During"
)

set.seed(456)

perm_results <- user_contrib %>%
  group_by(municipio_tag) %>%
  group_modify(~{
    df_city <- .x

    purrr::map_dfr(seq_len(nrow(comparisons)), function(i) {
      p1 <- comparisons$period1[i]
      p2 <- comparisons$period2[i]

      x1 <- df_city %>%
        filter(period_en == p1) %>%
        pull(n_records)

      x2 <- df_city %>%
        filter(period_en == p2) %>%
        pull(n_records)

      res <- perm_test_gini_diff(x1, x2, B = 9999L)

      res %>%
        mutate(
          period1        = p1,
          period2        = p2,
          contrast_label = comparisons$contrast_label[i]
        )
    })
  }) %>%
  ungroup() %>%
  relocate(
    municipio_tag, period1, period2, contrast_label,
    gini1, gini2, diff_obs, B,
    p_two_sided, p_greater, p_less
  )

perm_results

readr::write_csv(
  perm_results,
  file.path(out_dir, "12_permtest_gini_by_city_period_contrast.csv")
)
