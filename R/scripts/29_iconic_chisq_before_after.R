suppressPackageStartupMessages({
  library(readr); library(dplyr); library(tidyr)
  library(stringr); library(fs);  library(forcats)
})

out_dir <- "outputs"
dir_create(out_dir)

mestre <- readr::read_csv(
  "data/processed/02_mestre_filtrada_bioblitz.csv",
  show_col_types = FALSE
) %>%
  dplyr::mutate(
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
    ),
    quality_grade = tolower(as.character(quality_grade))
  )

mestre_rg <- mestre %>%
  filter(quality_grade == "research")

min_records_for_label <- 5L
iconic_col <- "taxon.iconic_taxon_name"
stopifnot(iconic_col %in% names(mestre_rg))

tab_iconic <- mestre_rg %>%
  mutate(
    iconic_group = if_else(
      is.na(.data[[iconic_col]]) | .data[[iconic_col]] == "",
      "Unknown",
      as.character(.data[[iconic_col]])
    )
  ) %>%
  count(municipio_tag, period_en, iconic_group, name = "n_records")

iconic_totals <- tab_iconic %>%
  group_by(iconic_group) %>%
  summarise(total_records = sum(n_records), .groups = "drop")

keep_groups <- iconic_totals %>%
  filter(total_records >= min_records_for_label) %>%
  arrange(desc(total_records)) %>%
  pull(iconic_group)

tab_iconic_lumped <- tab_iconic %>%
  mutate(
    iconic_plot = if_else(iconic_group %in% keep_groups, iconic_group, "Other")
  ) %>%
  group_by(municipio_tag, period_en, iconic_plot) %>%
  summarise(
    n_records = sum(n_records),
    .groups   = "drop"
  )

city_levels <- c("Araras",
                 "Campina do Monte Alegre",
                 "São Carlos",
                 "Sorocaba")

tab_iconic_lumped <- tab_iconic_lumped %>%
  mutate(
    municipio_tag = factor(municipio_tag, levels = city_levels),
    period_en     = factor(period_en, levels = c("Before","During","After"))
  )

tab_BA <- tab_iconic_lumped %>%
  filter(period_en %in% c("Before","After")) %>%
  select(municipio_tag, period_en, iconic_plot, n_records) %>%
  tidyr::pivot_wider(
    names_from  = period_en,
    values_from = n_records,
    values_fill = 0L
  ) %>%

  mutate(
    Before = ifelse(is.na(Before), 0L, Before),
    After  = ifelse(is.na(After),  0L, After)
  )

chisq_df <- tab_BA %>%
  rowwise() %>%
  mutate(
    total_BA   = Before + After,
    exp_BA     = total_BA / 2,
    chisq_stat = ifelse(
      exp_BA > 0,
      ((Before - exp_BA)^2 / exp_BA) + ((After - exp_BA)^2 / exp_BA),
      NA_real_
    ),
    df         = ifelse(exp_BA > 0, 1L, NA_integer_),
    p_value    = ifelse(
      !is.na(chisq_stat),
      pchisq(chisq_stat, df = df, lower.tail = FALSE),
      NA_real_
    ),

    min_expected      = exp_BA,
    chisq_approx_ok   = !is.na(exp_BA) & (exp_BA >= 5)
  ) %>%
  ungroup() %>%
  select(
    municipio_tag, iconic_plot,
    Before, After,
    expected_Before = exp_BA,
    expected_After  = exp_BA,
    total_BA,
    chisq_stat, df, p_value,
    min_expected, chisq_approx_ok
  ) %>%
  arrange(municipio_tag, iconic_plot)

print(chisq_df)

out_csv <- file.path(out_dir, "29_iconic_chisq_before_after_by_city_taxon.csv")
readr::write_csv(chisq_df, out_csv)

message("Tabela de qui-quadrado salva em: ", out_csv)
