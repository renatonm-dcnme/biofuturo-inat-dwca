suppressPackageStartupMessages({
  library(readr); library(dplyr); library(tidyr); library(ggplot2)
  library(stringr); library(fs);  library(forcats); library(scales)
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

iconic_palette <- c(
  "#E69F00",
  "#56B4E9",
  "#009E73",
  "#F0E442",
  "#0072B2",
  "#D55E00",
  "#CC79A7",
  "#000000",
  "#7570B3",
  "#E7298A",
  "#1B9E77",
  "#D95F02",
  "#66A61E",
  "#E6AB02",
  "#A6761D",
  "#B15928"
)

iconic_levels <- tab_iconic_lumped %>%
  distinct(iconic_plot) %>%
  mutate(is_other = if_else(iconic_plot == "Other", 1L, 0L)) %>%
  arrange(is_other, iconic_plot) %>%
  pull(iconic_plot)

tab_iconic_lumped <- tab_iconic_lumped %>%
  mutate(iconic_plot = factor(iconic_plot, levels = iconic_levels))

pal_vals <- setNames(rep("#999999", length(iconic_levels)), iconic_levels)

non_other <- setdiff(iconic_levels, "Other")
n_non_other <- length(non_other)
if (n_non_other > 0) {
  pal_vals[non_other] <- iconic_palette[seq_len(min(n_non_other, length(iconic_palette)))]
}
if ("Other" %in% names(pal_vals)) {
  pal_vals["Other"] <- "#999999"
}

ratio_df <- tab_iconic_lumped %>%
  filter(period_en %in% c("Before","After")) %>%
  select(municipio_tag, period_en, iconic_plot, n_records) %>%
  tidyr::pivot_wider(
    names_from  = period_en,
    values_from = n_records,
    values_fill = 0L
  ) %>%

  filter(Before > 0, After > 0) %>%
  mutate(
    ratio_after_before = After / Before
  )

print(ratio_df)

p_ratio <- ggplot(
  ratio_df,
  aes(x = iconic_plot, y = ratio_after_before, color = iconic_plot)
) +
  geom_hline(
    yintercept = 1,
    linetype   = "dashed",
    alpha      = 0.7
  ) +
  geom_point(
    size = 3,
    position = position_jitter(width = 0.05, height = 0),
    alpha = 0.9
  ) +
  scale_color_manual(
    values = pal_vals,
    name   = "Iconic taxon"
  ) +
  facet_wrap(~ municipio_tag, nrow = 2) +
  labs(
    x = "Iconic taxon",
    y = "After/Before ratio of records",
    title = "Change in research-grade records by iconic taxon after BioBlitz events",
    subtitle = paste0(
      "Points show the ratio of research-grade records After vs. Before for each municipality and iconic taxon.\n",
      "The dashed line (y = 1) indicates similar numbers of records Before and After."
    ),
    caption = paste0(
      "Only research-grade observations were included. ",
      "Only taxa with at least one record in both periods (Before and After) are shown. ",
      "'Other' groups iconic taxa with < ",
      min_records_for_label,
      " records across all municipalities and periods."
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position    = "bottom",
    axis.text.x        = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_blank()
  )

print(p_ratio)

out_csv <- file.path(out_dir, "28_iconic_ratio_by_city_taxon.csv")
out_png <- file.path(out_dir, "28_iconic_after_before_ratio_points.png")

readr::write_csv(ratio_df, out_csv)

ggsave(
  filename = out_png,
  plot     = p_ratio,
  width    = 11,
  height   = 7,
  dpi      = 300
)

message("Tabela salva em: ", out_csv)
message("Figura salva em: ", out_png)
