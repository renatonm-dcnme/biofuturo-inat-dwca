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

city_index <- tibble(
  municipio_tag = factor(city_levels, levels = city_levels),
  x = as.numeric(factor(city_levels, levels = city_levels))
)

tab_iconic_pos <- tab_iconic_lumped %>%
  left_join(city_index, by = "municipio_tag")

tab_before_after <- tab_iconic_pos %>%
  filter(period_en %in% c("Before","After")) %>%
  mutate(
    x_pos = case_when(
      period_en == "Before" ~ x - 0.25,
      period_en == "After"  ~ x + 0.25,
      TRUE ~ x
    )
  )

tab_during <- tab_iconic_pos %>%
  filter(period_en == "During") %>%
  mutate(
    x_pos = x
  )

totals_before_after <- tab_before_after %>%
  group_by(municipio_tag, period_en, x_pos) %>%
  summarise(total_records = sum(n_records), .groups = "drop")

totals_during <- tab_during %>%
  group_by(municipio_tag, period_en, x_pos) %>%
  summarise(total_records = sum(n_records), .groups = "drop")

totals_all <- bind_rows(totals_before_after, totals_during)

p_iconic <- ggplot() +

  geom_col(
    data = tab_before_after,
    aes(x = x_pos, y = n_records, fill = iconic_plot),
    width = 0.30,
    color = "white",
    linewidth = 0.25
  ) +

  geom_col(
    data = tab_during,
    aes(x = x_pos, y = n_records, fill = iconic_plot),
    width = 0.18,
    color = "white",
    linewidth = 0.25
  ) +

  geom_text(
    data = totals_all,
    aes(x = x_pos, y = total_records, label = period_en),
    vjust = -0.5,
    size = 3.0,
    inherit.aes = FALSE
  ) +
  scale_x_continuous(
    breaks = city_index$x,
    labels = city_index$municipio_tag
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0.02, 0.10))
  ) +
  scale_fill_manual(
    values = pal_vals,
    name   = "Iconic taxon"
  ) +
  labs(
    x = "Municipality",
    y = "Total number of records",
    title = "Research-grade iNaturalist records by iconic taxon, municipality and period",
    subtitle = paste0(
      "Stacked bars show total records per iconic taxon before, during, and after each BioBlitz.\n",
      "BioBlitz-day bars (During) are thinner to highlight the shorter (1-day) sampling window."
    ),
    caption = paste0(
      "Only research-grade observations were included. ",
      "'Other' groups iconic taxa with < ",
      min_records_for_label,
      " records across all municipalities and periods."
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    panel.grid.major.x = element_blank()
  ) +
  coord_cartesian(clip = "off")

print(p_iconic)

out_csv <- file.path(out_dir, "27_iconic_records_by_city_period_group.csv")
out_png <- file.path(out_dir, "27_iconic_records_stacked_grouped_bars.png")

readr::write_csv(tab_iconic_lumped, out_csv)

ggsave(
  filename = out_png,
  plot     = p_iconic,
  width    = 11,
  height   = 6.8,
  dpi      = 300
)

message("Tabela salva em: ", out_csv)
message("Figura salva em: ", out_png)
