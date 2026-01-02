suppressPackageStartupMessages({
  library(readr); library(dplyr); library(tidyr); library(ggplot2)
  library(stringr); library(fs);  library(forcats); library(scales)
})

if (!requireNamespace("patchwork", quietly = TRUE)) {
  stop("Package 'patchwork' is required. Install with: install.packages('patchwork')")
}
suppressPackageStartupMessages(library(patchwork))

out_dir <- "outputs"
dir_create(out_dir)

circle_label <- "\u25EF"
circle_size  <- 8
taxon_text_size <- 3.1
taxon_angle <- 45

circle_nudge_x <- -0.22

y_text_strip   <- 0.20
y_circle_strip <- -1.45
y_limits_strip <- c(-1.85, 0.75)

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

mestre_rg <- mestre %>% filter(quality_grade == "research")

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
  summarise(n_records = sum(n_records), .groups = "drop")

iconic_palette <- c(
  "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#000000",
  "#7570B3", "#E7298A", "#1B9E77", "#D95F02", "#66A61E", "#E6AB02", "#A6761D", "#B15928"
)

iconic_levels_all <- tab_iconic_lumped %>%
  distinct(iconic_plot) %>%
  mutate(is_other = if_else(iconic_plot == "Other", 1L, 0L)) %>%
  arrange(is_other, iconic_plot) %>%
  pull(iconic_plot)

pal_vals <- setNames(rep("#999999", length(iconic_levels_all)), iconic_levels_all)
non_other <- setdiff(iconic_levels_all, "Other")
n_non_other <- length(non_other)
if (n_non_other > 0) {
  pal_vals[non_other] <- iconic_palette[seq_len(min(n_non_other, length(iconic_palette)))]
}
if ("Other" %in% names(pal_vals)) pal_vals["Other"] <- "#999999"

ratio_df <- tab_iconic_lumped %>%
  filter(period_en %in% c("Before", "After")) %>%
  select(municipio_tag, period_en, iconic_plot, n_records) %>%
  tidyr::pivot_wider(
    names_from  = period_en,
    values_from = n_records,
    values_fill = 0L
  ) %>%
  filter(Before > 0, After > 0) %>%
  mutate(ratio_after_before = After / Before)

iconic_levels_plot <- iconic_levels_all[iconic_levels_all %in% unique(ratio_df$iconic_plot)]

ratio_df <- ratio_df %>%
  mutate(
    municipio_tag = factor(municipio_tag, levels = levels(mestre$municipio_tag)),
    iconic_plot   = factor(iconic_plot, levels = iconic_levels_plot)
  )

print(ratio_df)

p_main <- ggplot(
  ratio_df,
  aes(x = iconic_plot, y = ratio_after_before, color = iconic_plot)
) +
  geom_hline(yintercept = 1, linetype = "dashed", alpha = 0.7) +
  geom_point(
    size = 3,
    position = position_jitter(width = 0.05, height = 0),
    alpha = 0.9,
    show.legend = FALSE
  ) +
  scale_color_manual(values = pal_vals, guide = "none") +
  scale_x_discrete(drop = FALSE) +
  facet_wrap(~ municipio_tag, nrow = 2) +
  labs(
    x = NULL,
    y = "After/Before ratio of records"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x        = element_blank(),
    axis.ticks.x       = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position    = "none",
    strip.text         = element_text(face = "bold"),
    plot.title         = element_blank(),
    plot.subtitle      = element_blank(),
    plot.caption       = element_blank(),
    plot.margin        = margin(t = 8, r = 8, b = 2, l = 8)
  )

bottom_cities <- tail(levels(ratio_df$municipio_tag), 2)

axis_df <- tidyr::expand_grid(
  municipio_tag = bottom_cities,
  iconic_plot   = levels(ratio_df$iconic_plot)
) %>%
  mutate(
    municipio_tag = factor(municipio_tag, levels = levels(ratio_df$municipio_tag)),
    iconic_plot   = factor(iconic_plot, levels = levels(ratio_df$iconic_plot))
  )

p_strip <- ggplot(axis_df, aes(x = iconic_plot, y = 0)) +
  facet_wrap(~ municipio_tag, nrow = 1) +
  geom_text(
    aes(y = y_text_strip, label = as.character(iconic_plot)),
    angle = taxon_angle,
    hjust = 1,
    vjust = 0,
    size  = taxon_text_size
  ) +
  geom_text(
    aes(y = y_circle_strip, label = circle_label),
    color = "black",
    size  = circle_size,
    position = position_nudge(x = circle_nudge_x, y = 0),
    vjust = 0.5
  ) +
  scale_x_discrete(drop = FALSE) +
  scale_y_continuous(limits = y_limits_strip) +
  labs(x = "Major taxons", y = NULL) +
  coord_cartesian(clip = "off") +
  theme_void(base_size = 12) +
  theme(
    strip.text       = element_blank(),
    strip.background = element_blank(),
    axis.title.x     = element_text(size = 12, margin = margin(t = 8)),
    plot.margin      = margin(t = 0, r = 8, b = 18, l = 8)
  )

p_final <- p_main / p_strip + patchwork::plot_layout(heights = c(1, 0.30))

print(p_final)

out_csv  <- file.path(out_dir, "28_iconic_ratio_by_city_taxon.csv")
stem_fig <- file.path(out_dir, "28_iconic_after_before_ratio_points")
out_png  <- paste0(stem_fig, ".png")
out_tiff <- paste0(stem_fig, ".tiff")
out_eps  <- paste0(stem_fig, ".eps")

readr::write_csv(ratio_df, out_csv)

ggsave(out_png,  p_final, width = 11, height = 7, units = "in", dpi = 300, bg = "white")

ggsave(out_tiff, p_final, device = "tiff",
       width = 11, height = 7, units = "in",
       dpi = 600, compression = "lzw", bg = "white")

eps_device <- if (capabilities("cairo")) grDevices::cairo_ps else "eps"
ggsave(out_eps, p_final, device = eps_device, width = 11, height = 7, units = "in")

message("Tabela salva em: ", out_csv)
message("PNG  salvo em: ", out_png)
message("TIFF salvo em: ", out_tiff)
message("EPS  salvo em: ", out_eps)
