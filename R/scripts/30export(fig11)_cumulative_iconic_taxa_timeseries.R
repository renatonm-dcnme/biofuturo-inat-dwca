suppressPackageStartupMessages({
  library(readr);  library(dplyr);  library(tidyr);  library(ggplot2)
  library(tibble); library(fs);     library(purrr);  library(stringr)
  library(ggrepel)
})

if (!requireNamespace("patchwork", quietly = TRUE)) {
  stop("Package 'patchwork' is required. Install with install.packages('patchwork').")
}
suppressPackageStartupMessages(library(patchwork))

out_dir <- "outputs"
dir_create(out_dir)

master_csv <- "data/processed/02_mestre_filtrada_bioblitz.csv"
stopifnot(file.exists(master_csv))

circle_label <- "\u25EF"
circle_size  <- 8

legend_title_pt <- 12
pt_to_mm <- 0.3527778
legend_title_mm <- legend_title_pt * pt_to_mm

mestre <- readr::read_csv(master_csv, show_col_types = FALSE) %>%
  mutate(
    observed_on   = as.Date(observed_on),
    municipio_tag = as.character(municipio_tag),
    periodo       = as.character(periodo),
    period_en     = dplyr::recode(
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

okabe_ito_cities <- c(
  "Araras"                  = "#0072B2",
  "Campina do Monte Alegre" = "#D55E00",
  "São Carlos"              = "#CC79A7",
  "Sorocaba"                = "#999999"
)

min_records_for_label <- 5L

mestre_rg_iconic <- mestre %>%
  filter(quality_grade == "research") %>%
  mutate(
    iconic_group = as.character(`taxon.iconic_taxon_name`),
    iconic_group = if_else(
      is.na(iconic_group) | str_trim(iconic_group) == "",
      "Unknown",
      iconic_group
    )
  )

iconic_totals <- mestre_rg_iconic %>%
  count(iconic_group, name = "n_records")

iconic_map <- iconic_totals %>%
  mutate(
    iconic_plot = if_else(
      n_records >= min_records_for_label,
      iconic_group,
      "Other"
    )
  ) %>%
  select(iconic_group, iconic_plot)

mestre_rg_iconic <- mestre_rg_iconic %>%
  left_join(iconic_map, by = "iconic_group") %>%
  filter(!is.na(iconic_plot) & iconic_plot != "")

dat <- mestre_rg_iconic %>%
  mutate(
    obs_date   = observed_on,
    city_label = as.character(municipio_tag)
  )

event_lines_tbl <- tibble::tibble(
  city_label = c("São Carlos",
                 "Araras",
                 "Sorocaba",
                 "Campina do Monte Alegre"),
  date       = as.Date(c("2025-05-17",
                         "2025-05-24",
                         "2025-06-14",
                         "2025-06-28"))
)

event_windows_tbl <- event_lines_tbl %>%
  group_by(city_label) %>%
  summarise(
    start = min(date),
    end   = max(date),
    .groups = "drop"
  ) %>%
  mutate(
    win_start = start - 30L,
    win_end   = end + 30L
  )

first_seen_iconic <- dat %>%
  group_by(city_label, iconic_plot) %>%
  summarise(
    first_date = min(obs_date, na.rm = TRUE),
    .groups = "drop"
  )

expand_city <- function(city_nm, fs_tbl, win_tbl) {
  w <- win_tbl[win_tbl$city_label == city_nm, , drop = FALSE]
  if (nrow(w) == 0) return(NULL)

  days <- seq(w$win_start, w$win_end, by = "day")

  fs_city <- fs_tbl %>%
    filter(city_label == city_nm) %>%
    arrange(first_date)

  if (nrow(fs_city) == 0) {
    tibble(
      city_label      = city_nm,
      date            = days,
      cum_iconic_taxa = 0L
    )
  } else {
    tibble(
      city_label      = city_nm,
      date            = days,
      cum_iconic_taxa = findInterval(
        days,
        fs_city$first_date,
        rightmost.closed = TRUE
      )
    )
  }
}

cum_by_day <- bind_rows(
  lapply(unique(event_windows_tbl$city_label),
         expand_city,
         fs_tbl = first_seen_iconic,
         win_tbl = event_windows_tbl)
)

csv_out <- file.path(out_dir, "30_cumulative_iconic_taxa_by_day_from_master.csv")
readr::write_csv(cum_by_day %>% arrange(city_label, date), csv_out)
message("CSV: ", csv_out)

labels_df <- first_seen_iconic %>%
  inner_join(
    cum_by_day,
    by = c("city_label" = "city_label",
           "first_date" = "date")
  ) %>%
  filter(iconic_plot != "Other")

aras_fix_dates <- c(
  seq(as.Date("2025-05-19"), as.Date("2025-05-21"), by = "day"),
  as.Date("2025-05-26")
)

labels_araras_fix <- labels_df %>%
  filter(city_label == "Araras", first_date %in% aras_fix_dates)

labels_other <- labels_df %>%
  anti_join(labels_araras_fix, by = c("city_label", "iconic_plot", "first_date", "cum_iconic_taxa"))

p_main <- ggplot(
  cum_by_day,
  aes(x = date, y = cum_iconic_taxa, color = city_label)
) +
  geom_line(linewidth = 1.05, na.rm = TRUE) +
  geom_vline(
    data      = event_lines_tbl,
    aes(xintercept = as.numeric(date), color = city_label),
    linetype  = "dashed",
    linewidth = 0.6,
    alpha     = 0.85
  ) +

  ggrepel::geom_text_repel(
    data = labels_other,
    aes(x = first_date, y = cum_iconic_taxa, label = circle_label),
    color         = "black",
    size          = circle_size,
    box.padding   = 0.55,
    point.padding = 0.25,
    segment.size  = 0.3,
    segment.color = "grey45",
    show.legend   = FALSE,
    na.rm         = TRUE,
    max.overlaps  = Inf,
    direction     = "both"
  ) +

  ggrepel::geom_text_repel(
    data = labels_araras_fix,
    aes(x = first_date, y = cum_iconic_taxa, label = circle_label),
    color         = "black",
    size          = circle_size,
    nudge_y       = 2.3,
    box.padding   = 0.75,
    point.padding = 0.35,
    segment.size  = 0.3,
    segment.color = "grey45",
    show.legend   = FALSE,
    na.rm         = TRUE,
    max.overlaps  = Inf,
    direction     = "y"
  ) +
  scale_color_manual(
    values = okabe_ito_cities,
    name   = "Municipality"
  ) +
  guides(color = guide_legend(nrow = 1, byrow = TRUE)) +
  scale_x_date(
    date_breaks = "1 week",
    date_labels = "%d %b"
  ) +
  labs(
    x = "Date",
    y = "Cumulative number of major taxons"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position  = "bottom",
    legend.title     = element_text(size = legend_title_pt),
    panel.grid.minor = element_blank(),
    axis.title.y     = element_text(margin = margin(r = 5)),
    axis.title.x     = element_text(margin = margin(t = 5)),
    plot.title       = element_blank(),
    plot.subtitle    = element_blank(),
    plot.caption     = element_blank()
  )

taxon_levels <- labels_df %>%
  distinct(iconic_plot) %>%
  arrange(iconic_plot) %>%
  pull(iconic_plot)

taxon_levels <- taxon_levels[seq_len(min(length(taxon_levels), 9))]

taxon_legend_df <- tibble(iconic_plot = taxon_levels, idx = seq_along(taxon_levels)) %>%
  mutate(
    row = if_else(idx <= 5, 1L, 2L),
    col = if_else(idx <= 5, idx, idx - 5L),
    x_circle = col,
    x_text   = col + 0.22,

    y        = if_else(row == 1L, 1.00, 1.45)
  )

p_taxon <- ggplot(taxon_legend_df) +
  annotate(
    "text",
    x = 0.10, y = 1.225,
    label = "Major taxon",
    hjust = 0,
    fontface = "plain",
    size = legend_title_mm
  ) +
  geom_text(
    aes(x = x_circle, y = y, label = circle_label),
    color = "black",
    size  = circle_size
  ) +
  geom_text(
    aes(x = x_text, y = y, label = iconic_plot),
    hjust = 0,
    size  = 3.1
  ) +
  scale_x_continuous(
    limits = c(0, 6.6),
    breaks = NULL
  ) +
  scale_y_reverse(
    limits = c(1.70, 0.80),
    breaks = NULL
  ) +
  coord_cartesian(clip = "off") +
  theme_void(base_size = 12) +
  theme(
    plot.margin = margin(t = 0, r = 8, b = 0, l = 8)
  )

p_final <- p_main / p_taxon + patchwork::plot_layout(heights = c(1, 0.13))

print(p_final)

stem <- file.path(out_dir, "30_cumulative_iconic_taxa_timeseries_from_master")
png_out  <- paste0(stem, ".png")
tiff_out <- paste0(stem, ".tiff")
eps_out  <- paste0(stem, ".eps")

ggsave(png_out,  p_final, width = 11, height = 8.2, units = "in", dpi = 300, bg = "white")
message("PNG:  ", png_out)

ggsave(tiff_out, p_final, device = "tiff",
       width = 11, height = 8.2, units = "in",
       dpi = 600, compression = "lzw", bg = "white")
message("TIFF: ", tiff_out)

eps_device <- if (capabilities("cairo")) grDevices::cairo_ps else "eps"
ggsave(eps_out, p_final, device = eps_device,
       width = 11, height = 8.2, units = "in")
message("EPS:  ", eps_out)

endpoints <- cum_by_day %>%
  group_by(city_label) %>%
  summarise(
    cum_end = max(cum_iconic_taxa),
    .groups = "drop"
  )

n_iconic_taxa <- first_seen_iconic %>%
  count(city_label, name = "n_iconic_taxa")

qa_join <- left_join(endpoints, n_iconic_taxa, by = "city_label") %>%
  mutate(ok = (cum_end == n_iconic_taxa))

message("\n[QA] Cumulative endpoint vs. number of distinct iconic taxa per city:")
print(qa_join)
if (any(!qa_join$ok)) {
  warning("Endpoint ≠ number of distinct iconic taxa in some city; check 'first_seen_iconic' and windows.")
}
