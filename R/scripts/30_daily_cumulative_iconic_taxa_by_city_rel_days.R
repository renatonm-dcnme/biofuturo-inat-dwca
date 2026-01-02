suppressPackageStartupMessages({
  library(readr);  library(dplyr);  library(tidyr);  library(ggplot2)
  library(tibble); library(fs);     library(purrr);  library(stringr)
  library(ggrepel)
})

dir_create("outputs")

master_csv <- "data/processed/02_mestre_filtrada_bioblitz.csv"
stopifnot(file.exists(master_csv))

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

csv_out <- file.path("outputs", "30_cumulative_iconic_taxa_by_day_from_master.csv")
readr::write_csv(cum_by_day %>% arrange(city_label, date), csv_out)
message("CSV: ", csv_out)

labels_df <- first_seen_iconic %>%
  inner_join(
    cum_by_day,
    by = c("city_label" = "city_label",
           "first_date" = "date")
  ) %>%

  filter(iconic_plot != "Other")

p <- ggplot(
  cum_by_day,
  aes(x = date, y = cum_iconic_taxa, color = city_label)
) +
  geom_line(linewidth = 1.05, na.rm = TRUE) +
  geom_vline(
    data        = event_lines_tbl,
    aes(xintercept = as.numeric(date), color = city_label),
    linetype    = "dashed",
    linewidth   = 0.6,
    alpha       = 0.85,
    inherit.aes = FALSE
  ) +
  ggrepel::geom_label_repel(
    data = labels_df,
    aes(
      x     = first_date,
      y     = cum_iconic_taxa,
      label = iconic_plot,
      color = city_label
    ),
    size          = 3,
    label.size    = 0.15,
    box.padding   = 0.4,
    point.padding = 0.2,
    segment.size  = 0.3,
    show.legend   = FALSE,
    na.rm         = TRUE
  ) +
  scale_color_manual(
    values = okabe_ito_cities,
    name   = "Municipality"
  ) +
  scale_x_date(
    date_breaks = "1 week",
    date_labels = "%d %b"
  ) +
  labs(
    x = "Date",
    y = "Cumulative number of iconic taxa",
    title    = "Cumulative iconic taxa over time by municipality",
    subtitle = "Research-grade iNaturalist records; dashed vertical lines indicate Bioblitz dates"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position      = "bottom",
    panel.grid.minor     = element_blank(),
    plot.title           = element_text(face = "bold"),
    axis.title.y         = element_text(margin = margin(r = 5)),
    axis.title.x         = element_text(margin = margin(t = 5))
  )

png_out <- file.path("outputs", "30_cumulative_iconic_taxa_timeseries_from_master.png")
ggsave(png_out, p, width = 11, height = 6.8, dpi = 300)
message("PNG: ", png_out)

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
