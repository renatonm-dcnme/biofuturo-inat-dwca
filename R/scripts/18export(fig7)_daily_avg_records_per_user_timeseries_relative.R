suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(stringr)
  library(fs)
  library(purrr)
  library(tibble)
})

out_dir <- "outputs"
fs::dir_create(out_dir)

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
    )
  )

daily_avg <- mestre %>%
  dplyr::filter(!is.na(user.id)) %>%
  dplyr::group_by(municipio_tag, observed_on) %>%
  dplyr::summarise(
    total_records        = dplyr::n(),
    n_users              = dplyr::n_distinct(user.id),
    avg_records_per_user = total_records / n_users,
    period_en            = dplyr::first(period_en),
    .groups              = "drop"
  )

readr::write_csv(
  daily_avg,
  file.path(out_dir, "18_daily_avg_records_per_citizen_scientist_by_city_date.csv")
)

bioblitz_dates <- mestre %>%
  dplyr::filter(period_en == "During") %>%
  dplyr::group_by(municipio_tag) %>%
  dplyr::summarise(
    bioblitz_date = min(observed_on),
    .groups = "drop"
  )

daily_rel <- daily_avg %>%
  dplyr::left_join(bioblitz_dates, by = "municipio_tag") %>%
  dplyr::mutate(
    rel_day = as.integer(observed_on - bioblitz_date)
  ) %>%
  dplyr::filter(rel_day >= -30L, rel_day <= 30L)

readr::write_csv(
  daily_rel,
  file.path(out_dir, "18_daily_avg_records_per_citizen_scientist_by_city_rel_days.csv")
)

okabe_ito_cities <- c(
  "Araras"                  = "#0072B2",
  "Campina do Monte Alegre" = "#D55E00",
  "São Carlos"              = "#CC79A7",
  "Sorocaba"                = "#999999"
)

p <- ggplot(
  daily_rel,
  aes(
    x     = rel_day,
    y     = avg_records_per_user,
    color = municipio_tag
  )
) +
  geom_line(linewidth = 1.1) +
  geom_vline(
    xintercept = 0,
    linetype   = "dashed",
    alpha      = 0.7
  ) +
  scale_color_manual(
    values = okabe_ito_cities,
    name   = "Municipality"
  ) +
  labs(
    x = "Time (days)",
    y = "Average number of records per unique citizen scientist"

  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.title    = element_blank(),
    plot.subtitle = element_blank(),
    plot.caption  = element_blank()
  )

png_path  <- file.path(out_dir, "18_timeseries_daily_avg_records_per_citizen_scientist_by_city_rel_days.png")
tiff_path <- file.path(out_dir, "18_timeseries_daily_avg_records_per_citizen_scientist_by_city_rel_days.tiff")
eps_path  <- file.path(out_dir, "18_timeseries_daily_avg_records_per_citizen_scientist_by_city_rel_days.eps")

ggsave(png_path, p, width = 11, height = 6.5, units = "in", dpi = 300, bg = "white")
message("PNG saved at:  ", png_path)

ggsave(tiff_path, p, device = "tiff",
       width = 11, height = 6.5, units = "in",
       dpi = 600, compression = "lzw", bg = "white")
message("TIFF saved at: ", tiff_path)

eps_device <- if (capabilities("cairo")) grDevices::cairo_ps else "eps"
ggsave(eps_path, p, device = eps_device, width = 11, height = 6.5, units = "in")
message("EPS saved at:  ", eps_path)
