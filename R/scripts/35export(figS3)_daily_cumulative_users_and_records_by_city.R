suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(scales)
})

Sys.setlocale("LC_TIME", "C")

mestre_csv <- "data/processed/02_mestre_filtrada_bioblitz.csv"
out_dir    <- "outputs"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
stopifnot(file.exists(mestre_csv))

mestre <- readr::read_csv(
  mestre_csv,
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
  ) %>%
  filter(!is.na(observed_on), !is.na(municipio_tag))

bioblitz_dates <- mestre %>%
  filter(period_en == "During") %>%
  group_by(municipio_tag) %>%
  summarise(
    bioblitz_date = min(observed_on, na.rm = TRUE),
    .groups = "drop"
  )

okabe_ito_cities <- c(
  "Araras"                  = "#0072B2",
  "Campina do Monte Alegre" = "#D55E00",
  "São Carlos"              = "#CC79A7",
  "Sorocaba"                = "#999999"
)

daily_users <- mestre %>%
  filter(!is.na(user.id)) %>%
  group_by(municipio_tag, observed_on) %>%
  summarise(
    daily_users = n_distinct(user.id),
    .groups = "drop_last"
  ) %>%
  group_by(municipio_tag) %>%
  tidyr::complete(
    observed_on = seq(min(observed_on, na.rm = TRUE),
                      max(observed_on, na.rm = TRUE),
                      by = "day"),
    fill = list(daily_users = 0L)
  ) %>%
  arrange(municipio_tag, observed_on) %>%
  mutate(cum_users = cumsum(daily_users)) %>%
  ungroup()

s2_csv <- file.path(out_dir, "35_Figure_S2_daily_cumulative_unique_users.csv")
readr::write_csv(daily_users, s2_csv)

date_min <- min(daily_users$observed_on, na.rm = TRUE)
date_max <- max(daily_users$observed_on, na.rm = TRUE)

p_s2 <- ggplot(
  daily_users,
  aes(x = observed_on, y = cum_users, fill = municipio_tag)
) +
  geom_col(width = 0.8, color = NA) +
  geom_vline(
    data = bioblitz_dates,
    aes(xintercept = as.numeric(bioblitz_date), color = municipio_tag),
    linetype = "dashed",
    linewidth = 0.7,
    show.legend = FALSE
  ) +
  facet_wrap(vars(municipio_tag), ncol = 2, scales = "fixed") +
  scale_fill_manual(values = okabe_ito_cities, guide = "none") +
  scale_color_manual(values = okabe_ito_cities, guide = "none") +
  scale_x_date(
    limits = c(date_min, date_max),
    date_labels = "%b %d",
    date_breaks = "7 days",
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  scale_y_continuous(
    labels = label_number(accuracy = 1),
    expand = expansion(mult = c(0.02, 0.08))
  ) +
  labs(
    x = "Date",
    y = "Cumulative number of unique citizen scientists"
  ) +
  theme_bw(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title       = element_blank(),
    plot.subtitle    = element_blank(),
    plot.caption     = element_blank(),

    axis.text.x      = element_text(angle = 45, hjust = 1)
  )

s2_stem <- file.path(out_dir, "35_figure_S2_daily_cumulative_unique_users")
s2_png  <- paste0(s2_stem, ".png")
s2_tiff <- paste0(s2_stem, ".tiff")
s2_eps  <- paste0(s2_stem, ".eps")

ggsave(s2_png,  p_s2, width = 10, height = 7, units = "in", dpi = 300, bg = "white")
ggsave(s2_tiff, p_s2, device = "tiff",
       width = 10, height = 7, units = "in",
       dpi = 600, compression = "lzw", bg = "white")
eps_device <- if (capabilities("cairo")) grDevices::cairo_ps else "eps"
ggsave(s2_eps,  p_s2, device = eps_device, width = 10, height = 7, units = "in")

daily_records <- mestre %>%
  group_by(municipio_tag, observed_on) %>%
  summarise(
    daily_records = n(),
    .groups = "drop_last"
  ) %>%
  group_by(municipio_tag) %>%
  tidyr::complete(
    observed_on = seq(min(observed_on, na.rm = TRUE),
                      max(observed_on, na.rm = TRUE),
                      by = "day"),
    fill = list(daily_records = 0L)
  ) %>%
  arrange(municipio_tag, observed_on) %>%
  mutate(cum_records = cumsum(daily_records)) %>%
  ungroup()

s3_csv <- file.path(out_dir, "35_Figure_S3_daily_cumulative_records.csv")
readr::write_csv(daily_records, s3_csv)

p_s3 <- ggplot(
  daily_records,
  aes(x = observed_on, y = cum_records, fill = municipio_tag)
) +
  geom_col(width = 0.8, color = NA) +
  geom_vline(
    data = bioblitz_dates,
    aes(xintercept = as.numeric(bioblitz_date), color = municipio_tag),
    linetype = "dashed",
    linewidth = 0.7,
    show.legend = FALSE
  ) +
  facet_wrap(vars(municipio_tag), ncol = 2, scales = "fixed") +
  scale_fill_manual(values = okabe_ito_cities, guide = "none") +
  scale_color_manual(values = okabe_ito_cities, guide = "none") +
  scale_x_date(
    limits = c(date_min, date_max),
    date_labels = "%b %d",
    date_breaks = "7 days",
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  scale_y_continuous(
    labels = label_number(accuracy = 1),
    expand = expansion(mult = c(0.02, 0.08))
  ) +
  labs(
    x = "Date",
    y = "Cumulative number of records"
  ) +
  theme_bw(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title       = element_blank(),
    plot.subtitle    = element_blank(),
    plot.caption     = element_blank(),

    axis.text.x      = element_text(angle = 45, hjust = 1)
  )

s3_stem <- file.path(out_dir, "35_figure_S3_daily_cumulative_records")
s3_png  <- paste0(s3_stem, ".png")
s3_tiff <- paste0(s3_stem, ".tiff")
s3_eps  <- paste0(s3_stem, ".eps")

ggsave(s3_png,  p_s3, width = 10, height = 7, units = "in", dpi = 300, bg = "white")
ggsave(s3_tiff, p_s3, device = "tiff",
       width = 10, height = 7, units = "in",
       dpi = 600, compression = "lzw", bg = "white")
ggsave(s3_eps,  p_s3, device = eps_device, width = 10, height = 7, units = "in")

message(
  "Figure S2 saved at: ", s2_png,
  " | TIFF: ", s2_tiff,
  " | EPS: ", s2_eps,
  " | CSV: ", s2_csv,
  "\nFigure S3 saved at: ", s3_png,
  " | TIFF: ", s3_tiff,
  " | EPS: ", s3_eps,
  " | CSV: ", s3_csv
)
