suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(stringr)
  library(lubridate)
  library(scales)
  library(purrr)
})

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
  )

okabe_ito_periods <- c(
  "Before" = "#E69F00",
  "During" = "#56B4E9",
  "After"  = "#009E73"
)

parse_ts <- function(x) {
  suppressWarnings(lubridate::parse_date_time(
    x,
    orders = c(
      "Ymd HMSz","Ymd HMS","Ymd HMz","Ymd HM","Ymd Hz","Ymd H",
      "Y-m-d H:M:Sz","Y-m-d H:M:S","Y-m-d H:Mz","Y-m-d H:M","Y-m-d"
    ),
    tz = "UTC",
    quiet = TRUE
  ))
}

n <- nrow(mestre)

cand_created_raw <- list(
  if ("created_at" %in% names(mestre)) mestre[["created_at"]] else NULL,
  if ("created_at_utc" %in% names(mestre)) mestre[["created_at_utc"]] else NULL,
  if ("created_at_details.datetime" %in% names(mestre)) mestre[["created_at_details.datetime"]] else NULL,
  if ("created_at_details.date" %in% names(mestre)) {
    paste0(mestre[["created_at_details.date"]], " 12:00:00")
  } else NULL
)

parsed_list <- lapply(
  cand_created_raw,
  function(v) {
    if (is.null(v)) {
      rep(as.POSIXct(NA), n)
    } else {
      parse_ts(as.character(v))
    }
  }
)

if (length(parsed_list) == 0L) {
  stop("No candidate 'created_at' columns found in the master table.")
}

created_at_ts <- Reduce(dplyr::coalesce, parsed_list)

if (all(is.na(created_at_ts))) {
  stop("Could not interpret any 'created_at' timestamp columns in the master table.")
}

if ("time_observed_at" %in% names(mestre)) {
  obs_time_ts <- parse_ts(as.character(mestre[["time_observed_at"]]))
  missing_time <- is.na(obs_time_ts) & !is.na(mestre$observed_on)
  obs_time_ts[missing_time] <- lubridate::ymd_hms(
    paste(mestre$observed_on[missing_time], "12:00:00"),
    tz = "UTC"
  )
} else {
  obs_time_ts <- lubridate::ymd_hms(
    paste(mestre$observed_on, "12:00:00"),
    tz = "UTC"
  )
}

lag_df <- mestre %>%
  mutate(
    lag_days = as.numeric(difftime(created_at_ts, obs_time_ts, units = "days"))
  ) %>%
  filter(is.finite(lag_days)) %>%
  filter(!is.na(municipio_tag), !is.na(period_en)) %>%
  mutate(
    municipio_tag = factor(
      municipio_tag,
      levels = c("Araras",
                 "Campina do Monte Alegre",
                 "São Carlos",
                 "Sorocaba")
    ),
    period_en = factor(period_en, levels = c("Before", "During", "After")),

    lag_plot = pmax(lag_days, 0)
  )

ylim_max <- lag_df %>%
  summarise(p95 = quantile(lag_plot, 0.95, na.rm = TRUE)) %>%
  pull(p95)

ylim_max <- max(7, ylim_max)
ylim_max <- min(ylim_max, 60)

p_violin <- ggplot(lag_df, aes(x = period_en, y = lag_plot, fill = period_en)) +
  geom_violin(trim = TRUE, alpha = 0.8, color = NA) +
  stat_summary(fun = median, geom = "point", size = 2.2, color = "black") +
  facet_wrap(vars(municipio_tag), nrow = 2) +
  scale_fill_manual(values = okabe_ito_periods, name = "Period") +
  scale_y_continuous(
    trans  = "sqrt",
    breaks = c(0, 1, 3, 7, 14, 30, 60),
    limits = c(0, ylim_max),
    expand = expansion(mult = c(0.02, 0.06))
  ) +
  labs(
    x = NULL,
    y = "Upload lag (days)"

  ) +
  theme_bw(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position  = "bottom",
    plot.title       = element_blank(),
    plot.subtitle    = element_blank(),
    plot.caption     = element_blank()
  )

stem <- file.path(out_dir, "33_violin_upload_lag_by_city_period")
out_png  <- paste0(stem, ".png")
out_tiff <- paste0(stem, ".tiff")
out_eps  <- paste0(stem, ".eps")

ggsave(out_png, p_violin, width = 10, height = 7, units = "in", dpi = 300, bg = "white")

ggsave(out_tiff, p_violin, device = "tiff",
       width = 10, height = 7, units = "in",
       dpi = 600, compression = "lzw", bg = "white")

eps_device <- if (capabilities("cairo")) grDevices::cairo_ps else "eps"
ggsave(out_eps, p_violin, device = eps_device, width = 10, height = 7, units = "in")

readr::write_csv(
  lag_df %>% select(municipio_tag, period_en, observed_on, lag_days),
  file.path(out_dir, "33_upload_lag_by_observation.csv")
)

message(
  "Done! Files saved in '", out_dir, "':\n",
  "- ", out_png, "\n",
  "- ", out_tiff, "\n",
  "- ", out_eps, "\n",
  "- ", file.path(out_dir, "33_upload_lag_by_observation.csv")
)
