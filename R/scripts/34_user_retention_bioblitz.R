suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(ggplot2)
  library(scales)
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

user_first <- mestre %>%
  filter(!is.na(user.id)) %>%
  group_by(municipio_tag, user.id) %>%
  summarise(
    first_date   = min(observed_on, na.rm = TRUE),
    first_period = period_en[which.min(observed_on)],
    .groups = "drop"
  )

cohort <- user_first %>%
  filter(first_period == "During")

after_users <- mestre %>%
  filter(period_en == "After", !is.na(user.id)) %>%
  distinct(municipio_tag, user.id) %>%
  mutate(retained = TRUE)

retention <- cohort %>%
  left_join(after_users, by = c("municipio_tag", "user.id")) %>%
  mutate(retained = if_else(is.na(retained), FALSE, retained))

summary_city <- retention %>%
  group_by(municipio_tag) %>%
  summarise(
    n_cohort       = n(),
    n_retained     = sum(retained, na.rm = TRUE),
    retention_rate = if_else(n_cohort > 0, n_retained / n_cohort, NA_real_),
    .groups        = "drop"
  ) %>%
  mutate(
    municipio_tag = factor(
      municipio_tag,
      levels = c("Araras",
                 "Campina do Monte Alegre",
                 "São Carlos",
                 "Sorocaba")
    )
  ) %>%
  arrange(municipio_tag)

summary_path <- file.path(out_dir, "34_retention_summary_by_city.csv")
readr::write_csv(summary_city, summary_path)

okabe_ito_cities <- c(
  "Araras"                  = "#0072B2",
  "Campina do Monte Alegre" = "#D55E00",
  "São Carlos"              = "#CC79A7",
  "Sorocaba"                = "#999999"
)

pct_lab <- function(x) paste0(round(x * 100, 0), "%")

p <- ggplot(summary_city,
            aes(x = municipio_tag, y = retention_rate, fill = municipio_tag)) +
  geom_col(width = 0.65, na.rm = TRUE) +
  geom_text(
    aes(
      label = if_else(
        is.na(retention_rate),
        paste0("n=", n_cohort),
        paste0(pct_lab(retention_rate), " (", n_retained, "/", n_cohort, ")")
      )
    ),
    vjust = -0.6,
    size = 3.2
  ) +
  scale_y_continuous(
    labels = function(x) paste0(x * 100, "%"),
    limits = c(0, 0.25),
    expand = expansion(mult = c(0.02, 0.10))
  ) +
  scale_fill_manual(values = okabe_ito_cities, guide = "none") +
  labs(
    x = NULL,
    y = "Retention rate (30 days after the Bioblitz)",
    title = "Retention of users recruited during the Bioblitz",
    subtitle = paste(
      "Users whose first record in the ±30-day window occurred during the Bioblitz and",
      "who made ≥1 additional record within 30 days after the event in the same municipality."
    ),
    caption = paste(
      "Base: iNaturalist master windowed dataset (all quality grades as available).",
      "Cohort = first record During; Retained = ≥1 post-event record (After) in the same municipality."
    )
  ) +
  theme_minimal(base_size = 12)

png_path <- file.path(out_dir, "34_retention_rate_by_city_25pct.png")
ggsave(png_path, p, width = 10, height = 6, dpi = 300)

message("Figure saved at: ", png_path,
        " | Summary table saved at: ", summary_path)
