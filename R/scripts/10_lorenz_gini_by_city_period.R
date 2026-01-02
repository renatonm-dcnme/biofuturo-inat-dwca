suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(scales)
  library(tibble)
  library(fs)
  library(purrr)
})

rot_csv <- "data/processed/02_mestre_filtrada_bioblitz.csv"
stopifnot(fs::file_exists(rot_csv))

out_dir <- "outputs"
fs::dir_create(out_dir)

mestre <- readr::read_csv(
  rot_csv,
  show_col_types = FALSE
)

mestre <- mestre %>%
  mutate(
    observed_on   = as.Date(observed_on),
    municipio_tag = as.character(municipio_tag),
    periodo       = as.character(periodo),
    period_en = dplyr::recode(
      periodo,
      "Antes"   = "Before",
      "Durante" = "During",
      "Depois"  = "After"
    ),
    period_en = factor(
      period_en,
      levels = c("Before", "During", "After")
    ),
    municipio_tag = factor(
      municipio_tag,
      levels = c("Araras",
                 "Campina do Monte Alegre",
                 "São Carlos",
                 "Sorocaba")
    )
  )

user_contrib <- mestre %>%
  filter(!is.na(user.id)) %>%
  group_by(municipio_tag, period_en, user.id) %>%
  summarise(
    n_records = n(),
    .groups = "drop"
  )

gini_coef <- function(x) {
  x <- x[!is.na(x) & x >= 0]
  n <- length(x)
  if (n == 0) return(NA_real_)
  if (all(x == 0)) return(0)
  x <- sort(x)
  G <- sum((2 * seq_along(x) - n - 1) * x)
  G / (n * sum(x))
}

gini_summary <- user_contrib %>%
  group_by(municipio_tag, period_en) %>%
  summarise(
    n_users       = n(),
    total_records = sum(n_records),
    gini          = gini_coef(n_records),
    .groups       = "drop"
  )

readr::write_csv(
  gini_summary,
  file.path(out_dir, "10_gini_by_city_period.csv")
)

lorenz_curves <- user_contrib %>%
  group_by(municipio_tag, period_en) %>%
  group_modify(~{
    x <- .x$n_records
    x <- sort(x)
    n <- length(x)
    total <- sum(x)

    tibble(
      p = c(0, seq_len(n) / n),
      L = c(0, cumsum(x) / total)
    )
  }) %>%
  ungroup()

readr::write_csv(
  lorenz_curves,
  file.path(out_dir, "10_lorenz_points_by_city_period.csv")
)

labels_df <- gini_summary %>%
  mutate(
    x = 0.02,
    y = dplyr::case_when(
      period_en == "Before" ~ 0.98,
      period_en == "During" ~ 0.90,
      period_en == "After"  ~ 0.82,
      TRUE                  ~ 0.98
    ),
    label = paste0("Gini = ", scales::number(gini, accuracy = 0.01))
  )

period_pal <- c(
  "Before" = "#E69F00",
  "During" = "#56B4E9",
  "After"  = "#009E73"
)

label_period <- c(
  "Before" = "Before",
  "During" = "During",
  "After"  = "After"
)

p <- ggplot(lorenz_curves, aes(x = p, y = L, color = period_en)) +
  geom_abline(
    slope = 1, intercept = 0,
    linetype = "dashed", alpha = 0.6
  ) +
  geom_line(size = 0.8) +
  geom_text(
    data = labels_df,
    aes(x = x, y = y, label = label, color = period_en),
    inherit.aes = FALSE,
    hjust = 0, vjust = 1, size = 3.3
  ) +
  facet_wrap(vars(municipio_tag)) +
  scale_color_manual(
    values = period_pal,
    breaks = names(label_period),
    labels = label_period,
    name   = "Period"
  ) +
  scale_x_continuous(
    limits = c(0, 1),
    breaks = c(0, 0.25, 0.5, 0.75, 1),
    labels = c("0", "25", "50", "75", "100")
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = c(0, 0.25, 0.5, 0.75, 1),
    labels = c("0", "25", "50", "75", "100")
  ) +
  labs(
    x = "Cumulative share of users (%)",
    y = "Cumulative share of records (%)",
    title = "Lorenz curves of user contribution",
    subtitle = "By municipality and period (all quality grades). Higher concavity → higher inequality (higher Gini)."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom"
  )

p

ggsave(
  filename = file.path(out_dir, "10_lorenz_gini_by_city_period.png"),
  plot     = p,
  width    = 8,
  height   = 6,
  dpi      = 300
)
