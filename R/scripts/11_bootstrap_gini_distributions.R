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

gini_obs <- user_contrib %>%
  group_by(municipio_tag, period_en) %>%
  summarise(
    n_users       = n(),
    total_records = sum(n_records),
    gini_obs      = gini_coef(n_records),
    .groups       = "drop"
  )

readr::write_csv(
  gini_obs,
  file.path(out_dir, "11_gini_observed_by_city_period.csv")
)

set.seed(123)
B <- 2000L

boot_gini <- user_contrib %>%
  group_by(municipio_tag, period_en) %>%
  group_modify(~{
    x <- .x$n_records
    n <- length(x)

    boot_vals <- replicate(B, {
      sample_x <- sample(x, size = n, replace = TRUE)
      gini_coef(sample_x)
    })

    tibble(
      iter      = seq_len(B),
      gini_boot = boot_vals
    )
  }) %>%
  ungroup()

readr::write_csv(
  boot_gini,
  file.path(out_dir, "11_bootstrap_gini_samples_by_city_period.csv")
)

boot_summary <- boot_gini %>%
  group_by(municipio_tag, period_en) %>%
  summarise(
    gini_boot_mean   = mean(gini_boot, na.rm = TRUE),
    gini_boot_median = median(gini_boot, na.rm = TRUE),
    ci_lower         = quantile(gini_boot, 0.025, na.rm = TRUE),
    ci_upper         = quantile(gini_boot, 0.975, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(gini_obs,
            by = c("municipio_tag", "period_en"))

readr::write_csv(
  boot_summary,
  file.path(out_dir, "11_bootstrap_gini_summary_by_city_period.csv")
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

boot_gini_plot <- boot_gini %>%
  filter(!is.na(gini_boot),
         gini_boot >= 0,
         gini_boot <= 1)

lines_df <- boot_summary %>%
  transmute(
    municipio_tag,
    period_en,
    x_mean  = gini_boot_mean,
    x_low   = ci_lower,
    x_high  = ci_upper
  )

p_boot <- ggplot() +
  geom_histogram(
    data = boot_gini_plot,
    aes(x = gini_boot, fill = period_en),
    color     = "black",
    linewidth = 0.2,
    bins      = 30
  ) +
  geom_vline(
    data      = lines_df,
    aes(xintercept = x_mean),
    linewidth = 0.5
  ) +
  geom_vline(
    data      = lines_df,
    aes(xintercept = x_low),
    linetype  = "dotted",
    linewidth = 0.4
  ) +
  geom_vline(
    data      = lines_df,
    aes(xintercept = x_high),
    linetype  = "dotted",
    linewidth = 0.4
  ) +

  facet_grid(period_en ~ municipio_tag) +
  scale_fill_manual(
    values = period_pal,
    breaks = names(label_period),
    labels = label_period,
    name   = "Period"
  ) +
  scale_x_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.1),
    labels = scales::number_format(accuracy = 0.1)
  ) +
  labs(
    x = "Gini coefficient",
    y = "Frequency",
    title    = "Bootstrap distributions of Gini coefficients of user contribution",
    subtitle = "By municipality (columns) and period (rows). Solid lines: mean Gini; dotted lines: 95% bootstrap CI."
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    strip.text.x    = element_text(face = "bold"),
    strip.text.y    = element_blank(),
    axis.line       = element_line(linewidth = 0.3, colour = "black"),
    panel.grid.minor = element_blank()
  )

p_boot

ggsave(
  filename = file.path(out_dir, "11_bootstrap_gini_histograms_by_city_period.png"),
  plot     = p_boot,
  width    = 9,
  height   = 7,
  dpi      = 300
)
