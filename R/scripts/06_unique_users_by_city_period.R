suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(ggplot2)
  library(fs)
})

out_dir <- "outputs"
fs::dir_create(out_dir)

mestre <- readr::read_csv(
  "data/processed/02_mestre_filtrada_bioblitz.csv",
  show_col_types = FALSE
)

mestre <- mestre %>%
  mutate(
    observed_on   = as.Date(observed_on),
    municipio_tag = as.character(municipio_tag),
    periodo       = as.character(periodo)
  )

users_by_city_period <- mestre %>%
  filter(!is.na(user.id)) %>%
  mutate(

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
  ) %>%
  group_by(municipio_tag, period_en) %>%
  summarise(
    n_users = dplyr::n_distinct(user.id),
    .groups = "drop"
  )

group_spacing <- 1.6

users_plot <- users_by_city_period %>%
  mutate(
    mun_num = as.numeric(municipio_tag),
    mun_pos = mun_num * group_spacing,
    x = dplyr::case_when(
      period_en == "Before" ~ mun_pos - 0.3,
      period_en == "During" ~ mun_pos,
      period_en == "After"  ~ mun_pos + 0.3
    )
  )

expected_users <- users_by_city_period %>%
  filter(period_en %in% c("Before", "After")) %>%
  group_by(municipio_tag) %>%
  summarise(
    expected_users = mean(n_users),
    .groups = "drop"
  ) %>%
  mutate(
    mun_num = as.numeric(municipio_tag),
    mun_pos = mun_num * group_spacing,
    x_min   = mun_pos - 0.5,
    x_max   = mun_pos + 0.5
  )

okabe_ito <- c(
  "Before" = "#E69F00",
  "During" = "#56B4E9",
  "After"  = "#009E73"
)

width_before_after <- 0.36
width_during       <- 0.20

x_breaks <- unique(users_plot$mun_pos)
x_labels <- levels(users_by_city_period$municipio_tag)

p_users <- ggplot(
  users_plot,
  aes(x = x, y = n_users, fill = period_en)
) +

  geom_col(
    data = dplyr::filter(users_plot, period_en != "During"),
    width = width_before_after
  ) +

  geom_col(
    data = dplyr::filter(users_plot, period_en == "During"),
    width = width_during
  ) +

  geom_segment(
    data = expected_users,
    aes(
      x    = x_min,
      xend = x_max,
      y    = expected_users,
      yend = expected_users
    ),
    inherit.aes = FALSE,
    linetype = "dashed",
    size = 0.4,
    color = "grey30"
  ) +
  scale_fill_manual(values = okabe_ito, name = "Period") +
  scale_x_continuous(
    breaks = x_breaks,
    labels = x_labels
  ) +
  labs(
    title   = "Unique iNaturalist users per municipality and period",
    x       = "Municipality",
    y       = "Unique users",
    caption = "Dashed lines: mean number of users (Before and After) per municipality.\nNarrower bars indicate the shorter During period."
  ) +
  expand_limits(y = max(users_plot$n_users, expected_users$expected_users) * 1.1) +
  theme_minimal(base_size = 12)

p_users

ggsave(
  filename = file.path(out_dir, "06_barplot_unique_users_by_city_period.png"),
  plot     = p_users,
  width    = 8,
  height   = 5,
  dpi      = 300
)
