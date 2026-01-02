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

cs_by_city_period <- mestre %>%
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
    n_citizen_scientists = dplyr::n_distinct(user.id),
    .groups = "drop"
  )

group_spacing <- 1.6

cs_plot <- cs_by_city_period %>%
  mutate(
    mun_num = as.numeric(municipio_tag),
    mun_pos = mun_num * group_spacing,
    x = dplyr::case_when(
      period_en == "Before" ~ mun_pos - 0.3,
      period_en == "During" ~ mun_pos,
      period_en == "After"  ~ mun_pos + 0.3
    )
  )

expected_cs <- cs_by_city_period %>%
  filter(period_en %in% c("Before", "After")) %>%
  group_by(municipio_tag) %>%
  summarise(
    expected_citizen_scientists = mean(n_citizen_scientists),
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

x_breaks <- unique(cs_plot$mun_pos)
x_labels <- levels(cs_by_city_period$municipio_tag)

p_cs <- ggplot(
  cs_plot,
  aes(x = x, y = n_citizen_scientists, fill = period_en)
) +

  geom_col(
    data = dplyr::filter(cs_plot, period_en != "During"),
    width = width_before_after
  ) +

  geom_col(
    data = dplyr::filter(cs_plot, period_en == "During"),
    width = width_during
  ) +

  geom_segment(
    data = expected_cs,
    aes(
      x    = x_min,
      xend = x_max,
      y    = expected_citizen_scientists,
      yend = expected_citizen_scientists
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
    x = "Municipality",
    y = "Unique citizen scientist"

  ) +
  expand_limits(
    y = max(
      c(cs_plot$n_citizen_scientists, expected_cs$expected_citizen_scientists),
      na.rm = TRUE
    ) * 1.1
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title   = element_blank(),
    plot.caption = element_blank()
  )

print(p_cs)

png_path  <- file.path(out_dir, "06_barplot_unique_citizen_scientists_by_city_period.png")
tiff_path <- file.path(out_dir, "06_barplot_unique_citizen_scientists_by_city_period.tiff")
eps_path  <- file.path(out_dir, "06_barplot_unique_citizen_scientists_by_city_period.eps")

ggsave(png_path, p_cs, width = 8, height = 5, units = "in", dpi = 300, bg = "white")
message("PNG saved at:  ", png_path)

ggsave(tiff_path, p_cs, device = "tiff",
       width = 8, height = 5, units = "in",
       dpi = 600, compression = "lzw", bg = "white")
message("TIFF saved at: ", tiff_path)

eps_device <- if (capabilities("cairo")) grDevices::cairo_ps else "eps"
ggsave(eps_path, p_cs, device = eps_device, width = 8, height = 5, units = "in")
message("EPS saved at:  ", eps_path)
