suppressPackageStartupMessages({
  library(readr);  library(dplyr);  library(tidyr);  library(ggplot2)
  library(forcats); library(fs);    library(scales)
})

in_csv  <- "data/processed/02_mestre_filtrada_bioblitz_all_quality.csv"
out_dir <- "outputs"; dir_create(out_dir)
stopifnot(file.exists(in_csv))

mestre_all <- readr::read_csv(in_csv, show_col_types = FALSE) %>%
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

valid_q <- c("research", "needs_id", "casual")

obs <- mestre_all %>%
  filter(
    !is.na(municipio_tag),
    !is.na(period_en),
    !is.na(quality_grade),
    quality_grade %in% valid_q
  )

tab_qg <- obs %>%
  count(municipio_tag, period_en, quality_grade, name = "n") %>%
  complete(
    municipio_tag,
    period_en,
    quality_grade = valid_q,
    fill = list(n = 0L)
  ) %>%
  group_by(municipio_tag, period_en) %>%
  mutate(
    total = sum(n),
    prop  = ifelse(total > 0, n / total, NA_real_)
  ) %>%
  ungroup() %>%
  mutate(
    qg_label = forcats::fct_recode(
      quality_grade,
      "Research grade" = "research",
      "Needs ID"       = "needs_id",
      "Casual"         = "casual"
    )
  )

check_sum <- tab_qg %>%
  group_by(municipio_tag, period_en) %>%
  summarise(
    sum_prop      = sum(prop, na.rm = TRUE),
    total_records = sum(n),
    .groups       = "drop"
  )

readr::write_csv(
  tab_qg,
  file.path(out_dir, "19_quality_props_by_city_period.csv")
)
readr::write_csv(
  check_sum,
  file.path(out_dir, "19_check_sum_quality_props_by_panel.csv")
)

quality_pal <- c(
  "Research grade" = "#44AA99",
  "Needs ID"       = "#332288",
  "Casual"         = "#DDCC77"
)

lab_df <- tab_qg %>%
  mutate(pct_label = percent(prop, accuracy = 1))

label_period <- c(
  "Before" = "Before",
  "During" = "During",
  "After"  = "After"
)

p <- ggplot(tab_qg, aes(x = 1, y = prop, fill = qg_label)) +
  geom_col(color = "white", width = 1) +
  coord_polar(theta = "y") +
  facet_grid(
    rows = vars(municipio_tag),
    cols = vars(period_en),
    labeller = labeller(period_en = label_period),
    switch   = "y"
  ) +
  scale_fill_manual(
    values = quality_pal,
    name   = "Quality grade"
  ) +
  geom_text(
    data = lab_df,
    aes(x = 1, y = prop, label = pct_label),
    position = position_stack(vjust = 0.5),
    color    = "white",
    size     = 3.2,
    fontface = "bold"
  ) +
  labs(x = NULL, y = NULL) +
  theme_void(base_size = 12) +
  theme(
    legend.position    = "right",
    strip.placement    = "outside",
    strip.text.x       = element_text(face = "bold", margin = margin(b = 4)),
    strip.text.y.left  = element_text(face = "bold", margin = margin(r = 4)),
    plot.title         = element_blank(),
    plot.subtitle      = element_blank(),
    plot.caption       = element_blank()
  )

print(p)

png_path  <- file.path(out_dir, "19_pie_quality_by_city_period.png")
tiff_path <- file.path(out_dir, "19_pie_quality_by_city_period.tiff")
eps_path  <- file.path(out_dir, "19_pie_quality_by_city_period.eps")

ggsave(png_path, p, width = 11.5, height = 8.5, units = "in", dpi = 300, bg = "white")
message("PNG saved to:  ", png_path)

ggsave(tiff_path, p, device = "tiff",
       width = 11.5, height = 8.5, units = "in",
       dpi = 600, compression = "lzw", bg = "white")
message("TIFF saved to: ", tiff_path)

eps_device <- if (capabilities("cairo")) grDevices::cairo_ps else "eps"
ggsave(eps_path, p, device = eps_device, width = 11.5, height = 8.5, units = "in")
message(
  "EPS saved to:  ", eps_path,
  " | Check panel sums in: outputs/19_check_sum_quality_props_by_panel.csv"
)
