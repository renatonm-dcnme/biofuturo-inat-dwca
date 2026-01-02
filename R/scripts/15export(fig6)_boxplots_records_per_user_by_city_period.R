suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(ggplot2)
  library(fs)
  library(tibble)
})

mestre <- readr::read_csv(
  "data/processed/02_mestre_filtrada_bioblitz.csv",
  show_col_types = FALSE
) %>%
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
    )
  )

cat("### Dimensão da base mestre ###\n")
print(dim(mestre))

cs_contrib <- mestre %>%
  filter(!is.na(user.id)) %>%
  group_by(municipio_tag, period_en, user.id) %>%
  summarise(
    n_records = n(),
    .groups   = "drop"
  )

cat("\n### Dimensão da base cs_contrib (registros por citizen scientist × município × período) ###\n")
print(dim(cs_contrib))

cat("\n### Amostra de cs_contrib ###\n")
print(head(cs_contrib, 10))

outlier_to_drop <- cs_contrib %>%
  filter(municipio_tag == "São Carlos",
         period_en     == "Before") %>%
  filter(n_records == max(n_records)) %>%
  slice(1)

cat("\n### Outlier removido do GRÁFICO (São Carlos, Before) ###\n")
print(outlier_to_drop)

cs_contrib_plot <- cs_contrib %>%
  anti_join(outlier_to_drop,
            by = c("municipio_tag", "period_en", "user.id", "n_records"))

cat("\n### Dimensão da base cs_contrib_plot (sem o outlier extremo para o gráfico) ###\n")
print(dim(cs_contrib_plot))

cs_records_summary <- cs_contrib_plot %>%
  group_by(municipio_tag, period_en) %>%
  summarise(
    n_citizen_scientists = n(),
    mean_records         = mean(n_records),
    median_records       = median(n_records),
    sd_records           = sd(n_records),
    min_records          = min(n_records),
    max_records          = max(n_records),
    .groups = "drop"
  )

cat("\n### Resumo por município × período (n citizen scientists, média, mediana, etc.) ###\n")
print(cs_records_summary)

out_dir <- "outputs"
fs::dir_create(out_dir)

readr::write_csv(
  cs_records_summary,
  file.path(out_dir, "15_citizen_scientist_records_summary_by_city_period.csv")
)

cat("\nArquivo de resumo salvo em: outputs/15_citizen_scientist_records_summary_by_city_period.csv\n")

okabe_ito <- c(
  "Before" = "#E69F00",
  "During" = "#56B4E9",
  "After"  = "#009E73"
)

width_before_after <- 0.6
width_during       <- 0.35

y_max_global <- max(cs_contrib_plot$n_records, na.rm = TRUE)

n_labels <- cs_contrib_plot %>%
  group_by(municipio_tag, period_en) %>%
  summarise(
    n_citizen_scientists = n(),
    .groups = "drop"
  ) %>%
  mutate(
    y_pos = y_max_global * 1.05
  )

cat("\n### Tabela de n de citizen scientists por município × período (para rótulos no gráfico) ###\n")
print(n_labels)

p_box <- ggplot(
  cs_contrib_plot,
  aes(x = period_en, y = n_records, fill = period_en)
) +
  geom_boxplot(
    data  = dplyr::filter(cs_contrib_plot, period_en != "During"),
    width = width_before_after,
    outlier.size = 1.5,
    alpha = 0.9,
    size = 0.3
  ) +
  geom_boxplot(
    data  = dplyr::filter(cs_contrib_plot, period_en == "During"),
    width = width_during,
    outlier.size = 1.5,
    alpha = 0.9,
    size = 0.3
  ) +

  stat_summary(
    fun = mean,
    geom = "point",
    shape = 21,
    fill  = "white",
    color = "black",
    size  = 2.3,
    stroke = 0.4
  ) +

  geom_text(
    data = n_labels,
    aes(
      x     = period_en,
      y     = y_pos,
      label = paste0("n = ", n_citizen_scientists)
    ),
    inherit.aes = FALSE,
    size = 3
  ) +
  facet_wrap(~ municipio_tag, nrow = 2) +
  scale_fill_manual(values = okabe_ito, name = "Period") +
  scale_x_discrete(
    name   = "Period",
    limits = c("Before", "During", "After")
  ) +
  scale_y_continuous(name = "Records per unique citizen scientist") +
  expand_limits(y = y_max_global * 1.12) +

  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    strip.text      = element_text(face = "bold"),
    axis.title.x    = element_text(margin = margin(t = 6)),
    axis.title.y    = element_text(margin = margin(r = 6)),
    plot.title      = element_blank(),
    plot.subtitle   = element_blank(),
    plot.caption    = element_blank()
  )

print(p_box)

png_path  <- file.path(out_dir, "15_boxplots_records_per_citizen_scientist_by_city_period.png")
tiff_path <- file.path(out_dir, "15_boxplots_records_per_citizen_scientist_by_city_period.tiff")
eps_path  <- file.path(out_dir, "15_boxplots_records_per_citizen_scientist_by_city_period.eps")

ggsave(png_path, p_box, width = 8, height = 6, units = "in", dpi = 300, bg = "white")
message("PNG saved at:  ", png_path)

ggsave(tiff_path, p_box, device = "tiff",
       width = 8, height = 6, units = "in",
       dpi = 600, compression = "lzw", bg = "white")
message("TIFF saved at: ", tiff_path)

eps_device <- if (capabilities("cairo")) grDevices::cairo_ps else "eps"
ggsave(eps_path, p_box, device = eps_device, width = 8, height = 6, units = "in")
message("EPS saved at:  ", eps_path)
