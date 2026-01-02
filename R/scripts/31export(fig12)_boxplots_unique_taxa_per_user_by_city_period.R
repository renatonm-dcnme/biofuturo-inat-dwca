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
    ),
    quality_grade = tolower(as.character(quality_grade))
  )

cat("### Dimensão da base mestre ###\n")
print(dim(mestre))

mestre_rg <- mestre %>%
  filter(quality_grade == "research")

cat("\n### Dimensão da base mestre_rg (apenas research grade) ###\n")
print(dim(mestre_rg))

get_chr <- function(df, nm) {
  if (nm %in% names(df)) as.character(df[[nm]]) else rep(NA_character_, nrow(df))
}

cand <- c(
  "taxon_id",
  "community_taxon_id",
  "taxon.id",
  "scientific_name",
  "taxon.name",
  "community_taxon_id.raw",
  "taxon.id.raw",
  "taxon_id.raw",
  "scientific_name.raw",
  "taxon.name.raw",
  "species_guess"
)

mat <- tibble::tibble(
  !!!setNames(lapply(cand, function(nm) get_chr(mestre_rg, nm)), cand)
)

mestre_rg$taxon_key <- do.call(dplyr::coalesce, as.list(mat))

mestre_rg <- mestre_rg %>%
  filter(!is.na(taxon_key) & taxon_key != "")

cat("\n### Dimensão da base mestre_rg após filtrar taxon_key não vazia ###\n")
print(dim(mestre_rg))

user_taxa <- mestre_rg %>%
  filter(!is.na(user.id)) %>%
  group_by(municipio_tag, period_en, user.id) %>%
  summarise(
    n_unique_taxa = n_distinct(taxon_key),
    .groups       = "drop"
  )

cat("\n### Dimensão da base user_taxa (táxons únicos por usuário × município × período) ###\n")
print(dim(user_taxa))

cat("\n### Amostra de user_taxa ###\n")
print(head(user_taxa, 10))

user_taxa_summary <- user_taxa %>%
  group_by(municipio_tag, period_en) %>%
  summarise(
    n_users         = n(),
    mean_unique     = mean(n_unique_taxa),
    median_unique   = median(n_unique_taxa),
    sd_unique       = sd(n_unique_taxa),
    min_unique      = min(n_unique_taxa),
    max_unique      = max(n_unique_taxa),
    .groups         = "drop"
  )

cat("\n### Resumo por município × período (n usuários, média, mediana, etc.) ###\n")
print(user_taxa_summary)

out_dir <- "outputs"
fs::dir_create(out_dir)

readr::write_csv(
  user_taxa_summary,
  file.path(out_dir, "31_user_unique_taxa_summary_by_city_period.csv")
)

cat("\nArquivo de resumo salvo em: outputs/31_user_unique_taxa_summary_by_city_period.csv\n")

okabe_ito <- c(
  "Before" = "#E69F00",
  "During" = "#56B4E9",
  "After"  = "#009E73"
)

width_before_after <- 0.6
width_during       <- 0.35

y_max_global <- max(user_taxa$n_unique_taxa, na.rm = TRUE)

n_labels <- user_taxa %>%
  group_by(municipio_tag, period_en) %>%
  summarise(n_users = n(), .groups = "drop") %>%
  mutate(y_pos = y_max_global * 1.05)

cat("\n### Tabela de n de usuários por município × período (para rótulos no gráfico) ###\n")
print(n_labels)

p_box_taxa <- ggplot(
  user_taxa,
  aes(x = period_en, y = n_unique_taxa, fill = period_en)
) +
  geom_boxplot(
    data  = dplyr::filter(user_taxa, period_en != "During"),
    width = width_before_after,
    outlier.size = 1.5,
    alpha = 0.9,
    size = 0.3
  ) +
  geom_boxplot(
    data  = dplyr::filter(user_taxa, period_en == "During"),
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
      label = paste0("n = ", n_users)
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

  scale_y_continuous(name = "Unique taxons per citizen scientist") +
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

print(p_box_taxa)

stem <- file.path(out_dir, "31_boxplots_unique_taxa_per_user_by_city_period")
png_out  <- paste0(stem, ".png")
tiff_out <- paste0(stem, ".tiff")
eps_out  <- paste0(stem, ".eps")

ggsave(
  filename = png_out,
  plot     = p_box_taxa,
  width    = 8,
  height   = 6,
  units    = "in",
  dpi      = 300,
  bg       = "white"
)

ggsave(
  filename = tiff_out,
  plot     = p_box_taxa,
  device   = "tiff",
  width    = 8,
  height   = 6,
  units    = "in",
  dpi      = 600,
  compression = "lzw",
  bg       = "white"
)

eps_device <- if (capabilities("cairo")) grDevices::cairo_ps else "eps"
ggsave(
  filename = eps_out,
  plot     = p_box_taxa,
  device   = eps_device,
  width    = 8,
  height   = 6,
  units    = "in"
)

cat("\nFigura salva em:\n- ", png_out, "\n- ", tiff_out, "\n- ", eps_out, "\n", sep = "")
