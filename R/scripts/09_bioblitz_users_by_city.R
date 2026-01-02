suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(fs)
})

out_dir <- "outputs"
fs::dir_create(out_dir)

mestre <- readr::read_csv(
  "data/processed/02_mestre_filtrada_bioblitz.csv",
  show_col_types = FALSE
) %>%
  mutate(
    observed_on   = as.Date(observed_on),
    municipio_tag = as.character(municipio_tag)
  )

bioblitz_days <- tibble::tribble(
  ~municipio_tag,            ~observed_on,
  "São Carlos",              as.Date("2025-05-17"),
  "São Carlos",              as.Date("2025-05-18"),
  "Araras",                  as.Date("2025-05-24"),
  "Sorocaba",                as.Date("2025-06-14"),
  "Campina do Monte Alegre", as.Date("2025-06-28")
)

bioblitz_users <- mestre %>%
  inner_join(
    bioblitz_days,
    by = c("municipio_tag", "observed_on")
  ) %>%

  filter(!is.na(user.login) | !is.na(user.name)) %>%

  distinct(municipio_tag, user.id, user.login, user.name) %>%
  arrange(municipio_tag, user.login) %>%

  select(municipio_tag, user.login, user.name)

cat("### Prévia da tabela de usuários da Bioblitz ###\n")
print(head(bioblitz_users, 20))

out_csv <- file.path(out_dir, "09_bioblitz_users_by_city.csv")
readr::write_csv(bioblitz_users, out_csv)

cat("\nArquivo CSV salvo em:\n", out_csv, "\n")
