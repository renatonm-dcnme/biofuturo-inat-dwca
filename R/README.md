# R analysis scripts

This folder contains the **R scripts used in the manuscript analyses and figure generation**.
The scripts in `R/scripts/` are **comment-free copies** of the original analysis scripts, to keep the repository clean and easier to navigate.

## How to run
1. Ensure `data/processed/02_mestre_filtrada_bioblitz.csv` exists (see the repository root README for how to generate it).
2. Run scripts from the repository root so relative paths resolve correctly.
3. Most scripts write outputs to the `outputs/` folder.

## Script index

| Script | Figure | Purpose |
|---|---:|---|
| `06_unique_users_by_city_period.R` |  | Barplot de usuários únicos por município e período (Before/During/After), com municípi… |
| `06export(fig3)_unique_users_by_city_period.R` | Fig 3 | Barplot de citizen scientists únicos por município e período (Before/During/After), co… |
| `07_stats_unique_users.R` |  | Análises estatísticas de usuários únicos (Before / During / After) - Qui-quadrado exploratório … |
| `08_chisq_unique_users_by_city.R` |  | Filtrar apenas Araras, Before e After |
| `08b_chisq_unique_users_by_city.R` |  | Qui-quadrado simples por município: compara Before vs After de usuários únicos, usando … |
| `09_bioblitz_users_by_city.R` |  | Exporta CSV com usuários que fizeram registros nos dias da Bioblitz em cada município. |
| `09b_export_bioblitz_users.R` |  | Exporta CSV com usuários que fizeram registros nos dias da Bioblitz em cada município, inclu… |
| `10_lorenz_gini_by_city_period.R` |  | Lorenz curves and Gini coefficients of user contribution Base: 02_mestre_filtrada_biobl… |
| `10export(fig4)_lorenz_gini_by_city_period.R` | Fig 4 | Lorenz curves and Gini coefficients of citizen scientist contribution Base: 02_mestre_f… |
| `11_bootstrap_gini_distributions.R` |  | Bootstrap distributions and confidence intervals for Gini coefficients Base: 02_mestr… |
| `12_permutation_tests_gini.R` |  | Permutation tests for differences in Gini coefficients between periods Base: 02_mestre_filt… |
| `13_total_records_by_city_period.R` |  | Barplot de TOTAL de registros iNaturalist por município e período (Before/During/Afte… |
| `13export(fig5)_total_records_by_city_period.R` | Fig 5 | Barplot de TOTAL de registros iNaturalist por município e período (Before/During/Afte… |
| `14_chisq_total_records_by_city.R` |  | Qui-quadrado simples por município: compara Before vs After de TOTAL de registros, usa… |
| `15_boxplots_records_per_user_by_city_period.R` |  | Boxplots de registros por usuário único, por município e período (Before/… |
| `15export(fig6)_boxplots_records_per_user_by_city_period.R` | Fig 6 | Boxplots de registros por citizen scientist único, por município e períod… |
| `16_kruskal_records_per_user_by_city_period.R` |  | Análise de registros por usuário único (n_records) por município e período… |
| `17_mann_whitney_records_per_user_before_after.R` |  | Comparação Before vs After da distribuição de registros por usuário (n_… |
| `18_daily_avg_records_per_user_timeseries.R` |  | Daily average number of records per unique iNaturalist user around … |
| `18export(fig7)_daily_avg_records_per_user_timeseries_relative.R` | Fig 7 | Daily average number of records per unique iNaturalist citizen scie… |
| `18export2(revfig7)_daily_avg_records_per_user_timeseries_relative.R` |  | Daily average number of records per unique iNaturalist citizen scie… |
| `19_pie_quality_by_city_period.R` |  | Pie charts: share of quality_grade by municipality × period (each pie = 100% within pan… |
| `19export(fig8)_pie_quality_by_city_period.R` | Fig 8 | Pie charts: share of quality_grade by municipality × period (each pie = 100% within pan… |
| `27_iconic_records_stacked_bars.R` |  | Research-grade records by iconic taxon (stacked bars) per municipality and period Outp… |
| `27export(fig9)_iconic_records_stacked_bars.R` | Fig 9 | Research-grade records by iconic taxon (stacked bars) per municipality and period Outp… |
| `28_iconic_after_before_ratio.R` |  | After/Before ratio of research-grade records by iconic taxon and municipality Outputs: o… |
| `28export(fig10)_iconic_after_before_ratio.R` | Fig 10 | After/Before ratio of research-grade records by MAJOR TAXONS and municipality Outputs: o… |
| `29_iconic_chisq_before_after.R` |  | Chi-square tests comparing Before vs After research-grade records for iconic taxa in eac… |
| `30_daily_cumulative_iconic_taxa_by_city_rel_days.R` |  | Daily cumulative number of iconic taxa (iNaturalist iconic groups) around Biobli… |
| `30export(fig11)_cumulative_iconic_taxa_timeseries.R` | Fig 11 | Daily cumulative number of iconic taxa around Bioblitz (±30 days) Base: research… |
| `31_boxplots_unique_taxa_per_user_by_city_period.R` |  | Boxplots de número de táxons únicos (menor nível taxonômico disponíve… |
| `31export(fig12)_boxplots_unique_taxa_per_user_by_city_period.R` | Fig 12 | Boxplots: number of unique taxons (lowest taxonomic level available) … |
| `32_mann_whitney_unique_taxa_per_user_BA_by_city.R` |  | See script name. |
| `33_lag_upload_violin_by_city_period.R` |  | Upload lag (days) between photo date and iNaturalist record creation Violin plots… |
| `33export(figS1)_lag_upload_violin_by_city_period.R` | Fig S1 | Upload lag (days) between photo date and iNaturalist record creation Violin plots… |
| `34_user_retention_bioblitz.R` |  | Retention of users whose first record in the ±30-day window occurred during the Bioblitz a… |
| `35_daily_cumulative_users_and_records_by_city.R` |  | Figure S2: Daily cumulative number of unique iNaturalist users Figure S… |
| `35export(figS3)_daily_cumulative_users_and_records_by_city.R` | Fig S3 | Figure S2: Daily cumulative number of unique iNaturalist users Figure S… |
