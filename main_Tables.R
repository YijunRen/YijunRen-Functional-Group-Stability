
result_dir <- file.path("Dataset", "Table_Data")
out_dir <- "SI_Tables"
# Also accept the folder name Table_data.
if (!dir.exists(result_dir) && dir.exists(file.path("Dataset", "Table_data"))) {
  result_dir <- file.path("Dataset", "Table_data")
}

packages <- c("dplyr", "tidyr", "gt", "webshot2")
missing_packages <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages)) {
  stop("Missing R packages: ", paste(missing_packages, collapse = ", "))
}
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(gt)
})

table_files <- c(
  comparisons = "S1-S3_model_comparisons.csv",
  slopes      = "S4-S5_cross_field_slopes.csv",
  biomass     = "S6-S8_biomass_variance.csv",
  ecology     = "S10_complementary_ecology.csv",
  sensitivity = "S11-S16_sensitivity.csv"
)

fields <- c("Duolun", "CDR B", "CDR C")
groups <- c("All", "C3 grass", "C4 grass", "Forb")
metrics <- c("Scom", "Spop", "Asyn", "CPE", "SAE")
metric_labels <- c("Scom", "Spop", "Asyn", "CPE²", "SAE")
biomass_vars <- c("Mean biomass", "Total variance", "Species variance", "Covariance sum", "Community CV")
eco_labels <- c(Pmax = "Maximum species biomass proportion", CV_dom = "Dominant species temporal CV",
                mean_r_det = "Detrended mean pairwise correlation", turnover = "Interannual compositional turnover")

paths <- file.path(result_dir, unname(table_files))
if (any(!file.exists(paths))) {
  stop("Missing input CSV files. Check the data directory and filenames:\n",
       paste(paths[!file.exists(paths)], collapse = "\n"))
}
# Preserve column names on import and restore factor order when formatting the tables.
results <- setNames(lapply(paths, read.csv, stringsAsFactors = FALSE, check.names = FALSE,
                           fileEncoding = "UTF-8"), names(table_files))
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

fmt_num <- function(x) {
  ifelse(!is.finite(x), "—", ifelse(x != 0 & abs(x) < 0.01,
                                   sprintf("%.4f", x), sprintf("%.3f", x)))
}
fmt_p <- function(x) {
  ifelse(!is.finite(x), "—", ifelse(x < 0.001, "<0.001", sprintf("%.3f", x)))
}
fmt_ci <- function(slope, low, high) {
  ifelse(is.na(slope), "—", paste0(fmt_num(slope), " [", fmt_num(low), ", ", fmt_num(high), "]"))
}
display_labels <- function(d, plural = FALSE) {
  group_names <- if (plural) c("Whole", "C3 grasses", "C4 grasses", "Forbs") else c("Whole", groups[-1])
  mutate(d, Group = factor(Group, levels = groups, labels = group_names),
         Response = factor(Response, levels = metrics, labels = metric_labels))
}
style_table <- function(g) {
  g %>% cols_align("center", columns = everything()) %>%
    tab_style(style = cell_text(weight = "bold"), locations = cells_column_labels())
}
bold_spanners <- function(g) {
  tab_style(g, style = cell_text(weight = "bold"), locations = cells_column_spanners())
}
# Name each PNG using the table number followed by its title.
save_png <- function(g, number, title, ...) {
  filename <- sprintf("S%d %s.png", number, title)
  gtsave(g, filename = filename, path = out_dir, ...)
  message("Saved ", file.path(out_dir, filename))
}

# ---- S1–S3 ----
comparison_table <- function(d) {
  d %>% display_labels() %>% arrange(Group, Response) %>%
    dplyr::select(Group, Response, Comparison, Delta_R2, F_value, P_value, P_BH, Delta_AIC) %>%
    pivot_wider(names_from = Comparison, values_from = c(Delta_R2, F_value, P_value, P_BH, Delta_AIC)) %>%
    dplyr::select(Group, Response, ends_with("_Quadratic"), ends_with("_Treatment")) %>%
    gt() %>%
    tab_spanner("(A) Linear vs. Quadratic", columns = ends_with("_Quadratic")) %>%
    tab_spanner("(B) Linear vs. Treatment", columns = ends_with("_Treatment")) %>%
    cols_label(Response = "Metric",
               Delta_R2_Quadratic = html("&Delta;R<sup>2</sup>"), F_value_Quadratic = "F",
               P_value_Quadratic = html("<i>P</i>"), P_BH_Quadratic = html("<i>P</i><sub>BH</sub>"),
               Delta_AIC_Quadratic = html("&Delta;AIC"),
               Delta_R2_Treatment = html("&Delta;R<sup>2</sup>"), F_value_Treatment = "F",
               P_value_Treatment = html("<i>P</i>"), P_BH_Treatment = html("<i>P</i><sub>BH</sub>"),
               Delta_AIC_Treatment = html("&Delta;AIC")) %>%
    fmt_number(columns = matches("Delta_R2|F_value|Delta_AIC"), decimals = 3) %>%
    fmt(columns = matches("P_value|P_BH"), fns = fmt_p) %>% style_table() %>% bold_spanners()
}
for (i in seq_along(fields)) {
  d <- dplyr::filter(results$comparisons, Field == fields[i])
  save_png(comparison_table(d), i, paste("Model comparisons in", fields[i]))
}

# ---- S4–S5 ----
s4 <- results$slopes %>%
  mutate(Duolun = fmt_ci(Slope_Duolun, Low_Duolun, High_Duolun),
         `CDR B` = fmt_ci(Slope_CDR_B, Low_CDR_B, High_CDR_B),
         `CDR C` = fmt_ci(Slope_CDR_C, Low_CDR_C, High_CDR_C)) %>%
  display_labels(plural = TRUE) %>% arrange(Group, Response) %>%
  dplyr::select(Group, Response, all_of(fields), F_value, P_value, P_BH) %>% gt() %>%
  tab_spanner("Slope (95% CI)", columns = all_of(fields)) %>%
  tab_spanner("HC3 robust interaction test", columns = c(F_value, P_value, P_BH)) %>%
  cols_label(Response = "Metric", F_value = "F", P_value = html("<i>P</i>"), P_BH = html("<i>P</i><sub>BH</sub>")) %>%
  fmt_number(columns = F_value, decimals = 2) %>% fmt(columns = c(P_value, P_BH), fns = fmt_p) %>%
  style_table() %>% tab_options(table.font.size = px(14), data_row.padding = px(6))
save_png(s4, 4L, "Cross-field slope comparison (HC3)", zoom = 2, expand = 10)

s5 <- results$slopes %>%
  display_labels(plural = TRUE) %>% arrange(Group, Response) %>%
  dplyr::select(Group, Response, ends_with("_Duolun_CDR_B"), ends_with("_Duolun_CDR_C"), ends_with("_CDR_B_CDR_C")) %>% gt() %>%
  tab_spanner("Duolun vs. CDR B", columns = ends_with("_Duolun_CDR_B")) %>%
  tab_spanner("Duolun vs. CDR C", columns = ends_with("_Duolun_CDR_C")) %>%
  tab_spanner("CDR B vs. CDR C", columns = ends_with("_CDR_B_CDR_C")) %>%
  cols_label(Response = "Metric",
             Delta_Slope_Duolun_CDR_B = html("&Delta;Slope"), P_Holm_Duolun_CDR_B = html("<i>P</i><sub>Holm</sub>"),
             Delta_Slope_Duolun_CDR_C = html("&Delta;Slope"), P_Holm_Duolun_CDR_C = html("<i>P</i><sub>Holm</sub>"),
             Delta_Slope_CDR_B_CDR_C = html("&Delta;Slope"), P_Holm_CDR_B_CDR_C = html("<i>P</i><sub>Holm</sub>")) %>%
  fmt(columns = starts_with("Delta_Slope"), fns = fmt_num) %>% fmt(columns = starts_with("P_Holm"), fns = fmt_p) %>%
  style_table() %>% tab_options(table.font.size = px(14), data_row.padding = px(6))
save_png(s5, 5L, "Pairwise slope comparisons (HC3)", zoom = 2, expand = 10)

# ---- S6–S8 ----
biomass <- results$biomass %>%
  mutate(Response = factor(Response, levels = biomass_vars)) %>%
  transmute(Field, Response, Slope = round(Slope, 3), `95% CI` = sprintf("[%.3f, %.3f]", Low, High), P = fmt_p(P_value))
for (i in seq_along(fields)) {
  g <- biomass %>% dplyr::filter(Field == fields[i]) %>% arrange(Response) %>% dplyr::select(-Field) %>% gt() %>% style_table()
  save_png(g, i + 5L, paste("Biomass and variance responses in", fields[i]))
}

# ---- S10 ----
s10 <- results$ecology %>%
  mutate(Group = factor(Group, levels = groups[-1]),
         Metric = factor(Metric, levels = names(eco_labels), labels = unname(eco_labels))) %>%
  transmute(Metric, Group, Field = sub(" ", "_", Field), Slope_CI = fmt_ci(Slope, Low, High),
            P = fmt_p(P_value), P_BH = fmt_p(P_BH)) %>%
  pivot_wider(names_from = Field, values_from = c(Slope_CI, P, P_BH)) %>%
  dplyr::select(Metric, Group, ends_with("_Duolun"), ends_with("_CDR_B"), ends_with("_CDR_C")) %>%
  arrange(Metric, Group) %>% mutate(Metric = ifelse(duplicated(Metric), "", as.character(Metric))) %>%
  gt() %>% tab_spanner("Duolun", columns = ends_with("_Duolun")) %>%
  tab_spanner("CDR B", columns = ends_with("_CDR_B")) %>% tab_spanner("CDR C", columns = ends_with("_CDR_C")) %>%
  cols_label(Metric = "Ecological metric",
             Slope_CI_Duolun = "Slope [95% CI]", P_Duolun = html("<i>P</i>"), P_BH_Duolun = html("<i>P</i><sub>BH</sub>"),
             Slope_CI_CDR_B = "Slope [95% CI]", P_CDR_B = html("<i>P</i>"), P_BH_CDR_B = html("<i>P</i><sub>BH</sub>"),
             Slope_CI_CDR_C = "Slope [95% CI]", P_CDR_C = html("<i>P</i>"), P_BH_CDR_C = html("<i>P</i><sub>BH</sub>")) %>%
  style_table() %>% cols_align("left", columns = Metric) %>% bold_spanners() %>%
  tab_options(table.font.size = px(12), data_row.padding = px(9))
save_png(s10, 10L, "Complementary ecological analyses")

# ---- S11–S16 ----
sensitivity <- results$sensitivity %>% display_labels() %>% arrange(Field, Group, Response) %>%
  dplyr::select(Field, Group, Response, Analysis, `Slope (Full)` = Slope_full, Reduced = Slope_reduced,
         `Direction same` = Direction_same, `Significance change` = Significance_change)
reduced_labels <- c("Influence diagnostics" = "Slope (No influential)",
                    "High nitrogen sensitivity" = "Slope (Without highest N)")
first_number <- c("Influence diagnostics" = 11L, "High nitrogen sensitivity" = 14L)
analysis_title <- c("Influence diagnostics" = "Influence diagnostics",
                    "High nitrogen sensitivity" = "Sensitivity to excluding the highest nitrogen treatment")
for (analysis in names(reduced_labels)) {
  for (i in seq_along(fields)) {
    field <- fields[i]
    g <- sensitivity %>% dplyr::filter(Field == field, Analysis == analysis) %>% dplyr::select(-Field, -Analysis) %>%
      gt() %>% cols_label(Reduced = unname(reduced_labels[analysis])) %>%
      fmt(columns = c(`Slope (Full)`, Reduced), fns = fmt_num) %>% style_table()
    save_png(g, first_number[[analysis]] + i - 1L, paste(analysis_title[[analysis]], "in", field))
  }
}

message("Saved 15 supplementary tables to ", normalizePath(out_dir))
