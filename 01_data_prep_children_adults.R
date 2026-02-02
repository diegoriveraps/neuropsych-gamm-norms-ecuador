############################################################
# Project: Neuropsych-GAMM-Ecuador
# Script: 01_data_prep_children_adults.R
# Purpose:
#   1) Load and merge CHILD and ADULT Excel workbooks (multiple sheets each)
#   2) Apply exclusion criteria (mental health cutoffs, IQ cutoff, etc.)
#   3) Harmonize common variables and bind into one analysis dataset
#   4) Basic cleaning + coding standardization (sex, factors, missingness)
#
# Inputs (edit in CONFIG):
#   - "BASE DE DATOS (MADRE) - INEDITA NIÑXS.xlsx"
#   - "BDD MADRE - INEDITA ADULTOS.xlsx"
#
# Output objects (in memory):
#   - child_df, adult_df, combined_df, data
#
# Notes:
#   - This is a plain R script (no Quarto/Rmd).
#   - Keep raw Excel files outside version control if sensitive.
############################################################

## =========================
## 0) Setup
## =========================
rm(list = ls())
set.seed(20260202)

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(purrr)
  library(tidyr)
  library(stringr)
})

CONFIG <- list(
  # ---- file paths ----
  child_file = "BASE DE DATOS (MADRE) - INEDITA NIÑXS.xlsx",
  adult_file = "BDD MADRE - INEDITA ADULTOS.xlsx",
  
  # ---- sheet names ----
  child_sheets = c(
    "1_SOCIO", "2_2_ANSIEDAD", "2_3_CDI", "2_4_TONI_II",
    "3_2_SDMT", "3_5_WISCONSIN", "3_3_TMT", "3_7_STROOP"
  ),
  adult_sheets = c(
    "INFORMACIÓN_SOCIODEMOGRÁFICA", "PHQ-9", "GAD-7",
    "SDMT", "M-WCST", "TMT", "STROOP"
  ),
  
  # ---- merge keys ----
  child_id = "Id",
  adult_id = "CÓDIGO",
  
  # ---- exclusion cutoffs ----
  cutoffs = list(
    ansiedad_total_max = 14,
    cdi_total_max = 19,
    toni_ci_min = 80,
    mpe_max_child = 40,
    gad7_max = 10,
    phq9_max = 10
  ),
  
  # ---- basic cleaning ----
  education_z_min = -2,      # keep education_z > -2 OR NA (same as your rule)
  drop_na_ranges = list(
    common_cols_2_7 = 2:7,   # as in your code (after binding)
    col_19 = 19              # as in your code (after binding)
  ),
  
  # frequency tables
  freq_vars = c("sex", "province", "region", "zone")
)

## =========================
## 1) Helper functions
## =========================

load_and_merge_excel <- function(file_path, sheets, id_col) {
  # Reads multiple sheets and full-joins them by id_col
  dfs <- map(sheets, ~ read_excel(file_path, sheet = .x))
  names(dfs) <- sheets
  
  merged <- reduce(dfs, ~ full_join(.x, .y, by = id_col))
  return(merged)
}

zscore <- function(x) {
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}

standardize_sex <- function(x) {
  x <- as.character(x)
  case_when(
    x %in% c("FEMENINO", "MUJER", "Female", "FEMALE") ~ "Female",
    x %in% c("HOMBRE", "MASCULINO", "Male", "MALE") ~ "Male",
    TRUE ~ x
  )
}

print_freq_table <- function(df, var) {
  if (!var %in% names(df)) {
    message("Variable not found: ", var)
    return(invisible(NULL))
  }
  out <- df %>%
    count(.data[[var]], name = "Frequency") %>%
    arrange(desc(Frequency))
  cat("\nFrequency table for: ", var, "\n", sep = "")
  print(out)
  invisible(out)
}

## =========================
## 2) Load + merge CHILD data
## =========================
child_merged <- load_and_merge_excel(
  file_path = CONFIG$child_file,
  sheets    = CONFIG$child_sheets,
  id_col    = CONFIG$child_id
)

cat("\nCHILD merged dimensions: ", paste(dim(child_merged), collapse = " x "), "\n", sep = "")

## =========================
## 3) Exclusions: CHILD
## =========================
child_df <- child_merged %>%
  filter(is.na(`Ansiedad Total`) | `Ansiedad Total` <= CONFIG$cutoffs$ansiedad_total_max) %>%
  filter(is.na(`CDI Total`)      | `CDI Total`      <= CONFIG$cutoffs$cdi_total_max) %>%
  filter(is.na(`TONI CI`)        | `TONI CI`        >= CONFIG$cutoffs$toni_ci_min) %>%
  filter(is.na(mpe)              | mpe              <  CONFIG$cutoffs$mpe_max_child)

cat("\nCHILD after exclusions: ", paste(dim(child_df), collapse = " x "), "\n", sep = "")

## =========================
## 4) Load + merge ADULT data
## =========================
adult_merged <- load_and_merge_excel(
  file_path = CONFIG$adult_file,
  sheets    = CONFIG$adult_sheets,
  id_col    = CONFIG$adult_id
)

cat("\nADULT merged dimensions: ", paste(dim(adult_merged), collapse = " x "), "\n", sep = "")

## =========================
## 5) Exclusions: ADULT + harmonize ID
## =========================
adult_df <- adult_merged %>%
  filter(is.na(GAD_7) | GAD_7 <= CONFIG$cutoffs$gad7_max) %>%
  filter(is.na(PHQ_9) | PHQ_9 <= CONFIG$cutoffs$phq9_max)

# Rename adult ID column to match child ("Id")
if (CONFIG$adult_id %in% names(adult_df)) {
  adult_df <- adult_df %>% rename(Id = all_of(CONFIG$adult_id))
}

cat("\nADULT after exclusions: ", paste(dim(adult_df), collapse = " x "), "\n", sep = "")

## =========================
## 6) Create education z-scores (within group, as in your code)
## =========================
if ("education" %in% names(child_df)) child_df$education_z <- zscore(child_df$education)
if ("education" %in% names(adult_df)) adult_df$education_z <- zscore(adult_df$education)

## =========================
## 7) Bind CHILD + ADULT on common columns only
## =========================
common_cols <- intersect(names(child_df), names(adult_df))

child_common <- child_df %>%
  select(all_of(common_cols)) %>%
  mutate(source = "child")

adult_common <- adult_df %>%
  select(all_of(common_cols)) %>%
  mutate(source = "adult")

combined_df <- bind_rows(child_common, adult_common)

cat("\nCOMBINED dimensions: ", paste(dim(combined_df), collapse = " x "), "\n", sep = "")

## =========================
## 8) Standardize sex + basic checks
## =========================
if ("sex" %in% names(combined_df)) {
  combined_df <- combined_df %>%
    mutate(sex = standardize_sex(sex),
           sex = factor(sex))
}

if ("income_mensual" %in% names(combined_df)) {
  combined_df$income_mensual <- NULL
}

# Frequency tables (if variables exist)
walk(CONFIG$freq_vars, ~ print_freq_table(combined_df, .x))

## =========================
## 9) Missingness handling + filters (replicates your logic)
## =========================
# Drop NA in columns 2:7 (only if those positions exist)
rng <- CONFIG$drop_na_ranges$common_cols_2_7
if (ncol(combined_df) >= max(rng)) {
  combined_df <- combined_df %>% drop_na(all_of(names(combined_df)[rng]))
}

# Drop NA in column 19 (only if it exists)
c19 <- CONFIG$drop_na_ranges$col_19
if (ncol(combined_df) >= c19) {
  combined_df <- combined_df %>% drop_na(all_of(names(combined_df)[c19]))
}

# Filter by education_z rule: keep education_z > -2 OR NA
if ("education_z" %in% names(combined_df)) {
  combined_df <- combined_df %>%
    filter(education_z > CONFIG$education_z_min | is.na(education_z))
}

## =========================
## 10) Final analysis dataset
## =========================
# Use na.exclude (keeps NA structure for some modeling functions)
data <- na.exclude(combined_df)

# region as factor (if present)
if ("region" %in% names(data)) {
  data$region <- as.factor(data$region)
}

cat("\nFINAL analysis dataset (data) dimensions: ", paste(dim(data), collapse = " x "), "\n", sep = "")

# Quick snapshot
cat("\nSummary (first 20 variables):\n")
print(summary(data[, seq_len(min(20, ncol(data)))]))

## =========================
## 11) Reproducibility footer
## =========================
cat("\nSession info:\n")
print(sessionInfo())
