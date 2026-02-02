# neuropsych-gamm-ecuador

Reproducible R pipeline to develop Ecuadorian neuropsychological norms using **Generalized Additive (Mixed) Models (GAMM)**. The project harmonizes child/adult datasets, applies exclusion criteria, fits GAM(M) models with a single **holdout split (70/30)**, and stores final models for normative tables/percentiles.

## Structure
- `01_data_prep_children_adults.R` — load/merge Excel sheets, exclusions, harmonization, final `data`
- `02_fit_gamm_models.R` — fit GAM(M) models (Poisson/quasi-Poisson/NB/Gamma), holdout metrics, exports results + model summaries

## Requirements
R packages: `readxl`, `dplyr`, `tidyr`, `purrr`, `stringr`, `mgcv`, `openxlsx`

## Outputs
- `gamm_model_comparison_holdout.xlsx` — model fit + holdout performance + overfit signals
- `gamm_models_summary.txt` — summaries for selected final models (NB + Gamma)

