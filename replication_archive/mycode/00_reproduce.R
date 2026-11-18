# 00_reproduce.R
# Reproduction of the original study

# Runs the replication pipeline provided by Murphy et al. (2026). The original scripts regenerate the published figures and tables and validate reported results using in_text_claims.R.

# Run this script before the replication and extension analyses.

# 1. Install required packages -------------------------------------------

pkgs <- c(
  "tidyverse", "here", "ggrepel", "glue", "readxl", "Matrix",
  "broom", "estimatr", "metafor", "clubSandwich", "modelsummary",
  "boot", "xtable", "psych", "GGally", "remotes"
)

missing <- pkgs[!pkgs %in% rownames(installed.packages())]

if (length(missing) > 0) {
  install.packages(missing)
}

# Install excheckr from GitHub if unavailable locally.
# This dependency is not distributed through CRAN.

if (!"excheckr" %in% rownames(installed.packages())) {
  remotes::install_github("acoppock/excheckr")
}

# 2. Run the original replication pipeline -------------------------------

source(here::here("run_all.R"))
