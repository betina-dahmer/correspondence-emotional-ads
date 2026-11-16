# 00_reproduce.R

# Before changing anything, I want to check that the authors' own code and data give the same numbers that are in the paper. The authors made this easy: run_all.R runs every script in order and, at the end, in_text_claims.R checks every number in the article against the output (the "CLAIM SUMMARY" line).

# Packages  -----------------------------------------------------

# List every package the authors' code needs, check which ones are not installed on this computer, and install only those. Safe to rerun: if everything is already installed, it does nothing.

pkgs <- c("tidyverse", "here", "ggrepel", "glue", "readxl", "Matrix",
          "broom", "estimatr", "metafor", "clubSandwich", "modelsummary",
          "boot", "xtable", "psych", "GGally", "remotes")

missing <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(missing) > 0) install.packages(missing)

# excheckr is Coppock's own package (it checks that the data have the expected shape). It is not on CRAN, so install.packages() can't find it and gives a misleading error ("not available for this version of R").

# Note: the authors' README tells you to install it with install.packages() along with the CRAN packages, so following their instructions as written fails here. 
# Fix: get it straight from his GitHub.

if (!"excheckr" %in% rownames(installed.packages())) {
  remotes::install_github("acoppock/excheckr")
}

# Run the original replication pipeline ---------------------------------
source(here::here("run_all.R"))

# What happened --------------------------------------------------------
# All scripts ran OK.
# CLAIM SUMMARY: asserted=72 matched=67 mismatched=0 unasserted=6
# 72 - 67 - 0 = 5 claims not accounted for. Need to check in_text_claims.R to see why.

# Main results (Fig 2-4 slopes) reproduce. Nothing significant, same as the paper. But "matched" compares against corrected values, not always the published text. The script's own notes flag differences:
#   - alpha 0.88 (printed) vs 0.89
#   - Trump contrast ads: two (printed) vs three
#   - SEs: 0.042 vs 0.041, 0.023 vs 0.024, 0.055 vs 0.059,
#     0.039 vs 0.041
#   - two-ad subjects: 5,070 vs 5,077
#   - Republicans rating pro-Rep ad higher: 60.3% printed vs 87.1%
#     (60.3% is actually the independents' number)
#   TODO: confirm each one against the published PDF.

# R version I used: 4.5.0 (the authors used 4.6.0)
# metafor 5.2-1 (authors used 5.0-1). Same results anyway.

# Problems I hit: metafor, clubSandwich, psych, GGally missing;

