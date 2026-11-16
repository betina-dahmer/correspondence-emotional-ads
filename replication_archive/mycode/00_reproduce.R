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

# All scripts ran OK. All figures and tables from the article and appendix were regenerated in output/figures and output/tables. I compared Figures 1-4 against the published article and they match.

# CLAIM SUMMARY: asserted=72 matched=67 mismatched=0 unasserted=6

# What the numbers mean (from reading in_text_claims.R):
#   - "asserted" counts every claim() call, not only the ones compared.
#   - 67 claims have a published value and are compared. All match.
#   - 5 claims are marked expect = "derived": printed but not compared.
#     These are numbers the article hedges ("approximately 28,000",
#     "more than half the scale"), plus the alpha sample size and the
#     two-ad sample.
#   - "unasserted" = those 5 + the final claim_evidence() note = 6.
#   - So 72 - 67 = 5 is not a problem, just how the script labels things.
#
# Two things I learned by checking the audit against the published article:
#   1. The "article prints X, corrected to Y" notes refer to an earlier draft. The published version already has the corrected numbers.
#   2. The one real difference left is the two-ad sample (5,070 published, 5,077 in the code). Because it's marked "derived", it never shows up as a mismatch.

# The full published vs. reproduced comparison is in 01_replication_table.R.

# Problems I hit (and how I fixed them):

#   1. The first time I ran run_all.R, every script failed with "there is no package called 'metafor'". I didn't have all the packages from the README installed.
#      Missing: metafor, clubSandwich, psych, GGally, excheckr.
#      Fix: installed only the missing ones (section above).

#   2. excheckr still wouldn't install: "package 'excheckr' is not available for this version of R". That message is misleading: excheckr isn't on CRAN at all. It's Coppock's own package and lives on his GitHub.
#      Fix: remotes::install_github("acoppock/excheckr").