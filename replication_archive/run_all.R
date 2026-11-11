# Master replication script
# "Do Emotional Ads Persuade? Evidence from Real-Time Campaign Advertising Experiments"
# Murphy, Coppock, Hill, and Vavreck
# Political Communication
#
# Output: everything in output/figures/ and output/tables/, plus
#         output/session_info.txt and the in-text claims audit on stdout
# Depends on: code/helpers.R and the files in data/
# Description: runs every script in dependency order and records the environment.
#
# Run from anywhere inside the archive; paths resolve against the .here file at
# the archive root:
#   Rscript run_all.R
#
# Total runtime is roughly 25 minutes, of which about 9 are the two bootstraps.

run_bootstraps <- TRUE

run <- function(script) {
  cat(paste0("  Running ", script, " ... "))
  result <- system2("Rscript", here::here(script), stdout = TRUE, stderr = TRUE)
  status <- attr(result, "status")
  if (!is.null(status) && status != 0) {
    cat("FAILED\n")
    cat(paste(result, collapse = "\n"), "\n")
    stop(paste("Script failed:", script))
  }
  cat("OK\n")
  invisible(result)
}

cat("=== Step 1: Clean data ===\n")
run("code/clean_exps.R")
run("code/clean_individual_ratings.R")

cat("\n=== Step 2: Estimate ratings by PID ===\n")
run("code/estimate_ratings_pid.R")

cat("\n=== Step 3: Estimate CATEs ===\n")
run("code/estimate_all_cates.R")
run("code/estimate_all_cates_with_stretch.R")

if (run_bootstraps) {
  cat("\n=== Step 4: Bootstrap accuracy estimates (about 9 minutes) ===\n")
  run("code/estimate_individual_accuracy_bootstrap.R")
  run("code/estimate_group_accuracy_bootstrap.R")
} else {
  cat("\n=== Step 4: Skipping bootstraps (using the results shipped in data/estimates/) ===\n")
}

cat("\n=== Step 5: Meta-regressions and tables ===\n")
run("code/estimate_meta_regressions.R")
run("code/estimate_meta_regressions_with_stretch.R")

cat("\n=== Step 6: Main figures ===\n")
run("code/figure_1_emotion_by_partisanship.R")
run("code/figure_2_emotion_cate_favorability_vote.R")
run("code/figure_3_intended_emotion_cate_favorability_vote.R")
run("code/figure_4_emotion_cate_issue_turnout.R")

cat("\n=== Step 7: Appendix figures ===\n")
run("code/figure_a1_emotion_scree.R")
run("code/figure_a2_emotion_correlations.R")
run("code/figure_b3_emotion_by_party.R")
run("code/figure_c4_stretch_issue_turnout_emotion.R")
run("code/figure_c5_issue_turnout_intended_emotion.R")
run("code/figure_c6_stretch_issue_turnout_intended_emotion.R")
run("code/figure_e7_rating_correlations.R")
run("code/figure_e8_rating_scree.R")
run("code/figure_e9_ratings_by_party.R")
run("code/figure_e10_cates_from_ratings.R")
run("code/figure_e11_individual_accuracy.R")
run("code/figure_e12_group_accuracy.R")

cat("\n=== Step 8: Appendix tables ===\n")
run("code/table_d16_ad_emotion_coding.R")

cat("\n=== Step 9: In-text claims audit ===\n")
claims <- run("code/in_text_claims.R")
cat(paste(claims, collapse = "\n"), "\n")

cat("\n=== Step 10: Record the environment ===\n")

pkgs <- c("tidyverse", "dplyr", "ggplot2", "tidyr", "readr", "purrr", "tibble",
          "stringr", "forcats", "here", "ggrepel", "glue", "readxl", "Matrix",
          "broom", "estimatr", "metafor", "clubSandwich", "modelsummary",
          "boot", "xtable", "psych", "GGally", "excheckr")
versions <- vapply(
  pkgs,
  function(p) tryCatch(as.character(utils::packageVersion(p)), error = function(e) NA_character_),
  character(1)
)

writeLines(
  c(capture.output(sessionInfo()),
    "",
    "Package versions used by this pipeline:",
    sprintf("  %-14s %s", names(versions), ifelse(is.na(versions), "not installed", versions))),
  here::here("output", "session_info.txt")
)
cat("  Written to output/session_info.txt\n")

cat("\n=== Done ===\n")
cat("Figures saved to output/figures/ (PDF and PNG)\n")
cat("Tables saved to output/tables/\n")
cat("Environment recorded in output/session_info.txt\n")
