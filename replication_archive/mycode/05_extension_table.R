# 05_extension_table.R
# Differences in emotion-persuasion associations by partisan strength

# Reports differences between strong and weak/lean partisans within each party, with and without adjustment for ad side. Holm-adjusted p-values account for eight comparisons.

# Requires: 03_extension.R

# 1. Load data and dependencies ------------------------------------------

library(tidyverse)

out <- function(file) here::here("mycode", "output", file)

difference_test <- read_csv(
  out("extension_difference_test.csv"),
  show_col_types = FALSE
)

robustness_test <- read_csv(
  out("extension_robustness_test.csv"),
  show_col_types = FALSE
)

# 2. Format estimates -----------------------------------------------------

fmt <- function(est, se) {
  sprintf("%.3f (%.3f)", est, se)
}

# 3. Assemble extension table --------------------------------------------

extension_table <- difference_test |>
  select(
    party, index, dv,
    basic = estimate,
    basic_se = std.error,
    basic_holm = p.holm
  ) |>
  left_join(
    robustness_test |>
      select(
        party, index, dv,
        adjusted = estimate,
        adjusted_se = std.error,
        adjusted_holm = p.holm
      ),
    by = c("party", "index", "dv")
  ) |>
  transmute(
    Party = party,
    Emotion = recode(
      index,
      negative_index = "Negative",
      positive_index = "Positive"
    ),
    Outcome = dv,
    `Baseline (SE)` = fmt(basic, basic_se),
    `Baseline Holm p` = sprintf("%.3f", basic_holm),
    `Ad-side adjusted (SE)` = fmt(adjusted, adjusted_se),
    `Adjusted Holm p` = sprintf("%.3f", adjusted_holm)
  )

# 4. Inspect and export table --------------------------------------------

print(extension_table)

write_csv(
  extension_table,
  out("extension_table.csv")
)

print(
  xtable::xtable(
    extension_table,
    caption = paste(
      "Differences between strong and weak/lean partisans",
      "in emotion-persuasion associations"
    ),
    label = "tab:extension"
  ),
  include.rownames = FALSE,
  booktabs = TRUE,
  file = out("extension_table.tex")
)
