# 06_minimum_detectable_effects.R
# Statistical precision and minimum detectable effects

# Part A: Minimum detectable effects for the original Figure 2.
# Part B: Precision of partisan-strength interactions.

# Requires: 00_reproduce.R and 03_extension.R

library(tidyverse)

out <- function(file) here::here("mycode", "output", file)

# PART A: MINIMUM DETECTABLE EFFECTS -------------------------

# A1. Load original meta-regression estimates ----------------

pooled <- read_csv(
  here::here("output", "tables", "figure_2_meta_estimates.csv"),
  show_col_types = FALSE
) |>
  filter(
    dv %in% c("Favorability", "Vote Choice"),
    index %in% c("negative_index", "positive_index")
  ) |>
  select(index, dv, estimate, std.error)

# A2. Calculate the observed emotion range -------------------
# Use the 90th and 10th percentiles of emotional responses
# across ads, waves, and partisan groups.

emotion_spread <- read_rds(
  here::here("data", "estimates", "ratings_pid.rds")
) |>
  filter(
    rating_outcome %in% c("positive_index", "negative_index")
  ) |>
  summarise(
    p10 = quantile(rating_estimate, 0.10, na.rm = TRUE),
    p90 = quantile(rating_estimate, 0.90, na.rm = TRUE),
    .by = rating_outcome
  ) |>
  rename(index = rating_outcome) |>
  mutate(emotion_gap = p90 - p10)

print(emotion_spread)

# A3. Calculate the average signed CATE ----------------------

avg_effect <- read_rds(
  here::here("data", "estimates", "all_cates.rds")
) |>
  mutate(
    dv = case_when(
      str_detect(outcome, "favor") ~ "Favorability",
      str_detect(outcome, "general_vote") ~ "Vote Choice"
    )
  ) |>
  filter(!is.na(dv)) |>
  summarise(
    avg_ad_effect = mean(estimate),
    .by = dv
  )

print(avg_effect)

# A4. Calculate minimum detectable effects ------------------
# Normal approximation: 80% power, two-sided 5% test,
# and a fixed standard error.
# These are approximate power benchmarks, not confidence bounds.

mde_results <- pooled |>
  left_join(emotion_spread, by = "index") |>
  left_join(avg_effect, by = "dv") |>
  mutate(
    mde_slope = 2.80 * std.error,
    mde_gap_100 = 100 * mde_slope * emotion_gap,
    avg_ad_effect_100 = 100 * avg_ad_effect,
    ratio = mde_gap_100 / avg_ad_effect_100
  ) |>
  arrange(index, dv)

stopifnot(
  nrow(mde_results) == 4,
  all(is.finite(mde_results$mde_gap_100)),
  all(is.finite(mde_results$ratio))
)

print(mde_results, n = Inf, width = Inf)

# A5. Format and export Table 2 ------------------------------

mde_table <- mde_results |>
  transmute(
    Index = recode(
      index,
      negative_index = "Negative",
      positive_index = "Positive"
    ),
    Outcome = dv,
    `Slope (SE)` = sprintf("%.3f (%.3f)", estimate, std.error),
    `Minimum detectable gap` = sprintf("%.1f", mde_gap_100),
    `Average ad effect` = sprintf("%.1f", avg_ad_effect_100),
    Ratio = sprintf("%.1f", ratio)
  )

print(mde_table, n = Inf, width = Inf)

write_csv(mde_results, out("mde_results.csv"))
write_csv(mde_table, out("mde_table.csv"))

# PART B: PRECISION OF PARTISAN-STRENGTH INTERACTIONS --------


# 1. Load extension results -----------------------------------------------

difference_test <- readr::read_csv(
  here::here("mycode", "output", "extension_difference_test.csv"),
  show_col_types = FALSE
)

robustness_test <- readr::read_csv(
  here::here("mycode", "output", "extension_robustness_test.csv"),
  show_col_types = FALSE
)


# 2. Calculate confidence intervals ---------------------------------------

# CR2 confidence intervals use the model-specific degrees of freedom.
# Each interaction compares strong with weak/lean partisans.

add_precision <- function(data, specification) {
  data |>
    dplyr::mutate(
      specification = specification,
      critical_value = qt(0.975, df),
      ci_low = estimate - critical_value * std.error,
      ci_high = estimate + critical_value * std.error,
      
      # Convert slope differences to a 0-100 outcome scale
      estimate_100 = 100 * estimate,
      ci_low_100 = 100 * ci_low,
      ci_high_100 = 100 * ci_high,
      
      # Largest absolute slope difference compatible with the CI
      max_compatible_100 = pmax(
        abs(ci_low_100),
        abs(ci_high_100)
      )
    )
}

precision <- dplyr::bind_rows(
  add_precision(difference_test, "Baseline"),
  add_precision(robustness_test, "Ad-side adjusted")
)

stopifnot(
  nrow(precision) == 16,
  all(is.finite(precision$ci_low)),
  all(is.finite(precision$ci_high))
)


# 3. Summarize precision ---------------------------------------------------

precision_table <- precision |>
  dplyr::select(
    specification,
    party,
    index,
    dv,
    estimate,
    std.error,
    df,
    p.value,
    p.holm,
    ci_low,
    ci_high,
    estimate_100,
    ci_low_100,
    ci_high_100,
    max_compatible_100
  )

print(precision_table, n = Inf, width = Inf)


# 4. Summarize statistical evidence ---------------------------------------

precision_summary <- precision |>
  dplyr::summarise(
    n_tests = dplyr::n(),
    n_nominal_p05 = sum(p.value < 0.05),
    n_holm_p05 = sum(p.holm < 0.05),
    median_ci_width_100 = median(
      ci_high_100 - ci_low_100
    ),
    .by = specification
  )

precision_summary


# 5. Export results --------------------------------------------------------

readr::write_csv(
  precision_table,
  here::here("mycode", "output", "extension_precision.csv")
)

readr::write_csv(
  precision_summary,
  here::here("mycode", "output", "extension_precision_summary.csv")
)
