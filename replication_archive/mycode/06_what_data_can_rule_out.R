# 06_what_data_can_rule_out.R
# Precision of partisan-strength differences in emotional persuasion

# Evaluate confidence intervals for the eight emotion-by-strength
# interactions and their ad-side-adjusted robustness estimates.

# Run 03_extension.R before executing this script.


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
