# 08_sanders_sensitivity.R
# Sensitivity to the Sanders favorability normalization error

# Correct the duplicated normalization of favorBS_rev and compare the corrected estimates with the original extension results.

# Run 03_extension.R before executing this script.


# 1. Load dependencies and original results -------------------------------

source(here::here("code", "helpers.R"))

output_dir <- here::here("mycode", "output")

original_figure2 <- readr::read_csv(
  file.path(output_dir, "extension_figure_2_check.csv"),
  show_col_types = FALSE
)

original_interactions <- readr::read_csv(
  file.path(output_dir, "extension_difference_test.csv"),
  show_col_types = FALSE
)

original_robustness <- readr::read_csv(
  file.path(output_dir, "extension_robustness_test.csv"),
  show_col_types = FALSE
)


# 2. Reuse estimation functions from script 03 -----------------------------

# Load only function definitions, without rerunning script 03 or overwriting its existing results.

extension_code <- parse(
  file = here::here("mycode", "03_extension.R")
)

function_names <- c(
  "estimate_cates",
  "emotion_by_group",
  "build_meta_data",
  "fit_meta",
  "get_coef"
)

for (expr in extension_code) {
  if (
    is.call(expr) &&
    identical(expr[[1]], as.name("<-")) &&
    is.symbol(expr[[2]]) &&
    as.character(expr[[2]]) %in% function_names &&
    is.call(expr[[3]]) &&
    identical(expr[[3]][[1]], as.name("function"))
  ) {
    eval(expr)
  }
}

stopifnot(
  all(vapply(
    function_names,
    \(name) exists(name, mode = "function"),
    logical(1)
  ))
)

ad_df <- readr::read_csv(
  here::here("data", "ad_df.csv"),
  show_col_types = FALSE
)


# 3. Correct Sanders favorability -----------------------------------------

exps_corrected <- readr::read_rds(
  here::here("data", "exps_clean.rds")
) |>
  dplyr::mutate(
    favorBS_rev = 4 * favorBS_rev + 1,
    pid_group = dplyr::case_when(
      pid_7_pre == 1     ~ "Strong Democrat",
      pid_7_pre %in% 2:3 ~ "Weak/lean Democrat",
      pid_7_pre == 4     ~ "Independent",
      pid_7_pre %in% 5:6 ~ "Weak/lean Republican",
      pid_7_pre == 7     ~ "Strong Republican"
    )
  )

stopifnot(
  all(
    exps_corrected$favorBS_rev[
      !is.na(exps_corrected$favorBS_rev)
    ] %in% c(0, 0.25, 0.5, 0.75, 1)
  )
)


# 4. Re-estimate the original Figure 2 specification -------------------------

check <- build_meta_data(exps_corrected, "pid_3_pre")

corrected_figure2 <- tidyr::expand_grid(
  index = c("negative_index", "positive_index"),
  dv = c("Favorability", "Vote Choice")
) |>
  dplyr::mutate(
    result = purrr::map2(
      index, dv,
      \(i, v) get_coef(
        fit_meta(
          check$d,
          check$V,
          check$d$dv == v,
          i
        ),
        "emotion"
      )
    )
  ) |>
  tidyr::unnest(result)


# 5. Re-estimate partisan-strength interactions -----------------------

ext <- build_meta_data(exps_corrected, "pid_group")

ext$d <- ext$d |>
  dplyr::mutate(
    strong = as.numeric(
      stringr::str_detect(group, "Strong")
    ),
    valence = dplyr::if_else(
      ad_valence %in% c(
        "Pro Trump", "Anti Clinton",
        "Pro Kasich", "Pro Cruz"
      ),
      "Pro Republican",
      "Pro Democrat"
    )
  )

corrected_interactions <- tidyr::expand_grid(
  party = c("Democrat", "Republican"),
  index = c("negative_index", "positive_index"),
  dv = c("Favorability", "Vote Choice")
) |>
  dplyr::mutate(
    result = purrr::pmap(
      list(party, index, dv),
      \(p, i, v) get_coef(
        fit_meta(
          ext$d,
          ext$V,
          stringr::str_detect(ext$d$group, p) &
            ext$d$dv == v,
          i,
          mods = ~ emotion * strong
        ),
        "emotion:strong"
      )
    )
  ) |>
  tidyr::unnest(result) |>
  dplyr::mutate(
    p.holm = p.adjust(p.value, method = "holm")
  )


# 6. Re-estimate ad-side-adjusted interactions ----------------------------

corrected_robustness <- tidyr::expand_grid(
  party = c("Democrat", "Republican"),
  index = c("negative_index", "positive_index"),
  dv = c("Favorability", "Vote Choice")
) |>
  dplyr::mutate(
    result = purrr::pmap(
      list(party, index, dv),
      \(p, i, v) get_coef(
        fit_meta(
          ext$d,
          ext$V,
          stringr::str_detect(ext$d$group, p) &
            ext$d$dv == v,
          i,
          mods = ~ emotion * strong + valence * strong
        ),
        "emotion:strong"
      )
    )
  ) |>
  tidyr::unnest(result) |>
  dplyr::mutate(
    p.holm = p.adjust(p.value, method = "holm")
  )


# 7. Compare original and corrected results -------------------------------

compare_results <- function(original, corrected, keys) {
  original |>
    dplyr::inner_join(
      corrected,
      by = keys,
      suffix = c("_original", "_corrected")
    ) |>
    dplyr::mutate(
      estimate_change = estimate_corrected - estimate_original,
      se_change = std.error_corrected - std.error_original
    )
}

figure2_comparison <- compare_results(
  original_figure2,
  corrected_figure2,
  c("index", "dv")
)

interaction_comparison <- compare_results(
  original_interactions,
  corrected_interactions,
  c("party", "index", "dv")
)

robustness_comparison <- compare_results(
  original_robustness,
  corrected_robustness,
  c("party", "index", "dv")
)

stopifnot(
  nrow(figure2_comparison) == 4,
  nrow(interaction_comparison) == 8,
  nrow(robustness_comparison) == 8
)


# 8. Summarize changes in statistical evidence ----------------------------

sensitivity_summary <- dplyr::bind_rows(
  original_interactions |>
    dplyr::mutate(
      specification = "Baseline",
      version = "Original"
    ),
  corrected_interactions |>
    dplyr::mutate(
      specification = "Baseline",
      version = "Corrected"
    ),
  original_robustness |>
    dplyr::mutate(
      specification = "Ad-side adjusted",
      version = "Original"
    ),
  corrected_robustness |>
    dplyr::mutate(
      specification = "Ad-side adjusted",
      version = "Corrected"
    )
) |>
  dplyr::summarise(
    n_tests = dplyr::n(),
    n_nominal_p05 = sum(p.value < 0.05),
    n_holm_p05 = sum(p.holm < 0.05),
    .by = c(specification, version)
  )

print(figure2_comparison, n = Inf, width = Inf)
print(interaction_comparison, n = Inf, width = Inf)
print(robustness_comparison, n = Inf, width = Inf)
print(sensitivity_summary, n = Inf, width = Inf)


# 9. Export results --------------------------------------------------------

readr::write_csv(
  figure2_comparison,
  file.path(output_dir, "sanders_figure2_sensitivity.csv")
)

readr::write_csv(
  interaction_comparison,
  file.path(output_dir, "sanders_interaction_sensitivity.csv")
)

readr::write_csv(
  robustness_comparison,
  file.path(output_dir, "sanders_robustness_sensitivity.csv")
)

readr::write_csv(
  sensitivity_summary,
  file.path(output_dir, "sanders_sensitivity_summary.csv")
)