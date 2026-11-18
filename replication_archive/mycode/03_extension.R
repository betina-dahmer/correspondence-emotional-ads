# 03_extension.R
# Partisan strength and emotional persuasion

# Reproduces the published Figure 2 specification, then estimates emotion-persuasion associations by partisan strength.

# Run 00_reproduce.R before executing this script.

# Ad exposure is randomized; emotional responses are not.

# 1. Load data and dependencies ------------------------------------------

source(here::here("code", "helpers.R"))

exps_clean <- read_rds(here::here("data", "exps_clean.rds"))
ad_df      <- read_csv(here::here("data", "ad_df.csv"), show_col_types = FALSE)

# 2. Define partisan-strength groups -------------------------------------

exps_clean <- exps_clean |>
  mutate(pid_group = case_when(
    pid_7_pre == 1     ~ "Strong Democrat",
    pid_7_pre %in% 2:3 ~ "Weak/lean Democrat",
    pid_7_pre == 4     ~ "Independent",
    pid_7_pre %in% 5:6 ~ "Weak/lean Republican",
    pid_7_pre == 7     ~ "Strong Republican"
  ))

count(exps_clean, pid_3_pre, pid_group)

# 3. Estimate ad treatment effects by group -------------------------------
# For each outcome, wave, and group, compare ad exposure with placebo.
# Attack-ad estimates are signed toward the intended persuasive direction.

estimate_cates <- function(data, group_var) {
  
  long <- data |>
    pivot_longer(c(favorDT_rev, favorHC_rev, favorBS_rev, favorTC_rev,
                   favorJK_rev, general_vote_HC, general_vote_DT),
                 names_to = "outcome") |>
    filter(
      (outcome == "favorDT_rev" & ad_valence %in% c("Pro Trump", "Anti Trump", "Control")) |
        (outcome == "general_vote_DT" & ad_valence %in% c("Pro Trump", "Anti Trump", "Control") &
           election_phase == "General Election") |
        (outcome == "favorHC_rev" & ad_valence %in% c("Pro Clinton", "Anti Clinton", "Control")) |
        (outcome == "general_vote_HC" & ad_valence %in% c("Pro Clinton", "Anti Clinton", "Control") &
           election_phase == "General Election") |
        (outcome == "favorBS_rev" & ad_valence %in% c("Pro Sanders", "Control")) |
        (outcome == "favorTC_rev" & ad_valence %in% c("Pro Cruz", "Control")) |
        (outcome == "favorJK_rev" & ad_valence %in% c("Pro Kasich", "Control"))
    ) |>
    filter(!is.na(value))
  
  fits <- long |>
    split(list(long$outcome, long$date, long[[group_var]])) |>
    discard(~ nrow(.x) == 0) |>
    discard(~ length(unique(.x$ad_id_fac)) == 1) |>
    map(~ lm_robust(value ~ ad_id_fac + pid_7_pre, weights = weights, data = .x))
  
  all_cates <- suppressMessages(tibble(
    name        = names(fits),
    tidy_output = map(fits, tidy),
    vcov_output = map(fits, vcov)
  )) |>
    separate(name, into = c("outcome", "date", "group"), sep = "\\.") |>
    mutate(fit_id = row_number())
  
  estimates <- all_cates |>
    rename(outcome_name = outcome) |>
    unnest(tidy_output) |>
    filter(!term %in% c("(Intercept)", "pid_7_pre")) |>
    mutate(ad_id = as.numeric(str_remove(term, "ad_id_fac")),
           date  = as.Date(date)) |>
    select(-outcome) |>
    rename(outcome = outcome_name) |>
    left_join(ad_df, by = join_by(ad_id))
  
  V <- all_cates |>
    pull(vcov_output) |>
    map(\(x) x[!rownames(x) %in% c("(Intercept)", "pid_7_pre"),
               !colnames(x) %in% c("(Intercept)", "pid_7_pre"), drop = FALSE]) |>
    bdiag()
  
  anti <- str_detect(estimates$ad_valence, "Anti")
  estimates <- estimates |>
    mutate(estimate = if_else(anti, -estimate, estimate))
  V[anti, ] <- -V[anti, ]
  V[, anti] <- -V[, anti]
  
  stopifnot(nrow(estimates) == nrow(V))
  
  v_diag   <- Matrix::diag(V)
  v_row_ok <- rowSums(!is.finite(as.matrix(V))) == 0
  estimates <- estimates |>
    mutate(var_ok = is.finite(std.error) & v_row_ok & v_diag > 0) |>
    mutate(usable = all(var_ok), .by = fit_id)
  
  list(estimates = estimates, V = V)
}

# 4. Summarize emotional responses ---------------------------------------
# Compute mean post-exposure emotion indices for each ad, wave, and group.

emotion_by_group <- function(data, group_var) {
  data |>
    filter(ad_id != 3) |>
    group_by(ad_id, date, group = .data[[group_var]]) |>
    summarise(positive_index = mean(positive_index, na.rm = TRUE),
              negative_index = mean(negative_index, na.rm = TRUE),
              .groups = "drop")
}

# 5. Assemble meta-analysis inputs ---------------------------------------
# Match ad effects to group-level emotion indices and their covariance matrix.

build_meta_data <- function(data, group_var) {
  cates <- estimate_cates(data, group_var)
  d <- cates$estimates |>
    left_join(emotion_by_group(data, group_var), by = c("ad_id", "date", "group")) |>
    mutate(dv = if_else(str_detect(outcome, "favor"), "Favorability", "Vote Choice"),
           id = row_number())
  stopifnot(nrow(d) == nrow(cates$V))
  list(d = d, V = cates$V)
}

# 6. Fit random-effects meta-regressions ---------------------------------
# Use the estimated sampling covariance and wave-clustered CR2 inference.

fit_meta <- function(d, V, keep, index, mods = ~ emotion) {
  d$emotion <- d[[index]]
  keep <- keep & !is.na(d$emotion) & d$usable
  Vk <- as.matrix(V[keep, keep])
  Vk <- (Vk + t(Vk)) / 2
  rma.mv(yi = estimate, mods = mods, random = ~ 1 | id,
         V = Vk, data = d[keep, ]) |>
    robust(cluster = d$date[keep], clubSandwich = TRUE)
}

# Extract the coefficient and uncertainty for a specified model term.

get_coef <- function(fit, term) {
  j <- which(rownames(fit$beta) == term)
  
  tibble(
    estimate = fit$beta[j],
    std.error = fit$se[j],
    df = fit$dfs[j],
    p.value = fit$pval[j],
    n_cates = fit$k
  )
}

# 7. Validate against the published Figure 2 ----------------------------
# Recover the authors' 3 group specification before extending it.

check <- build_meta_data(exps_clean, "pid_3_pre")
count(check$d, dv)

figure_2_check <- expand_grid(index = c("negative_index", "positive_index"),
                              dv = c("Favorability", "Vote Choice")) |>
  mutate(result = map2(index, dv, \(i, v)
                       get_coef(fit_meta(check$d, check$V, check$d$dv == v, i), "emotion"))) |>
  unnest(result) |>
  mutate(across(c(estimate, std.error), \(x) round(x, 3)))

figure_2_check

# 8. Estimate emotion-persuasion slopes by partisan group ----------------

ext <- build_meta_data(exps_clean, "pid_group")
count(ext$d, group, dv)

ext$d |>
  filter(!usable) |>
  select(group, outcome, date, ad_title, estimate, std.error)

slopes_by_group <- expand_grid(
  group = c("Strong Democrat", "Weak/lean Democrat", "Independent",
            "Weak/lean Republican", "Strong Republican"),
  index = c("negative_index", "positive_index"),
  dv    = c("Favorability", "Vote Choice")
) |>
  mutate(result = pmap(list(group, index, dv), \(g, i, v)
                       get_coef(fit_meta(ext$d, ext$V, ext$d$group == g & ext$d$dv == v, i), "emotion"))) |>
  unnest(result)

slopes_by_group

# 9. Test differences between strong and weak/lean partisans -------------
# The emotion-by-strength interaction is the difference in slopes within party.

ext$d <- ext$d |> mutate(strong = as.numeric(str_detect(group, "Strong")))

difference_test <- expand_grid(
  party = c("Democrat", "Republican"),
  index = c("negative_index", "positive_index"),
  dv    = c("Favorability", "Vote Choice")
) |>
  mutate(result = pmap(list(party, index, dv), \(p, i, v)
                       get_coef(fit_meta(ext$d, ext$V,
                                         str_detect(ext$d$group, p) & ext$d$dv == v,
                                         i, mods = ~ emotion * strong),
                                "emotion:strong"))) |>
  unnest(result)

difference_test

# 10. Robustness: adjust for the partisan side of the ad ------------------
# Include ad side and its interaction with partisan strength.
# This adjustment does not identify a causal effect of emotion.

ext$d <- ext$d |>
  mutate(valence = if_else(ad_valence %in% c("Pro Trump", "Anti Clinton", "Pro Kasich", "Pro Cruz"),
                           "Pro Republican", "Pro Democrat"))

robustness_test <- expand_grid(
  party = c("Democrat", "Republican"),
  index = c("negative_index", "positive_index"),
  dv    = c("Favorability", "Vote Choice")
) |>
  mutate(result = pmap(list(party, index, dv), \(p, i, v)
                       get_coef(fit_meta(ext$d, ext$V,
                                         str_detect(ext$d$group, p) & ext$d$dv == v,
                                         i, mods = ~ emotion * strong + valence * strong),
                                "emotion:strong"))) |>
  unnest(result)

difference_test <- difference_test |> mutate(p.holm = p.adjust(p.value, method = "holm"))
robustness_test <- robustness_test |> mutate(p.holm = p.adjust(p.value, method = "holm"))

difference_test
robustness_test

# 11. Export analysis results --------------------------------------------

dir.create(here::here("mycode", "output"), showWarnings = FALSE)
write_csv(figure_2_check,  here::here("mycode", "output", "extension_figure_2_check.csv"))
write_csv(slopes_by_group, here::here("mycode", "output", "extension_slopes_by_group.csv"))
write_csv(difference_test, here::here("mycode", "output", "extension_difference_test.csv"))
write_csv(robustness_test, here::here("mycode", "output", "extension_robustness_test.csv"))

# 12. Leave-one-ad-out sensitivity analysis -------------------------------
# Re-estimate the Republican positive-emotion/favorability interaction,
# excluding one ad (across all its waves) at a time.
# Compare the baseline specification with the ad-side-adjusted model.

rep_fav <- str_detect(ext$d$group, "Republican") & ext$d$dv == "Favorability"

ad_titles <- ext$d |> filter(rep_fav) |> distinct(ad_id, ad_title)

leave_one_out <- ad_titles |>
  mutate(
    basic = map(ad_id, \(a)
                get_coef(fit_meta(ext$d, ext$V, rep_fav & ext$d$ad_id != a,
                                  "positive_index", mods = ~ emotion * strong),
                         "emotion:strong")),
    ad_side_adjusted = map(ad_id, \(a)
                    get_coef(fit_meta(ext$d, ext$V, rep_fav & ext$d$ad_id != a,
                                      "positive_index", mods = ~ emotion * strong + valence * strong),
                             "emotion:strong"))
  ) |>
  unnest(c(basic, ad_side_adjusted), names_sep = "_")

# Summarize the range of leave-one-ad-out estimates and nominal p-values.
leave_one_out |>
  summarise(n_ads           = n(),
            basic_min       = min(basic_estimate),
            basic_max       = max(basic_estimate),
            basic_n_p05     = sum(basic_p.value < 0.05),
            ad_side_adjusted_min   = min(ad_side_adjusted_estimate),
            ad_side_adjusted_max   = max(ad_side_adjusted_estimate),
            ad_side_adjusted_n_p05 = sum(ad_side_adjusted_p.value < 0.05))

# Inspect ads associated with the smallest and largest baseline estimates.
leave_one_out |>
  arrange(basic_estimate) |>
  select(ad_title, basic_estimate, basic_p.value, ad_side_adjusted_estimate, ad_side_adjusted_p.value) |>
  slice(c(1:3, (n() - 2):n()))

# Export
write_csv(
  leave_one_out,
  here::here("mycode", "output", "extension_leave_one_out.csv")
)