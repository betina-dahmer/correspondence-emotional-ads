# =============================================================================
# 03_extension.R
# My extension: do emotional ads work differently for strong partisans?
#
# Paper: Murphy, Coppock, Hill & Vavreck (2026), "Do Emotional Ads Persuade?"
# Political Communication. https://doi.org/10.1080/10584609.2026.2726266
# =============================================================================
#
# The question:
# The authors split everyone into 3 party groups (Democrat, Independent,
# Republican). Each partisan group mixes strong partisans, weak partisans and
# leaners (see 02_explore.R). Then, for Figure 2, they pool all groups into one
# line. I ask whether the relationship between emotion and persuasion is
# different for STRONG partisans.
#
# How (same method as the authors, just with finer party groups):
#   Step 1. Estimate the effect of each ad (CATE), in each wave, separately for
#           5 groups: strong Dem (1), weak/lean Dem (2-3), independent (4),
#           weak/lean Rep (5-6), strong Rep (7).
#   Step 2. Measure how much emotion each ad caused, within each group.
#   Step 3. Meta-regression of CATEs on emotion, one line per group, plus a
#           direct test of whether the strong partisans' line differs from the
#           weak partisans' line.
#
# Before running the extension, I run my code with the authors' 3 groups and
# check that I get the published Figure 2 numbers. If my code reproduces their
# result, I can trust it when I change the groups.
#
# This script only does the math and saves the results as CSV files.
# The figure and table for the paper are made in 04_figures_tables.R.
#
# Run 00_reproduce.R first (this script uses helpers.R and the data folder).
# =============================================================================

source(here::here("code", "helpers.R"))   # loads tidyverse, estimatr, metafor, Matrix

exps_clean <- read_rds(here::here("data", "exps_clean.rds"))
ad_df      <- read_csv(here::here("data", "ad_df.csv"), show_col_types = FALSE)

# New, finer party groups -------------------------------------------------

exps_clean <- exps_clean |>
  mutate(pid_group = case_when(
    pid_7_pre == 1     ~ "Strong Democrat",
    pid_7_pre %in% 2:3 ~ "Weak/lean Democrat",
    pid_7_pre == 4     ~ "Independent",
    pid_7_pre %in% 5:6 ~ "Weak/lean Republican",
    pid_7_pre == 7     ~ "Strong Republican"
  ))

count(exps_clean, pid_3_pre, pid_group)   # check: every person in exactly one group
# Strong Dem 7,441 | Weak/lean Dem 5,845 | Independent 4,750 |
# Weak/lean Rep 5,822 | Strong Rep 4,821


# Step 1: CATEs by group --------------------------------------------------
# Same code as the authors' estimate_all_cates.R, with two changes:
#   - it splits by whatever group variable I give it (group_var);
#   - it keeps only favorability and vote choice, the outcomes in Figure 2.
# Within strong groups, the pid_7_pre control is constant and drops out
# (same "rank deficient" message as for independents in the original).

estimate_cates <- function(data, group_var) {
  
  long <- data |>
    pivot_longer(c(favorDT_rev, favorHC_rev, favorBS_rev, favorTC_rev,
                   favorJK_rev, general_vote_HC, general_vote_DT),
                 names_to = "outcome") |>
    # Each ad is judged only by the candidate it is about (same rules as the authors)
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
  
  # One regression per outcome x wave x group: each ad vs. the placebo
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
    mutate(fit_id = row_number())          # which regression each CATE came from
  
  estimates <- all_cates |>
    rename(outcome_name = outcome) |>
    unnest(tidy_output) |>
    filter(!term %in% c("(Intercept)", "pid_7_pre")) |>
    mutate(ad_id = as.numeric(str_remove(term, "ad_id_fac")),
           date  = as.Date(date)) |>
    select(-outcome) |>
    rename(outcome = outcome_name) |>
    left_join(ad_df, by = join_by(ad_id))
  
  # Uncertainty of each CATE (block-diagonal: ads in the same regression share
  # a placebo group, so their CATEs are correlated)
  V <- all_cates |>
    pull(vcov_output) |>
    map(\(x) x[!rownames(x) %in% c("(Intercept)", "pid_7_pre"),
               !colnames(x) %in% c("(Intercept)", "pid_7_pre"), drop = FALSE]) |>
    bdiag()
  
  # Flip attack ads so that positive = the ad worked as its sponsor wanted
  anti <- str_detect(estimates$ad_valence, "Anti")
  estimates <- estimates |>
    mutate(estimate = if_else(anti, -estimate, estimate))
  V[anti, ] <- -V[anti, ]
  V[, anti] <- -V[, anti]
  
  stopifnot(nrow(estimates) == nrow(V))   # rows of estimates and V must line up
  
  # Flag regressions whose uncertainty can't be computed. With very small cells
  # or almost no variation in the outcome, the robust (HC2) variance can come
  # out negative or NaN ("NaNs produced" warnings). Those CATEs can't enter the
  # meta-regression, so I drop every CATE from that regression and report them.
  v_diag   <- Matrix::diag(V)
  v_row_ok <- rowSums(!is.finite(as.matrix(V))) == 0   # no NaN/Inf anywhere in the row
  estimates <- estimates |>
    mutate(var_ok = is.finite(std.error) & v_row_ok & v_diag > 0) |>
    mutate(usable = all(var_ok), .by = fit_id)
  
  list(estimates = estimates, V = V)
}


# Step 2: emotion by group -------------------------------------------------
# Same as the authors' estimate_ratings_pid.R: the average emotion index among
# the people in each group who saw each ad in each wave. (Their lm_robust(y ~ 1)
# is just the mean, so I use mean() directly.)

emotion_by_group <- function(data, group_var) {
  data |>
    filter(ad_id != 3) |>                       # no emotion ratings for the placebo
    group_by(ad_id, date, group = .data[[group_var]]) |>
    summarise(positive_index = mean(positive_index, na.rm = TRUE),
              negative_index = mean(negative_index, na.rm = TRUE),
              .groups = "drop")
}


# Put steps 1 and 2 together -----------------------------------------------

build_meta_data <- function(data, group_var) {
  cates <- estimate_cates(data, group_var)
  d <- cates$estimates |>
    left_join(emotion_by_group(data, group_var), by = c("ad_id", "date", "group")) |>
    mutate(dv = if_else(str_detect(outcome, "favor"), "Favorability", "Vote Choice"),
           id = row_number())
  stopifnot(nrow(d) == nrow(cates$V))       # the join must not add or drop rows
  list(d = d, V = cates$V)
}


# Step 3: meta-regression ----------------------------------------------------
# Same model as the authors: random effects, block-diagonal V, standard errors
# clustered by wave (CR2, clubSandwich).

fit_meta <- function(d, V, keep, index, mods = ~ emotion) {
  d$emotion <- d[[index]]
  keep <- keep & !is.na(d$emotion) & d$usable
  Vk <- as.matrix(V[keep, keep])
  Vk <- (Vk + t(Vk)) / 2   # remove tiny rounding asymmetries (rma.mv requires exact symmetry)
  rma.mv(yi = estimate, mods = mods, random = ~ 1 | id,
         V = Vk, data = d[keep, ]) |>
    robust(cluster = d$date[keep], clubSandwich = TRUE)
}

# Pull one coefficient out of a fitted model
get_coef <- function(fit, term) {
  j <- which(rownames(fit$beta) == term)
  tibble(estimate = fit$beta[j], std.error = fit$se[j],
         p.value = fit$pval[j], n_cates = fit$k)
}


# =============================================================================
# Check: does my code reproduce Figure 2 with the authors' 3 groups?
# =============================================================================
# Expected (published Figure 2):
#   negative, Favorability:  0.014 (0.017)    negative, Vote Choice: -0.005 (0.041)
#   positive, Favorability:  0.008 (0.014)    positive, Vote Choice:  0.022 (0.021)
#   and 174 favorability CATEs, 102 vote choice CATEs.

check <- build_meta_data(exps_clean, "pid_3_pre")
count(check$d, dv)    # should be 174 and 102

figure_2_check <- expand_grid(index = c("negative_index", "positive_index"),
                              dv = c("Favorability", "Vote Choice")) |>
  mutate(result = map2(index, dv, \(i, v)
                       get_coef(fit_meta(check$d, check$V, check$d$dv == v, i), "emotion"))) |>
  unnest(result) |>
  mutate(across(c(estimate, std.error), \(x) round(x, 3)))

figure_2_check

# Result: it matches. 174 favorability and 102 vote choice CATEs, and all four
# slopes and SEs are identical to the published Figure 2 (0.014 (0.017),
# -0.005 (0.041), 0.008 (0.014), 0.022 (0.021)). So my own code reproduces the
# authors' main result, and I can trust it when I change the groups.


# =============================================================================
# Extension: one line per group
# =============================================================================

ext <- build_meta_data(exps_clean, "pid_group")
count(ext$d, group, dv)    # 58 favorability and 34 vote choice CATEs per group

# CATEs dropped because their uncertainty couldn't be computed
ext$d |>
  filter(!usable) |>
  select(group, outcome, date, ad_title, estimate, std.error)

# Only 1 dropped: strong Democrats, Trump vote, "Role Model", 2016-07-18.
# Almost no strong Democrat votes for Trump, so there is no variation in the
# outcome and the SE can't be computed (a floor effect).

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

# Result: none of the 20 group-specific slopes is significant at 0.05. Even
# among strong partisans, more emotional ads are not clearly more persuasive.
# Closest: strong Republicans, positive emotion, favorability: 0.079 (0.039),
# p = 0.08. SEs for strong Republicans are ~3x the pooled SEs (0.039 vs 0.014).


# =============================================================================
# Extension: is the strong partisans' line different from the weak partisans'?
# =============================================================================
# Looking at two separate lines is not enough: one can be "significant" and the
# other not, without the two being different. So I fit one model with both
# groups and an interaction emotion x strong. The interaction is the
# difference between the two slopes, and its p-value is the actual test.

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

# Result: one of 8 tests is significant at 0.05. Among Republicans, the slope
# of favorability CATEs on the POSITIVE index is 0.127 higher for strong than
# for weak/lean Republicans (SE 0.049, p = 0.027).
# Strong Rep slope: 0.079 (0.039). Weak/lean Rep slope: -0.047 (0.042).


# =============================================================================
# Robustness: is it emotion, or is it the party of the ad?
# =============================================================================
# The ads that make strong Republicans hopeful are mostly pro-Republican ads.
# So the difference above could just mean "strong partisans respond more to
# their own party's ads", not "hope persuades". To check, I add the side of
# the ad (pro-Democrat or pro-Republican), and let its effect differ for strong
# and weak partisans. If emotion:strong stays similar, the result is about
# emotion. If it shrinks to zero, it was about the party of the ad.

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

# Multiple comparisons: 8 difference tests. Holm correction for both versions.
difference_test <- difference_test |> mutate(p.holm = p.adjust(p.value, method = "holm"))
robustness_test <- robustness_test |> mutate(p.holm = p.adjust(p.value, method = "holm"))

difference_test
robustness_test

# Result:
# - Controlling for the side of the ad (and letting its effect differ for
#   strong vs. weak partisans) does NOT make the Republican difference go away.
#   It gets bigger:
#     positive emotion, favorability: 0.127 -> 0.153 (p = 0.032)
#     positive emotion, vote choice:  0.172 -> 0.214 (p = 0.156)
#     negative emotion, favorability: -0.102 -> -0.175 (p = 0.071)
#     negative emotion, vote choice:  -0.231 -> -0.338 (p = 0.065)
#   So it's not just "strong partisans like their own party's ads".
# - All four Republican tests point the same way: compared with weak/lean
#   Republicans, strong Republicans respond MORE to ads that evoke positive
#   emotions and LESS to ads that evoke negative ones. No such pattern for
#   Democrats.
# - BUT after the Holm correction for 8 tests, nothing is significant (smallest
#   adjusted p = 0.215 without the control, 0.254 with it). So this is a
#   consistent, suggestive pattern, not a finding. The data are too imprecise
#   to confirm it.
# - Emotion is still not randomized, so other features of the ads could
#   explain the pattern.


# Save -------------------------------------------------------------------------
# The figure and table for the paper are made from these files in
# 04_figures_tables.R.

dir.create(here::here("mycode", "output"), showWarnings = FALSE)
write_csv(figure_2_check,  here::here("mycode", "output", "extension_figure_2_check.csv"))
write_csv(slopes_by_group, here::here("mycode", "output", "extension_slopes_by_group.csv"))
write_csv(difference_test, here::here("mycode", "output", "extension_difference_test.csv"))
write_csv(robustness_test, here::here("mycode", "output", "extension_robustness_test.csv"))