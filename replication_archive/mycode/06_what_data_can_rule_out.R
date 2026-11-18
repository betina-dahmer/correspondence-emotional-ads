# =============================================================================
# 06_what_data_can_rule_out.R
# Is "no relationship" really "no relationship"? What the data can rule out
#
# Paper: Murphy, Coppock, Hill & Vavreck (2026), "Do Emotional Ads Persuade?"
# Political Communication. https://doi.org/10.1080/10584609.2026.2726266
# =============================================================================
#
# The idea:
# The article concludes that emotion does not predict persuasion, because the
# slopes in Figure 2 are not significant. But "not significant" only means
# "no relationship" if the estimate is precise. A null result from imprecise
# data is also compatible with a real relationship that the data are too
# noisy to see.
#
# So I ask two questions, for the original pooled analysis (Figure 2) and for
# my party groups (03_extension.R):
#   1. What range of slopes is compatible with the data? (95% CI)
#   2. How big would the true slope need to be for this analysis to detect it
#      80% of the time? (minimum detectable effect, MDE = 2.8 x SE)
#
# Slopes are hard to read on their own, so I translate them into something
# concrete: how much bigger is the effect of a high-emotion ad than a
# low-emotion ad (90th vs. 10th percentile of emotion across ads), and how
# does that compare with the average effect of an ad?
#
# Run 00_reproduce.R and 03_extension.R first.
# =============================================================================

library(tidyverse)

out <- function(file) here::here("mycode", "output", file)


# 1. The slopes and their SEs ------------------------------------------------

# Original Figure 2 (authors' pipeline, unrounded)
pooled <- read_csv(here::here("output", "tables", "figure_2_meta_estimates.csv"),
                   show_col_types = FALSE) |>
  filter(dv %in% c("Favorability", "Vote Choice")) |>
  transmute(group = "All (original Figure 2)", index, dv, estimate, std.error)

# My party groups (03_extension.R)
by_group <- read_csv(out("extension_slopes_by_group.csv"), show_col_types = FALSE) |>
  select(group, index, dv, estimate, std.error)

slopes <- bind_rows(pooled, by_group)


# 2. A realistic change in emotion -------------------------------------------
# The emotion index runs from 0 to 1, but real ads don't cover that whole
# range. So instead of "from 0 to 1", I use the gap between a low-emotion ad
# (10th percentile) and a high-emotion ad (90th percentile), measured the same
# way the meta-regression uses emotion: average per ad, wave and group.

spread <- function(d) {
  d |>
    summarise(p10 = quantile(emotion, 0.10, na.rm = TRUE),
              p90 = quantile(emotion, 0.90, na.rm = TRUE),
              .by = c(group, index)) |>
    mutate(emotion_gap = p90 - p10)
}

# Pooled: the authors' emotion averages by ad, wave and party (3 groups)
spread_pooled <- read_rds(here::here("data", "estimates", "ratings_pid.rds")) |>
  filter(rating_outcome %in% c("positive_index", "negative_index")) |>
  transmute(group = "All (original Figure 2)", index = rating_outcome,
            emotion = rating_estimate) |>
  spread()

# My 5 groups: same averages, with the finer party groups
spread_groups <- read_rds(here::here("data", "exps_clean.rds")) |>
  filter(ad_id != 3) |>
  mutate(group = case_when(
    pid_7_pre == 1     ~ "Strong Democrat",
    pid_7_pre %in% 2:3 ~ "Weak/lean Democrat",
    pid_7_pre == 4     ~ "Independent",
    pid_7_pre %in% 5:6 ~ "Weak/lean Republican",
    pid_7_pre == 7     ~ "Strong Republican"
  )) |>
  summarise(positive_index = mean(positive_index, na.rm = TRUE),
            negative_index = mean(negative_index, na.rm = TRUE),
            .by = c(ad_id, date, group)) |>
  pivot_longer(c(positive_index, negative_index),
               names_to = "index", values_to = "emotion") |>
  spread()

emotion_spread <- bind_rows(spread_pooled, spread_groups)
emotion_spread


# 3. The benchmark: the average effect of an ad ------------------------------
# From the authors' CATEs (already signed so that positive = the ad worked).

avg_effect <- read_rds(here::here("data", "estimates", "all_cates.rds")) |>
  mutate(dv = case_when(str_detect(outcome, "favor")        ~ "Favorability",
                        str_detect(outcome, "general_vote") ~ "Vote Choice")) |>
  filter(!is.na(dv)) |>
  summarise(avg_ad_effect = mean(estimate), .by = dv)

avg_effect


# 4. What the data can and can't rule out ------------------------------------

rule_out <- slopes |>
  left_join(emotion_spread, by = c("group", "index")) |>
  left_join(avg_effect, by = "dv") |>
  mutate(
    # Range of slopes compatible with the data
    ci_low  = estimate - 1.96 * std.error,
    ci_high = estimate + 1.96 * std.error,
    # Smallest slope this analysis would detect 80% of the time (5% test)
    mde_slope = 2.80 * std.error,
    # The same, in concrete terms: how much bigger the effect of a
    # high-emotion ad would be than that of a low-emotion ad
    mde_effect_gap = mde_slope * emotion_gap,
    # ...as a share of the average effect of an ad
    mde_vs_avg_effect = mde_effect_gap / avg_ad_effect,
    # Largest gap (either direction) the 95% CI can't rule out
    max_gap_not_ruled_out = pmax(abs(ci_low), abs(ci_high)) * emotion_gap
  )

rule_out |>
  select(group, index, dv, estimate, std.error, mde_slope,
         emotion_gap, mde_effect_gap, avg_ad_effect, mde_vs_avg_effect) |>
  print(n = Inf, width = Inf)

# How to read mde_vs_avg_effect:
#   about 1 -> the analysis could only detect a relationship in which a
#              high-emotion ad is better than a low-emotion ad by as much as
#              the whole effect of an average ad. Anything smaller would go
#              unnoticed most of the time.
#   well below 1 -> the analysis is precise enough to see a fairly small
#              relationship, so a null result is real evidence of no
#              relationship.

# Result: [write here what I find, for the original analysis and for strong
# Republicans]


# Save ------------------------------------------------------------------------

write_csv(rule_out, out("what_data_can_rule_out.csv"))

# Result:
# Benchmark: an average ad moves favorability by 1.2 points (0-100 scale) and
# vote choice by 0.4 points.
#
# Original analysis (Figure 2): to detect a relationship 80% of the time, a
# high-emotion ad (90th percentile) would need an effect larger than a
# low-emotion ad (10th percentile) by:
#   favorability: 1.6 points (negative index), 1.4 points (positive index),
#                 i.e. 1.3x and 1.1x the effect of an average ad
#   vote choice:  3.9 points (negative), 2.2 points (positive),
#                 i.e. 9.5x and 5.2x the effect of an average ad
# So emotion would have to more than double an ad's effect for the original
# analysis to reliably see it. The data rule out a LARGE role for emotion,
# but not a modest one. "Emotion does not predict persuasion" is stronger
# than what the data can show.
# (The vote ratios are inflated because the average vote effect is almost
# zero, so report them in points too.)
#
# My groups: ratios between 2x and 45x. Splitting the groups costs a lot of
# precision. Strong Republicans, positive emotion, favorability: the analysis
# could only detect a relationship ~3x the average ad effect. That's why the
# pattern in 03_extension.R stays suggestive.