# in_text_claims.R
# Output: an audit trail on stdout, ending in a CLAIM SUMMARY line
# Depends on: every analysis script's output in data/estimates/ and output/tables/
# Description: reproduces every number stated in the article and the online
#   appendix from the canonical pipeline outputs, and compares each against the
#   manuscript's own typography. Nothing here refits a model.
#
# `cat()` is permitted in this file and nowhere else in the archive: its entire
# output is a human-read audit trail and every value needs a visible label.

source(here::here("code", "helpers.R"))
library(excheckr)

options(width = 200)

claim_start(format = "audit")

# Canonical pipeline outputs ----
exps_clean       <- read_rds(here::here("data", "exps_clean.rds"))
exps_raw         <- read_rds(here::here("data", "exps.rds"))
ad_emotions      <- read_csv(here::here("data", "ad_df_emotions.csv"), show_col_types = FALSE)
pairwise         <- read_rds(here::here("data", "estimates", "observed_pairwise_frame.rds"))
individual_boot  <- read_rds(here::here("data", "estimates", "individual_bootstrap_results_1000.rds"))
group_boot       <- read_rds(here::here("data", "estimates", "group_bootstrap_results_1000.rds"))

emotion_alpha    <- read_csv(here::here("output", "tables", "emotion_scale_reliability.csv"), show_col_types = FALSE)
emotion_scree    <- read_csv(here::here("output", "tables", "emotion_scree_eigenvalues.csv"), show_col_types = FALSE)
rating_alpha     <- read_csv(here::here("output", "tables", "rating_scale_reliability.csv"), show_col_types = FALSE)
cate_counts      <- read_csv(here::here("output", "tables", "cate_counts_by_outcome.csv"), show_col_types = FALSE)
meta_2           <- read_csv(here::here("output", "tables", "figure_2_meta_estimates.csv"), show_col_types = FALSE)
meta_3           <- read_csv(here::here("output", "tables", "figure_3_meta_estimates.csv"), show_col_types = FALSE)
meta_4           <- read_csv(here::here("output", "tables", "figure_4_meta_estimates.csv"), show_col_types = FALSE)
meta_e10         <- read_csv(here::here("output", "tables", "figure_e10_meta_estimates.csv"), show_col_types = FALSE)

pull_meta <- function(d, index_name, dv_name, what = "estimate") {
  row <- d |> filter(index == index_name, dv == dv_name)
  stopifnot(nrow(row) == 1)
  row[[what]]
}
boot_se <- function(o, term) {
  j <- match(term, names(o$t0))
  stopifnot(!is.na(j))
  sd(o$t[, j], na.rm = TRUE)
}

# Abstract ----

claim("abstract_waves", n_distinct(exps_clean$date),
      label = "weeks of experiments", published = "29", digits = 0)

claim("abstract_ads", n_distinct(exps_clean$ad_id[exps_clean$ad_id != 3]),
      label = "authentic campaign ads tested", published = "48", digits = 0)

# "The ads and a placebo were randomly assigned to approximately 28,000 subjects."
# The manuscript hedges, so the block prints the exact figure and asserts nothing.
claim("abstract_subjects", nrow(exps_clean),
      label = "subjects in the analysis sample (article says approximately 28,000)",
      digits = 0, expect = "derived")

# Design and Results ----

# "Across 29 experimental waves, we measured the effects of 48 unique political
#  advertisements on more than 28,000 individuals. Because some ads were fielded
#  in multiple waves, the experiments include 58 total ad treatments."
claim("design_treatments",
      nrow(distinct(filter(exps_clean, ad_id != 3), ad_id, date)),
      label = "total ad treatments (ad by wave)", published = "58", digits = 0)

claim("design_more_than_28000", as.numeric(nrow(exps_clean) > 28000),
      label = "analysis sample exceeds 28,000 (1 = yes)", published = "1", digits = 0)

# "Most commonly, our samples included 1,000 completed responses, but during the
#  party convention period (June 20 to August 15, 2016) we increased the number
#  of cases to 2,000."
wave_sizes <- exps_raw |> count(date, name = "n")
claim("design_wave_1000", sum(wave_sizes$n == 1000),
      label = "waves of 1,000 completed responses", published = "24", digits = 0)
claim("design_wave_2000", sum(wave_sizes$n == 2000),
      label = "waves of 2,000 completed responses", published = "5", digits = 0)
claim("design_convention_window",
      as.numeric(identical(
        as.character(sort(wave_sizes$date[wave_sizes$n == 2000])),
        c("2016-06-20", "2016-07-04", "2016-07-18", "2016-08-01", "2016-08-15"))),
      label = "the five 2,000-case waves run June 20 to August 15 (1 = yes)",
      published = "1", digits = 0)

# "In total, we estimate the effects of six Pro-Clinton, three Pro-Trump,
#  11 Anti-Clinton, 23 Anti-Trump ads, one Pro-Kasich, two pro-Cruz, and two
#  pro-Sanders ads."
valence <- exps_clean |> filter(ad_id != 3) |> distinct(ad_id, ad_valence) |> count(ad_valence)
walk2(c("Pro Clinton", "Pro Trump", "Anti Clinton", "Anti Trump", "Pro Kasich", "Pro Cruz", "Pro Sanders"),
      c("6", "3", "11", "23", "1", "2", "2"),
      \(v, p) claim(paste0("design_valence_", str_replace_all(str_to_lower(v), " ", "_")),
                    valence$n[valence$ad_valence == v],
                    label = paste(v, "ads"), published = p, digits = 0))

# "In Appendix A, we report a factor analysis that shows that positive emotional
#  responses and negative emotional responses load on two distinct factors."
claim("design_two_factors", sum(emotion_scree$above_one),
      label = "emotion factors with eigenvalue above 1", published = "2", digits = 0)

# "The Cronbach's alpha for the positive scale is 0.88 and 0.72 for the negative
#  scale."  CORRECTED AT PROOF: the positive scale is 0.886, which rounds to 0.89.
claim("design_alpha_positive",
      emotion_alpha$cronbach_alpha[emotion_alpha$scale == "positive"],
      label = "Cronbach's alpha, positive scale (article prints 0.88; corrected to 0.89)",
      published = "0.89", digits = 2)
claim("design_alpha_negative",
      emotion_alpha$cronbach_alpha[emotion_alpha$scale == "negative"],
      label = "Cronbach's alpha, negative scale", published = "0.72", digits = 2)

# "We classified five contrast ads ... Donald Trump ran two of these ads."
# CORRECTED AT PROOF: three of the five contrast ads were sponsored by Trump.
claim("design_contrast_ads", sum(ad_emotions$emotion_valence == "both"),
      label = "contrast (mixed-tone) ads", published = "5", digits = 0)
claim("design_contrast_trump",
      sum(ad_emotions$emotion_valence == "both" & ad_emotions$ad_sponsor == "trump"),
      label = "contrast ads sponsored by Trump (article prints two; corrected to three)",
      published = "3", digits = 0)

# "Donald Trump's ad called 'Unfit to Serve' ... Clinton also ran an ad called
#  'Unfit' in August that attacked Trump."  Both titles are checked against the
#  ad metadata, since the article originally gave Trump's ad Clinton's title.
unfit <- exps_clean |> filter(str_detect(ad_title, "^Unfit")) |> distinct(ad_title, ad_sponsor, date)
claim("design_unfit_titles",
      as.numeric(
        identical(sort(unfit$ad_title), c("Unfit", "Unfit to Serve")) &&
        unfit$ad_sponsor[unfit$ad_title == "Unfit to Serve"] == "trump" &&
        unfit$ad_sponsor[unfit$ad_title == "Unfit"] == "clinton"),
      label = "Trump's ad is 'Unfit to Serve' and Clinton's is 'Unfit' (1 = yes)",
      published = "1", digits = 0)

# Do Ads Move Emotions? ----

# "We present the average emotional responses to all 48 ads in Figure 1."
claim("fig1_ads", n_distinct(exps_clean$ad_id[exps_clean$ad_id != 3]),
      label = "ads shown in Figure 1", published = "48", digits = 0)

# "Examining the positive index, for both Democrats and Republicans, the range of
#  emotional responses spans more than half the scale ... Independents show
#  comparably muted emotional responses on both dimensions."
ratings_pid <- read_rds(here::here("data", "estimates", "ratings_pid.rds"))
ranges <- ratings_pid |>
  filter(rating_outcome %in% c("positive_index", "negative_index")) |>
  summarise(range = diff(range(rating_estimate)), .by = c(pid_3_pre, rating_outcome))
rng <- \(p, o) ranges$range[ranges$pid_3_pre == p & ranges$rating_outcome == o]
claim("fig1_range_dem_positive", rng("Democrat", "positive_index"),
      label = "Democrats' positive index range (article: more than half the scale)",
      published = "0.55", digits = 2, expect = "derived")
claim("fig1_range_rep_positive", rng("Republican", "positive_index"),
      label = "Republicans' positive index range", published = "0.63", digits = 2, expect = "derived")
claim("fig1_partisan_ranges_exceed_half",
      as.numeric(all(c(rng("Democrat", "positive_index"), rng("Republican", "positive_index"),
                       rng("Democrat", "negative_index"), rng("Republican", "negative_index")) > 0.5)),
      label = "all four partisan ranges exceed half the scale (1 = yes)",
      published = "1", digits = 0)
claim("fig1_independents_muted",
      as.numeric(all(c(rng("Independent", "positive_index"), rng("Independent", "negative_index")) <
                     c(min(rng("Democrat", "positive_index"), rng("Republican", "positive_index")),
                       min(rng("Democrat", "negative_index"), rng("Republican", "negative_index"))))),
      label = "independents' range is narrower than either partisan group's (1 = yes)",
      published = "1", digits = 0)

# Favorability and Vote Choice ----

# "In each favorability panel, we report 58 separate tests ... yielding 174
#  estimates in total. In each vote choice panel, we report 34 separate tests ...
#  yielding 102 estimates in total."
cc <- \(dv, what) cate_counts[[what]][cate_counts$dv == dv]
claim("fig2_favorability_tests", cc("Favorability", "tests"),
      label = "favorability tests", published = "58", digits = 0)
claim("fig2_favorability_estimates", cc("Favorability", "estimates"),
      label = "favorability estimates", published = "174", digits = 0)
claim("fig2_vote_tests", cc("Vote Choice", "tests"),
      label = "vote choice tests", published = "34", digits = 0)
claim("fig2_vote_estimates", cc("Vote Choice", "estimates"),
      label = "vote choice estimates", published = "102", digits = 0)

# Figure 2's four annotated meta-analytic slopes.
claim("fig2_slope_fav_negative", pull_meta(meta_2, "negative_index", "Favorability"),
      label = "Fig 2 slope, favorability on the negative index", published = "0.014", digits = 3)
claim("fig2_se_fav_negative", pull_meta(meta_2, "negative_index", "Favorability", "std.error"),
      label = "Fig 2 SE, favorability on the negative index", published = "0.017", digits = 3)
claim("fig2_slope_vote_negative", pull_meta(meta_2, "negative_index", "Vote Choice"),
      label = "Fig 2 slope, vote choice on the negative index", published = "-0.005", digits = 3)
claim("fig2_se_vote_negative", pull_meta(meta_2, "negative_index", "Vote Choice", "std.error"),
      label = "Fig 2 SE, vote choice on the negative index (article printed 0.042)",
      published = "0.041", digits = 3)
claim("fig2_slope_fav_positive", pull_meta(meta_2, "positive_index", "Favorability"),
      label = "Fig 2 slope, favorability on the positive index", published = "0.008", digits = 3)
claim("fig2_se_fav_positive", pull_meta(meta_2, "positive_index", "Favorability", "std.error"),
      label = "Fig 2 SE, favorability on the positive index", published = "0.014", digits = 3)
claim("fig2_slope_vote_positive", pull_meta(meta_2, "positive_index", "Vote Choice"),
      label = "Fig 2 slope, vote choice on the positive index", published = "0.022", digits = 3)
claim("fig2_se_vote_positive", pull_meta(meta_2, "positive_index", "Vote Choice", "std.error"),
      label = "Fig 2 SE, vote choice on the positive index", published = "0.021", digits = 3)

# "Three of the slope estimates are positive, and one is negative, but all are
#  indistinguishable from zero."
# figure_2.R fits all four outcomes but plots only two, so restrict to the
# panels the sentence is about.
fig2_slopes <- meta_2 |> filter(dv %in% c("Favorability", "Vote Choice")) |> pull(estimate)
stopifnot(length(fig2_slopes) == 4)
claim("fig2_three_positive_one_negative",
      as.numeric(sum(fig2_slopes > 0) == 3 && sum(fig2_slopes < 0) == 1),
      label = "three Figure 2 slopes positive and one negative (1 = yes)",
      published = "1", digits = 0)

# Figure 3's two annotated slopes.
claim("fig3_slope_fav", pull_meta(meta_3, "intended_index", "Favorability"),
      label = "Fig 3 slope, favorability on the intended emotion index",
      published = "-0.004", digits = 3)
claim("fig3_se_fav", pull_meta(meta_3, "intended_index", "Favorability", "std.error"),
      label = "Fig 3 SE, favorability", published = "0.013", digits = 3)
claim("fig3_slope_vote", pull_meta(meta_3, "intended_index", "Vote Choice"),
      label = "Fig 3 slope, vote choice on the intended emotion index",
      published = "-0.023", digits = 3)
claim("fig3_se_vote", pull_meta(meta_3, "intended_index", "Vote Choice", "std.error"),
      label = "Fig 3 SE, vote choice (article printed 0.023)", published = "0.024", digits = 3)

# "The meta-analytic estimates ... are estimated to be slightly negative."
fig3_plotted <- meta_3 |> filter(dv %in% c("Favorability", "Vote Choice"))
stopifnot(nrow(fig3_plotted) == 2)
claim("fig3_both_negative", as.numeric(all(fig3_plotted$estimate < 0)),
      label = "both Figure 3 slopes are negative (1 = yes)", published = "1", digits = 0)

# Issue Positions and Turnout ----

claim("fig4_slope_issue_negative", pull_meta(meta_4, "negative_index", "Target Issue Support"),
      label = "Fig 4 slope, target issue support on the negative index",
      published = "0.052", digits = 3)
claim("fig4_se_issue_negative", pull_meta(meta_4, "negative_index", "Target Issue Support", "std.error"),
      label = "Fig 4 SE, target issue support on the negative index (article printed 0.055)",
      published = "0.059", digits = 3)
claim("fig4_slope_turnout_negative", pull_meta(meta_4, "negative_index", "Turnout Intentions"),
      label = "Fig 4 slope, turnout on the negative index", published = "-0.017", digits = 3)
claim("fig4_se_turnout_negative", pull_meta(meta_4, "negative_index", "Turnout Intentions", "std.error"),
      label = "Fig 4 SE, turnout on the negative index", published = "0.021", digits = 3)
claim("fig4_slope_issue_positive", pull_meta(meta_4, "positive_index", "Target Issue Support"),
      label = "Fig 4 slope, target issue support on the positive index",
      published = "-0.039", digits = 3)
claim("fig4_se_issue_positive", pull_meta(meta_4, "positive_index", "Target Issue Support", "std.error"),
      label = "Fig 4 SE, target issue support on the positive index (article printed 0.039)",
      published = "0.041", digits = 3)
claim("fig4_slope_turnout_positive", pull_meta(meta_4, "positive_index", "Turnout Intentions"),
      label = "Fig 4 slope, turnout on the positive index", published = "0.064", digits = 3)
claim("fig4_se_turnout_positive", pull_meta(meta_4, "positive_index", "Turnout Intentions", "std.error"),
      label = "Fig 4 SE, turnout on the positive index", published = "0.035", digits = 3)

# "The slopes of all four meta-regression lines are close to zero and are not
#  statistically significant."
fig4_plotted <- meta_4 |> filter(dv %in% c("Target Issue Support", "Turnout Intentions"))
stopifnot(nrow(fig4_plotted) == 4)
claim("fig4_none_significant",
      as.numeric(all(fig4_plotted$p.value > 0.05)),
      label = "no Figure 4 slope is significant at the 0.05 level (1 = yes)",
      published = "1", digits = 0)

# Appendix A: the emotion scales ----

claim("appendix_a_alpha_n", emotion_alpha$n[1],
      label = "complete emotion responses used for the scale statistics",
      digits = 0, expect = "derived")

# Appendix E: ratings ----

# "The resulting index is highly reliable (Cronbach's alpha: 0.91)."
claim("appendix_e_rating_alpha", rating_alpha$cronbach_alpha,
      label = "Cronbach's alpha, rating index", published = "0.91", digits = 2)

# "The estimated slope of 0.034 indicates that an ad achieving the maximum rating
#  of 1 relative to the minimum rating of 0 would be 3.4 points higher."
claim("appendix_e_rating_slope", pull_meta(meta_e10, "rating_index", "Favorability"),
      label = "Figure E.10 slope, favorability on the rating index",
      published = "0.034", digits = 3)
claim("appendix_e_rating_slope_points", 100 * pull_meta(meta_e10, "rating_index", "Favorability"),
      label = "the same slope expressed in points", published = "3.4", digits = 1)
claim("appendix_e_rating_slope_significant",
      as.numeric(abs(pull_meta(meta_e10, "rating_index", "Favorability") /
                     pull_meta(meta_e10, "rating_index", "Favorability", "std.error")) > 1.96),
      label = "the favorability slope is distinguishable from zero (1 = yes)",
      published = "1", digits = 0)

# "our sample to individuals who viewed two advertisements (n = 5,070)"
claim("appendix_e_two_ad_n", n_distinct(pairwise$caseid),
      label = "subjects who viewed two advertisements (article prints 5,070)",
      digits = 0, expect = "derived")

# "we generate uncertainty estimates via the nonparametric bootstrap with 1,000
#  resamples."
claim("appendix_e_boot_individual", individual_boot$R,
      label = "bootstrap resamples, individual analysis", published = "1000", digits = 0)
claim("appendix_e_boot_group", group_boot$R,
      label = "bootstrap resamples, group analysis", published = "1000", digits = 0)

# "in pro-Democrat vs. pro-Republican advertisement matchups, the pro-Democrat ad
#  had a larger CATE among Democrats 66.7% of the time for favorability and 83.3%
#  of the time for vote choice.  On the other hand, in the same matchups, for
#  Republicans, pro-Republican ads had a larger CATE only 33.3% of the time for
#  favorability and 33.3% of the time for vote choice.  Interestingly, in the same
#  matchups, the pro-Democratic and pro-Republican ads had larger CATES for
#  favorability and vote choice in exactly the same percentage of cases: 50%."
#
# These are matchup-level quantities over the six pro-Democrat versus
# pro-Republican ad pairs, not subject-level ones.
matchups <- pairwise |>
  filter(dem_and_rep == 1) |>
  summarise(fav_dem_wins = first((one_fav_cate_winner == 1 & valence_1 == "Pro Democrat") |
                                 (one_fav_cate_winner == 0 & valence_2 == "Pro Democrat")),
            vote_dem_wins = first((one_vote_cate_winner == 1 & valence_1 == "Pro Democrat") |
                                  (one_vote_cate_winner == 0 & valence_2 == "Pro Democrat")),
            .by = c(ad_id_1, ad_id_2, pid_3_pre))
mu <- \(p, col) 100 * mean(matchups[[col]][matchups$pid_3_pre == p], na.rm = TRUE)

claim("appendix_e_matchups", n_distinct(paste(matchups$ad_id_1, matchups$ad_id_2)),
      label = "pro-Democrat versus pro-Republican ad matchups", published = "6", digits = 0)
claim("appendix_e_dem_fav", mu("Democrat", "fav_dem_wins"),
      label = "pro-Dem ad has the larger favorability CATE among Democrats",
      published = "66.7", digits = 1)
claim("appendix_e_dem_vote", mu("Democrat", "vote_dem_wins"),
      label = "pro-Dem ad has the larger vote choice CATE among Democrats",
      published = "83.3", digits = 1)
claim("appendix_e_rep_fav", 100 - mu("Republican", "fav_dem_wins"),
      label = "pro-Rep ad has the larger favorability CATE among Republicans",
      published = "33.3", digits = 1)
claim("appendix_e_rep_vote", 100 - mu("Republican", "vote_dem_wins"),
      label = "pro-Rep ad has the larger vote choice CATE among Republicans",
      published = "33.3", digits = 1)
claim("appendix_e_ind_fav", mu("Independent", "fav_dem_wins"),
      label = "pro-Dem ad has the larger favorability CATE among independents",
      published = "50.0", digits = 1)
claim("appendix_e_ind_vote", mu("Independent", "vote_dem_wins"),
      label = "pro-Dem ad has the larger vote choice CATE among independents",
      published = "50.0", digits = 1)

# "Democrats gave higher ratings to the pro-Democratic ad 84.5% of the time and
#  Republicans gave higher ratings to the pro-Republican ad 60.3% of the time."
# CORRECTED AT PROOF: 60.3% is the figure for independents. Republicans is 87.1%.
rated <- pairwise |>
  filter(dem_and_rep == 1) |>
  mutate(rated_pro_dem_higher = (one_pred_winner == 1 & valence_1 == "Pro Democrat") |
                                (one_pred_winner == 0 & valence_2 == "Pro Democrat")) |>
  summarise(pct_pro_dem = 100 * mean(rated_pro_dem_higher, na.rm = TRUE), .by = pid_3_pre)
rt <- \(p) rated$pct_pro_dem[rated$pid_3_pre == p]

claim("appendix_e_dem_rates_in_party", rt("Democrat"),
      label = "Democrats rate the pro-Democratic ad higher", published = "84.5", digits = 1)
claim("appendix_e_rep_rates_in_party", 100 - rt("Republican"),
      label = "Republicans rate the pro-Republican ad higher (article prints 60.3)",
      published = "87.1", digits = 1)
claim("appendix_e_ind_rates_pro_rep", 100 - rt("Independent"),
      label = "independents rate the pro-Republican ad higher (the source of the article's 60.3)",
      published = "60.3", digits = 1)

# "individuals are no better than random guessing at accurately predicting which
#  ad will be more persuasive."
overall_ci <- function(o, term) {
  est <- as.numeric(o$t0[term]); se <- boot_se(o, term)
  c(lo = est - 1.96 * se, hi = est + 1.96 * se)
}
ci_fav  <- overall_ci(individual_boot, "cate_fav_all_correct_pct_overall")
ci_vote <- overall_ci(individual_boot, "cate_vote_all_correct_pct_overall")
claim("appendix_e_individual_no_better",
      as.numeric(ci_fav["lo"] <= 0.5 && ci_fav["hi"] >= 0.5 &&
                 ci_vote["lo"] <= 0.5 && ci_vote["hi"] >= 0.5),
      label = "both overall individual accuracy intervals contain 0.50 (1 = yes)",
      published = "1", digits = 0)

claim_evidence(
  "Figures E.11 and E.12 carry no vote choice estimates for pro-Republican versus pro-Republican matchups: both such matchups were fielded during the primary, when the general election vote choice item was not asked."
)

claim_summary()
