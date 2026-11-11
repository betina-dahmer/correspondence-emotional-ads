# Murphy, Coppock, Hill, and Vavreck
# "Do Emotional Ads Persuade? Evidence from Real-Time Campaign Advertising Experiments"
# Political Communication
#
# estimate_individual_accuracy_bootstrap.R: Bootstraps individual-level pairwise prediction accuracy (1000 resamples).

source(here::here("code", "helpers.R"))

set.seed(343)

# Load pre-data
exps_pre <- read_rds(here::here("data", "exps.rds"))
ad_df <- read_csv(here::here("data", "ad_df.csv"))
cates <- read_rds(here::here("data", "estimates", "all_cates.rds"))
two_ads_key <- read_rds(here::here("data", "two_ads_key.rds"))

exps_pre <- 
  exps_pre |>
  # Only need to estimate CATEs for later that at one point were part of the two-ad treatment
  filter(ad_id %in% c(3) | 
           ad_id %in% two_ads_key$ad_id_1 | 
           ad_id %in% two_ads_key$ad_id_2 |
           is.na(ad_id)) |>
  mutate(
    favorDT_rev = (favorDT_rev - 1)/4,
    favorHC_rev = (favorHC_rev - 1)/4,
    favorBS_rev = (favorBS_rev - 1)/4,
    favorBS_rev = (favorBS_rev - 1)/4,
    favorJK_rev = (favorJK_rev - 1)/4,
    favorTC_rev = (favorTC_rev - 1)/4
  )

minimal_exps <- 
  exps_pre |>
  select(caseid, ad_id, ad_id_fac, ad_valence, election_phase, date, wave,
         pid_3_pre, pid_7_pre, weights,
         favorHC_rev, favorDT_rev, favorJK_rev, favorTC_rev, favorBS_rev,
         general_vote_HC, general_vote_DT, assignment,
         quality1, run1, truth1, mem1, fair1,
         quality2, run2, truth2, mem2, fair2,
         quality3, run3, truth3, mem3, fair3)

two_ads_pre <-
  exps_pre |>
  filter(assignment == "Two Ads") |>
  mutate(
    quality1 = if_else(quality1 == 997, NA_real_, quality1),
    run1 = -1 * run1 + 4,
    fair1 = if_else(fair1 == 6, NA_real_, -1 * fair1 + 6),
    truth1 = if_else(truth1 == 6, NA_real_, -1 * truth1 + 6),
    mem1 = if_else(mem1 == 6, NA_real_, -1 * mem1 + 6),
    quality2 = if_else(quality2 == 997, NA_real_, quality2),
    run2 = -1 * run2 + 4,
    fair2 = if_else(fair2 == 6, NA_real_, -1 * fair2 + 6),
    truth2 = if_else(truth2 == 6, NA_real_, -1 * truth2 + 6),
    mem2 = if_else(mem2 == 6, NA_real_, -1 * mem2 + 6)
  ) |>
  mutate(
    quality1 = (quality1 - 1) / 99,
    fair1 = (fair1 - 1) / 4,
    truth1 = (truth1 - 1) / 4,
    mem1 = (mem1 - 1) / 4,
    run1 = (run1 - 1) / 2,
    quality2 = (quality2 - 1) / 99,
    fair2 = (fair2 - 1) / 4,
    truth2 = (truth2 - 1) / 4,
    mem2 = (mem2 - 1) / 4,
    run2 = (run2 - 1) / 2,
    index_1 = rowMeans(pick(c(quality1, fair1, truth1, mem1, run1)), na.rm = TRUE),
    index_2 = rowMeans(pick(c(quality2, fair2, truth2, mem2, run2)), na.rm = TRUE)
  ) |>
  select(caseid, wave, date, pid_3_pre, pid_7_pre, index_1, index_2)

# Join two_ads_key
two_ads_pre <-
  two_ads_pre |>
  left_join(two_ads_key, by = join_by(wave))

#two_ads_pre |> count(ad_valence_1)
# valence characteristics
two_ads_pre <- 
  two_ads_pre |>
  mutate(
    #pro_HC_1 = if_else(ad_valence_1 %in% c("Pro Clinton", "Anti Trump"), 1, 0),
    #pro_DT_1 = if_else(ad_valence_1 %in% c("Pro Trump", "Anti Clinton"), 1, 0),
    #pro_HC_2 = if_else(ad_valence_2 %in% c("Pro Clinton", "Anti Trump"), 1, 0),
    #pro_DT_2 = if_else(ad_valence_2 %in% c("Pro Trump", "Anti Clinton"), 1, 0),
    pro_dem_1 = if_else(ad_valence_1 %in% c("Pro Clinton", "Anti Trump", "Pro Sanders"), 1, 0),
    pro_dem_2 = if_else(ad_valence_2 %in% c("Pro Clinton", "Anti Trump", "Pro Sanders"), 1, 0),
    same_valence = if_else(pro_dem_1 == pro_dem_2, 1, 0))

    #HC_and_DT = if_else(pro_HC_1 == 1 & pro_DT_2 == 1 |
    #                     pro_HC_2 == 1 & pro_DT_1 == 1, 1, 0))

#indices <- sample(1:nrow(minimal_exps), nrow(minimal_exps), replace = TRUE)
#data <- minimal_exps
#new_exps <- data

# Builds the subject-level pairwise comparison frame for one resample. Split out
# of get_a() so the same frame can be built once on the observed data, written to
# output/, and read back by in_text_claims.R.
build_pairwise_frame <- function(data, indices) {
  
  new_data <- data[indices,]
  caseids <- new_data$caseid

  # Estimate cates
  exps <-
    new_data
  
  exps_long <-
    exps |>
    pivot_longer(c(favorDT_rev, favorHC_rev, favorTC_rev, favorJK_rev, favorBS_rev,
                   general_vote_HC, general_vote_DT),
                 names_to = "outcome") |> 
    filter(!is.na(value))
  
  # Estimate cates
  
  fits <-
    exps_long |>
    split(list(exps_long$outcome, exps_long$date, exps_long$pid_3_pre)) |>
    discard(~ nrow(.x) == 0) |> 
    discard(~ is.null(.x)) |> 
    discard(~ length(unique(.x$ad_id_fac)) == 1) |>
    map( ~ lm_robust(
      value ~ ad_id_fac + pid_7_pre,
      weights = weights,
      data = .
    ))
  
  all_cates <- tibble(
    name = names(fits),
    tidy_output = map(fits, tidy),
    vcov_output = map(fits, vcov)
  )
  
  all_cates <- 
    all_cates |> 
    separate(name, into = c("outcome", "date", "pid_3_pre"), sep = "\\.")
  
  estimates <-
    all_cates |>
    rename(outcome_name = outcome) |>
    unnest(tidy_output) |>
    filter(!term %in% c("(Intercept)", "pid_7_pre")) |> 
    mutate(ad_id = as.numeric(str_remove(term, "ad_id_fac")),
           date = as.Date(date)) |> 
    select(-outcome) |> 
    rename(outcome = outcome_name)
  
  estimates <- 
    estimates |> 
    left_join(ad_df, by = join_by(ad_id))
  
  anti_vector <- str_detect(estimates$ad_valence, "Anti")
  
  estimates <- 
    estimates |> 
    mutate(
      estimate = if_else(anti_vector, estimate * -1, estimate),
      conf.low = if_else(anti_vector, conf.low * -1, conf.low),
      conf.high = if_else(anti_vector, conf.high * -1, conf.high)
    )
  
  cates <-
    estimates |>
    mutate(
      target_candidate =
        case_when(
          str_detect(ad_valence, "Trump") ~ "Trump",
          str_detect(ad_valence, "Clinton") ~ "Clinton",
          str_detect(ad_valence, "Cruz") ~ "Cruz",
          str_detect(ad_valence, "Sanders") ~ "Sanders",
          str_detect(ad_valence, "Kasich") ~ "Kasich"
        ),
      target_attitude =
        case_when(
          str_detect(outcome, "DT") ~ "Trump",
          str_detect(outcome, "HC") ~ "Clinton",
          str_detect(outcome, "TC") ~ "Cruz",
          str_detect(outcome, "BS") ~ "Sanders",
          str_detect(outcome, "JK") ~ "Kasich"
        )
    ) |>
    filter(target_attitude == target_candidate)
  
  fav_cates <- 
    cates |>
    filter(str_detect(outcome, "favor"))  |>
    summarize(estimate = weighted.mean(estimate, weight = df),
              .by = c(ad_id, pid_3_pre, ad_valence))
  vote_cates <- 
    cates |>
    filter(str_detect(outcome, "vote")) |>
    summarize(estimate = weighted.mean(estimate, weight = df),
              .by = c(ad_id, pid_3_pre, ad_valence))
  
# Two Ads
  # Carry each resampled subject's multiplicity into the two-ad stage. A
  # `caseid %in% caseids` filter would admit a subject drawn three times only
  # once, which subsamples roughly 63% of the two-ad subjects instead of
  # resampling them and understates the accuracy standard errors.
  two_ads <-
    tibble(caseid = caseids) |>
    inner_join(two_ads_pre, by = join_by(caseid))

  # Join cates
  two_ads_ates_cates <- 
    two_ads |>
    left_join(
      rename(
        fav_cates,
        ad_id_1 = ad_id,
        ad_valence_1 = ad_valence,
        ad_fav_cate_estimate_1 = estimate
      ), by = join_by(ad_id_1, pid_3_pre, ad_valence_1)
    ) |>
    left_join(
      rename(
        vote_cates,
        ad_id_1 = ad_id,
        ad_valence_1 = ad_valence,
        ad_vote_cate_estimate_1 = estimate
      ), by = join_by(ad_id_1, pid_3_pre, ad_valence_1)
    )  |>
    left_join(
      rename(
        fav_cates,
        ad_id_2 = ad_id,
        ad_valence_2 = ad_valence,
        ad_fav_cate_estimate_2 = estimate
      ), by = join_by(ad_id_2, pid_3_pre, ad_valence_2)
    ) |>
    left_join(
      rename(
        vote_cates,
        ad_id_2 = ad_id,
        ad_valence_2 = ad_valence,
        ad_vote_cate_estimate_2 = estimate
      ), by = join_by(ad_id_2, pid_3_pre, ad_valence_2)
    )
  
  # correct and valence statisticsstatistics
  individual_pairwise_frame <-
    two_ads_ates_cates |>
    mutate(
      one_pred_winner = if_else(index_1 > index_2, 1, 0),
      one_fav_cate_winner = if_else(ad_fav_cate_estimate_1 > ad_fav_cate_estimate_2, 1, 0),
      one_vote_cate_winner = if_else(ad_vote_cate_estimate_1 > ad_vote_cate_estimate_2, 1, 0),
      cate_fav_correct = if_else(one_pred_winner == one_fav_cate_winner, 1, 0),
      cate_vote_correct = if_else(one_pred_winner == one_vote_cate_winner, 1, 0),
      ad_wins_fav_and_vote_cate = if_else(one_fav_cate_winner == one_vote_cate_winner, 1, 0)
    )
  
  # Individual pairwise bootstrap
  individual_pairwise_frame <- 
    individual_pairwise_frame |>
    mutate(
      valence_1 = if_else(ad_valence_1 %in% c("Pro Trump", "Anti Clinton", "Pro Kasich", "Pro Cruz"), "Pro Republican", "Pro Democrat"),
      valence_2 = if_else(ad_valence_2 %in% c("Pro Trump", "Anti Clinton", "Pro Kasich", "Pro Cruz"), "Pro Republican", "Pro Democrat"),
      pro_dem = if_else(valence_1 == "Pro Democrat" & valence_2 == "Pro Democrat", 1, 0),
      pro_rep = if_else(valence_1 == "Pro Republican" & valence_2 == "Pro Republican", 1, 0),
      dem_and_rep = if_else((pro_dem == 0 & pro_rep == 0), 1, 0)
    )
  
  individual_pairwise_frame
}

get_a <- function(data, indices) {
  individual_pairwise_frame <- build_pairwise_frame(data, indices)

  # The vote-choice cells for pro-Republican-vs-pro-Republican matchups are not
  # computed: both such matchups (waves 7 and 8) ran during the primary, when the
  # general-election vote choice item was not asked, so the subset is empty by
  # design rather than by missingness.
  c(
  
    # overall
    cate_fav_all_correct_pct_overall = mean(individual_pairwise_frame$cate_fav_correct, na.rm = TRUE),
    cate_fav_all_correct_pct_HCDT = mean(individual_pairwise_frame$cate_fav_correct[individual_pairwise_frame$dem_and_rep == 1], na.rm = TRUE),
    cate_fav_all_correct_pct_dem_ads = mean(individual_pairwise_frame$cate_fav_correct[individual_pairwise_frame$pro_dem == 1], na.rm = TRUE),
    cate_fav_all_correct_pct_rep_ads = mean(individual_pairwise_frame$cate_fav_correct[individual_pairwise_frame$pro_rep == 1], na.rm = TRUE),
    
    cate_vote_all_correct_pct_overall = mean(individual_pairwise_frame$cate_vote_correct, na.rm = TRUE),
    cate_vote_all_correct_pct_HCDT = mean(individual_pairwise_frame$cate_vote_correct[individual_pairwise_frame$dem_and_rep == 1], na.rm = TRUE),
    cate_vote_all_correct_pct_dem_ads = mean(individual_pairwise_frame$cate_vote_correct[individual_pairwise_frame$pro_dem == 1], na.rm = TRUE),
    
    # dems
    cate_fav_dem_correct_pct_overall = mean(individual_pairwise_frame$cate_fav_correct[individual_pairwise_frame$pid_3_pre == "Democrat"], na.rm = TRUE),
    cate_fav_dem_correct_pct_HCDT = mean(individual_pairwise_frame$cate_fav_correct[individual_pairwise_frame$dem_and_rep == 1 & individual_pairwise_frame$pid_3_pre == "Democrat"], na.rm = TRUE),
    cate_fav_dem_correct_pct_dem_ads = mean(individual_pairwise_frame$cate_fav_correct[individual_pairwise_frame$pro_dem == 1 & individual_pairwise_frame$pid_3_pre == "Democrat"], na.rm = TRUE),
    cate_fav_dem_correct_pct_rep_ads = mean(individual_pairwise_frame$cate_fav_correct[individual_pairwise_frame$pro_rep == 1 & individual_pairwise_frame$pid_3_pre == "Democrat"], na.rm = TRUE),
    
    cate_vote_dem_correct_pct_overall = mean(individual_pairwise_frame$cate_vote_correct[individual_pairwise_frame$pid_3_pre == "Democrat"], na.rm = TRUE),
    cate_vote_dem_correct_pct_HCDT = mean(individual_pairwise_frame$cate_vote_correct[individual_pairwise_frame$dem_and_rep == 1 & individual_pairwise_frame$pid_3_pre == "Democrat"], na.rm = TRUE),
    cate_vote_dem_correct_pct_dem_ads = mean(individual_pairwise_frame$cate_vote_correct[individual_pairwise_frame$pro_dem == 1 & individual_pairwise_frame$pid_3_pre == "Democrat"], na.rm = TRUE),
    
    # inds
    cate_fav_ind_correct_pct_overall = mean(individual_pairwise_frame$cate_fav_correct[individual_pairwise_frame$pid_3_pre == "Independent"], na.rm = TRUE),
    cate_fav_ind_correct_pct_HCDT = mean(individual_pairwise_frame$cate_fav_correct[individual_pairwise_frame$dem_and_rep == 1 & individual_pairwise_frame$pid_3_pre == "Independent"], na.rm = TRUE),
    cate_fav_ind_correct_pct_dem_ads = mean(individual_pairwise_frame$cate_fav_correct[individual_pairwise_frame$pro_dem == 1 & individual_pairwise_frame$pid_3_pre == "Independent"], na.rm = TRUE),
    cate_fav_ind_correct_pct_rep_ads = mean(individual_pairwise_frame$cate_fav_correct[individual_pairwise_frame$pro_rep == 1 & individual_pairwise_frame$pid_3_pre == "Independent"], na.rm = TRUE),
    
    cate_vote_ind_correct_pct_overall = mean(individual_pairwise_frame$cate_vote_correct[individual_pairwise_frame$pid_3_pre == "Independent"], na.rm = TRUE),
    cate_vote_ind_correct_pct_HCDT = mean(individual_pairwise_frame$cate_vote_correct[individual_pairwise_frame$dem_and_rep == 1 & individual_pairwise_frame$pid_3_pre == "Independent"], na.rm = TRUE),
    cate_vote_ind_correct_pct_dem_ads = mean(individual_pairwise_frame$cate_vote_correct[individual_pairwise_frame$pro_dem == 1 & individual_pairwise_frame$pid_3_pre == "Independent"], na.rm = TRUE),
    
    # reps
    cate_fav_rep_correct_pct_overall = mean(individual_pairwise_frame$cate_fav_correct[individual_pairwise_frame$pid_3_pre == "Republican"], na.rm = TRUE),
    cate_fav_rep_correct_pct_HCDT = mean(individual_pairwise_frame$cate_fav_correct[individual_pairwise_frame$dem_and_rep == 1 & individual_pairwise_frame$pid_3_pre == "Republican"], na.rm = TRUE),
    cate_fav_rep_correct_pct_dem_ads = mean(individual_pairwise_frame$cate_fav_correct[individual_pairwise_frame$pro_dem == 1 & individual_pairwise_frame$pid_3_pre == "Republican"], na.rm = TRUE),
    cate_fav_rep_correct_pct_rep_ads = mean(individual_pairwise_frame$cate_fav_correct[individual_pairwise_frame$pro_rep == 1 & individual_pairwise_frame$pid_3_pre == "Republican"], na.rm = TRUE),
    
    cate_vote_rep_correct_pct_overall = mean(individual_pairwise_frame$cate_vote_correct[individual_pairwise_frame$pid_3_pre == "Republican"], na.rm = TRUE),
    cate_vote_rep_correct_pct_HCDT = mean(individual_pairwise_frame$cate_vote_correct[individual_pairwise_frame$dem_and_rep == 1 & individual_pairwise_frame$pid_3_pre == "Republican"], na.rm = TRUE),
    cate_vote_rep_correct_pct_dem_ads = mean(individual_pairwise_frame$cate_vote_correct[individual_pairwise_frame$pro_dem == 1 & individual_pairwise_frame$pid_3_pre == "Republican"], na.rm = TRUE)
    
  )
  
}

individual_pairwise_results <- boot(minimal_exps, statistic = get_a, R = 1000, stype = "i")

# Save
write_rds(individual_pairwise_results, here::here("data", "estimates", "individual_bootstrap_results_1000.rds"))

# Build the pairwise frame once on the observed data and persist it, so the
# percentages reported in Appendix E are computed from a pipeline output rather
# than from an object that only exists inside the bootstrap statistic.
observed_pairwise_frame <-
  build_pairwise_frame(minimal_exps, seq_len(nrow(minimal_exps)))

write_rds(observed_pairwise_frame, here::here("data", "estimates", "observed_pairwise_frame.rds"))
