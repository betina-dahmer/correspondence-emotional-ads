# Murphy, Coppock, Hill, and Vavreck
# "Do Emotional Ads Persuade? Evidence from Real-Time Campaign Advertising Experiments"
# Political Communication
#
# clean_exps.R: Cleans raw survey data for the main analysis sample.

source(here::here("code", "helpers.R"))

exps <- read_rds(here::here("data", "exps.rds"))


# Schema assertions: fail loudly if the raw file is not the one this script expects.
stopifnot(
  nrow(exps) == 34000,
  dplyr::n_distinct(exps$date) == 29,
  all(c("caseid", "ad_id", "ad_valence", "date", "wave", "pid_3_pre", "pid_7_pre",
        "weights", "assignment") %in% names(exps)),
  # emotion items arrive one per ad slot and are collapsed below
  all(paste0(rep(c("happy", "hopeful", "worried", "angry"), each = 3), 1:3) %in% names(exps))
)

exps_clean <-
  exps |>
  # removing control group and ad_id 5 and Pro and Anti ads
  filter(!is.na(ad_id), 
         ! ad_id %in% c(5)) |>
  mutate(
    quality = coalesce(quality1, quality2, quality3),
    angry = coalesce(angry1, angry2, angry3),
    worried = coalesce(worried1, worried2, worried3),
    hopeful = coalesce(hopeful1, hopeful2, hopeful3),
    happy = coalesce(happy1, happy2, happy3),
    
    run = coalesce(run1, run2, run3),
    fair = coalesce(fair1, fair2, fair3),
    truth = coalesce(truth1, truth2, truth3),
    mem = coalesce(mem1, mem2, mem3),
    tone = coalesce(tone1, tone2, tone3)
  )

exps_clean <-
  exps_clean |>
  mutate(
    # higher values reflect higher values of the variable
    quality = if_else(quality == 997, NA_real_, quality),
    run = -1 * run + 4,
    fair = if_else(fair == 6, NA_real_, -1 * fair + 6),
    truth = if_else(truth == 6, NA_real_, -1 * truth + 6),
    mem = if_else(mem == 6, NA_real_, -1 * mem + 6),
    tone = -1 * tone + 6,
    quality = (quality - 1) / 99,
    fair = (fair - 1) / 4,
    truth = (truth - 1) / 4,
    mem = (mem - 1) / 4,
    run = (run - 1) / 2,
    angry    = (4 - angry) / 3,
    worried  = (4 - worried) / 3,
    hopeful  = (4 - hopeful) / 3,
    happy    = (4 - happy) / 3,
    
    index = rowMeans(pick(c(
      quality, fair, truth, mem, run
    )), na.rm = TRUE),
    positive_index = rowMeans(pick(c(hopeful, happy)), na.rm = TRUE),
    negative_index = rowMeans(pick(c(angry, worried)), na.rm = TRUE)
  ) |>
  mutate(favorDT_rev = (favorDT_rev - 1)/4,
         favorHC_rev = (favorHC_rev - 1)/4,
         favorBS_rev = (favorBS_rev - 1)/4,
         favorBS_rev = (favorBS_rev - 1)/4,
         favorJK_rev = (favorJK_rev - 1)/4,
         favorTC_rev = (favorTC_rev - 1)/4) |> 
  mutate(will_vote = case_when(election_phase == "General Election" & match_DTHC %in% c(1, 2) ~ 1,
                               election_phase == "General Election" & match_DTHC == 9 ~ 0,
                               TRUE ~ NA)) |>
  mutate(
    # after this recode, higher values indicate stronger support
    Base1IS_01 = (4 - Base1IS)/3,
    Base1TX_01 = (4 - Base1TX)/3,
    Base1BF_01 = (4 - Base1BF)/3,
    Base1PC_01 = (4 - Base1PC)/3,
    Base1AB_01 = (4 - Base1AB)/3,
    Base1IW_01 = (4 - Base1IW)/3,
    Base1EP_01 = (4 - Base1EP)/3,
    Base1GC_01 = (4 - Base1GC)/3,
    Base1HA_01 = (4 - Base1HA)/3,
    Base1GN_01 = (4 - Base1GN)/3,
    
    #collasping two gun control measures
    Base1GN_01 = if_else(is.na(Base1GN_01), Base1GC_01, Base1GN_01)
  )
  
stopifnot(
  nrow(exps_clean) == 28679,
  dplyr::n_distinct(exps_clean$date) == 29,
  dplyr::n_distinct(exps_clean$ad_id[exps_clean$ad_id != 3]) == 48,
  nrow(dplyr::distinct(dplyr::filter(exps_clean, ad_id != 3), ad_id, date)) == 58
)

write_rds(exps_clean, here::here("data", "exps_clean.rds"))
