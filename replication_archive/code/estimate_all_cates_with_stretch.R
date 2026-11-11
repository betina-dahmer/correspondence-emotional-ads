# Murphy, Coppock, Hill, and Vavreck
# "Do Emotional Ads Persuade? Evidence from Real-Time Campaign Advertising Experiments"
# Political Communication
#
# estimate_all_cates_with_stretch.R: Estimates CATEs for all outcomes with stretch issue coding.

source(here::here("code", "helpers.R"))

exps_clean <- read_rds(here::here("data", "exps_clean.rds"))
ad_df <- read_csv(here::here("data", "ad_df.csv"))
base_codes <- read_csv(here::here("data", "ad_id_to_base_by_hand.csv"))

base_codes <-
  base_codes |> 
  mutate(outcome = paste0(outcome, "_01"))

# Run all regressions -----------------------------------------------------

exps_long <-
  exps_clean |>
  pivot_longer(c(favorDT_rev,
                 favorHC_rev,
                 favorBS_rev,
                 favorTC_rev,
                 favorJK_rev,
                 general_vote_HC,
                 general_vote_DT,
                 will_vote,
                 Base1IS_01,
                 Base1TX_01,
                 Base1BF_01,
                 Base1PC_01,
                 Base1AB_01,
                 Base1IW_01,
                 Base1EP_01,
                 Base1GC_01,
                 Base1HA_01,
                 Base1GN_01),
               names_to = "outcome") |>
  left_join(base_codes) |>
  mutate(target_issue_stretch = if_else(ad_id == 3, 1, target_issue_stretch)) |>
  filter(outcome %in% c("favorDT_rev", "favorHC_rev", "favorBS_rev", "favorTC_rev", "favorJK_rev", "general_vote_DT", "general_vote_HC", "will_vote") | target_issue_stretch == 1) |> # getting rid of non-targeted issues
  filter(
    (outcome %in% c("favorDT_rev") & ad_valence %in% c("Pro Trump", "Anti Trump", "Control") |
       outcome %in% c("general_vote_DT") & ad_valence %in% c("Pro Trump", "Anti Trump", "Control") & election_phase == "General Election" |
       outcome %in% c("favorHC_rev") & ad_valence %in% c("Pro Clinton", "Anti Clinton", "Control") |
       outcome %in% c("general_vote_HC") & ad_valence %in% c("Pro Clinton", "Anti Clinton", "Control") & election_phase == "General Election" | 
       (outcome %in% c("favorBS_rev") & ad_valence %in% c("Pro Sanders", "Control")) |
       (outcome %in% c("favorTC_rev") & ad_valence %in% c("Pro Cruz", "Control")) |
       (outcome %in% c("favorJK_rev") & ad_valence %in% c("Pro Kasich", "Control"))) |
      (outcome == "will_vote") | 
      (str_detect(outcome, "Base1"))
  ) |> 
  filter(!is.na(value))

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
    conf.high = if_else(anti_vector, conf.high * -1, conf.high),
  )

block_diagonal_vcov <-
  all_cates |>
  pull(vcov_output) |>
  map(\(x) x[!rownames(x) %in% c("(Intercept)", "pid_7_pre"), !colnames(x) %in% c("(Intercept)", "pid_7_pre")]) |>
  bdiag()

block_diagonal_vcov[anti_vector, ] <- block_diagonal_vcov[anti_vector, ] * -1
block_diagonal_vcov[, anti_vector] <- block_diagonal_vcov[, anti_vector] * -1

write_rds(estimates, here::here("data", "estimates", "all_cates_with_stretch.rds"))
write_rds(block_diagonal_vcov, here::here("data", "estimates", "block_diagonal_vcov_with_stretch.rds"))

