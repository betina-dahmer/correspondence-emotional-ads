# Murphy, Coppock, Hill, and Vavreck
# "Do Emotional Ads Persuade? Evidence from Real-Time Campaign Advertising Experiments"
# Political Communication
#
# estimate_ratings_pid.R: Estimates average ad ratings by partisanship and experimental wave.

source(here::here("code", "helpers.R"))

exps_clean <- read_rds(here::here("data", "exps_clean.rds"))
exps_clean <- exps_clean |>
  filter(ad_id != 3) # Do not need ratings of control ad

dvs <- c(
  "quality",
  "fair",
  "truth",
  "mem",
  "run",
  "index",
  "angry",
  "worried",
  "hopeful",
  "happy",
  "positive_index",
  "negative_index"
)

results_list <- list()

for (i in 1:length(dvs)) {
  local_formula <- formula(paste0(dvs[i], " ~ 1"))
  
  results_list[[i]] <-
    exps_clean |>
    group_by(ad_id, pid_3_pre, date) |>
    reframe(tidy(lm_robust(local_formula, data = pick(everything()))))
  
}

results_df <- bind_rows(results_list)

results_df <-
  results_df |> 
  transmute(
    ad_id,
    pid_3_pre,
    date,
    rating_estimate = estimate,
    rating_std.error = std.error,
    rating_statistic = statistic,
    rating_p.value = p.value,
    rating_conf.low = conf.low,
    rating_conf.high = conf.high,
    rating_outcome = outcome
  )

write_rds(results_df, file = here::here("data", "estimates", "ratings_pid.rds"))
