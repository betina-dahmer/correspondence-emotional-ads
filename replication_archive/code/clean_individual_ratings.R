# Murphy, Coppock, Hill, and Vavreck
# "Do Emotional Ads Persuade? Evidence from Real-Time Campaign Advertising Experiments"
# Political Communication
#
# clean_individual_ratings.R: Cleans raw survey data for the individual-level ratings analysis.

source(here::here("code", "helpers.R"))

exps <- read_rds(here::here("data", "exps.rds"))

individual_ratings <-
  exps |>
  # removing control group and ad_id 5, which is mislabeled
  filter(!is.na(ad_id), !ad_id %in% c(3, 5)) |>
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

individual_ratings <-
  individual_ratings |>
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
  )

individual_ratings <-
  individual_ratings |>
  filter(
    ad_valence %in% c("Pro Trump", "Pro Clinton", 
                      "Anti Trump", "Anti Clinton"),
    ad_sponsor != "cruz"
  )

write_rds(individual_ratings, here::here("data", "individual_ratings_clean.rds"))
