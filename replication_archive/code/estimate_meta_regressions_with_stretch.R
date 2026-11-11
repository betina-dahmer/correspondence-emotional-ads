# Murphy, Coppock, Hill, and Vavreck
# "Do Emotional Ads Persuade? Evidence from Real-Time Campaign Advertising Experiments"
# Political Communication
#
# estimate_meta_regressions_with_stretch.R: Estimates meta-regressions using stretch issue coding. Produces Tables C5, C10, C15.

source(here::here("code", "helpers.R"))

# Load Data
ratings <- read_rds(here::here("data", "estimates", "ratings_pid.rds"))
ad_df_edited <- read_csv(here::here("data", "ad_df_emotions.csv"))
cates <- read_rds(here::here("data", "estimates", "all_cates_with_stretch.rds"))
block_diagonal_vcov <- read_rds(here::here("data", "estimates", "block_diagonal_vcov_with_stretch.rds"))

# Positive
positive_index_ratings <- 
  ratings |>
  filter(rating_outcome == "positive_index")

cates_with_positive <- 
  cates |>
  left_join(positive_index_ratings) |>
  mutate(
    outcome = case_when(str_detect(outcome, "favor") ~ "Favorability",
                        str_detect(outcome, "general_vote") ~ "Vote Choice",
                        str_detect(outcome, "will_vote") ~ "Turnout",
                        str_detect(outcome, "Base") ~ "Issue"),
    valence = if_else(ad_valence %in% c("Pro Trump","Anti Clinton", "Pro Kasich", "Pro Cruz"), "Pro Republican", "Pro Democrat")
  )

# Negative
negative_index_ratings <- 
  ratings |>
  filter(rating_outcome == "negative_index")

cates_with_negative <- 
  cates |>
  left_join(negative_index_ratings) |>
  mutate(
    outcome = case_when(str_detect(outcome, "favor") ~ "Favorability",
                        str_detect(outcome, "general_vote") ~ "Vote Choice",
                        str_detect(outcome, "will_vote") ~ "Turnout",
                        str_detect(outcome, "Base") ~ "Issue"),
    valence = if_else(ad_valence %in% c("Pro Trump","Anti Clinton", "Pro Kasich", "Pro Cruz"), "Pro Republican", "Pro Democrat")
  )

# Intended Emotions
intended_index_ratings <- 
  ratings |>
  left_join(ad_df_edited) |>
  filter(rating_outcome %in% c("positive_index", "negative_index")) |>
  select(ad_id, pid_3_pre, date, rating_estimate, rating_outcome, ad_valence, emotion_valence) |>
  pivot_wider(names_from = rating_outcome, values_from = rating_estimate) |>
  mutate(intended_index = case_when(emotion_valence == "negative" ~ negative_index - positive_index,
                                    emotion_valence == "positive" ~ positive_index - negative_index,
                                    emotion_valence == "both" ~ positive_index + negative_index)) |>
  pivot_longer(cols = c(positive_index, negative_index, intended_index), names_to = "rating_outcome", values_to = "rating_estimate") |>
  filter(rating_outcome == "intended_index")

cates_with_intended <- 
  cates |>
  left_join(intended_index_ratings) |>
  mutate(
    outcome = case_when(str_detect(outcome, "favor") ~ "Favorability",
                        str_detect(outcome, "general_vote") ~ "Vote Choice",
                        str_detect(outcome, "will_vote") ~ "Turnout",
                        str_detect(outcome, "Base") ~ "Issue"),
    valence = if_else(ad_valence %in% c("Pro Trump","Anti Clinton", "Pro Kasich", "Pro Cruz"), "Pro Republican", "Pro Democrat")
  )

pid_3_pre_s <- unique(cates_with_positive$pid_3_pre)
valence_s <- unique(cates_with_positive$valence)
outcome_s <- unique(cates_with_positive$outcome)

fitlist <- list()

# Positive

cates_with_positive |> 
  group_by(pid_3_pre, outcome) |> 
  summarise(n = n())

cates_with_positive <- 
  cates_with_positive |> 
  mutate(id = 1:n())

# All

for (k in seq_along(outcome_s)) {
  # print(paste0(pid_3_pre_s[i]))
  # print(paste0(rating_outcome_s[j]))
  # print(paste0(outcome_s[k]))
  subset <-
    cates_with_positive$outcome == outcome_s[k]
  
  if (sum(subset) <= 2) {
    fitlist <- c(fitlist, list(NULL))
  } else{
    fitlist <- c(fitlist, list(
      rma.mv(
        yi = estimate,
        mods = ~ rating_estimate,
        random = ~ 1 | id,
        V = block_diagonal_vcov[subset, subset], 
        data = cates_with_positive[subset, ]
      ) |> robust(cluster = date, clubSandwich = TRUE)
    )
    )
  }
  names(fitlist)[length(fitlist)] <- glue("positive", outcome_s[k], .sep = ".")
  
}

# CATEs

for(i in seq_along(pid_3_pre_s)) {
  for (j in seq_along(valence_s)) {
    for (k in seq_along(outcome_s)) {
      # print(paste0(pid_3_pre_s[i]))
      # print(paste0(rating_outcome_s[j]))
      # print(paste0(outcome_s[k]))
      subset <-
        cates_with_positive$pid_3_pre == pid_3_pre_s[i] &
        cates_with_positive$valence == valence_s[j] &
        cates_with_positive$outcome == outcome_s[k]
      
      if (sum(subset) <= 2) {
        fitlist <- c(fitlist, list(NULL))
      } else{
        fitlist <- c(fitlist, list(
          rma.mv(
            yi = estimate,
            mods = ~ rating_estimate,
            random = ~ 1 | id,
            V = block_diagonal_vcov[subset, subset], 
            data = cates_with_positive[subset, ]
          ) |> robust(cluster = date, clubSandwich = TRUE)
        )
        )
      }
      names(fitlist)[length(fitlist)] <- glue("positive", pid_3_pre_s[i], valence_s[j], outcome_s[k], .sep = ".")
      
    }
  }
}

# Negative

cates_with_negative |> 
  group_by(pid_3_pre, outcome) |> 
  summarise(n = n())

cates_with_negative <- 
  cates_with_negative |> 
  mutate(id = 1:n())

# All

for (k in seq_along(outcome_s)) {
  # print(paste0(pid_3_pre_s[i]))
  # print(paste0(rating_outcome_s[j]))
  # print(paste0(outcome_s[k]))
  subset <-
    cates_with_negative$outcome == outcome_s[k]
  
  if (sum(subset) <= 2) {
    fitlist <- c(fitlist, list(NULL))
  } else{
    fitlist <- c(fitlist, list(
      rma.mv(
        yi = estimate,
        mods = ~ rating_estimate,
        random = ~ 1 | id,
        V = block_diagonal_vcov[subset, subset], 
        data = cates_with_negative[subset, ]
      ) |> robust(cluster = date, clubSandwich = TRUE)
    )
    )
  }
  names(fitlist)[length(fitlist)] <- glue("negative", outcome_s[k], .sep = ".")
  
}

# CATEs

for(i in seq_along(pid_3_pre_s)) {
  for (j in seq_along(valence_s)) {
    for (k in seq_along(outcome_s)) {
      # print(paste0(pid_3_pre_s[i]))
      # print(paste0(rating_outcome_s[j]))
      # print(paste0(outcome_s[k]))
      subset <-
        cates_with_negative$pid_3_pre == pid_3_pre_s[i] &
        cates_with_negative$valence == valence_s[j] &
        cates_with_negative$outcome == outcome_s[k]
      
      if (sum(subset) <= 2) {
        fitlist <- c(fitlist, list(NULL))
      } else{
        fitlist <- c(fitlist, list(
          rma.mv(
            yi = estimate,
            mods = ~ rating_estimate,
            random = ~ 1 | id,
            V = block_diagonal_vcov[subset, subset], 
            data = cates_with_negative[subset, ]
          ) |> robust(cluster = date, clubSandwich = TRUE)
        )
        )
      }
      names(fitlist)[length(fitlist)] <- glue("negative", pid_3_pre_s[i], valence_s[j], outcome_s[k], .sep = ".")
      
    }
  }
}

# Intended

cates_with_intended |> 
  group_by(pid_3_pre, outcome) |> 
  summarise(n = n())

cates_with_intended <- 
  cates_with_intended |> 
  mutate(id = 1:n())

# All

for (k in seq_along(outcome_s)) {
  # print(paste0(pid_3_pre_s[i]))
  # print(paste0(rating_outcome_s[j]))
  # print(paste0(outcome_s[k]))
  subset <-
    cates_with_intended$outcome == outcome_s[k]
  
  if (sum(subset) <= 2) {
    fitlist <- c(fitlist, list(NULL))
  } else{
    fitlist <- c(fitlist, list(
      rma.mv(
        yi = estimate,
        mods = ~ rating_estimate,
        random = ~ 1 | id,
        V = block_diagonal_vcov[subset, subset], 
        data = cates_with_intended[subset, ]
      ) |> robust(cluster = date, clubSandwich = TRUE)
    )
    )
  }
  names(fitlist)[length(fitlist)] <- glue("intended", outcome_s[k], .sep = ".")
  
}

# CATEs

for(i in seq_along(pid_3_pre_s)) {
  for (j in seq_along(valence_s)) {
    for (k in seq_along(outcome_s)) {
      # print(paste0(pid_3_pre_s[i]))
      # print(paste0(rating_outcome_s[j]))
      # print(paste0(outcome_s[k]))
      subset <-
        cates_with_intended$pid_3_pre == pid_3_pre_s[i] &
        cates_with_intended$valence == valence_s[j] &
        cates_with_intended$outcome == outcome_s[k]
      
      if (sum(subset) <= 2) {
        fitlist <- c(fitlist, list(NULL))
      } else{
        fitlist <- c(fitlist, list(
          rma.mv(
            yi = estimate,
            mods = ~ rating_estimate,
            random = ~ 1 | id,
            V = block_diagonal_vcov[subset, subset], 
            data = cates_with_intended[subset, ]
          ) |> robust(cluster = date, clubSandwich = TRUE)
        )
        )
      }
      names(fitlist)[length(fitlist)] <- glue("intended", pid_3_pre_s[i], valence_s[j], outcome_s[k], .sep = ".")
      
    }
  }
}

# Fully specified models

# Positive
for (k in seq_along(outcome_s)) {
  # print(paste0(pid_3_pre_s[i]))
  # print(paste0(rating_outcome_s[j]))
  # print(paste0(outcome_s[k]))
  subset <-
    cates_with_positive$outcome == outcome_s[k]
  
  if (sum(subset) <= 2) {
    fitlist <- c(fitlist, list(NULL))
  } else{
    fitlist <- c(fitlist, list(
      rma.mv(
        yi = estimate,
        mods = ~ rating_estimate + valence + pid_3_pre,
        random = ~ 1 | id,
        V = block_diagonal_vcov[subset, subset], 
        data = cates_with_positive[subset, ]
      ) |> robust(cluster = date, clubSandwich = TRUE)
    )
    )
  }
  names(fitlist)[length(fitlist)] <- glue("all.positive", outcome_s[k], .sep = ".")
  
}

# Negative
for (k in seq_along(outcome_s)) {
  # print(paste0(pid_3_pre_s[i]))
  # print(paste0(rating_outcome_s[j]))
  # print(paste0(outcome_s[k]))
  subset <-
    cates_with_negative$outcome == outcome_s[k]
  
  if (sum(subset) <= 2) {
    fitlist <- c(fitlist, list(NULL))
  } else{
    fitlist <- c(fitlist, list(
      rma.mv(
        yi = estimate,
        mods = ~ rating_estimate + valence + pid_3_pre,
        random = ~ 1 | id,
        V = block_diagonal_vcov[subset, subset], 
        data = cates_with_negative[subset, ]
      ) |> robust(cluster = date, clubSandwich = TRUE)
    )
    )
  }
  names(fitlist)[length(fitlist)] <- glue("all.negative", outcome_s[k], .sep = ".")
  
}

# Intended
for (k in seq_along(outcome_s)) {
  # print(paste0(pid_3_pre_s[i]))
  # print(paste0(rating_outcome_s[j]))
  # print(paste0(outcome_s[k]))
  subset <-
    cates_with_intended$outcome == outcome_s[k]
  
  if (sum(subset) <= 2) {
    fitlist <- c(fitlist, list(NULL))
  } else{
    fitlist <- c(fitlist, list(
      rma.mv(
        yi = estimate,
        mods = ~ rating_estimate + valence + pid_3_pre,
        random = ~ 1 | id,
        V = block_diagonal_vcov[subset, subset], 
        data = cates_with_intended[subset, ]
      ) |> robust(cluster = date, clubSandwich = TRUE)
    )
    )
  }
  names(fitlist)[length(fitlist)] <- glue("all.intended", outcome_s[k], .sep = ".")
  
}

fitlist <- 
  fitlist |>
  discard( ~ is.null(.x))

# Tables

# Make gof map
gof_map <- 
  tribble(
    ~raw,      ~clean,          ~fmt,  ~omit,
    "nobs",   "Observations",     0,  FALSE,
    "Q.test.p", "Q-test p",       3, FALSE
  )

# Positive
# Target Issues
modelsummary(list("Dem (PD)" = fitlist$`positive.Democrat.Pro Democrat.Issue`,
                  "Dem (PR)" = fitlist$`positive.Democrat.Pro Republican.Issue`,
                  "Ind (PD)" = fitlist$`positive.Independent.Pro Democrat.Issue`,
                  "Ind (PR)" = fitlist$`positive.Independent.Pro Republican.Issue`,
                  "Rep (PD)" = fitlist$`positive.Republican.Pro Democrat.Issue`,
                  "Rep (PR)" = fitlist$`positive.Republican.Pro Republican.Issue`,
                  "All" = fitlist$`positive.Issue`,
                  "All (2)" = fitlist$`all.positive.Issue`),
             coef_map = c("intercept" = "Intercept",
                          "rating_estimate" = "Positive Index",
                          "valencePro Republican" = "Pro Republican",
                          "pid_3_preIndependent" = "Independent",
                          "pid_3_preRepublican" = "Republican"),
             gof_map = gof_map,
             stars = c('*' = .05),
             title = "Predicting Target Issue (Stretch Coding) CATEs from Positive Emotions",
             output = here::here("output", "tables", "table_c5_issue_stretch_positive_emotion.tex"))

# Negative
# Target Issues
modelsummary(list("Dem (PD)" = fitlist$`negative.Democrat.Pro Democrat.Issue`,
                  "Dem (PR)" = fitlist$`negative.Democrat.Pro Republican.Issue`,
                  "Ind (PD)" = fitlist$`negative.Independent.Pro Democrat.Issue`,
                  "Ind (PR)" = fitlist$`negative.Independent.Pro Republican.Issue`,
                  "Rep (PD)" = fitlist$`negative.Republican.Pro Democrat.Issue`,
                  "Rep (PR)" = fitlist$`negative.Republican.Pro Republican.Issue`,
                  "All" = fitlist$`negative.Issue`,
                  "All (2)" = fitlist$`all.negative.Issue`),
             coef_map = c("intercept" = "Intercept",
                          "rating_estimate" = "Negative Index",
                          "valencePro Republican" = "Pro Republican",
                          "pid_3_preIndependent" = "Independent",
                          "pid_3_preRepublican" = "Republican"),
             gof_map = gof_map,
             stars = c('*' = .05),
             title = "Predicting Target Issue (Stretch Coding) CATEs from Negative Emotions",
             output = here::here("output", "tables", "table_c10_issue_stretch_negative_emotion.tex"))

# Intended
# Target Issues
modelsummary(list("Dem (PD)" = fitlist$`intended.Democrat.Pro Democrat.Issue`,
                  "Dem (PR)" = fitlist$`intended.Democrat.Pro Republican.Issue`,
                  "Ind (PD)" = fitlist$`intended.Independent.Pro Democrat.Issue`,
                  "Ind (PR)" = fitlist$`intended.Independent.Pro Republican.Issue`,
                  "Rep (PD)" = fitlist$`intended.Republican.Pro Democrat.Issue`,
                  "Rep (PR)" = fitlist$`intended.Republican.Pro Republican.Issue`,
                  "All" = fitlist$`intended.Issue`,
                  "All (2)" = fitlist$`all.intended.Issue`),
             coef_map = c("intercept" = "Intercept",
                          "rating_estimate" = "Intended Emotion Index",
                          "valencePro Republican" = "Pro Republican",
                          "pid_3_preIndependent" = "Independent",
                          "pid_3_preRepublican" = "Republican"),
             gof_map = gof_map,
             stars = c('*' = .05),
             title = "Predicting Target Issue (Stretch Coding) CATEs from Intended Emotions" ,
             output = here::here("output", "tables", "table_c15_issue_stretch_intended_emotion.tex"))

