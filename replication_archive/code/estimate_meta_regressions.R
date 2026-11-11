# Murphy, Coppock, Hill, and Vavreck
# "Do Emotional Ads Persuade? Evidence from Real-Time Campaign Advertising Experiments"
# Political Communication
#
# estimate_meta_regressions.R: Estimates meta-regressions of CATEs on emotion and rating indices. Produces Tables C1-C4, C6-C9, C11-C14, E17-E18.

source(here::here("code", "helpers.R"))

# Load Data
ratings <- read_rds(here::here("data", "estimates", "ratings_pid.rds"))
ad_df_edited <- read_csv(here::here("data", "ad_df_emotions.csv"))
cates <- read_rds(here::here("data", "estimates", "all_cates.rds"))
block_diagonal_vcov <- read_rds(here::here("data", "estimates", "block_diagonal_vcov.rds"))

# Ratings
rating_index_ratings <-
  ratings |>
  filter(rating_outcome == "index")

cates_with_ratings <- 
  cates |>
  left_join(rating_index_ratings) |>
  mutate(
    outcome = case_when(str_detect(outcome, "favor") ~ "Favorability",
                        str_detect(outcome, "general_vote") ~ "Vote Choice",
                        str_detect(outcome, "will_vote") ~ "Turnout",
                        str_detect(outcome, "Base") ~ "Issue"),
    valence = if_else(ad_valence %in% c("Pro Trump","Anti Clinton", "Pro Kasich", "Pro Cruz"), "Pro Republican", "Pro Democrat")
  )

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
    valence = if_else(ad_valence %in% c("Pro Trump", "Anti Clinton", "Pro Kasich", "Pro Cruz"), "Pro Republican", "Pro Democrat")
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
    valence = if_else(ad_valence %in% c("Pro Trump", "Anti Clinton", "Pro Kasich", "Pro Cruz"), "Pro Republican", "Pro Democrat")
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

pid_3_pre_s <- unique(cates_with_ratings$pid_3_pre)
valence_s <- unique(cates_with_ratings$valence)
outcome_s <- unique(cates_with_ratings$outcome)

fitlist <- list()

# Ratings

cates_with_ratings |> 
  group_by(pid_3_pre, valence, outcome) |> 
  summarise(n = n())

cates_with_ratings <- 
  cates_with_ratings |> 
  mutate(id = 1:n())

# All

for (k in seq_along(outcome_s)) {
  # print(paste0(pid_3_pre_s[i]))
  # print(paste0(rating_outcome_s[j]))
  # print(paste0(outcome_s[k]))
  subset <-
    cates_with_ratings$outcome == outcome_s[k]
  
  if (sum(subset) <= 2) {
    fitlist <- c(fitlist, list(NULL))
  } else{
    fitlist <- c(fitlist, list(
      rma.mv(
        yi = estimate,
        mods = ~ rating_estimate,
        random = ~ 1 | id,
        V = block_diagonal_vcov[subset, subset], 
        data = cates_with_ratings[subset, ]
      ) |> robust(cluster = date, clubSandwich = TRUE)
    )
    )
  }
  names(fitlist)[length(fitlist)] <- glue("ratings", outcome_s[k], .sep = ".")
  
}

# CATEs
for(i in seq_along(pid_3_pre_s)) {
  for (j in seq_along(valence_s)) {
    for (k in seq_along(outcome_s)) {
      # print(paste0(pid_3_pre_s[i]))
      # print(paste0(rating_outcome_s[j]))
      # print(paste0(outcome_s[k]))
      subset <-
        cates_with_ratings$pid_3_pre == pid_3_pre_s[i] &
        cates_with_ratings$valence == valence_s[j] &
        cates_with_ratings$outcome == outcome_s[k]
      
      if (sum(subset) <= 2) {
        fitlist <- c(fitlist, list(NULL))
      } else{
        fitlist <- c(fitlist, list(
          rma.mv(
            yi = estimate,
            mods = ~ rating_estimate,
            random = ~ 1 | id,
            V = block_diagonal_vcov[subset, subset], 
            data = cates_with_ratings[subset, ]
          ) |> robust(cluster = date, clubSandwich = TRUE)
        )
        )
      }
      names(fitlist)[length(fitlist)] <- glue("ratings", pid_3_pre_s[i], valence_s[j], outcome_s[k], .sep = ".")
      
    }
  }
}

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

# Ratings
for (k in seq_along(outcome_s)) {
  # print(paste0(pid_3_pre_s[i]))
  # print(paste0(rating_outcome_s[j]))
  # print(paste0(outcome_s[k]))
  subset <-
    cates_with_ratings$outcome == outcome_s[k]
  
  if (sum(subset) <= 2) {
    fitlist <- c(fitlist, list(NULL))
  } else{
    fitlist <- c(fitlist, list(
      rma.mv(
        yi = estimate,
        mods = ~ rating_estimate + valence + pid_3_pre,
        random = ~ 1 | id,
        V = block_diagonal_vcov[subset, subset], 
        data = cates_with_ratings[subset, ]
      ) |> robust(cluster = date, clubSandwich = TRUE)
    )
    )
  }
  names(fitlist)[length(fitlist)] <- glue("all.ratings", outcome_s[k], .sep = ".")
  
}

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
# Favorability
modelsummary(list("Dem (PD)" = fitlist$`positive.Democrat.Pro Democrat.Favorability`,
                  "Dem (PR)" = fitlist$`positive.Democrat.Pro Republican.Favorability`,
                  "Ind (PD)" = fitlist$`positive.Independent.Pro Democrat.Favorability`,
                  "Ind (PR)" = fitlist$`positive.Independent.Pro Republican.Favorability`,
                  "Rep (PD)" = fitlist$`positive.Republican.Pro Democrat.Favorability`,
                  "Rep (PR)" = fitlist$`positive.Republican.Pro Republican.Favorability`,
                  "All" = fitlist$positive.Favorability,
                  "All (2)" = fitlist$all.positive.Favorability),
             coef_map = c("intercept" = "Intercept",
                          "rating_estimate" = "Positive Index",
                          "valencePro Republican" = "Pro Republican",
                          "pid_3_preIndependent" = "Independent",
                          "pid_3_preRepublican" = "Republican"),
             gof_map = gof_map,
             stars = c('*' = .05),
             title = "Predicting Favorability CATEs from Positive Emotions",
             output = here::here("output", "tables", "table_c1_favorability_positive_emotion.tex"))

# Vote Choice
modelsummary(list("Dem (PD)" = fitlist$`positive.Democrat.Pro Democrat.Vote Choice`,
                  "Dem (PR)" = fitlist$`positive.Democrat.Pro Republican.Vote Choice`,
                  "Ind (PD)" = fitlist$`positive.Independent.Pro Democrat.Vote Choice`,
                  "Ind (PR)" = fitlist$`positive.Independent.Pro Republican.Vote Choice`,
                  "Rep (PD)" = fitlist$`positive.Republican.Pro Democrat.Vote Choice`,
                  "Rep (PR)" = fitlist$`positive.Republican.Pro Republican.Vote Choice`,
                  "All" = fitlist$`positive.Vote Choice`,
                  "All (2)" = fitlist$`all.positive.Vote Choice`),
             coef_map = c("intercept" = "Intercept",
                          "rating_estimate" = "Positive Index",
                          "valencePro Republican" = "Pro Republican",
                          "pid_3_preIndependent" = "Independent",
                          "pid_3_preRepublican" = "Republican"),
             gof_map = gof_map,
             stars = c('*' = .05),
             title = "Predicting Vote Choice CATEs from Positive Emotions",
             output = here::here("output", "tables", "table_c2_vote_choice_positive_emotion.tex"))

# Turnout
modelsummary(list("Dem (PD)" = fitlist$`positive.Democrat.Pro Democrat.Turnout`,
                  "Dem (PR)" = fitlist$`positive.Democrat.Pro Republican.Turnout`,
                  "Ind (PD)" = fitlist$`positive.Independent.Pro Democrat.Turnout`,
                  "Ind (PR)" = fitlist$`positive.Independent.Pro Republican.Turnout`,
                  "Rep (PD)" = fitlist$`positive.Republican.Pro Democrat.Turnout`,
                  "Rep (PR)" = fitlist$`positive.Republican.Pro Republican.Turnout`,
                  "All" = fitlist$`positive.Turnout`,
                  "All (2)" = fitlist$`all.positive.Turnout`),
             coef_map = c("intercept" = "Intercept",
                          "rating_estimate" = "Positive Index",
                          "valencePro Republican" = "Pro Republican",
                          "pid_3_preIndependent" = "Independent",
                          "pid_3_preRepublican" = "Republican"),
             gof_map = gof_map,
             stars = c('*' = .05),
             title = "Predicting Turnout Intentions CATEs from Positive Emotions",
             output = here::here("output", "tables", "table_c3_turnout_positive_emotion.tex"))

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
             title = "Predicting Target Issue CATEs from Positive Emotions",
             output = here::here("output", "tables", "table_c4_issue_positive_emotion.tex"))

# Negative
# Favorability
modelsummary(list("Dem (PD)" = fitlist$`negative.Democrat.Pro Democrat.Favorability`,
                  "Dem (PR)" = fitlist$`negative.Democrat.Pro Republican.Favorability`,
                  "Ind (PD)" = fitlist$`negative.Independent.Pro Democrat.Favorability`,
                  "Ind (PR)" = fitlist$`negative.Independent.Pro Republican.Favorability`,
                  "Rep (PD)" = fitlist$`negative.Republican.Pro Democrat.Favorability`,
                  "Rep (PR)" = fitlist$`negative.Republican.Pro Republican.Favorability`,
                  "All" = fitlist$negative.Favorability,
                  "All (2)" = fitlist$all.negative.Favorability),
             coef_map = c("intercept" = "Intercept",
                          "rating_estimate" = "Negative Index",
                          "valencePro Republican" = "Pro Republican",
                          "pid_3_preIndependent" = "Independent",
                          "pid_3_preRepublican" = "Republican"),
             gof_map = gof_map,
             stars = c('*' = .05),
             title = "Predicting Favorability CATEs from Negative Emotions",
             output = here::here("output", "tables", "table_c6_favorability_negative_emotion.tex"))

# Vote Choice
modelsummary(list("Dem (PD)" = fitlist$`negative.Democrat.Pro Democrat.Vote Choice`,
                  "Dem (PR)" = fitlist$`negative.Democrat.Pro Republican.Vote Choice`,
                  "Ind (PD)" = fitlist$`negative.Independent.Pro Democrat.Vote Choice`,
                  "Ind (PR)" = fitlist$`negative.Independent.Pro Republican.Vote Choice`,
                  "Rep (PD)" = fitlist$`negative.Republican.Pro Democrat.Vote Choice`,
                  "Rep (PR)" = fitlist$`negative.Republican.Pro Republican.Vote Choice`,
                  "All" = fitlist$`negative.Vote Choice`,
                  "All (2)" = fitlist$`all.negative.Vote Choice`),
             coef_map = c("intercept" = "Intercept",
                          "rating_estimate" = "Negative Index",
                          "valencePro Republican" = "Pro Republican",
                          "pid_3_preIndependent" = "Independent",
                          "pid_3_preRepublican" = "Republican"),
             gof_map = gof_map,
             stars = c('*' = .05),
             title = "Predicting Vote Choice CATEs from Negative Emotions",
             output = here::here("output", "tables", "table_c7_vote_choice_negative_emotion.tex"))

# Turnout
modelsummary(list("Dem (PD)" = fitlist$`negative.Democrat.Pro Democrat.Turnout`,
                  "Dem (PR)" = fitlist$`negative.Democrat.Pro Republican.Turnout`,
                  "Ind (PD)" = fitlist$`negative.Independent.Pro Democrat.Turnout`,
                  "Ind (PR)" = fitlist$`negative.Independent.Pro Republican.Turnout`,
                  "Rep (PD)" = fitlist$`negative.Republican.Pro Democrat.Turnout`,
                  "Rep (PR)" = fitlist$`negative.Republican.Pro Republican.Turnout`,
                  "All" = fitlist$`negative.Turnout`,
                  "All (2)" = fitlist$`all.negative.Turnout`),
             coef_map = c("intercept" = "Intercept",
                          "rating_estimate" = "Negative Index",
                          "valencePro Republican" = "Pro Republican",
                          "pid_3_preIndependent" = "Independent",
                          "pid_3_preRepublican" = "Republican"),
             gof_map = gof_map,
             stars = c('*' = .05),
             title = "Predicting Turnout Intentions CATEs from Negative Emotions",
             output = here::here("output", "tables", "table_c8_turnout_negative_emotion.tex"))

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
             title = "Predicting Target Issue CATEs from Negative Emotions",
             output = here::here("output", "tables", "table_c9_issue_negative_emotion.tex"))

# Intended
# Favorability
modelsummary(list("Dem (PD)" = fitlist$`intended.Democrat.Pro Democrat.Favorability`,
                  "Dem (PR)" = fitlist$`intended.Democrat.Pro Republican.Favorability`,
                  "Ind (PD)" = fitlist$`intended.Independent.Pro Democrat.Favorability`,
                  "Ind (PR)" = fitlist$`intended.Independent.Pro Republican.Favorability`,
                  "Rep (PD)" = fitlist$`intended.Republican.Pro Democrat.Favorability`,
                  "Rep (PR)" = fitlist$`intended.Republican.Pro Republican.Favorability`,
                  "All" = fitlist$intended.Favorability,
                  "All (2)" = fitlist$all.intended.Favorability),
             coef_map = c("intercept" = "Intercept",
                          "rating_estimate" = "Intended Emotion Index",
                          "valencePro Republican" = "Pro Republican",
                          "pid_3_preIndependent" = "Independent",
                          "pid_3_preRepublican" = "Republican"),
             gof_map = gof_map,
             stars = c('*' = .05),
             title = "Predicting Favorability CATEs from Intended Emotions",
             output = here::here("output", "tables", "table_c11_favorability_intended_emotion.tex"))

# Vote Choice
modelsummary(list("Dem (PD)" = fitlist$`intended.Democrat.Pro Democrat.Vote Choice`,
                  "Dem (PR)" = fitlist$`intended.Democrat.Pro Republican.Vote Choice`,
                  "Ind (PD)" = fitlist$`intended.Independent.Pro Democrat.Vote Choice`,
                  "Ind (PR)" = fitlist$`intended.Independent.Pro Republican.Vote Choice`,
                  "Rep (PD)" = fitlist$`intended.Republican.Pro Democrat.Vote Choice`,
                  "Rep (PR)" = fitlist$`intended.Republican.Pro Republican.Vote Choice`,
                  "All" = fitlist$`intended.Vote Choice`,
                  "All (2)" = fitlist$`all.intended.Vote Choice`),
             coef_map = c("intercept" = "Intercept",
                          "rating_estimate" = "Intended Emotion Index",
                          "valencePro Republican" = "Pro Republican",
                          "pid_3_preIndependent" = "Independent",
                          "pid_3_preRepublican" = "Republican"),
             gof_map = gof_map,
             stars = c('*' = .05),
             title = "Predicting Vote Choice CATEs from Intended Emotions",
             output = here::here("output", "tables", "table_c12_vote_choice_intended_emotion.tex"))

# Turnout
modelsummary(list("Dem (PD)" = fitlist$`intended.Democrat.Pro Democrat.Turnout`,
                  "Dem (PR)" = fitlist$`intended.Democrat.Pro Republican.Turnout`,
                  "Ind (PD)" = fitlist$`intended.Independent.Pro Democrat.Turnout`,
                  "Ind (PR)" = fitlist$`intended.Independent.Pro Republican.Turnout`,
                  "Rep (PD)" = fitlist$`intended.Republican.Pro Democrat.Turnout`,
                  "Rep (PR)" = fitlist$`intended.Republican.Pro Republican.Turnout`,
                  "All" = fitlist$`intended.Turnout`,
                  "All (2)" = fitlist$`all.intended.Turnout`),
             coef_map = c("intercept" = "Intercept",
                          "rating_estimate" = "Intended Emotion Index",
                          "valencePro Republican" = "Pro Republican",
                          "pid_3_preIndependent" = "Independent",
                          "pid_3_preRepublican" = "Republican"),
             gof_map = gof_map,
             stars = c('*' = .05),
             title = "Predicting Turnout Intentions CATEs from Intended Emotions",
             output = here::here("output", "tables", "table_c13_turnout_intended_emotion.tex"))

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
             title = "Predicting Target Issue CATEs from Intended Emotions",
             output = here::here("output", "tables", "table_c14_issue_intended_emotion.tex"))

# Ratings
# Favorability
modelsummary(list("Dem (PD)" = fitlist$`ratings.Democrat.Pro Democrat.Favorability`,
                  "Dem (PR)" = fitlist$`ratings.Democrat.Pro Republican.Favorability`,
                  "Ind (PD)" = fitlist$`ratings.Independent.Pro Democrat.Favorability`,
                  "Ind (PR)" = fitlist$`ratings.Independent.Pro Republican.Favorability`,
                  "Rep (PD)" = fitlist$`ratings.Republican.Pro Democrat.Favorability`,
                  "Rep (PR)" = fitlist$`ratings.Republican.Pro Republican.Favorability`,
                  "All" = fitlist$ratings.Favorability,
                  "All (2)" = fitlist$all.ratings.Favorability),
             coef_map = c("intercept" = "Intercept",
                          "rating_estimate" = "Rating Index",
                          "valencePro Republican" = "Pro Republican",
                          "pid_3_preIndependent" = "Independent",
                          "pid_3_preRepublican" = "Republican"),
             gof_map = gof_map,
             stars = c('*' = .05),
             title = "Predicting Favorability CATEs from Ratings",
             output = here::here("output", "tables", "table_e17_favorability_ratings.tex"))

# Vote Choice
modelsummary(list("Dem (PD)" = fitlist$`ratings.Democrat.Pro Democrat.Vote Choice`,
                  "Dem (PR)" = fitlist$`ratings.Democrat.Pro Republican.Vote Choice`,
                  "Ind (PD)" = fitlist$`ratings.Independent.Pro Democrat.Vote Choice`,
                  "Ind (PR)" = fitlist$`ratings.Independent.Pro Republican.Vote Choice`,
                  "Rep (PD)" = fitlist$`ratings.Republican.Pro Democrat.Vote Choice`,
                  "Rep (PR)" = fitlist$`ratings.Republican.Pro Republican.Vote Choice`,
                  "All" = fitlist$`ratings.Vote Choice`,
                  "All (2)" = fitlist$`all.ratings.Vote Choice`),
             coef_map = c("intercept" = "Intercept",
                          "rating_estimate" = "Rating Index",
                          "valencePro Republican" = "Pro Republican",
                          "pid_3_preIndependent" = "Independent",
                          "pid_3_preRepublican" = "Republican"),
             gof_map = gof_map,
             stars = c('*' = .05),
             title = "Predicting Vote Choice CATEs from Ratings",
             output = here::here("output", "tables", "table_e18_vote_choice_ratings.tex"))

