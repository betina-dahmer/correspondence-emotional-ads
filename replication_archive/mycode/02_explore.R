# 02_explore.R
# Data exploration and partisan identification
#
# Examines the available variables, partisan identification coding, and subgroup sample sizes before the extension.

# Requires: 00_reproduce.R

# 1. Load data -----------------------------------------------------------

source(here::here("code", "helpers.R"))

exps_clean <- read_rds(
  here::here("data", "exps_clean.rds")
)

# 2. Inspect available variables -----------------------------------------

dim(exps_clean)
names(exps_clean)

# The deposited data retain the seven-point and three-category measures of pre-treatment partisan identification.

# 3. Examine partisan identification -------------------------------------

# Compare the seven-point and three-category party ID measures.
# Partisan leaners are classified with their preferred party.

table(
  exps_clean$pid_3_pre,
  exps_clean$pid_7_pre
)

# Examine the five partisan groups used in the extension.

exps_clean |>
  mutate(
    party_group = case_when(
      pid_7_pre == 1 ~ "Strong Democrat",
      pid_7_pre %in% 2:3 ~ "Weak/lean Democrat",
      pid_7_pre == 4 ~ "Independent",
      pid_7_pre %in% 5:6 ~ "Weak/lean Republican",
      pid_7_pre == 7 ~ "Strong Republican"
    )
  ) |>
  count(party_group)

# 4. Examine subgroup sample sizes ---------------------------------------

# Count strong Republicans in each ad-by-wave cell, including placebo observations.

exps_clean |>
  filter(pid_7_pre == 7) |>
  count(date, ad_title) |>
  summarise(
    cells = n(),
    min_n = min(n),
    median_n = median(n),
    max_n = max(n)
  )
