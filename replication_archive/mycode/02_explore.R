# 02_explore.R
# Getting to know the data

# Before I extend the analysis, I want to know what's actually in the data: which variables exist, how party ID was coded, and whether there are enough strong Republicans to look at them separately.

source(here::here("code", "helpers.R"))
exps_clean <- read_rds(here::here("data", "exps_clean.rds"))

# What's in the data ----------------------------------------------------
dim(exps_clean)    # 28,679 people, 87 variables
names(exps_clean)
# No ideology, religion, age, race, or education in the deposited data.
# The README says the authors removed 324 of the 387 original columns for privacy and kept only what their scripts use. The only political trait left is party ID (pid_7_pre and pid_3_pre), measured before people saw the ad.


# How was pid_3 built from pid_7? ---------------------------------------
# The data have two versions of party ID. pid_7_pre is the detailed 7-point scale (1 = strong Democrat ... 4 = pure independent ... 7 = strong Republican). pid_3_pre is the short 3-group version that the authors use to split the effects. pid_3 was built by grouping pid_7 points, and the open question is where the leaners (3 and 5) went: into "independent" or into the party they lean toward?

# Crossing the two variables shows exactly which pid_7 points ended up in each pid_3 group.

table(exps_clean$pid_3_pre, exps_clean$pid_7_pre)

# - Independents are only pid_7 = 4. So inside the independent subgroup the pid_7_pre control is constant and gets dropped (that's the "rank deficient" warnings in estimate_all_cates.R). Doesn't change anything, because random assignment already makes groups comparable.

# - Leaners (3 and 5) are counted as partisans. Standard choice, but still a choice worth mentioning.

# - Strong Republicans (7): 4,821 people, the largest Republican group. The authors never separate them from weak Republicans and leaners.


# How many strong Republicans per comparison? -----------------------------
# Each CATE compares people who saw one ad with people who saw the placebo, in one wave. So what matters for my extension is not the 4,821 total, but how many strong Republicans fall into each ad-by-wave cell. (The placebo shows up here as its own "ad".)

exps_clean |>
  filter(pid_7_pre == 7) |>
  count(date, ad_title) |>
  summarise(cells = n(),
            min_n = min(n),
            median_n = median(n),
            max_n = max(n))


# Result: 86 cells (58 ad-by-wave treatments + the placebo cell in each wave).

# Strong Republicans per cell: min 25, median 45.5, max 132.

# What this means: a typical comparison is ~45 strong Republicans who saw an ad vs. ~45 who saw the placebo. Each single estimate will be very noisy.

# The meta-regression pools all the estimates, so the extension is still doable, but it has to be framed as exploratory, and I need to discuss power: a null result could mean no effect OR not enough people to see one.