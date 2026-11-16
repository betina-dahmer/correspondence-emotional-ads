# 01_explore.R
# Getting to know the data 

source(here::here("code", "helpers.R"))
exps_clean <- read_rds(here::here("data", "exps_clean.rds"))

dim(exps_clean)    # 28,679 people, 87 variables
names(exps_clean)
# No ideology, religion, age, race, or education in the deposited data.
# The only political trait is party ID (pid_7_pre and pid_3_pre).

# How was pid_3 built from pid_7?
table(exps_clean$pid_3_pre, exps_clean$pid_7_pre)

# - Independents are only pid_7 = 4. So inside the independent subgroup the pid_7_pre control is constant and gets dropped (that's the "rank deficient" warnings in estimate_all_cates.R).

# - Leaners (3 and 5) are counted as partisans. Standard choice, but still a choice worth mentioning.

# - Strong Republicans (7): 4,821 people, the largest Republican group. Enough for my extension, but spread over 29 waves, so each wave-level estimate will be noisy.