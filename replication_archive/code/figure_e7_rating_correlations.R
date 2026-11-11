# Murphy, Coppock, Hill, and Vavreck
# "Do Emotional Ads Persuade? Evidence from Real-Time Campaign Advertising Experiments"
# Political Communication
#
# figure_e7_rating_correlations.R: Produces Figure E.7, correlations among rating index components.

source(here::here("code", "helpers.R"))

exps_clean <- read_rds(here::here("data", "exps_clean.rds"))

# Ratings
g_pairs_ratings <-
  exps_clean |>
  select(quality, fair, truth, mem, run, index) |>
  GGally::ggpairs(lower = list(continuous = GGally::wrap(
    "points",
    alpha = 0.05,
    stroke = 0,
    position =
      position_jitter(height = .1, width = .1)
  ))) +
  theme_bw() +
  theme(strip.background = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 8))


ggsave(filename = here::here("output", "figures", "figure_e7_rating_correlations.png"), g_pairs_ratings, width = 6.5, height = 6.5)
ggsave(filename = here::here("output", "figures", "figure_e7_rating_correlations.pdf"), g_pairs_ratings, width = 6.5, height = 6.5)
