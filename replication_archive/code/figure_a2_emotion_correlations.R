# Murphy, Coppock, Hill, and Vavreck
# "Do Emotional Ads Persuade? Evidence from Real-Time Campaign Advertising Experiments"
# Political Communication
#
# figure_a2_emotion_correlations.R: Produces Figure A.2, correlations among emotion index components.

source(here::here("code", "helpers.R"))

# Load Data
exps_clean <- read_rds(here::here("data", "exps_clean.rds"))
exps_clean <- exps_clean |>
  filter(ad_id != 3) # Do not need ratings of control ad

g_pairs_emotions <-
  exps_clean |>
  select(angry, worried, happy, hopeful, negative_index, positive_index) |>
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


ggsave(filename = here::here("output", "figures", "figure_a2_emotion_correlations.png"), g_pairs_emotions, width = 6.5, height = 6.5)
ggsave(filename = here::here("output", "figures", "figure_a2_emotion_correlations.pdf"), g_pairs_emotions, width = 6.5, height = 6.5)

