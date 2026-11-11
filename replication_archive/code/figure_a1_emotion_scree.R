# Murphy, Coppock, Hill, and Vavreck
# "Do Emotional Ads Persuade? Evidence from Real-Time Campaign Advertising Experiments"
# Political Communication
#
# figure_a1_emotion_scree.R: Produces Figure A.1, scree plot for emotion index factor analysis.

source(here::here("code", "helpers.R"))

# Load Data
exps_clean <- read_rds(here::here("data", "exps_clean.rds"))
exps_clean <- exps_clean |>
  filter(ad_id != 3) # Do not need ratings of control ad

# Emotions
# emotions matrix for scale validation
emotions_matrix <-
  exps_clean |> 
  select(ad_id, happy, hopeful, worried, angry)

# Dropping NA values and removing ad_id varibale to perform EFA
only_emotions <- 
  emotions_matrix |>
  select(-ad_id) |>
  drop_na()

# Now calculating eigen values and plotting scree plot
emotions_ev <- eigen(cor(only_emotions))

emotions_scree_data <- 
  tibble(
    factors = 1:4,
    eigen_values = emotions_ev$values
  )

# Rating index scree plot
emotions_index_scree_plot <- 
  emotions_scree_data |>
  ggplot(aes(x = factors, y = eigen_values)) + 
  geom_point() + 
  geom_line() + 
  geom_hline(yintercept = 1, linetype = "dashed") + 
  labs(
    x = "Emotions Factors",
    y = "Eigenvalues of Factors"
  ) + 
  theme_bw()


ggsave(here::here("output", "figures", "figure_a1_emotion_scree.pdf"), 
      emotions_index_scree_plot, width = 6.5, height = 4)
ggsave(here::here("output", "figures", "figure_a1_emotion_scree.png"), 
      emotions_index_scree_plot, width = 6.5, height = 4, dpi = 300)

# Persist the scale statistics the appendix and main text report, so
# in_text_claims.R reads a pipeline output rather than recomputing them.
readr::write_csv(
  tibble(
    scale = c("positive", "negative"),
    items = c("happy, hopeful", "worried, angry"),
    cronbach_alpha = c(
      psych::alpha(only_emotions[, c("happy", "hopeful")])$total$raw_alpha,
      psych::alpha(only_emotions[, c("worried", "angry")])$total$raw_alpha
    ),
    n = nrow(only_emotions)
  ),
  here::here("output", "tables", "emotion_scale_reliability.csv")
)

readr::write_csv(
  emotions_scree_data |> mutate(above_one = eigen_values > 1),
  here::here("output", "tables", "emotion_scree_eigenvalues.csv")
)
