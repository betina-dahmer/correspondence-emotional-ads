# Murphy, Coppock, Hill, and Vavreck
# "Do Emotional Ads Persuade? Evidence from Real-Time Campaign Advertising Experiments"
# Political Communication
#
# figure_e8_rating_scree.R: Produces Figure E.8, scree plot for rating index factor analysis.

source(here::here("code", "helpers.R"))

# Load Data
exps_clean <- read_rds(here::here("data", "exps_clean.rds"))

# Ratings matrix for scale validation
ratings_matrix <-
  exps_clean |> 
  select(ad_id, quality, mem, run, fair, truth)

# Dropping NA values and removing ad_id varibale to perform EFA
only_ratings <- 
  ratings_matrix |>
  select(-ad_id) |>
  drop_na()

# Now calculating eigen values and plotting scree plot
ev <- eigen(cor(only_ratings))

scree_data <- 
  tibble(
    factors = 1:5,
    eigen_values = ev$values
  )

# Rating index scree plot
rating_index_scree_plot <- 
  scree_data |>
  ggplot(aes(x = factors, y = eigen_values)) + 
  geom_point() + 
  geom_line() + 
  geom_hline(yintercept = 1, linetype = "dashed") + 
  labs(
    x = "Rating Factors",
    y = "Eigenvalues of Factors"
  ) + 
  theme_bw()


ggsave(here::here("output", "figures", "figure_e8_rating_scree.pdf"), 
       rating_index_scree_plot, width = 6.5, height = 4)
ggsave(here::here("output", "figures", "figure_e8_rating_scree.png"), 
       rating_index_scree_plot, width = 6.5, height = 4, dpi = 300)

# Persist the rating scale's reliability, which the appendix reports as 0.91 and
# which no script previously computed.
readr::write_csv(
  tibble(
    scale = "rating index",
    items = "quality, mem, run, fair, truth",
    cronbach_alpha = psych::alpha(only_ratings)$total$raw_alpha,
    n = nrow(only_ratings)
  ),
  here::here("output", "tables", "rating_scale_reliability.csv")
)

readr::write_csv(
  scree_data |> mutate(above_one = eigen_values > 1),
  here::here("output", "tables", "rating_scree_eigenvalues.csv")
)
