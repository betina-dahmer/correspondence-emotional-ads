# Murphy, Coppock, Hill, and Vavreck
# "Do Emotional Ads Persuade? Evidence from Real-Time Campaign Advertising Experiments"
# Political Communication
#
# figure_e10_cates_from_ratings.R: Produces Figure E.10, relationship between rating index and CATEs.

source(here::here("code", "helpers.R"))

# Load Data
ratings <- read_rds(here::here("data", "estimates", "ratings_pid.rds"))
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

fitlist <- 
  fitlist |>
  discard( ~ is.null(.x))

meta_estimates <-
  fitlist |>
  discard( ~ is.null(.x)) |>
  map( ~ tidy(.x, conf.int = TRUE)) |>
  bind_rows(.id = "name") |>
  separate(name,
           into = c("index", "dv"),
           sep = "\\.") |>
  filter(term == "rating_estimate") |>
  mutate(entry = paste0(format(round(estimate, 3), nsmall = 3), " ", "(", format(round(std.error, 3), nsmall = 3), ")")) |>
  mutate(row_facet = paste0(dv))  |>
  filter(dv %in% c("Vote Choice", "Favorability"))

cates_with_ratings <-
  cates_with_ratings |>
  mutate(row_facet = paste0(outcome))  |>
  filter(outcome %in% c("Vote Choice", "Favorability")) |>
  mutate(pid_valence = factor(
    paste(pid_3_pre, valence, sep = ", "),
    levels = c(
      "Democrat, Pro Democrat",
      "Independent, Pro Democrat",
      "Republican, Pro Democrat",
      "Democrat, Pro Republican",
      "Independent, Pro Republican",
      "Republican, Pro Republican"
    )
  ))

# All meta predictions
label_df <-
  cates_with_ratings |> ungroup() |> select(outcome) |> rename(dv = outcome) |> distinct()

# Meta preds
newmods <- seq(0, 1, length.out = 1000)

preds <-
  fitlist |>
  map( ~ predict(., newmods = newmods)) |> 
  map(as_tibble) |> 
  map_df(~mutate(., rating_estimate = newmods), .id = "split") |> 
  separate(split, c("index", "dv"), sep = "\\.") |> 
  mutate(estimate = pred)  |>
  mutate(row_facet = paste0(dv))

preds <-
  left_join(preds, label_df)  |>
  filter(dv %in% c("Vote Choice", "Favorability"))

g_cate_favorability_and_vote_choice <-
  cates_with_ratings |>
  ggplot(aes(rating_estimate, estimate)) +
  geom_point(aes(color = pid_valence, shape = pid_valence)) +
  geom_linerange(aes(ymin = conf.low, ymax = conf.high, color = pid_valence), alpha = 0.2) +
  geom_ribbon(data = preds,
              aes(ymin = ci.lb, ymax = ci.ub),
              alpha = 0.15) +
  geom_line(data = preds, alpha = 0.75) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = gray(0.5)) +
  facet_wrap( ~ row_facet, ncol = 2) +
  geom_text(
    data = meta_estimates,
    aes(label = entry),
    x = .15,
    y = .22,
    hjust = 0
  ) +
  coord_cartesian(ylim = c(-.4, .4), xlim = c(.15, .85)) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.key.size = unit(0.4, "cm"),
    legend.text = element_text(size = 7),
    strip.background = element_blank()
  ) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE)) +
  scale_color_manual(
    name = NULL,
    values = c(
      "Democrat, Pro Democrat"      = "#1F3A93",
      "Independent, Pro Democrat"   = "#7C2C55",
      "Republican, Pro Democrat"    = "#D91E18",
      "Democrat, Pro Republican"    = "#1F3A93",
      "Independent, Pro Republican" = "#7C2C55",
      "Republican, Pro Republican"  = "#D91E18"
    )
  ) +
  scale_shape_manual(
    name = NULL,
    values = c(
      "Democrat, Pro Democrat"      = 16,
      "Independent, Pro Democrat"   = 16,
      "Republican, Pro Democrat"    = 16,
      "Democrat, Pro Republican"    = 17,
      "Independent, Pro Republican" = 17,
      "Republican, Pro Republican"  = 17
    )
  ) +
  labs(
    x = "Average Rating Index Score",
    y = "Conditional Average Treatment Effect"
  )


ggsave(here::here("output", "figures", "figure_e10_cates_from_ratings.pdf"), g_cate_favorability_and_vote_choice,
       width = 6.5, height = 6.5)
ggsave(here::here("output", "figures", "figure_e10_cates_from_ratings.png"), g_cate_favorability_and_vote_choice,
       width = 6.5, height = 6.5, dpi = 300)


# Persist the annotated meta-analytic slopes so in_text_claims.R can read the
# same numbers the figure prints, rather than refitting them.
readr::write_csv(
  dplyr::bind_rows(
    "rating_index" = meta_estimates,
    .id = "index"
  ) |>
    dplyr::select(index, dv, estimate, std.error, dplyr::any_of(c("p.value", "conf.low", "conf.high"))),
  here::here("output", "tables", "figure_e10_meta_estimates.csv")
)
