# Murphy, Coppock, Hill, and Vavreck
# "Do Emotional Ads Persuade? Evidence from Real-Time Campaign Advertising Experiments"
# Political Communication
#
# figure_c6_stretch_issue_turnout_intended_emotion.R: Produces Figure C.6, intended emotion vs. CATEs with stretch issue coding.

source(here::here("code", "helpers.R"))

ratings_pid <- read_rds(here::here("data", "estimates", "ratings_pid.rds"))
ad_df <- read_csv(here::here("data", "ad_df.csv"))
cates <- read_rds(here::here("data", "estimates", "all_cates_with_stretch.rds"))

ad_df_edited <- read_csv(here::here("data", "ad_df_emotions.csv"))

cates_block_diagonal_vcov <- read_rds(here::here("data", "estimates", "block_diagonal_vcov_with_stretch.rds"))

intended_ratings_pid <-
  ratings_pid |> 
  left_join(ad_df_edited) |>
  filter(rating_outcome %in% c("positive_index", "negative_index")) |>
  select(ad_id, pid_3_pre, date, rating_estimate, rating_outcome, ad_valence, emotion_valence) |>
  pivot_wider(names_from = rating_outcome, values_from = rating_estimate) |>
  mutate(intended_index = case_when(emotion_valence == "negative" ~ negative_index - positive_index,
                                    emotion_valence == "positive" ~ positive_index - negative_index,
                                    emotion_valence == "both" ~ positive_index + negative_index)) |>
  pivot_longer(cols = c(positive_index, negative_index, intended_index), names_to = "rating_outcome", values_to = "rating_estimate")

intended_pid <-
  intended_ratings_pid |>
  filter(rating_outcome == "intended_index") |>
  left_join(ad_df, by = "ad_id")

cates_with_ratings <-
  cates |>
  left_join(intended_pid, by = join_by(date, pid_3_pre, ad_id, ad_title, ad_sponsor)) |>
  mutate(dv = case_when(str_detect(outcome, "favor") ~ "Favorability",
                        str_detect(outcome, "general_vote") ~ "Vote Choice",
                        str_detect(outcome, "will_vote") ~ "Turnout",
                        str_detect(outcome, "Base") ~ "Issue"),
         Valence = if_else(ad_valence %in% c("Anti Trump", "Pro Clinton", "Pro Sanders"), "Pro Democrat ad", "Pro Republican ad"))

# Regression Loop
dv_s <- unique(cates_with_ratings$dv)
pid_3_pre_s <- unique(cates_with_ratings$pid_3_pre)

fitlist <- list()

cates_with_ratings |>
  group_by(dv) |>
  summarise(n = n())

cates_with_ratings <-
  cates_with_ratings  |> 
  mutate(id = 1:n())

for (k in seq_along(dv_s)) {
  
  subset <-
    cates_with_ratings$dv == dv_s[k] 
  
  if (sum(subset) <= 2) {
    fitlist <- c(fitlist, list(NULL))
  } else{
    fitlist <- c(fitlist, list(
      rma.mv(
        yi = estimate,
        mods = ~ rating_estimate,
        random = ~ 1 | id,
        V = cates_block_diagonal_vcov[subset, subset], 
        data = cates_with_ratings[subset, ]
      ) |> robust(cluster = date, clubSandwich = TRUE)
    )
    )
  }
  names(fitlist)[length(fitlist)] <- glue(dv_s[k], .sep = ".")
  
}

fitlist <- 
  fitlist |>
  discard( ~ is.null(.x))

intended_meta_estimates <-
  fitlist |>
  discard( ~ is.null(.x)) |>
  map( ~ tidy(.x, conf.int = TRUE)) |>
  bind_rows(.id = "name") |>
  separate(name,
           into = c("dv"),
           sep = "\\.") |>
  filter(term == "rating_estimate") |>
  mutate(entry = paste0(format(round(estimate, 3), nsmall = 3), " ", "(", format(round(std.error, 3), nsmall = 3), ")")) |>
  mutate(row_facet = paste0(dv)) |>
  mutate(rating_outcome = "intended_index")  |>
  filter(dv %in% c("Turnout", "Issue"))

intended_cates_with_ratings <- 
  cates_with_ratings |>
  mutate(row_facet = paste0(dv))

# All meta predictions
label_df <-
  cates_with_ratings |> ungroup() |> select(dv) |> distinct()

# Meta preds
newmods <- seq(-1.25, 1.25, length.out = 1000)

preds <-
  fitlist |>
  map( ~ predict(., newmods = newmods)) |> 
  map(as_tibble) |> 
  map_df(~mutate(., rating_estimate = newmods), .id = "split") |> 
  separate(split, c("dv"), sep = "\\.") |> 
  mutate(estimate = pred)  |>
  mutate(row_facet = paste0(dv)) |>
  mutate(rating_outcome = "intended_index")  |>
  filter(dv %in% c("Turnout", "Issue"))

intended_preds <-
  left_join(preds, label_df)  |>
  filter(dv %in% c("Turnout", "Issue"))

# All meta predictions
label_df <-
  cates_with_ratings |> ungroup() |> select(dv) |> distinct()

cates_with_ratings <- intended_cates_with_ratings  |>
  filter(dv %in% c("Turnout", "Issue")) |>
  mutate(pid_valence = factor(
    paste(pid_3_pre, Valence, sep = ", "),
    levels = c(
      "Democrat, Pro Democrat ad",
      "Independent, Pro Democrat ad",
      "Republican, Pro Democrat ad",
      "Democrat, Pro Republican ad",
      "Independent, Pro Republican ad",
      "Republican, Pro Republican ad"
    )
  ))

meta_estimates <- intended_meta_estimates  |>
  filter(dv %in% c("Turnout", "Issue"))

g_emotion_cate_favorability_and_vote_choice <-
  cates_with_ratings |>
  ggplot(aes(rating_estimate, estimate)) +
  geom_point(aes(color = pid_valence, shape = pid_valence)) +
  geom_linerange(aes(ymin = conf.low, ymax = conf.high, color = pid_valence), alpha = 0.2) +
  geom_ribbon(data = preds,
              aes(ymin = ci.lb, ymax = ci.ub),
              alpha = 0.15) +
  facet_grid(
    ~ row_facet,
    labeller = labeller(
      row_facet = c(
        "Issue"  = "Target Issue Support (Stretch Coding)",
        "Turnout" = "Turnout Intentions"
      )
    )
  ) +
  geom_line(data = preds, alpha = 0.75) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = gray(0.5)) +
  geom_text(
    data = meta_estimates,
    aes(label = entry),
    x = .87,
    y = .35
  ) +
  coord_cartesian(ylim = c(-.4, .4), xlim = c(-1.25, 1.25)) +
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
      "Democrat, Pro Democrat ad"      = "#1F3A93",
      "Independent, Pro Democrat ad"   = "#7C2C55",
      "Republican, Pro Democrat ad"    = "#D91E18",
      "Democrat, Pro Republican ad"    = "#1F3A93",
      "Independent, Pro Republican ad" = "#7C2C55",
      "Republican, Pro Republican ad"  = "#D91E18"
    )
  ) +
  scale_shape_manual(
    name = NULL,
    values = c(
      "Democrat, Pro Democrat ad"      = 16,
      "Independent, Pro Democrat ad"   = 16,
      "Republican, Pro Democrat ad"    = 16,
      "Democrat, Pro Republican ad"    = 17,
      "Independent, Pro Republican ad" = 17,
      "Republican, Pro Republican ad"  = 17
    )
  ) +
  labs(
    x = "Intended Emotion Index Score",
    y = "Conditional Average Treatment Effect"
  )


ggsave(here::here("output", "figures", "figure_c6_stretch_issue_turnout_intended_emotion.pdf"), g_emotion_cate_favorability_and_vote_choice,
       width = 6.5, height = 4.5)
ggsave(here::here("output", "figures", "figure_c6_stretch_issue_turnout_intended_emotion.png"), g_emotion_cate_favorability_and_vote_choice,
       width = 6.5, height = 4.5, dpi = 300)

