# Murphy, Coppock, Hill, and Vavreck
# "Do Emotional Ads Persuade? Evidence from Real-Time Campaign Advertising Experiments"
# Political Communication
#
# figure_1_emotion_by_partisanship.R: Produces Figure 1, distribution of emotional responses to ads by partisanship.

source(here::here("code", "helpers.R"))

ratings <- read_rds(here::here("data", "estimates", "ratings_pid.rds"))
ad_df <- read_csv(here::here("data", "ad_df.csv"))

# Manipulate Data
ratings_new <-
  ratings |>
  left_join(ad_df, by = c("ad_id")) |>
  select(
    ad_id,
    ad_valence,
    date,
    pid_3_pre,
    rating_estimate,
    rating_std.error,
    rating_outcome,
    rating_conf.low,
    rating_conf.high
  ) |>
  mutate(valence = if_else(
    ad_valence %in% c("Pro Trump", "Anti Clinton", "Pro Kasich", "Pro Cruz"),
    "Pro Republican",
    "Pro Democrat"
  ))

gg_df <-
  ratings_new |>
  filter(rating_outcome %in% c("index", "negative_index", "positive_index")) |>
  pivot_wider(
    names_from = rating_outcome,
    values_from = c(
      rating_estimate,
      rating_std.error,
      rating_conf.high,
      rating_conf.low
    )
  ) |> 
  mutate(pid_3_pre = case_match(pid_3_pre, 
                                "Democrat" ~ "Democrats",
                                "Independent" ~ "Independents",
                                "Republican" ~ "Republicans")) |>
  left_join(ad_df) |> 
  mutate(ad_title_valence = paste0(ad_title, "-", ad_valence))

figure_1_emotion_by_partisanship <-
  ggplot(
    gg_df,
    aes(
      rating_estimate_positive_index,
      rating_estimate_negative_index,
      color = valence,
      shape = valence
    )
  ) +
  geom_point() +
  # Democrats
  geom_text_repel(data = gg_df |>
                    filter(ad_title == "Quotes" & pid_3_pre == "Democrats" & date == "2016-04-05"), 
                  aes(label = ad_title_valence),
                  size = 2.5,
                  
                  segment.size = 0.25,
                  min.segment.length = 0,
                  nudge_x = -0.10, nudge_y = 0.10
  ) +
  geom_text_repel(data = gg_df |>
                    filter(ad_title == "Moments" & pid_3_pre == "Democrats"), 
                  aes(label = ad_title_valence),
                  size = 2.5,
                  
                  segment.size = 0.25,
                  min.segment.length = 0,
                  nudge_x = 0.2, nudge_y = -0.05
  ) +
  geom_text_repel(data = gg_df |>
                    filter(ad_title == "Outsource" & pid_3_pre == "Democrats"), 
                  aes(label = ad_title_valence),
                  size = 2.5,
                  
                  segment.size = 0.25,
                  min.segment.length = 0,
                  nudge_x = -0.1, nudge_y = -0.02
  ) +
  geom_text_repel(data = gg_df |>
                    filter(ad_title == "America's Bullies" & pid_3_pre == "Democrats"), 
                  aes(label = ad_title_valence),
                  size = 2.5,
                  
                  segment.size = 0.25,
                  min.segment.length = 0,
                  nudge_x = 0.15, nudge_y = 0.1
  ) +
  # Republicans
  geom_text_repel(data = gg_df |>
                    filter(ad_title == "Consumer Benefit" & pid_3_pre == "Republicans"), 
                  aes(label = ad_title_valence),
                  size = 2.5,
                  
                  segment.size = 0.25,
                  min.segment.length = 0,
                  nudge_x = 0.3, nudge_y = -0.05
  ) +
  geom_text_repel(data = gg_df |>
                    filter(ad_title == "Someplace" & pid_3_pre == "Republicans"), 
                  aes(label = ad_title_valence),
                  size = 2.5,
                  
                  segment.size = 0.25,
                  min.segment.length = 0,
                  nudge_x = -0.01, nudge_y = -0.05
  ) +
  geom_text_repel(data = gg_df |>
                    filter(ad_title == "Outsource" & pid_3_pre == "Republicans"), 
                  aes(label = ad_title_valence),
                  size = 2.5,
                  
                  segment.size = 0.25,
                  min.segment.length = 0,
                  nudge_x = 0.01, nudge_y = 0.15
  ) +
  geom_text_repel(data = gg_df |>
                    filter(ad_title == "Change" & pid_3_pre == "Republicans"), 
                  aes(label = ad_title_valence),
                  size = 2.5,
                  
                  segment.size = 0.25,
                  min.segment.length = 0,
                  nudge_x = 0.2, nudge_y = 0.05
  ) +
  # Independents
  geom_text_repel(data = gg_df |>
                    filter(ad_title == "Unfit to Serve" & pid_3_pre == "Independents"), 
                  aes(label = ad_title_valence),
                  size = 2.5,
                  
                  segment.size = 0.25,
                  min.segment.length = 0,
                  nudge_x = 0.01, nudge_y = 0.15
  ) +
  geom_text_repel(data = gg_df |>
                    filter(ad_title == "Depend" & pid_3_pre == "Independents"), 
                  aes(label = ad_title_valence),
                  size = 2.5,
                  
                  segment.size = 0.25,
                  min.segment.length = 0,
                  nudge_x = -0.01, nudge_y = -0.05
  ) +
  geom_text_repel(data = gg_df |>
                    filter(ad_title == "Role Model" & pid_3_pre == "Independents" & date == "2016-07-18"), 
                  aes(label = ad_title_valence),
                  size = 2.5,
                  
                  segment.size = 0.25,
                  min.segment.length = 0,
                  nudge_x = 0.5, nudge_y = 0.1
  ) +
  geom_text_repel(data = gg_df |>
                    filter(ad_title == "Consumer Benefit" & pid_3_pre == "Independents"), 
                  aes(label = ad_title_valence),
                  size = 2.5,
                  
                  segment.size = 0.25,
                  min.segment.length = 0,
                  nudge_x = 0.2, nudge_y = -0.2
  ) +
  theme_bw() +
  theme(
    legend.position = "none",
    strip.background = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 8)
  ) +
  scale_color_manual(values = bpr_colors[c(1, 3)]) +
  scale_x_continuous(breaks = seq(0, 1, length.out = 5), limits = c(0, 1)) +
  scale_y_continuous(breaks = seq(0, 1, length.out = 5), limits = c(0, 1)) +
  #coord_equal() +
  facet_wrap( ~ pid_3_pre) +
  labs(x = "Positive Emotion Index", y = "Negative Emotion Index")


ggsave(here::here("output", "figures", "figure_1_emotion_by_partisanship.pdf"),
       figure_1_emotion_by_partisanship,
       width = 6.5,
       height = 3.0)
ggsave(here::here("output", "figures", "figure_1_emotion_by_partisanship.png"),
       figure_1_emotion_by_partisanship,
       width = 6.5,
       height = 3.0, dpi = 300)

