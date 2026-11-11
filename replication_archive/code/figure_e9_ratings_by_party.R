# Murphy, Coppock, Hill, and Vavreck
# "Do Emotional Ads Persuade? Evidence from Real-Time Campaign Advertising Experiments"
# Political Communication
#
# figure_e9_ratings_by_party.R: Produces Figure E.9, average ad ratings among Republicans and Democrats.

source(here::here("code", "helpers.R"))

ratings <- read_rds(here::here("data", "estimates", "ratings_pid.rds"))
ad_df <- read_csv(here::here("data", "ad_df.csv"))

ratings <- 
  ratings |>
  filter(rating_outcome %in% c("quality", "fair", "truth", "mem", "run", "index"))
  
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
  mutate(
    valence = if_else(
      ad_valence %in% c("Pro Trump", "Anti Clinton", "Pro Kasich", "Pro Cruz"),
      "Pro Republican",
      "Pro Democrat"
    )
  )

dem_rep <-
  ratings_new |>
  filter(pid_3_pre != "Independent") |>
  pivot_wider(
    names_from = pid_3_pre,
    values_from = c(
      rating_estimate,
      rating_std.error,
      rating_conf.high,
      rating_conf.low
    )
  ) |>
  rename(
    iv_estimate = rating_estimate_Democrat,
    iv_conf.low = rating_conf.low_Democrat,
    iv_conf.high = rating_conf.high_Democrat,
    dv_estimate = rating_estimate_Republican,
    dv_conf.low = rating_conf.low_Republican,
    dv_conf.high = rating_conf.high_Republican
  ) |>
  mutate(
    comparison = "Dem_Rep",
    rating_outcome_label =
      factor(
        rating_outcome,
        levels = c("quality", "fair", "truth", "mem", "run", "index"),
        labels = c(
          "Ad quality",
          "Ad fairness",
          "Ad truthfulness",
          "Ad memorability",
          "Recommend running ad",
          "Rating index"
        )
      )
  )

# Relationship between Democrats and Independents
label_df <- 
  tibble(
    iv_estimate = c(0.35, 0.65),
    dv_estimate = c(0.90, 0.15),
    rating_outcome_label = factor("Rating index"),
    valence = c("Pro Republican", "Pro Democrat"),
    label = c("Pro Republican ads", "Pro Democrat ads")
  )

g_dem_rep <- 
  ggplot(dem_rep, aes(x = iv_estimate, y = dv_estimate, color = valence)) + 
  geom_linerange(aes(xmin = iv_conf.low, xmax = iv_conf.high), linewidth = 0.25, alpha = 0.5) + 
  geom_linerange(aes(ymin = dv_conf.low, ymax = dv_conf.high), linewidth = 0.25, alpha = 0.5) +
  geom_point(alpha = 0.9, stroke = 0) + 
  geom_text(data = label_df, aes(label = label), size = 3) +
  facet_wrap(~rating_outcome_label, ncol = 3) + 
  theme_bw() + 
  theme(legend.position = "none", 
        strip.background = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 8)) + 
  scale_color_manual(values = bpr_colors[c(1, 3)]) + 
  scale_x_continuous(breaks = seq(0, 1, length.out = 5), limits = c(0, 1)) +
  scale_y_continuous(breaks = seq(0, 1, length.out = 5), limits = c(0, 1)) +
  labs(
    x = "Average ratings among Democrats",
    y = "Average ratings among Republicans"
  )


ggsave(here::here("output", "figures", "figure_e9_ratings_by_party.pdf"), g_dem_rep, width = 6.5, height = 5)
ggsave(here::here("output", "figures", "figure_e9_ratings_by_party.png"), g_dem_rep, width = 6.5, height = 5, dpi = 300)

