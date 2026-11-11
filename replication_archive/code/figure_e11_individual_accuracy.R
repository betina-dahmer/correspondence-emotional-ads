# Murphy, Coppock, Hill, and Vavreck
# "Do Emotional Ads Persuade? Evidence from Real-Time Campaign Advertising Experiments"
# Political Communication
#
# figure_e11_individual_accuracy.R: Produces Figure E.11, individual-level pairwise prediction accuracy.

source(here::here("code", "helpers.R"))

individual_results <- read_rds(here::here("data", "estimates", "individual_bootstrap_results_1000.rds"))

# Individual
individual_plot_frame <-
  data.frame(term = names(individual_results$t0), statistic = individual_results$t0[1:length(individual_results$t0)])

individual_plot_frame <-
  individual_plot_frame |>
  left_join(individual_results |> tidy(), by = join_by(term, statistic))

individual_plot_frame <- 
  individual_plot_frame |>
  mutate(
    bias = round(bias, 3),
    std.error = round(std.error, 3),
    conf.high = round(statistic + 1.96 * std.error, digits =  3),
    conf.low = round(statistic - 1.96 * std.error, digits =  3),
    `CI_95` = paste0("(", conf.low, ", ", conf.high, ")"),
    correct = paste0(statistic, " ", `CI_95`),
    statistic = round(statistic, digits = 3),
    statistic_text = format(round(statistic, digits = 3), nsmall = 3),
    dv = if_else(str_detect(term, "fav"), "Favorability", "Vote Choice"),
    moderator = case_when(str_detect(term, "_sig") ~ "Significant (p < 0.05)",
                          str_detect(term, "insig") ~ "Non-significant",
                          str_detect(term, "above") ~ "Above Median d",
                          str_detect(term, "below") ~ "Below Median d",
                          TRUE ~ "Overall"),
    moderator = factor(moderator, levels = c( "Below Median d", "Above Median d",
                                              "Non-significant", "Significant (p < 0.05)", "Overall")),
    valence = case_when(str_detect(term, "HCDT")  & str_detect(term, "sate") ~ "Dem v. Rep (ATEs)",
                        str_detect(term, "dem_ads")  & str_detect(term, "sate") ~ "Dem v. Dem (ATEs)",
                        str_detect(term, "rep_ads")  & str_detect(term, "sate") ~ "Rep v. Rep (ATEs)",
                        str_detect(term, "overall")  & str_detect(term, "sate") ~ "Overall (ATEs)",
                        str_detect(term, "HCDT")  & str_detect(term, "cate") ~ "Dem v. Rep (CATEs)",
                        str_detect(term, "dem_ads")  & str_detect(term, "cate") ~ "Dem v. Dem (CATEs)",
                        str_detect(term, "rep_ads")  & str_detect(term, "cate") ~ "Rep v. Rep (CATEs)",
                        str_detect(term, "overall")  & str_detect(term, "cate") ~ "Overall (CATEs)",
                        TRUE ~ NA),
    valence = factor(valence, levels = c("Rep v. Rep (ATEs)", "Dem v. Dem (ATEs)", "Dem v. Rep (ATEs)", "Overall (ATEs)",
                                         "Rep v. Rep (CATEs)", "Dem v. Dem (CATEs)", "Dem v. Rep (CATEs)", "Overall (CATEs)")),
    pid_3 = case_when(str_detect(term, "e_dem_") | str_detect(term, "v_dem_") ~ "Democrats",
                      str_detect(term, "ind_") ~ "Independents",
                      str_detect(term, "e_rep_") | str_detect(term, "v_rep_") ~ "Republicans",
                      str_detect(term, "_all_") ~ "Overall"),
    name = term
  )  |>
  mutate(pid_3 = factor(pid_3, levels = c("Overall", "Democrats", "Independents", "Republicans")))

# Only CATEs

g_individual_accuracy_cates <-
  individual_plot_frame |>
  filter(str_detect(valence, "(CATEs)")) |> 
  ggplot(aes(x = statistic, y = valence)) +
  geom_point(aes(color = pid_3), size = 1) +
  geom_text(aes(label = statistic_text), vjust = -.5, size = 3) +
  geom_linerange(aes(xmin = conf.low, xmax = conf.high, color = pid_3)) +
  geom_vline(xintercept = .5, linetype = "dashed") +
  scale_color_manual(values = c("black", bpr_colors)) + 
  facet_grid(pid_3 ~ dv,
             scales = "free_y") +
  theme_bw() +
  theme(legend.position = "none", strip.background = element_blank()) + 
  labs(x = "Proportion of Advertisement Pairs Correctly Ordered", y = "") + 
  coord_cartesian(xlim = c(0, 1)) + 
  scale_x_continuous(breaks = c(0.00, 0.25, 0.50, 0.75, 1.00),
                     labels = c("0.00", "0.25", "0.50", "0.75", "1.00"))


ggsave(here::here("output", "figures", "figure_e11_individual_accuracy.pdf"), g_individual_accuracy_cates, width = 6.5, height = 5.5)
ggsave(here::here("output", "figures", "figure_e11_individual_accuracy.png"), g_individual_accuracy_cates, width = 6.5, height = 5.5, dpi = 300)

