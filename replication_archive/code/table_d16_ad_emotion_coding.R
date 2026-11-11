# Murphy, Coppock, Hill, and Vavreck
# "Do Emotional Ads Persuade? Evidence from Real-Time Campaign Advertising Experiments"
# Political Communication
#
# table_d16_ad_emotion_coding.R: Produces Table D.16, intended emotion coding for each advertisement.

source(here::here("code", "helpers.R"))

ad_df_edited <- read_csv(here::here("data", "ad_df_emotions.csv"))
exps_clean <- read_rds(here::here("data", "exps_clean.rds"))

ad_df_edited |>
  select(ad_id, ad_title, ad_valence, emotion_valence) |>
  filter(ad_id %in% exps_clean$ad_id) |>
  arrange(ad_id) |>
  xtable(digits = c(0, 0, 0, 0, 0)) |> 
  print.xtable(include.colnames = FALSE, include.rownames = FALSE, only.contents = TRUE, hline.after = c(), file = here::here("output", "tables", "table_d16_ad_emotion_coding.tex"))

