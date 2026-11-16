# 02_replication_table.R
# Replication table: published results vs. my reproduced results

# Panel A: design and sample numbers. 
# Panel B: the meta-regression slopes and SEs printed in Figures 2-4. Published values are typed in from the PDF (page numbers noted). Reproduced values come from what run_all.R saved.

source(here::here("code", "helpers.R"))
exps_clean <- read_rds(here::here("data", "exps_clean.rds"))
pairwise   <- read_rds(here::here("data", "estimates", "observed_pairwise_frame.rds"))
counts     <- read_csv(here::here("output", "tables", "cate_counts_by_outcome.csv"),
                       show_col_types = FALSE)

# Panel A: design and sample ----------------------------------------------

panel_a <- tribble(
  ~quantity,                               ~published, ~reproduced,
  "Experimental waves (p. 4)",             29,  n_distinct(exps_clean$date),
  "Unique ads (p. 4)",                     48,  n_distinct(exps_clean$ad_id[exps_clean$ad_id != 3]),
  "Ad treatments, ad x wave (p. 4)",       58,  nrow(distinct(filter(exps_clean, ad_id != 3), ad_id, date)),
  "Analysis sample (p. 1: approx. 28,000)", NA, nrow(exps_clean),
  "Favorability estimates (p. 7)",         174, counts$estimates[counts$dv == "Favorability"],
  "Vote choice estimates (p. 7)",          102, counts$estimates[counts$dv == "Vote Choice"],
  "Two-ad sample (appendix p. 22)",        5070, n_distinct(pairwise$caseid)
) |>
  mutate(match = published == reproduced)

View(panel_a)

# Panel B: meta-regression slopes (Figures 2-4) ---------------------------

# What the authors' pipeline estimated
meta <- bind_rows(
  read_csv(here::here("output", "tables", "figure_2_meta_estimates.csv"),
           show_col_types = FALSE) |> mutate(figure = "Figure 2"),
  read_csv(here::here("output", "tables", "figure_3_meta_estimates.csv"),
           show_col_types = FALSE) |> mutate(figure = "Figure 3"),
  read_csv(here::here("output", "tables", "figure_4_meta_estimates.csv"),
           show_col_types = FALSE) |> mutate(figure = "Figure 4")
) |>
  select(figure, index, dv, estimate, std.error)

# What the published article prints (slope and SE in each panel)
published <- tribble(
  ~figure,    ~index,           ~dv,                    ~pub_est, ~pub_se,
  "Figure 2", "negative_index", "Favorability",           0.014,  0.017,
  "Figure 2", "negative_index", "Vote Choice",           -0.005,  0.041,
  "Figure 2", "positive_index", "Favorability",           0.008,  0.014,
  "Figure 2", "positive_index", "Vote Choice",            0.022,  0.021,
  "Figure 3", "intended_index", "Favorability",          -0.004,  0.013,
  "Figure 3", "intended_index", "Vote Choice",           -0.023,  0.024,
  "Figure 4", "negative_index", "Target Issue Support",   0.052,  0.059,
  "Figure 4", "negative_index", "Turnout Intentions",    -0.017,  0.021,
  "Figure 4", "positive_index", "Target Issue Support",  -0.039,  0.041,
  "Figure 4", "positive_index", "Turnout Intentions",     0.064,  0.035
)

panel_b <- published |>
  left_join(meta, by = c("figure", "index", "dv")) |>
  mutate(
    rep_est = round(estimate, 3),
    rep_se  = round(std.error, 3),
    # compare after rounding to 3 decimals, the precision the article prints
    match   = abs(rep_est - pub_est) < 1e-9 & abs(rep_se - pub_se) < 1e-9,
    index   = recode(index, negative_index = "Negative",
                     positive_index = "Positive",
                     intended_index = "Intended")
  ) |>
  select(figure, index, dv, pub_est, rep_est, pub_se, rep_se, match)

View(panel_b)

# Save ------------------------------------------------------------------

dir.create(here::here("mycode", "output"), showWarnings = FALSE)
write_csv(panel_a, here::here("mycode", "output", "replication_table_panel_a.csv"))
write_csv(panel_b, here::here("mycode", "output", "replication_table_panel_b.csv"))

# LaTeX versions
print(xtable::xtable(panel_b, digits = 3),
      include.rownames = FALSE,
      file = here::here("mycode", "output", "replication_table_panel_b.tex"))

# Result: everything matches except the two-ad sample (published 5,070, reproduced 5,077). The analysis sample has no exact published value (the article says "approximately 28,000"); the code gives 28,679. All 10 slopes and SEs in Figures 2-4 match to the printed 3 decimals.
