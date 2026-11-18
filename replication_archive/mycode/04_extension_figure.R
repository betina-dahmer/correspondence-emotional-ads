# =============================================================================
# 04_extension_figure.R
# Every table and figure in my paper, made from the saved results
#
# Paper: Murphy, Coppock, Hill & Vavreck (2026), "Do Emotional Ads Persuade?"
# Political Communication. https://doi.org/10.1080/10584609.2026.2726266
# =============================================================================
#
# The analysis scripts (01 to 03) do the math and save the numbers as CSV files
# in mycode/output/. This script only reads those files and turns them into
# what goes in the paper. So to change a color, a label or a number format,
# I only rerun this script, not the whole analysis.
#
# Run 01_replication_table.R and 03_extension.R first.
#
# What goes where in the paper:
#   Table 1  (Replication)  <- built by hand in main.tex from 01's CSVs
#   Figure 1 (Extension)    <- extension_slopes_by_group.pdf
#   Table 2  (Extension)    <- extension_table.tex
# =============================================================================

library(tidyverse)

out <- function(file) here::here("mycode", "output", file)

slopes_by_group <- read_csv(out("extension_slopes_by_group.csv"), show_col_types = FALSE)
difference_test <- read_csv(out("extension_difference_test.csv"), show_col_types = FALSE)
robustness_test <- read_csv(out("extension_robustness_test.csv"), show_col_types = FALSE)


# Figure 1: slope of each group's line, with 95% confidence intervals --------
# Colors by party (like the original article), filled vs. open points for
# strong vs. weak/lean partisans.

extension_plot <- slopes_by_group |>
  mutate(
    group = factor(group, levels = c("Strong Democrat", "Weak/lean Democrat", "Independent",
                                     "Weak/lean Republican", "Strong Republican")),
    party = case_when(str_detect(group, "Democrat")   ~ "Democrat",
                      str_detect(group, "Republican") ~ "Republican",
                      TRUE                            ~ "Independent"),
    strength = case_when(str_detect(group, "Strong") ~ "Strong",
                         str_detect(group, "Weak")   ~ "Weak/lean",
                         TRUE                        ~ "Independent"),
    index = recode(index, negative_index = "Negative emotion",
                   positive_index = "Positive emotion")
  ) |>
  ggplot(aes(x = estimate, y = group, color = party, shape = strength)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_pointrange(aes(xmin = estimate - 1.96 * std.error,
                      xmax = estimate + 1.96 * std.error),
                  size = 0.5) +
  facet_grid(index ~ dv) +
  scale_y_discrete(limits = rev) +
  scale_color_manual(values = c("Democrat"    = "blue",
                                "Independent" = "purple",
                                "Republican"  = "red"),
                     guide = "none") +
  scale_shape_manual(values = c("Strong" = 16, "Weak/lean" = 1, "Independent" = 17),
                     guide = "none") +
  labs(x = "Slope of ad effects on the emotion index (95% CI)", y = NULL) +
  theme_bw() +
  theme(panel.grid.minor = element_blank())

extension_plot
ggsave(out("extension_slopes_by_group.pdf"), extension_plot, width = 8, height = 5)

