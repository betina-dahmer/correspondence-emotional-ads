# 04_extension_figure.R
# Emotion-persuasion associations by partisan strength
#
# Visualizes the group-specific meta-regression estimates produced by 03_extension.R.

# Requires: 03_extension.R

# 1. Load data and dependencies ------------------------------------------

library(tidyverse)

out <- function(file) here::here("mycode", "output", file)

slopes_by_group <- read_csv(
  out("extension_slopes_by_group.csv"),
  show_col_types = FALSE
)

# 2. Prepare figure data --------------------------------------------------

extension_plot <- slopes_by_group |>
  mutate(
    group = factor(
      group,
      levels = c(
        "Strong Democrat",
        "Weak/lean Democrat",
        "Independent",
        "Weak/lean Republican",
        "Strong Republican"
      )
    ),
    party = case_when(
      str_detect(group, "Democrat") ~ "Democrat",
      str_detect(group, "Republican") ~ "Republican",
      TRUE ~ "Independent"
    ),
    strength = case_when(
      str_detect(group, "Strong") ~ "Strong",
      str_detect(group, "Weak") ~ "Weak/lean",
      TRUE ~ "Independent"
    ),
    index = recode(
      index,
      negative_index = "Negative emotion",
      positive_index = "Positive emotion"
    )
  ) |>
  
  # 3. Plot group-specific coefficients ----------------------------------

ggplot(aes(
  x = estimate,
  y = group,
  color = party,
  shape = strength
)) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    color = "gray50"
  ) +
  geom_pointrange(
    aes(
      xmin = estimate - qt(0.975, df) * std.error,
      xmax = estimate + qt(0.975, df) * std.error
    ),
    size = 0.5
  ) +
  facet_grid(index ~ dv) +
  scale_y_discrete(limits = rev) +
  scale_color_manual(
    values = c(
      "Democrat" = "blue",
      "Independent" = "purple",
      "Republican" = "red"
    ),
    guide = "none"
  ) +
  scale_shape_manual(
    values = c(
      "Strong" = 16,
      "Weak/lean" = 1,
      "Independent" = 17
    ),
    guide = "none"
  ) +
  labs(
    x = "Meta-regression coefficient (95% CI)",
    y = NULL
  ) +
  theme_bw() +
  theme(
    panel.grid.minor = element_blank()
  )

# 4. Export figure -------------------------------------------------------

extension_plot

ggsave(
  out("extension_slopes_by_group.pdf"),
  extension_plot,
  width = 8,
  height = 5
)
