# 07_precision_figure.R
# Confidence intervals for partisan-strength interactions

# Plot the eight emotion-by-strength interactions from script 06, comparing baseline and ad-side-adjusted specifications.

# Requires: 06_minimum_detectable_effects.R

# 1. Load results ----------------------------------------------------------

precision <- readr::read_csv(
  here::here("mycode", "output", "extension_precision.csv"),
  show_col_types = FALSE
)


# 2. Prepare figure data ---------------------------------------------------

plot_data <- precision |>
  dplyr::mutate(
    emotion = dplyr::recode(
      index,
      negative_index = "Negative emotion",
      positive_index = "Positive emotion"
    ),
    party = factor(
      party,
      levels = c("Democrat", "Republican")
    ),
    emotion = factor(
      emotion,
      levels = c("Negative emotion", "Positive emotion")
    ),
    dv = factor(
      dv,
      levels = c("Favorability", "Vote Choice")
    ),
    specification = factor(
      specification,
      levels = c("Baseline", "Ad-side adjusted")
    )
  )

stopifnot(nrow(plot_data) == 16)


# 3. Plot confidence intervals --------------------------------------------

precision_plot <- ggplot2::ggplot(
  plot_data,
  ggplot2::aes(
    x = estimate_100,
    y = emotion,
    xmin = ci_low_100,
    xmax = ci_high_100,
    color = specification
  )
) +
  ggplot2::geom_vline(
    xintercept = 0,
    linetype = "dashed",
    color = "grey50"
  ) +
  ggplot2::geom_pointrange(
    position = ggplot2::position_dodge(width = 0.5),
    linewidth = 0.6
  ) +
  ggplot2::facet_grid(
    party ~ dv,
    scales = "free_x"
  ) +
  ggplot2::labs(
    title = "Partisan strength and emotional persuasion",
    subtitle = "Differences in emotion-persuasion slopes: strong vs. weak/lean partisans",
    x = "Difference in slope (0-100 outcome scale)",
    y = NULL,
    color = "Specification",
    caption = paste(
      "Points show interaction estimates; bars show pointwise 95% CR2 confidence intervals.",
      "Intervals are not adjusted for multiple comparisons."
    )
  ) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(
    legend.position = "bottom",
    panel.grid.minor = ggplot2::element_blank(),
    strip.text = ggplot2::element_text(face = "bold"),
    plot.title = ggplot2::element_text(face = "bold")
  )

print(precision_plot)


# 4. Export figure ---------------------------------------------------------

ggplot2::ggsave(
  filename = here::here(
    "mycode", "output", "extension_precision.png"
  ),
  plot = precision_plot,
  width = 11,
  height = 6,
  dpi = 300
)

ggplot2::ggsave(
  filename = here::here(
    "mycode", "output", "extension_precision.pdf"
  ),
  plot = precision_plot,
  width = 11,
  height = 6
)
