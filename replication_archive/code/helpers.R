# Murphy, Coppock, Hill, and Vavreck
# "Do Emotional Ads Persuade? Evidence from Real-Time Campaign Advertising Experiments"
# Political Communication
#
# helpers.R: Loads all required packages and defines shared utility functions.

library(tidyverse)
library(here)
library(ggrepel)

library(glue)
library(readxl)

library(Matrix)

library(broom)
library(estimatr)
library(metafor)
library(clubSandwich)
library(modelsummary)

library(boot)
library(xtable)

bpr_colors <- c("#1F3A93", "#7C2C55", "#D91E18")

format_num <- function(x, digits = 3) {
  x <- as.numeric(x)
  sprintf(paste0("%.", digits, "f"), x)
}

glance_custom.rma <- \(x) data.frame(
  "nobs" = x$k,
  "Q-test p" = format_num(x$QEp, 3)
)
