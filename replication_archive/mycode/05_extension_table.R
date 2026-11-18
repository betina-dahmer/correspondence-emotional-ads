# 04_extension_table.R

# Table 2: strong vs. weak partisans -----------------------------------------
# One row per test. Two versions of the difference (strong minus weak slope):
# the basic one, and one comparing ads from the same side. Holm-adjusted
# p-values for 8 tests.

fmt <- function(est, se) sprintf("%.3f (%.3f)", est, se)

extension_table <- difference_test |>
  select(party, index, dv,
         basic = estimate, basic_se = std.error, basic_holm = p.holm) |>
  left_join(robustness_test |>
              select(party, index, dv,
                     side = estimate, side_se = std.error, side_holm = p.holm),
            by = c("party", "index", "dv")) |>
  transmute(
    Party   = party,
    Emotion = recode(index, negative_index = "Negative", positive_index = "Positive"),
    Outcome = dv,
    `Strong - weak (SE)`   = fmt(basic, basic_se),
    `Holm p`               = sprintf("%.2f", basic_holm),
    `Same-side ads (SE)`   = fmt(side, side_se),
    `Holm p `              = sprintf("%.2f", side_holm)   # extra space: column names must differ
  )

View(extension_table)

write_csv(extension_table, out("extension_table.csv"))

print(xtable::xtable(extension_table,
                     caption = "Difference between strong and weak/lean partisans in the slope of ad effects on emotion",
                     label = "tab:extension"),
      include.rownames = FALSE, booktabs = TRUE,
      file = out("extension_table.tex"))