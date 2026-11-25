
# Emotional Campaign Advertising: Replication and Extension

Replication and extension of Murphy, Coppock, Hill, and Vavreck (2026), *“Do Emotional Ads Persuade? Evidence from Real-Time Campaign Advertising Experiments,”* **Political Communication**. https://doi.org/10.1080/10584609.2026.2726266

This project reproduces the authors' results and examines whether the association between ads' emotional responses and persuasive effects differs between strong partisans and weak partisans/leaners. It also evaluates the precision of those differences and their sensitivity to a preprocessing error in the original replication code.

## Research design

The original study uses 29 waves of randomized advertising experiments conducted during the 2016 U.S. presidential campaign. The present extension retains the authors' ad-level meta-regression approach, with estimated ad effects (CATEs) and their sampling covariance matrices, but disaggregates partisan identification into five groups:

| Group | Seven-point party identification (`pid_7_pre`) |
| --- | --- |
| Strong Democrat | 1 |
| Weak/lean Democrat | 2–3 |
| Independent | 4 |
| Weak/lean Republican | 5–6 |
| Strong Republican | 7 |

The extension considers positive and negative emotion indices and two outcomes: candidate favorability and vote choice. Within Democrats and Republicans separately, interaction models compare the emotion–persuasion slopes for strong versus weak/lean partisans. Additional models adjust for the partisan side of the advertisement. Inference uses wave-clustered CR2 standard errors, with Holm adjustment across the eight interaction tests within each specification.

**Interpretation:** Ad assignment is randomized, but the measured emotional response is not. The meta-regression slopes are associations between emotional responses and estimated ad effects, not identified causal effects of emotion itself.

## Reproducing the analyses

Run scripts from the **`replication_archive/` directory**, which must also be the root resolved by `here::here()`, with the original authors' replication files and data in their expected locations. The authors' pipeline is run by `run_all.R`; the original code is in `code/` and data in `data/`. The project-specific scripts are in `mycode/` and write outputs to `mycode/output/`.

Start a clean R session in `replication_archive/`. Before running the scripts, check that the expected directories and project root are available:

```r
getwd()
here::here()
stopifnot(
  dir.exists("code"),
  dir.exists("data"),
  dir.exists("mycode"),
  normalizePath(here::here()) == normalizePath(getwd())
)
```

If the root check fails, resolve the project-root configuration before proceeding. In R, run the scripts in this order:

```r
source("mycode/00_reproduce.R")
source("mycode/01_replication_table.R")
source("mycode/02_explore.R")
source("mycode/03_extension.R")
source("mycode/04_extension_figure.R")
source("mycode/05_extension_table.R")
source("mycode/06_minimum_detectable_effects.R")
source("mycode/07_precision_figure.R")
source("mycode/08_sanders_sensitivity.R")
```

The first script checks and installs required packages, including `excheckr` from `acoppock/excheckr` on GitHub, and runs the authors' original pipeline. Later scripts assume the resulting data and estimates exist. Scripts 04, 05, and 07 generate presentation outputs from saved analysis results; the other scripts conduct replication, exploration, estimation, or sensitivity analysis. Script 08 reuses the estimation functions defined in script 03 without rerunning its full pipeline.

### Script guide

| Script | Purpose |
| --- | --- |
| `00_reproduce.R` | Run the authors' replication pipeline and audit published claims. |
| `01_replication_table.R` | Assemble the comparison of published and reproduced estimates. |
| `02_explore.R` | Explore the experimental data and partisan composition. |
| `03_extension.R` | Reproduce the published Figure 2 specification, estimate five-group slopes, test strong-versus-weak/lean differences, adjust for ad side, apply Holm correction, and conduct leave-one-ad-out checks. |
| `04_extension_figure.R` | Generate the extension's group-specific slope figure. |
| `05_extension_table.R` | Generate the extension's interaction-results table. |
| `06_minimum_detectable_effects.R` | Calculate minimum detectable effects and summarize statistical precision, including CR2 confidence intervals for the baseline and adjusted interaction estimates. |
| `07_precision_figure.R` | Plot pointwise 95% confidence intervals for the interaction estimates. |
| `08_sanders_sensitivity.R` | Correct the Sanders favorability normalization in memory, re-estimate Figure 2 and interaction models, and compare original with corrected results. |

### Selected output files

Files below are written under `mycode/output/`:

- `extension_figure_2_check.csv`: reproduced Figure 2 estimates using the original specification.
- `extension_slopes_by_group.csv`: group-specific emotion–persuasion slopes.
- `extension_difference_test.csv`: baseline strong-versus-weak/lean interactions.
- `extension_robustness_test.csv`: interactions adjusted for the partisan side of the ad.
- `extension_leave_one_out.csv`: leave-one-ad-out sensitivity results.
- `extension_precision.csv` and `extension_precision_summary.csv`: confidence intervals and precision summaries.
- `extension_precision.png` and `extension_precision.pdf`: visualization of pointwise confidence intervals.
- `sanders_figure2_sensitivity.csv`: published-specification estimates before and after correcting Sanders favorability.
- `sanders_interaction_sensitivity.csv`: baseline interaction comparisons.
- `sanders_robustness_sensitivity.csv`: ad-side-adjusted interaction comparisons.
- `sanders_sensitivity_summary.csv`: counts of nominally and Holm-significant interactions before and after correction.

The extension figure and table are generated separately by scripts 04 and 05.

## Results

### Replication

The original replication pipeline completed successfully. Its claim audit reported **72 claims checked, 67 matched, and zero mismatched**. Five claims are designated as derived rather than directly checked against published values. All ten published meta-regression slopes and corresponding standard errors in Figures 2 through 4 are reproduced to their reported precision. A separate discrepancy concerns an appendix two-ad sample size: 5,070 in the published article versus 5,077 in the replication code.

### Statistical precision and minimum detectable effects

Under a normal approximation with a fixed standard error, 80% power, and a two-sided 5% significance level, the minimum detectable gap between high- and low-emotion ads (90th versus 10th percentile) ranges from 1.4 to 1.6 points for favorability and from 2.2 to 3.9 points for vote choice, on a 0–100 outcome scale. These estimates suggest limited power to detect modest but substantively meaningful associations, particularly for vote choice. They are approximate power benchmarks, not exact power calculations for the clustered meta-regression or bounds on effects excluded by the data.

### Partisan-strength extension

The eight baseline interaction tests compare strong with weak/lean partisans across party, emotion index, and outcome. One of eight baseline interactions is nominally significant at the 5% level (Republican positive emotion × favorability), but **none survives Holm adjustment**. The ad-side-adjusted specification also has one nominally significant interaction and none significant after Holm adjustment.

These results do **not** establish that the slopes are equal: the pointwise 95% CR2 confidence intervals are often wide. Median confidence-interval width is 36.1 points in the baseline and 39.0 points in the adjusted specification when coefficients are expressed on a 0–100 outcome scale for a full-unit change in the emotion index. These are not estimates for a typical observed difference between ads. The intervals shown in script 07 are **pointwise**, not multiplicity-adjusted.

A leave-one-ad-out analysis examines the Republican positive-emotion × favorability interaction. Its nominal significance is not driven by a single ad, but this does not change the multiple-testing result.

### Sanders favorability sensitivity

The authors' cleaning code (`code/clean_exps.R`, lines 69–70) applies the `(x - 1) / 4` transformation **twice** to `favorBS_rev`, unlike the other candidate-favorability outcomes. Script 08 reverses the second transformation in memory using `favorBS_rev = 4 * favorBS_rev + 1`, then re-estimates the CATEs, sampling covariance matrices, and meta-regressions. The original source files are not modified.

The correction changes both the magnitude and precision of the pooled favorability estimates:

| Emotion | Original estimate (SE) | Corrected estimate (SE) | Corrected p-value |
| --- | ---: | ---: | ---: |
| Positive | 0.008 (0.014) | 0.026 (0.025) | 0.310 |
| Negative | 0.014 (0.017) | 0.008 (0.034) | 0.824 |

Neither slope is statistically significant before or after correction. The number of statistically significant interactions is also unchanged:

| Specification | Original: nominal *p* < .05 | Corrected: nominal *p* < .05 | Original: Holm *p* < .05 | Corrected: Holm *p* < .05 |
| --- | ---: | ---: | ---: | ---: |
| Baseline (8 tests) | 1 | 1 | 0 | 0 |
| Ad-side adjusted (8 tests) | 1 | 1 | 0 | 0 |

The identity of the nominally significant adjusted interaction changes after correction; consequently, the stability of these counts should **not** be interpreted as stability of every individual estimate.

### Estimation note

The five-group estimation generates variance warnings for some small or low-variation cells. The resulting meta-regression input identifies **one unusable CATE**: Strong Democrats, Trump vote choice (`general_vote_DT`), the *Role Model* ad, July 18, 2016, with a `NaN` standard error. The same CATE is unusable in both the original and Sanders-corrected analyses; it is excluded by the existing `usable` filter.

## Scope and limitations

- Emotion is measured after exposure and is not independently randomized, so the analysis is associational with respect to emotion.
- Splitting the sample into five partisan groups reduces precision; a nonsignificant interaction is not proof of no heterogeneity.
- The eight interaction tests are corrected using Holm's method separately within each model specification.
- Confidence intervals in the precision figure are pointwise 95% intervals, not simultaneous intervals.
- The Sanders correction is a sensitivity analysis, not a replacement for the original published-data replication.

## References

Murphy, John, Alexander Coppock, Seth J. Hill, and Lynn Vavreck. 2026. “Do Emotional Ads Persuade? Evidence from Real-Time Campaign Advertising Experiments.” *Political Communication*. https://doi.org/10.1080/10584609.2026.2726266

Original replication materials: Harvard Dataverse. https://doi.org/10.7910/DVN/CB3LP1
