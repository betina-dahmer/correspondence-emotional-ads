Replication Archive

"Do Emotional Ads Persuade? Evidence from Real-Time Campaign Advertising
Experiments"

John Murphy, Alexander Coppock, Seth J. Hill, and Lynn Vavreck

Political Communication
https://doi.org/10.1080/10584609.2026.2726266

Archive DOI: https://doi.org/10.7910/DVN/CB3LP1


OVERVIEW

This archive contains all code and data needed to reproduce every figure,
table, and in-text number reported in the paper and online appendix. Every
number stated in the article is checked against the pipeline by
code/in_text_claims.R, which run_all.R runs as its penultimate step and
which prints a labelled audit trail ending in a CLAIM SUMMARY line.


REQUIREMENTS

R (version 4.4 or later) with the following packages:

  tidyverse, here, ggrepel, glue, readxl, Matrix, broom, estimatr,
  metafor, clubSandwich, modelsummary, boot, xtable, psych, GGally,
  excheckr

Install:

  install.packages(c(
    "tidyverse", "here", "ggrepel", "glue", "readxl", "Matrix", "broom",
    "estimatr", "metafor", "clubSandwich", "modelsummary", "boot",
    "xtable", "psych", "GGally", "excheckr"
  ))

psych and GGally are used by two appendix figures and are called with the
package name attached (psych::alpha, GGally::ggpairs) so that the rest of
the pipeline runs without them.


QUICKSTART

There is no setwd() anywhere in this archive. All paths resolve with
here::here() against the .here marker file at the archive root, so scripts
run correctly from any working directory inside the archive.

  Rscript run_all.R

or, from an R session with the archive open:

  source("run_all.R")

Runtime is about 25 minutes, of which roughly 9 are the two bootstraps.
Individual scripts in code/ can also be run on their own, in the order
below.


VERSIONS

The archive does not pin package versions. Every run of run_all.R writes
output/session_info.txt recording the R version and the version of every
package the pipeline loads, so the environment behind any given set of
outputs is always on record. The archive was last run under R 4.6.0 with
metafor 5.0-1 and clubSandwich 0.7.0.


VARIANCE ESTIMATION

The meta-analytic results (Figures 2, 3 and 4 and the corresponding
appendix figures and tables) use metafor::rma.mv() with a block-diagonal
variance-covariance matrix, and cluster-robust standard errors obtained via

  robust(cluster = date, clubSandwich = TRUE)

which is the CR2 estimator with Satterthwaite degrees of freedom described
by Pustejovsky and Tipton (2018) and cited in the paper's methods section.
The subgroup models cluster on 15 to 27 experimental waves and the pooled
models on 29, which is the small-sample regime that correction exists for.


WHAT IS IN THE DATA, AND WHAT IS NOT

The survey files in this archive carry only the variables the analysis code
actually reads. data/exps.rds holds 63 of the 387 columns on the original
YouGov file: the treatment assignment, the ad identifiers and their coding,
the emotion and ad-rating items, the favorability, vote-choice and issue
outcomes, the pre-treatment partisanship measures used as covariates, the
survey weights, and the case, wave and date keys. The 324 columns that no
script reads were removed before deposit, among them date of birth, gender,
race, education, income, marital and family status, employment, religious
affiliation and its follow-ups, military and union membership, sexuality,
disability, state of residence, media market, and a large battery of
favorability and issue items the paper does not analyze.

This is a disclosure-limitation measure, not a change to the analysis. It
was verified by running the full pipeline against the reduced file and
comparing every result to a run against the unreduced one: the two agree
exactly, to the last bit, at every intermediate estimate and in every figure
and table. Nothing reported in the paper or the appendix depends on a
removed column.

What remains is still individual-level survey data. caseid is a
pseudonymous panel key carried because the individual-level bootstrap in
code/estimate_individual_accuracy_bootstrap.R resamples respondents; it is
not a name or a contact detail, but it is a per-respondent identifier, and
rows remain linkable to each other across the file by design.

Researchers who need a variable that is not here should contact the authors.

DIRECTORY STRUCTURE

  replication_archive/
    .here                     Root marker used by here::here()
    README.txt                This file
    run_all.R                 Master script; runs everything in order
    code/
      helpers.R               Packages, colors, and shared utilities
      clean_exps.R            Cleans the main survey data; asserts the
                              expected schema on both input and output
      clean_individual_ratings.R
      estimate_ratings_pid.R
      estimate_all_cates.R
      estimate_all_cates_with_stretch.R
      estimate_meta_regressions.R
      estimate_meta_regressions_with_stretch.R
      estimate_individual_accuracy_bootstrap.R
      estimate_group_accuracy_bootstrap.R
      figure_*.R              One script per figure, named for what the
                              figure shows
      table_d16_ad_emotion_coding.R
      in_text_claims.R        Reproduces and checks every number in the
                              article and appendix
    data/
      exps.rds                Survey data as fielded, before the cleaning
                              steps (34,000 respondents, 63 analysis
                              variables; see WHAT IS IN THE DATA above)
      exps_clean.rds          Analysis sample (28,679 respondents)
      individual_ratings_clean.rds
      two_ads_key.rds
      ad_df.csv               Ad metadata
      ad_df_emotions.csv      Ad metadata with intended emotion coding
      ad_id_to_base_by_hand.csv
      bases_df.xlsx
      estimates/              Intermediate results, all regenerated by
                              run_all.R
    output/
      figures/                Every figure as both PDF and PNG
      tables/                 LaTeX tables, plus the CSV intermediates
                              in_text_claims.R reads
      session_info.txt        Environment for the most recent run


NAMING

Figures and tables are named for what they show, with the number giving
the position in the document and the letter giving the appendix section:

  figure_2_emotion_cate_favorability_vote.pdf  is Figure 2
  figure_e11_individual_accuracy.pdf           is Figure E.11
  table_c7_vote_choice_negative_emotion.tex    is Table C.7

Each generating script carries the name of its primary output.


SCRIPT EXECUTION ORDER

  1. Cleaning
       clean_exps.R
       clean_individual_ratings.R
  2. Ratings by partisanship
       estimate_ratings_pid.R
  3. CATE estimation
       estimate_all_cates.R
       estimate_all_cates_with_stretch.R
  4. Bootstraps (about 9 minutes; seeded with set.seed(343))
       estimate_individual_accuracy_bootstrap.R
       estimate_group_accuracy_bootstrap.R
  5. Meta-regressions and appendix tables
       estimate_meta_regressions.R
       estimate_meta_regressions_with_stretch.R
  6. Main figures
  7. Appendix figures
  8. Appendix table D.16
  9. In-text claims audit
 10. Environment record


OUTPUTS

Main text figures

  Figure  Script
  1       figure_1_emotion_by_partisanship.R
  2       figure_2_emotion_cate_favorability_vote.R
  3       figure_3_intended_emotion_cate_favorability_vote.R
  4       figure_4_emotion_cate_issue_turnout.R

Appendix figures

  Figure  Script
  A.1     figure_a1_emotion_scree.R
  A.2     figure_a2_emotion_correlations.R
  B.3     figure_b3_emotion_by_party.R
  C.4     figure_c4_stretch_issue_turnout_emotion.R
  C.5     figure_c5_issue_turnout_intended_emotion.R
  C.6     figure_c6_stretch_issue_turnout_intended_emotion.R
  E.7     figure_e7_rating_correlations.R
  E.8     figure_e8_rating_scree.R
  E.9     figure_e9_ratings_by_party.R
  E.10    figure_e10_cates_from_ratings.R
  E.11    figure_e11_individual_accuracy.R
  E.12    figure_e12_group_accuracy.R

Appendix tables

  Table       Script
  C.1-C.4     estimate_meta_regressions.R
  C.5         estimate_meta_regressions_with_stretch.R
  C.6-C.9     estimate_meta_regressions.R
  C.10        estimate_meta_regressions_with_stretch.R
  C.11-C.14   estimate_meta_regressions.R
  C.15        estimate_meta_regressions_with_stretch.R
  D.16        table_d16_ad_emotion_coding.R
  E.17-E.18   estimate_meta_regressions.R


SEEDS

Both bootstrap scripts call set.seed(343) immediately after sourcing
helpers.R and draw 1,000 resamples. Each resample is a draw of respondents
with replacement; the two-advertisement subsample inherits each drawn
respondent's multiplicity, so the accuracy statistics are bootstrapped
rather than subsampled.

Figures E.11 and E.12 carry no vote choice estimates for pro-Republican
versus pro-Republican matchups. Both such matchups were fielded during the
primary, when the general election vote choice question was not asked, so
those cells are empty by design and are not computed.


DATA DESCRIPTION

exps.rds contains individual-level survey responses from 29 weekly waves
of experiments run from March to November 2016 during the U.S.
presidential election. Subjects were randomly assigned to view a campaign
advertisement or a placebo (a car insurance commercial). The file includes
pre-treatment covariates, treatment assignment, emotional responses,
subjective ratings, candidate evaluations, vote choice, issue positions,
and turnout intentions. clean_exps.R drops respondents assigned two
advertisements from the main analysis sample, leaving 28,679 of 34,000.


CONTACT

Alexander Coppock, Northwestern University
