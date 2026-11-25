# Correspondence: Do Emotional Ads Persuade?

Replication and extension of Murphy et al. (2026), *Do Emotional Ads Persuade? Evidence from Real-Time Campaign Advertising Experiments*.

## Repository structure

The original replication archive is retained to preserve the original directory structure and ensure compatibility with the existing code.

- [`replication_archive/mycode/`](replication_archive/mycode/): All original code developed for this correspondence, including replication, extensions, and sensitivity analyses.
- [`replication_archive/mycode/output/`](replication_archive/mycode/output/): Results generated for this correspondence.
- `replication_archive/code/`: Original authors' replication scripts.
- `replication_archive/data/`: Original authors' data.
- `replication_archive/output/`: Original replication outputs.

## Replication instructions

All scripts developed for this correspondence are located in the `mycode` directory. The original authors' scripts and data are retained separately within the replication archive.

## Code and replication workflow

All scripts developed for this correspondence are located in [`replication_archive/mycode/`](replication_archive/mycode/).

| Script | Description |
|---|---|
| `00_reproduce.R` | Runs the original replication pipeline. |
| `01_replication_table.R` | Compares published and reproduced estimates and generates Table 1. |
| `02_explore.R` | Conducts exploratory analyses of the original data. |
| `03_extension.R` | Extends the original analysis by disaggregating partisan groups and testing heterogeneity by partisan strength. |
| `04_extension_figure.R` | Generates Figure 1, displaying group-specific meta-regression slopes. |
| `05_extension_table.R` | Generates Table 3, reporting partisan-strength interactions and Holm-adjusted p-values. |
| `06_minimum_detectable_effects.R` | Calculates minimum detectable effects and generates Table 2. |
| `07_precision_figure.R` | Generates a supplementary visualization of statistical precision. |
| `08_sanders_sensitivity.R` | Corrects the duplicated Sanders favorability normalization and re-estimates the original Figure 2 models and the partisan-strength extension. |

### Execution order

The scripts are numbered according to the intended replication workflow, from `00` to `08`.

All scripts should be executed with `replication_archive/` as the project root. Results generated for this correspondence are stored in `replication_archive/mycode/output/`.

The original authors' code and data are retained separately from the code developed for this correspondence.

## Replication materials

The original replication materials are available through the Harvard Dataverse:

https://doi.org/10.7910/DVN/CB3LP1

The original directory structure has been retained to preserve compatibility with the authors' replication code.
