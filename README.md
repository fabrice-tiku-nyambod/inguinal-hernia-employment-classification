# Employment classification and complicated presentation of inguinal hernia

Analysis pipeline for a retrospective cross-sectional study of commercially
insured adults undergoing inguinal hernia repair, asking whether hourly workers
present with obstructed or gangrenous disease more often than salaried workers,
and whether patient out-of-pocket cost accounts for any difference.

This repository contains **code only**. No data, manuscript, or figures are
published here.

## Data

The study uses the Merative MarketScan Commercial Claims and Encounters
Database, 2016 to 2021. The database is proprietary and licensed, and
patient-level data cannot be redistributed. Independent researchers may obtain
access by licensing it directly from Merative.

Nothing in this repository will reproduce without that licensed extract.

## Running the pipeline

Point the pipeline at the licensed SAS extract, either by setting the
`MARKETSCAN_DIR` environment variable or by editing the fallback path in
`code/01_config.R`. Then:

```
cd code
Rscript run_all.R
```

Each step runs in a clean R session and the run stops on the first failure.
Expect a few minutes for `03`; the rest are fast.

| Script | Purpose |
|---|---|
| `01_config.R` | Data path, code sets, CPI deflators, label lookups. The only file that needs changing, and only if you are not setting `MARKETSCAN_DIR`. |
| `03_build_cohort.R` | Raw SAS claim lines to the reconciled cohort, 130,033 patients |
| `04_job_class.R` | Exposure derivation, primary models, subgroups, cost sharing |
| `05_manuscript_tables.R` | Formatted tables |
| `06_figures.R` | Figures, 1200 dpi TIFF |
| `07_industry_and_ddd_feasibility.R` | Industry sensitivity analysis; feasibility record for a policy-variation design that was tested and not pursued |
| `09_wordcount.R` | Journal word and exhibit limits |
| `10_ipw.R` | Inverse probability of selection weighting for cohort retention |
| `08_`, `11_` | Submission package assembly |
| `tests/test_pipeline.R` | Regression suite |

There is no `02`; the numbering is shared with companion analyses of the same
extract.

## Tests

```
cd code
Rscript tests/test_pipeline.R
```

The suite asserts the cohort size, the exposure derivation, the direction and
magnitude of every published estimate, and the internal consistency of the
generated tables. It exists because two errors during development of a companion
analysis were silent: a script succeeded while reading a superseded cohort, and
an edit removed a section without failing. Assertions catch that class of
failure.

Several checks encode findings rather than mechanics, so the build fails if a
published number changes without the manuscript changing with it. One asserts
that the attenuation of the estimate under selection weighting remains disclosed.

## Method notes

- Case definition requires a repair procedure code with a concordant K40
  diagnosis **on the same claim line**, not merely on the same encounter.
- Three effect measures are reported. The adjusted risk difference is primary and
  is obtained by g-computation with a delta-method confidence interval.
- Realized out-of-pocket cost is incurred at or after the presentation it would
  be invoked to explain, so it is treated as a robustness check and never as a
  mediator.
- `output/DDD_NOT_PURSUED.md` records a state policy-variation design that was
  tested and abandoned, with the numbers behind that decision.
- `output/ANALYSIS_LOG.md` records when each analysis entered the study. No
  protocol was registered, so the manuscript describes analyses by purpose rather
  than claiming prespecification.

Both records are generated into `output/`, which is not tracked here.

## Software

R 4.6.0. Packages: dplyr, tidyr, haven, purrr, stringr, splines, sandwich,
lmtest, ggplot2, officer, flextable.

## License

[To be chosen by the authors.]
