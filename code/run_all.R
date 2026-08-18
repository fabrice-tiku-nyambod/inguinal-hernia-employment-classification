# run_all.R -- rebuild the employment-classification study end to end, then assert the result.
#
#   cd code && Rscript run_all.R
#
# Scripts are ordered by dependency and each is run in a clean R session, so a
# stale object left in the workspace cannot make a later step appear to succeed.
# 03 reads the raw SAS files and takes a few minutes; the rest are fast.

if (!file.exists("01_config.R"))
  stop("run this from the code/ directory: cd code && Rscript run_all.R",
       call. = FALSE)

RS <- file.path(R.home("bin"), "Rscript")

steps <- c("03_build_cohort.R",      # raw claims -> reconciled cohort (130 033)
           "04_job_class.R",         # exposure, models, subgroups, cost sharing
           "05_manuscript_tables.R", # tables -> paperB_tables.docx + CSVs
           "06_figures.R",           # Figure 1 forest plot -> 600 dpi TIFF
           "07_industry_and_ddd_feasibility.R",  # eTable 2 + the DDD feasibility record
           "10_ipw.R",               # selection weighting for cohort retention
           file.path("tests", "test_pipeline.R"))

for (s in steps) {
  cat("\n", strrep("=", 70), "\n>>> ", s, "\n", strrep("=", 70), "\n", sep = "")
  st <- system2(RS, shQuote(s))
  if (st != 0) stop("FAILED: ", s, " (exit ", st, ")", call. = FALSE)
}
cat("\nEmployment-classification study rebuilt; all assertions passed.\n")
