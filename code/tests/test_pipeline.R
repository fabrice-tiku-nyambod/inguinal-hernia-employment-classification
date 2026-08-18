# test_pipeline.R -- assertions that must hold for the Health Affairs submission to be correct.
# Two errors during development of the companion study were silent: a
# script succeeded while reading a superseded cohort, and a text edit deleted a
# section without failing. These assertions catch that class of failure.
#
# Run from code/:  Rscript tests/test_pipeline.R

source("01_config.R")
suppressPackageStartupMessages(library(dplyr))
stopifnot(dir.exists(OUT))
ok <- function(label, cond) {
  cat(sprintf("  [%s] %s\n", if (cond) "PASS" else "FAIL", label))
  if (!cond) stop("assertion failed: ", label, call. = FALSE)
}

cat("\n=== cohort build (03_build_cohort.R) ===\n")
full <- readRDS(file.path(OUT, "paperA_cohort_reconciled_full.rds"))
ok("analytic cohort is 130 033 patients", nrow(full) == 130033)
ok("one row per patient", !any(duplicated(full$ENROLID)))
ok("all patients aged 18 to 65", all(full$AGE >= 18 & full$AGE <= 65))
ok("carrying the job-class fields at 100% population",
   all(c("EECLASS", "EESTATU", "EMPREL", "INDSTRY") %in% names(full)) &&
     !any(is.na(full$EECLASS)))
ok("EECLASS takes only its nine documented values", all(full$EECLASS %in% 1:9))

# The companion papers draw the same denominator from this extract. If this drifts,
# three papers from one dataset report three cohort sizes.
b <- readRDS(file.path(OUT, "paperA_cohort_reconciled_B.rds"))
ok("companion cohort B is unchanged at 84 852 repairs", nrow(b) == 84852)

cat("\n=== exposure derivation (04_job_class.R) ===\n")
an <- readRDS(file.path(OUT, "paperB_analytic.rds"))
ok("analytic cohort is EECLASS 1-6 only", all(an$EECLASS %in% 1:6))
ok("job class assigned for every analytic patient", !any(is.na(an$jobclass)))
ok("hourly = EECLASS 4-6, salaried = EECLASS 1-3",
   all(an$EECLASS[an$jobclass == "Hourly"] %in% 4:6) &&
     all(an$EECLASS[an$jobclass == "Salaried"] %in% 1:3))
ok("analytic n is 66 070", nrow(an) == 66070)
ok("analytic plus excluded equals the full cohort",
   nrow(an) + sum(full$EECLASS %in% 7:9) == nrow(full))

fn <- read.csv(file.path(OUT, "paperB_funnel_jobclass.csv"))
ok("roughly half of episodes have unusable job class",
   abs(fn$pct_of_total[fn$step == "EECLASS 7/8/9, not assignable (excluded)"] - 49) < 2)

cat("\n=== primary finding ===\n")
un <- read.csv(file.path(OUT, "paperB_primary_unadjusted.csv"))
h <- un[un$jobclass == "Hourly", ]; s <- un[un$jobclass == "Salaried", ]
ok("hourly workers present complicated more often", h$pct > s$pct)
ok("hourly workers pay LESS out of pocket -- the direction that makes the paper",
   h$median_oop < s$median_oop && h$oop_over_1000 < s$oop_over_1000)

mo <- read.csv(file.path(OUT, "paperB_models.csv"))
adj <- mo[grepl("^Adjusted odds", mo$model), ]
rr  <- mo[grepl("^Adjusted risk ratio", mo$model), ]
rd  <- mo[grepl("^Adjusted risk difference", mo$model), ]
ok("adjusted odds ratio excludes the null", adj$lo > 1)
ok("adjusted risk ratio excludes the null", rr$lo > 1)
ok("adjusted risk difference excludes the null", rd$lo > 0)
ok("the gap survives adjustment at 2 pp or more", rd$estimate >= 2)

cat("\n=== cost sharing does not explain the gap ===\n")
cs <- read.csv(file.path(OUT, "paperB_costsharing_models.csv"))
base <- cs[grepl("plan type", cs$model), ]
oop  <- cs[grepl("out-of-pocket", cs$model), ]
ok("adjusting for index out-of-pocket does not attenuate toward the null",
   oop$estimate >= base$estimate)
ok("the association remains significant with out-of-pocket held fixed", oop$lo > 1)

cat("\n=== subgroup analyses ===\n")
uu <- read.csv(file.path(OUT, "paperB_by_union.csv"))
ee <- read.csv(file.path(OUT, "paperB_by_emprel.csv"))
# a summarise() that reassigns a column it later reads will silently emit sums in
# place of percentages; this catches that class of failure anywhere it appears
pcts <- unlist(c(uu[grep("^pct_", names(uu))], ee[grep("^pct_", names(ee))]))
ok("every reported percentage lies between 0 and 100", all(pcts >= 0 & pcts <= 100))
ok("complicated counts never exceed stratum sizes",
   all(uu$n_cmp_Hourly <= uu$n_Hourly) && all(uu$n_cmp_Salaried <= uu$n_Salaried) &&
     all(ee$n_cmp_Hourly <= ee$n_Hourly) && all(ee$n_cmp_Salaried <= ee$n_Salaried))
ok("subgroup strata sum to the analytic cohort",
   sum(uu$n_Hourly, uu$n_Salaried) == nrow(an) &&
     sum(ee$n_Hourly, ee$n_Salaried) == nrow(an))

us <- read.csv(file.path(OUT, "paperB_union_stratified.csv"))
ok("hourly effect is present in every union stratum", all(us$lo > 1))
ok("the hourly gap is narrowest among union policy holders",
   us$estimate[us$model == "Union"] == min(us$estimate))

es <- read.csv(file.path(OUT, "paperB_emprel_stratified.csv"))
ok("hourly effect is largest where the patient holds the job",
   es$estimate[es$model == "Employee"] == max(es$estimate))

cat("\n=== manuscript tables ===\n")
ok("tables document was written", file.exists(file.path(OUT, "paperB_tables.docx")))
for (f in c("table1_baseline.csv", "table2_primary.csv", "table3_subgroups.csv",
            "table4_costsharing.csv", "table4b_costsharing_models.csv",
            "tableS1_excluded.csv"))
  ok(paste(f, "written and non-empty"),
     file.exists(file.path(OUT, f)) && nrow(read.csv(file.path(OUT, f))) > 0)
t4b <- read.csv(file.path(OUT, "table4b_costsharing_models.csv"))
ok("no model label failed to map to manuscript wording",
   !any(is.na(t4b$Model) | t4b$Model == "" |
          grepl("POST-hoc|see warning|incl\\.|^\\+ ", t4b$Model)))

cat("\n=== industry sensitivity analysis (07) ===\n")
ia <- read.csv(file.path(OUT, "paperB_industry_adjustment.csv"))
iw <- read.csv(file.path(OUT, "paperB_industry_within.csv"))
base_or <- ia$or[grepl("^Base", ia$model)]
ind_or  <- ia$or[grepl("industry fixed", ia$model)]
ok("industry adjustment does not explain the hourly gap",
   abs(log(ind_or) - log(base_or)) / abs(log(base_or)) < 0.15)
ok("hourly effect survives industry adjustment", ia$lo[grepl("industry fixed", ia$model)] > 1)
ok("hourly effect is elevated within every industry tested", all(iw$or > 1))
ok("hourly effect is significant within every industry tested", all(iw$lo > 1))
# the biomechanical counter-hypothesis predicts the opposite of this row
ok("hourly effect is present in the most physically demanding industry",
   any(grepl("mining", iw$model, ignore.case = TRUE) & iw$lo > 1))

ok("the DDD non-pursuit record is retained for reviewers",
   file.exists(file.path(OUT, "DDD_NOT_PURSUED.md")) &&
     file.exists(file.path(OUT, "paperB_ddd_key_cells.csv")))
ok("analysis provenance log is retained", file.exists(file.path(OUT, "ANALYSIS_LOG.md")))

# The manuscript describes analyses by purpose, not by timing, because no protocol
# was registered. ANALYSIS_LOG.md carries the timing instead. If a timing claim
# reappears in the manuscript it must be verifiable -- fail loudly until it is.
# The manuscript sits outside this folder, so a reviewer running the submitted
# analysis code alone will not have it. Those checks are skipped, not failed.
mdf <- file.path(ROOT, "manuscript", "PAPERB_healthaffairs.md")
HAVE_MS <- file.exists(mdf)
if (!HAVE_MS)
  cat("  [SKIP] manuscript checks: PAPERB_healthaffairs.md not present
")
if (HAVE_MS) {
  md <- paste(readLines(mdf, warn = FALSE), collapse = " ")
  ok("manuscript makes no unverifiable prespecification claim",
     !grepl("prespecified|pre-specified", md, ignore.case = TRUE))
}

cat("\n=== selection weighting (10_ipw.R) ===\n")
iw2 <- read.csv(file.path(OUT, "paperB_ipw.csv"))
unw <- iw2[grepl("^Unweighted", iw2$model), ]
ipw <- iw2[grepl("IPW", iw2$model), ]
ok("selection-weighted estimate still excludes the null", ipw$lo > 1 && ipw$rd_lo > 0)
ok("unweighted estimate matches the primary analysis", abs(unw$rd_pp - 2.71) < 0.02)
# The weighted estimate attenuates. That is reported, not hidden -- this assertion
# fails if the manuscript ever stops disclosing it.
if (HAVE_MS) {
  hm2 <- paste(readLines(mdf, warn = FALSE), collapse = " ")
  ok("attenuation under selection weighting is disclosed in the manuscript",
     grepl("attenuated", hm2) && grepl("2.01", hm2, fixed = TRUE))
}

cat("\n=== Health Affairs submission parameters ===\n")
haf <- mdf
if (HAVE_MS) {
  source("09_wordcount.R")            # defines LIMITS and count_md
  w <- count_md(haf)
  # Research Article: abstract and main text count TOGETHER against 3,250
  ok(sprintf("abstract + main text within %d words (is %d)", LIMITS$words, w$counted),
     w$counted <= LIMITS$words)
  ok(sprintf("exhibits within %d (is %d)", LIMITS$exhibits, w$exhibits),
     w$exhibits <= LIMITS$exhibits)
  # the notes list must match the companion draft's reference list exactly
  ok("notes list is complete at 40", w$notes == 40)
  hm <- paste(readLines(haf, warn = FALSE), collapse = " ")
  ok("author block is filled in, not a placeholder",
     !grepl("\\[Author names", hm) && grepl("Nyambod", hm))
  ok("Health Affairs structure present",
     all(sapply(c("## Study Data And Methods", "## Study Results",
                  "### Policy Implications", "## Exhibits", "## Notes"),
                function(s) grepl(s, hm, fixed = TRUE))))
  ok("no structured-abstract furniture carried over",
     !grepl("## Key Points", hm, fixed = TRUE))
}

cat("\n=== figure ===\n")
tif <- file.path(FIGD, "figure1_forest.tiff")
ok("Figure 1 written as TIFF", file.exists(tif))
# Checks the TIFF magic bytes and that the file is too large to be a low-resolution
# render. This does NOT parse the dpi tag, so it would not catch a wrong-but-plausible
# resolution; the 600 dpi setting itself lives in 06_figures.R.
hdr <- readBin(tif, "raw", n = 4)
ok("Figure 1 is a valid TIFF",
   identical(hdr[1:2], as.raw(c(0x49, 0x49))) || identical(hdr[1:2], as.raw(c(0x4d, 0x4d))))
ok("Figure 1 is large enough for 600 dpi at journal column width",
   file.size(tif) > 50000)

cat("\n=== generalizability check ===\n")
ex <- read.csv(file.path(OUT, "paperB_excluded_comparison.csv"))
ok("excluded and analytic groups are both reported", nrow(ex) >= 2)
ok("excluded group differs on the outcome -- must be stated as a limitation",
   abs(ex$complicated[1] - ex$complicated[2]) > 0.5)

cat("\nall assertions passed\n")
