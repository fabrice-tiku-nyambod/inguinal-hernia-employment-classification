# 05_manuscript_tables.R -- publication tables for the employment-classification study.
#
# This is a PRESENTATION layer. It formats estimates computed in 04_job_class.R and
# re-specifies no model; the only numbers computed here are descriptive (counts,
# medians, standardized mean differences). If an estimate is not in a CSV written by
# 04, it does not belong in this script.
#
# Input : output/paperB_*.csv, output/paperB_analytic.rds
# Output: output/paperB_tables.docx  +  output/tableN_*.csv

source("01_config.R")

options(warn = 1)
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(officer); library(flextable)
})

SERIF <- "Times New Roman"   # global rule: serif in every figure and table
rd <- function(f) read.csv(file.path(OUT, f), check.names = FALSE)

# AMA numeric conventions: no leading zero on P, three decimals, "<.001" floor.
fmt_p <- function(p) ifelse(is.na(p), "",
                     ifelse(p < .001, "<.001", sub("^0\\.", ".", sprintf("%.3f", p))))
fmt_ci <- function(e, lo, hi, d = 2) sprintf(paste0("%.", d, "f (%.", d, "f-%.", d, "f)"),
                                             e, lo, hi)
n_pct <- function(n, p) sprintf("%s (%.1f)", format(n, big.mark = ",", trim = TRUE), p)

an <- readRDS(file.path(OUT, "paperB_analytic.rds"))
H <- an$jobclass == "Hourly"

# ---------------------------------------------------------------------------
# Table 1. Baseline characteristics by job class
# ---------------------------------------------------------------------------
# Standardized mean differences rather than P values: at n = 66,070 every trivial
# imbalance is "significant", so P values here would mislead rather than inform.
smd_bin  <- function(p1, p0) (p1 - p0) / sqrt((p1 * (1 - p1) + p0 * (1 - p0)) / 2)
smd_cont <- function(x) (mean(x[H]) - mean(x[!H])) /
  sqrt((var(x[H]) + var(x[!H])) / 2)

row_cont <- function(var, label) {
  x <- an[[var]]
  data.frame(Characteristic = label, Level = "Median (IQR)",
             Hourly   = sprintf("%s (%s-%s)", round(median(x[H])),
                                round(quantile(x[H], .25)), round(quantile(x[H], .75))),
             Salaried = sprintf("%s (%s-%s)", round(median(x[!H])),
                                round(quantile(x[!H], .25)), round(quantile(x[!H], .75))),
             SMD = sprintf("%.3f", abs(smd_cont(x))))
}
row_cat <- function(var, label, levels_keep = NULL) {
  x <- as.character(an[[var]])
  lv <- if (is.null(levels_keep)) sort(unique(x)) else levels_keep
  out <- lapply(seq_along(lv), function(i) {
    l <- lv[i]
    n1 <- sum(x == l & H); n0 <- sum(x == l & !H)
    p1 <- n1 / sum(H);     p0 <- n0 / sum(!H)
    data.frame(Characteristic = if (i == 1) label else "", Level = l,
               Hourly = n_pct(n1, 100 * p1), Salaried = n_pct(n0, 100 * p0),
               SMD = sprintf("%.3f", abs(smd_bin(p1, p0))))
  })
  bind_rows(out)
}

t1 <- bind_rows(
  row_cont("AGE", "Age, y"),
  row_cat("sex", "Sex") %>% mutate(Level = recode(Level, "1" = "Male", "2" = "Female")),
  row_cat("region", "Census region"),
  row_cat("plan", "Health plan type"),
  row_cat("yearf", "Year of surgery"),
  row_cat("emprel_lab", "Patient relation to policy holder"),
  row_cat("union", "Union status of policy holder"),
  row_cat("approach", "Operative approach"),
  row_cont("allowed_r", "Allowed amount, 2021 US$"),
  row_cont("oop_r", "Out-of-pocket cost, 2021 US$"),
  # row_cat labels only its first level; these keep just the TRUE row, so the
  # label has to be reinstated after the filter
  row_cat("bilateral", "Bilateral repair") %>% filter(Level == "TRUE") %>%
    mutate(Level = "Yes", Characteristic = "Bilateral repair"),
  row_cat("recurrent", "Recurrent repair") %>% filter(Level == "TRUE") %>%
    mutate(Level = "Yes", Characteristic = "Recurrent repair"))
names(t1)[3:4] <- c(sprintf("Hourly (n = %s)", format(sum(H), big.mark = ",")),
                    sprintf("Salaried (n = %s)", format(sum(!H), big.mark = ",")))
write.csv(t1, file.path(OUT, "table1_baseline.csv"), row.names = FALSE)

# ---------------------------------------------------------------------------
# Table 2. Job class and complicated presentation
# ---------------------------------------------------------------------------
un <- rd("paperB_primary_unadjusted.csv")
mo <- rd("paperB_models.csv")
h <- un[un$jobclass == "Hourly", ]; s <- un[un$jobclass == "Salaried", ]
g <- function(pat) mo[grepl(pat, mo$model), ]
aor <- g("^Adjusted odds"); arr <- g("^Adjusted risk ratio"); ard <- g("^Adjusted risk diff")
cor_ <- g("^Crude odds")

t2 <- bind_rows(
  data.frame(Measure = "Complicated presentation, No. (%)",
             Hourly = n_pct(h$complicated, h$pct), Salaried = n_pct(s$complicated, s$pct),
             Estimate = "", `P value` = "", check.names = FALSE),
  data.frame(Measure = "Unadjusted risk difference, pp (95% CI)", Hourly = "", Salaried = "",
             Estimate = fmt_ci(h$crude_rd_pp, h$crude_rd_lo, h$crude_rd_hi),
             `P value` = fmt_p(h$chisq_p), check.names = FALSE),
  data.frame(Measure = "Unadjusted odds ratio (95% CI)", Hourly = "", Salaried = "",
             Estimate = fmt_ci(cor_$estimate, cor_$lo, cor_$hi),
             `P value` = fmt_p(cor_$p), check.names = FALSE),
  data.frame(Measure = "Adjusted risk difference, pp (95% CI)", Hourly = "", Salaried = "",
             Estimate = fmt_ci(ard$estimate, ard$lo, ard$hi),
             `P value` = "", check.names = FALSE),
  data.frame(Measure = "Adjusted odds ratio (95% CI)", Hourly = "", Salaried = "",
             Estimate = fmt_ci(aor$estimate, aor$lo, aor$hi),
             `P value` = fmt_p(aor$p), check.names = FALSE),
  data.frame(Measure = "Adjusted risk ratio (95% CI)", Hourly = "", Salaried = "",
             Estimate = fmt_ci(arr$estimate, arr$lo, arr$hi),
             `P value` = fmt_p(arr$p), check.names = FALSE))
write.csv(t2, file.path(OUT, "table2_primary.csv"), row.names = FALSE)

# ---------------------------------------------------------------------------
# Table 3. Subgroup analyses
# ---------------------------------------------------------------------------
us <- rd("paperB_union_stratified.csv"); uu <- rd("paperB_by_union.csv")
es <- rd("paperB_emprel_stratified.csv"); ee <- rd("paperB_by_emprel.csv")

# both subgroup CSVs carry the same schema (n_*, cmp_*, pct_*, gap_pp) by construction
# in 04, so one code path formats both blocks.
sub_block <- function(strat, raw, key, header, order) {
  raw   <- raw[match(order, raw[[key]]), ]
  strat <- strat[match(order, strat$model), ]
  blank <- data.frame(Subgroup = header, Hourly = "", Salaried = "", Difference = "",
                      `Adjusted OR (95% CI)` = "", `P value` = "", check.names = FALSE)
  bind_rows(blank,
    data.frame(Subgroup = paste0("  ", order),
               Hourly   = sprintf("%s/%s (%.1f)",
                                  format(raw$n_cmp_Hourly, big.mark = ",", trim = TRUE),
                                  format(raw$n_Hourly, big.mark = ",", trim = TRUE),
                                  raw$pct_Hourly),
               Salaried = sprintf("%s/%s (%.1f)",
                                  format(raw$n_cmp_Salaried, big.mark = ",", trim = TRUE),
                                  format(raw$n_Salaried, big.mark = ",", trim = TRUE),
                                  raw$pct_Salaried),
               Difference = sprintf("%.2f", raw$gap_pp),
               `Adjusted OR (95% CI)` = fmt_ci(strat$estimate, strat$lo, strat$hi),
               `P value` = fmt_p(strat$p), check.names = FALSE))
}

t3 <- bind_rows(
  sub_block(us, uu, "union", "Union status of policy holder",
            c("Non-union", "Union", "Other")),
  data.frame(Subgroup = sprintf("  Interaction P = %s", fmt_p(us$interaction_p[1])),
             Hourly = "", Salaried = "", Difference = "",
             `Adjusted OR (95% CI)` = "", `P value` = "", check.names = FALSE),
  sub_block(es, ee, "emprel_lab", "Patient relation to policy holder",
            c("Employee", "Spouse", "Child/other")),
  data.frame(Subgroup = sprintf("  Interaction P = %s", fmt_p(es$interaction_p[1])),
             Hourly = "", Salaried = "", Difference = "",
             `Adjusted OR (95% CI)` = "", `P value` = "", check.names = FALSE))
write.csv(t3, file.path(OUT, "table3_subgroups.csv"), row.names = FALSE)

# ---------------------------------------------------------------------------
# Table 4. Cost sharing
# ---------------------------------------------------------------------------
cs <- rd("paperB_costsharing.csv"); cm <- rd("paperB_costsharing_models.csv")
ch <- cs[cs$jobclass == "Hourly", ]; cl <- cs[cs$jobclass == "Salaried", ]
t4 <- bind_rows(
  data.frame(Measure = "Median out-of-pocket cost, US$",
             Hourly = format(ch$median_oop, big.mark = ","),
             Salaried = format(cl$median_oop, big.mark = ",")),
  data.frame(Measure = "Mean out-of-pocket cost, US$",
             Hourly = format(ch$mean_oop, big.mark = ","),
             Salaried = format(cl$mean_oop, big.mark = ",")),
  data.frame(Measure = "Out-of-pocket cost >$1000, %",
             Hourly = sprintf("%.1f", ch$oop_over_1000),
             Salaried = sprintf("%.1f", cl$oop_over_1000)),
  data.frame(Measure = "No out-of-pocket liability, %",
             Hourly = sprintf("%.1f", ch$pct_zero_oop),
             Salaried = sprintf("%.1f", cl$pct_zero_oop)),
  data.frame(Measure = "Mean deductible paid, US$",
             Hourly = format(ch$mean_deduct, big.mark = ","),
             Salaried = format(cl$mean_deduct, big.mark = ",")),
  data.frame(Measure = "Enrolled in CDHP or HDHP, %",
             Hourly = sprintf("%.1f", ch$pct_hdhp),
             Salaried = sprintf("%.1f", cl$pct_hdhp)))
MODEL_LABELS <- c(
  "No benefit-design adjustment" =
    "Demographic adjustment only (age, sex, region, year)",
  "+ plan type (incl. CDHP/HDHP) = primary model" =
    "Adding health plan type (prospective benefit design); primary model",
  "+ log index out-of-pocket (POST-hoc, see warning)" =
    "Adding realized out-of-pocket cost at the index encounter")
t4b <- cm %>% transmute(Model = MODEL_LABELS[model],
                        `Hourly OR (95% CI)` = fmt_ci(estimate, lo, hi),
                        `P value` = fmt_p(p))
stopifnot(!any(is.na(t4b$Model)))   # a renamed model in 04 must not silently blank out
write.csv(t4, file.path(OUT, "table4_costsharing.csv"), row.names = FALSE)
write.csv(t4b, file.path(OUT, "table4b_costsharing_models.csv"), row.names = FALSE)

# ---------------------------------------------------------------------------
# Table 5. Industry gradient and the within-industry hourly effect
# ---------------------------------------------------------------------------
# 07 writes these as raw estimates. They are formatted here, in the presentation
# layer, so that no raw p-value or unrounded ratio reaches an exhibit.
ig <- rd("paperB_industry_gradient.csv")
iw <- rd("paperB_industry_within.csv")

t5 <- ig %>%
  transmute(Industry = industry,
            `No.` = format(n, big.mark = ",", trim = TRUE),
            `Hourly, %` = sprintf("%.1f", pct_hourly),
            `Complicated, No. (%)` = sprintf("%s (%.1f)",
                                             format(n_cmp, big.mark = ",", trim = TRUE),
                                             pct_cmp),
            `Hourly %` = sprintf("%.1f", pct_cmp_hourly),
            `Salaried %` = sprintf("%.1f", pct_cmp_salaried)) %>%
  left_join(iw %>% transmute(Industry = model,
                             `Hourly OR (95% CI)` = fmt_ci(or, lo, hi),
                             `P value` = fmt_p(p)),
            by = "Industry") %>%
  mutate(across(everything(), ~ ifelse(is.na(.x), "NA", .x)))

# industries too small to model carry a footnote marker rather than a blank cell
t5$`Hourly OR (95% CI)`[t5$`Hourly OR (95% CI)` == "NA"] <- "Not modeledᵃ"
t5$`P value`[t5$`P value` == "NA"] <- ""
write.csv(t5, file.path(OUT, "table5_industry.csv"), row.names = FALSE)

# ---------------------------------------------------------------------------
# Table S1. Analytic vs excluded
# ---------------------------------------------------------------------------
ex <- rd("paperB_excluded_comparison.csv")
ts1 <- ex %>% transmute(Group = grp, `No.` = format(n, big.mark = ","),
                        `Mean age, y` = age, `Male, %` = male,
                        `South region, %` = south, `CDHP/HDHP, %` = hdhp,
                        `Median out-of-pocket, US$` = format(median_oop, big.mark = ","),
                        `Inpatient, %` = inpatient, `Complicated, %` = complicated)
write.csv(ts1, file.path(OUT, "tableS1_excluded.csv"), row.names = FALSE)

# ---------------------------------------------------------------------------
# Assemble the Word document
# ---------------------------------------------------------------------------
mkft <- function(df, widths = NULL) {
  ft <- flextable(df) %>% theme_booktabs() %>%
    font(fontname = SERIF, part = "all") %>% fontsize(size = 9, part = "all") %>%
    bold(part = "header") %>% padding(padding.top = 2, padding.bottom = 2, part = "all") %>%
    align(align = "left", part = "all") %>%
    align(j = 2:ncol(df), align = "right", part = "all")
  if (is.null(widths)) autofit(ft) else width(ft, width = widths)
}
ttl <- function(doc, x) body_add_par(doc, x, style = "heading 2")
note <- function(doc, x) body_add_par(doc, x, style = "Normal")

doc <- read_docx() %>%
  body_add_par("Job class and complicated presentation of inguinal hernia", style = "heading 1") %>%
  body_add_par(sprintf("Tables generated %s from the reconciled MarketScan cohort (N = 130,033; job class assignable in %s).",
                       format(Sys.Date(), "%d %B %Y"), format(nrow(an), big.mark = ",")),
               style = "Normal") %>%

  ttl("Table 1. Baseline characteristics by job class") %>%
  body_add_flextable(mkft(t1)) %>%
  note("Abbreviations: CDHP, consumer-directed health plan; HDHP, high-deductible health plan; IQR, interquartile range; SMD, standardized mean difference.") %>%
  note("Job class is derived from EECLASS, which records the employment classification of the policy holder rather than of the patient. Standardized mean differences are reported in place of P values because at this sample size trivial imbalances reach statistical significance. Costs are expressed in constant 2021 US dollars using the CPI-U.") %>%
  body_add_break() %>%

  ttl("Table 2. Job class and complicated presentation at the index encounter") %>%
  body_add_flextable(mkft(t2)) %>%
  note("Abbreviations: CI, confidence interval; pp, percentage points.") %>%
  note("Complicated presentation is defined as ICD-10-CM K40.0, K40.1, K40.3, or K40.4 (obstruction or gangrene) recorded on the repair claim line. Adjusted models include age (restricted cubic spline, 4 knots), sex, census region, health plan type, and year of surgery. The adjusted risk difference is the average marginal effect obtained by g-computation with a delta-method confidence interval; the adjusted risk ratio is from a modified Poisson model with robust variance. Comorbidity adjustment is not possible in this extract.") %>%
  body_add_break() %>%

  ttl("Table 3. Subgroup analyses") %>%
  body_add_flextable(mkft(t3)) %>%
  note("Values in the Hourly and Salaried columns are the number of patients with complicated presentation divided by the number of patients in the stratum (%). Difference is the absolute difference in percentage points. Adjusted odds ratios are from the primary model fitted within each stratum. Interaction P values are from likelihood ratio tests against the primary model. Union status and relation to the policy holder both describe the policy holder's employment, not the patient's own, except where the patient is the employee.") %>%
  body_add_break() %>%

  ttl("Table 4. Cost sharing at the index encounter, by job class") %>%
  body_add_flextable(mkft(t4)) %>%
  body_add_par("", style = "Normal") %>%
  body_add_flextable(mkft(t4b)) %>%
  note("Out-of-pocket cost is the sum of copayment, coinsurance, and deductible for the index encounter, in constant 2021 US dollars. Because this cost is realized at or after the presentation it is used to explain, and scales with the complexity of care delivered, it is reported as a sensitivity analysis rather than as a mediator. Health plan type, which is fixed before the encounter, is the prospective measure of benefit design and is included in the primary model.") %>%
  body_add_break() %>%

  ttl("Table S1. Comparison of the analytic cohort with patients excluded for unassignable job class") %>%
  body_add_flextable(mkft(ts1)) %>%
  note("Patients with EECLASS values 7, 8, or 9 cannot be classified as hourly or salaried and are excluded from the analytic cohort. They differ from included patients on geography, benefit design, and the study outcome, which constrains generalizability.")

print(doc, target = file.path(OUT, "paperB_tables.docx"))
cat("\nWrote paperB_tables.docx and 6 table CSVs to", OUT, "\n")
