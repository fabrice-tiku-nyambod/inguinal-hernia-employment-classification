# 04_job_class.R -- "Employment Classification and Complicated Presentation" primary analysis.
#
# Question: among commercially insured adults undergoing inguinal hernia repair,
# do hourly workers present with complicated disease (K40.0/.1/.3/.4, obstruction
# or gangrene) more often than salaried workers, and does out-of-pocket cost
# explain the difference?
#
# Input : output/paperA_cohort_reconciled_full.rds  (130 033 patients, 03_build_cohort.R)
# Output: output/paperB_*.csv
#
# Everything here is measured AT the index encounter. Nothing requiring follow-up
# or lookback is computable in this extract -- see MARKETSCAN_CAVEATS.md.

source("01_config.R")

options(warn = 1)
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(splines); library(sandwich); library(lmtest)
})

pct  <- function(x, d = 1) round(100 * mean(x), d)
wr   <- function(x, f) { write.csv(x, file.path(OUT, f), row.names = FALSE); invisible(x) }
show <- function(title, x) {
  cat("\n================ ", title, " ================\n", sep = "")
  print(as.data.frame(x), row.names = FALSE)
}

# ---------------------------------------------------------------------------
# 1. Exposure derivation
# ---------------------------------------------------------------------------
# EECLASS, verified in START_HERE.md:
#   1 salary non-union, 2 salary union, 3 salary other   -> Salaried
#   4 hourly non-union, 5 hourly union, 6 hourly other   -> Hourly
#   7 non-union, 8 union, 9 unknown                      -> job class not assignable
REGION_LABELS <- c("1" = "Northeast", "2" = "North Central", "3" = "South",
                   "4" = "West", "5" = "Unknown")

d <- readRDS(file.path(OUT, "paperA_cohort_reconciled_full.rds")) %>%
  mutate(
    jobclass = case_when(EECLASS %in% 1:3 ~ "Salaried",
                         EECLASS %in% 4:6 ~ "Hourly",
                         TRUE ~ NA_character_),
    union    = case_when(EECLASS %in% c(2, 5) ~ "Union",
                         EECLASS %in% c(1, 4) ~ "Non-union",
                         EECLASS %in% c(3, 6) ~ "Other",
                         TRUE ~ NA_character_),
    hourly   = as.integer(jobclass == "Hourly"),
    region   = factor(REGION_LABELS[as.character(REGION)]),
    yearf    = factor(year),
    plan     = factor(plan),
    sex      = factor(SEX),
    cmp      = as.integer(complicated),
    # pre-encounter cost-sharing proxy: consumer-directed / high-deductible design
    hdhp     = as.integer(plan %in% c("CDHP", "HDHP")))

stopifnot(nrow(d) == 130033)

an  <- d %>% filter(!is.na(jobclass))          # analytic cohort
exc <- d %>% filter(is.na(jobclass))           # EECLASS 7/8/9

funnel <- tribble(
  ~step,                                              ~n,
  "index repairs, reconciled cohort",                 nrow(d),
  "EECLASS 1-6, job class assignable (ANALYTIC)",      nrow(an),
  "  Salaried (EECLASS 1-3)",                          sum(an$jobclass == "Salaried"),
  "  Hourly (EECLASS 4-6)",                            sum(an$jobclass == "Hourly"),
  "EECLASS 7/8/9, not assignable (excluded)",          nrow(exc)) %>%
  mutate(pct_of_total = round(100 * n / nrow(d), 1))
show("EXPOSURE FUNNEL", funnel); wr(funnel, "paperB_funnel_jobclass.csv")

# ---------------------------------------------------------------------------
# 2. Table 1 -- cohort characteristics by job class
# ---------------------------------------------------------------------------
cat_rows <- function(df, var, label) {
  df %>% count(jobclass, lev = as.character(.data[[var]])) %>%
    group_by(jobclass) %>% mutate(v = sprintf("%s (%.1f)", format(n, big.mark = " "),
                                              100 * n / sum(n))) %>%
    ungroup() %>% select(jobclass, lev, v) %>%
    pivot_wider(names_from = jobclass, values_from = v) %>%
    mutate(variable = label, .before = 1) %>% rename(level = lev)
}
num_row <- function(df, var, label) {
  df %>% group_by(jobclass) %>%
    summarise(v = sprintf("%.0f (%.0f-%.0f)", median(.data[[var]]),
                          quantile(.data[[var]], .25), quantile(.data[[var]], .75)),
              .groups = "drop") %>%
    pivot_wider(names_from = jobclass, values_from = v) %>%
    mutate(variable = label, level = "median (IQR)", .before = 1)
}

t1 <- bind_rows(
  num_row(an, "AGE", "Age, years"),
  cat_rows(an, "sex", "Sex"),
  cat_rows(an, "region", "Census region"),
  cat_rows(an, "plan", "Plan type"),
  cat_rows(an, "yearf", "Year of surgery"),
  cat_rows(an, "approach", "Surgical approach"),
  cat_rows(an, "union", "Union status"),
  num_row(an, "allowed_r", "Allowed amount, 2021 USD"),
  num_row(an, "oop_r", "Out-of-pocket, 2021 USD"),
  cat_rows(an, "bilateral", "Bilateral repair"),
  cat_rows(an, "recurrent", "Recurrent repair"),
  cat_rows(an, "complicated", "Complicated presentation"))
show("TABLE 1 -- n (%) or median (IQR), by job class", t1)
wr(t1, "paperB_table1.csv")

# ---------------------------------------------------------------------------
# 3. Primary outcome, unadjusted
# ---------------------------------------------------------------------------
un <- an %>% group_by(jobclass) %>%
  summarise(n = n(), complicated = sum(cmp), pct = pct(cmp, 2),
            median_oop = round(median(oop_r)), oop_over_1000 = pct(oop_r > 1000, 1),
            .groups = "drop")
tab <- table(an$jobclass, an$cmp)
cs  <- chisq.test(tab)
p_h <- mean(an$cmp[an$hourly == 1]); p_s <- mean(an$cmp[an$hourly == 0])
n_h <- sum(an$hourly == 1);          n_s <- sum(an$hourly == 0)
se  <- sqrt(p_h * (1 - p_h) / n_h + p_s * (1 - p_s) / n_s)
rd  <- p_h - p_s

show("PRIMARY OUTCOME, UNADJUSTED", un)
cat(sprintf("\n  crude risk difference (hourly - salaried) = %.2f pp (95%% CI %.2f to %.2f)\n",
            100 * rd, 100 * (rd - 1.96 * se), 100 * (rd + 1.96 * se)))
cat(sprintf("  crude risk ratio = %.3f | chi-square P = %.3g\n", p_h / p_s, cs$p.value))
wr(un %>% mutate(crude_rd_pp = round(100 * rd, 2),
                 crude_rd_lo = round(100 * (rd - 1.96 * se), 2),
                 crude_rd_hi = round(100 * (rd + 1.96 * se), 2),
                 chisq_p = signif(cs$p.value, 3)), "paperB_primary_unadjusted.csv")

# ---------------------------------------------------------------------------
# 4. Adjusted association
# ---------------------------------------------------------------------------
# Adjustment set from START_HERE: age, sex, census region, plan type, year.
# Age enters as a restricted cubic spline; the age-complication relationship is
# not assumed linear. No comorbidity adjustment is possible (see caveats).
F_BASE <- cmp ~ hourly + ns(AGE, 4) + sex + region + plan + yearf

# average marginal effect of `hourly` on the probability scale, by g-computation,
# with a delta-method standard error (no bootstrap needed for a smooth AME)
ame <- function(fit, data) {
  d1 <- transform(data, hourly = 1); d0 <- transform(data, hourly = 0)
  X1 <- model.matrix(delete.response(terms(fit)), d1)
  X0 <- model.matrix(delete.response(terms(fit)), d0)
  b  <- coef(fit); V <- vcov(fit)
  p1 <- plogis(X1 %*% b); p0 <- plogis(X0 %*% b)
  g  <- colMeans(as.vector(p1 * (1 - p1)) * X1 - as.vector(p0 * (1 - p0)) * X0)
  est <- mean(p1 - p0); s <- sqrt(drop(t(g) %*% V %*% g))
  c(rd = est, lo = est - 1.96 * s, hi = est + 1.96 * s)
}
or_row <- function(fit, label, robust = FALSE, term = "hourly") {
  V  <- if (robust) vcovHC(fit, type = "HC0") else vcov(fit)
  ct <- coeftest(fit, vcov. = V)[term, ]
  data.frame(model = label, estimate = exp(ct[1]),
             lo = exp(ct[1] - 1.96 * ct[2]), hi = exp(ct[1] + 1.96 * ct[2]),
             p = ct[4])
}

m_crude <- glm(cmp ~ hourly, binomial, an)
m_adj   <- glm(F_BASE, binomial, an)
# modified Poisson: adjusted RISK RATIO, robust SE. Preferred over the OR when the
# outcome is ~10% and the OR would overstate the relative effect.
m_pois  <- glm(F_BASE, poisson(link = "log"), an)

rd_adj <- ame(m_adj, an)
mods <- bind_rows(
  or_row(m_crude, "Crude odds ratio"),
  or_row(m_adj,   "Adjusted odds ratio (age, sex, region, plan, year)"),
  or_row(m_pois,  "Adjusted risk ratio (modified Poisson, robust SE)", robust = TRUE)) %>%
  mutate(across(c(estimate, lo, hi), ~ round(.x, 3)), p = signif(p, 3))

show("ADJUSTED ASSOCIATION -- hourly vs salaried", mods)
cat(sprintf("\n  adjusted risk difference (g-computation) = %.2f pp (95%% CI %.2f to %.2f)\n",
            100 * rd_adj["rd"], 100 * rd_adj["lo"], 100 * rd_adj["hi"]))
cat(sprintf("  crude was %.2f pp -- the gap must survive adjustment to be reportable\n",
            100 * rd))
wr(bind_rows(mods, data.frame(model = "Adjusted risk difference, pp (g-computation)",
                              estimate = round(100 * rd_adj["rd"], 2),
                              lo = round(100 * rd_adj["lo"], 2),
                              hi = round(100 * rd_adj["hi"], 2), p = NA)),
   "paperB_models.csv")

# does the association differ by union status?
m_int <- glm(update(F_BASE, . ~ . + hourly:union + union), binomial, an)
cat("\n  hourly x union interaction, likelihood-ratio P = ",
    signif(anova(m_adj, m_int, test = "LRT")$`Pr(>Chi)`[2], 3), "\n", sep = "")

# column names here are mirrored in paperB_by_emprel.csv so 05 can format both blocks
# through one code path: n_*, cmp_*, pct_*, gap_pp
by_u <- an %>% group_by(union, jobclass) %>%
  summarise(n = n(), n_cmp = sum(cmp), pct = pct(cmp, 2), .groups = "drop") %>%
  pivot_wider(names_from = jobclass, values_from = c(n, n_cmp, pct)) %>%
  mutate(gap_pp = round(pct_Hourly - pct_Salaried, 2))
show("COMPLICATED PRESENTATION BY UNION STATUS AND JOB CLASS", by_u)
wr(by_u, "paperB_by_union.csv")

# adjusted hourly effect within each union stratum, for the subgroup table
strat_fit <- function(df, var) {
  bind_rows(lapply(sort(unique(df[[var]])), function(l) {
    s <- df %>% filter(.data[[var]] == l) %>% mutate(across(where(is.factor), droplevels))
    if (nrow(s) < 500 || n_distinct(s$hourly) < 2) return(NULL)
    or_row(glm(F_BASE, binomial, s), as.character(l)) %>%
      mutate(n = nrow(s), .after = model)
  })) %>% mutate(across(c(estimate, lo, hi), ~ round(.x, 3)), p = signif(p, 3))
}
u_strat <- strat_fit(an, "union") %>%
  mutate(interaction_p = signif(anova(m_adj, m_int, test = "LRT")$`Pr(>Chi)`[2], 3))
show("ADJUSTED HOURLY ODDS RATIO, STRATIFIED BY UNION STATUS", u_strat)
wr(u_strat, "paperB_union_stratified.csv")

# ---------------------------------------------------------------------------
# 5. Excluded EECLASS 7/8/9 vs the analytic cohort
# ---------------------------------------------------------------------------
# If the excluded half differs systematically on the outcome, the analytic
# cohort is not representative of the extract and that limits generalizability.
cmp_grp <- function(df, g) {
  df %>% mutate(grp = g) %>% group_by(grp) %>%
    summarise(n = n(), age = round(mean(AGE), 1), male = pct(sex == "1"),
              south = pct(region == "South"), hdhp = pct(hdhp == 1),
              median_oop = round(median(oop_r)), median_allowed = round(median(allowed_r)),
              inpatient = pct(is.na(setting)), complicated = pct(cmp, 2), .groups = "drop")
}
exc_cmp <- bind_rows(cmp_grp(an, "Analytic (EECLASS 1-6)"),
                     cmp_grp(exc, "Excluded (EECLASS 7/8/9)"),
                     cmp_grp(d %>% filter(EECLASS == 9), "  of which EECLASS 9 (unknown)"))
show("EXCLUDED VS ANALYTIC COHORT", exc_cmp); wr(exc_cmp, "paperB_excluded_comparison.csv")
cat(sprintf("\n  complicated, analytic %.2f%% vs excluded %.2f%%, P = %.3g\n",
            100 * mean(an$cmp), 100 * mean(exc$cmp),
            prop.test(c(sum(an$cmp), sum(exc$cmp)), c(nrow(an), nrow(exc)))$p.value))

# ---------------------------------------------------------------------------
# 6. Does cost sharing explain the gap?
# ---------------------------------------------------------------------------
# TEMPORALITY WARNING. oop_r is the patient's liability FOR THE INDEX ENCOUNTER,
# so it is realized at or after the presentation it is meant to explain -- a
# complicated presentation drives more expensive care, which drives higher
# out-of-pocket cost. It is therefore NOT a mediator in the causal sense, and the
# analysis below is reported as a proxy for benefit generosity, not as mediation.
# `hdhp` (CDHP/HDHP plan) is the one cost-sharing feature fixed BEFORE the
# encounter, so it carries the pre-encounter interpretation.
# 10 encounters carry a negative net out-of-pocket (claim reversals); floored at 0
# for the log transform only, never dropped.
an <- an %>% mutate(oop_pos = pmax(oop_r, 0), deduct_r = deduct * defl)

cs_tab <- an %>% group_by(jobclass) %>%
  summarise(n = n(), median_oop = round(median(oop_r)), mean_oop = round(mean(oop_r)),
            oop_over_1000 = pct(oop_r > 1000), pct_zero_oop = pct(oop_r <= 0),
            mean_deduct = round(mean(deduct_r)), pct_any_deduct = pct(deduct_r > 0),
            pct_hdhp = pct(hdhp == 1), .groups = "drop")
show("COST SHARING BY JOB CLASS", cs_tab); wr(cs_tab, "paperB_costsharing.csv")

# Benefit design is already in the base model: `plan` carries the CDHP/HDHP levels,
# so a separate hdhp term is aliased with it. The informative contrast is therefore
# the model WITHOUT plan type against the model WITH it -- if adjusting for benefit
# design leaves the hourly estimate untouched, cost-sharing design is not the path.
F_NOPLAN <- cmp ~ hourly + ns(AGE, 4) + sex + region + yearf
m_noplan <- glm(F_NOPLAN, binomial, an)
m_oop    <- glm(update(F_BASE, . ~ . + log1p(oop_pos)), binomial, an)

cie <- bind_rows(
  or_row(m_noplan, "No benefit-design adjustment"),
  or_row(m_adj,    "+ plan type (incl. CDHP/HDHP) = primary model"),
  or_row(m_oop,    "+ log index out-of-pocket (POST-hoc, see warning)")) %>%
  mutate(across(c(estimate, lo, hi), ~ round(.x, 3)), p = signif(p, 3))
show("HOURLY ODDS RATIO WITH AND WITHOUT COST-SHARING ADJUSTMENT", cie)
cat(sprintf("\n  change in the hourly log-odds on adding plan type:            %+.1f%%\n",
            100 * (coef(m_adj)["hourly"] - coef(m_noplan)["hourly"]) / coef(m_noplan)["hourly"]))
cat(sprintf("  change in the hourly log-odds on adding index out-of-pocket: %+.1f%%\n",
            100 * (coef(m_oop)["hourly"] - coef(m_adj)["hourly"]) / coef(m_adj)["hourly"]))
cat("  a change near zero means cost sharing does not account for the job-class gap;\n")
cat("  a change AWAY from the null means the gap widens once cost sharing is held fixed\n")

# HDHP enrollment against the outcome, estimated where it is not aliased
hd <- or_row(glm(update(F_NOPLAN, . ~ . + hdhp), binomial, an), "x", term = "hdhp")
cat(sprintf("\n  HDHP/CDHP enrollment vs complicated presentation: OR %.3f (%.3f-%.3f), P = %.3g\n",
            hd$estimate, hd$lo, hd$hi, hd$p))
wr(cie, "paperB_costsharing_models.csv")

# ---------------------------------------------------------------------------
# 7. Secondary exposures -- EESTATU and EMPREL
# ---------------------------------------------------------------------------
# Value labels verified 13 August 2026 against the IBM Watson Health CCAE/MDCR
# Data Dictionary (2019 edition, 508-compliant copy at
# Desktop/JSCOR/old/IRB training resources/, pages 18-19). That copy carries a
# real text layer -- see MARKETSCAN_CAVEATS.md.
EESTATU_LABELS <- c("1" = "Active full time", "2" = "Active part time or seasonal",
                    "3" = "Early retiree", "4" = "Medicare-eligible retiree",
                    "5" = "Retiree, status unknown", "6" = "COBRA continuee",
                    "7" = "Long-term disability", "8" = "Surviving spouse/dependent",
                    "9" = "Other/unknown")
EMPREL_LABELS  <- c("1" = "Employee", "2" = "Spouse", "3" = "Child/other",
                    "4" = "Dependent, relation unknown")

sec <- bind_rows(
  d %>% count(code = EESTATU) %>% mutate(variable = "EESTATU (employment status)",
                                         label = EESTATU_LABELS[as.character(code)]),
  d %>% count(code = EMPREL) %>% mutate(variable = "EMPREL (relation to employee)",
                                        label = EMPREL_LABELS[as.character(code)])) %>%
  left_join(bind_rows(
    d %>% group_by(code = EESTATU) %>% summarise(p = pct(cmp, 2), .groups = "drop") %>%
      mutate(variable = "EESTATU (employment status)"),
    d %>% group_by(code = EMPREL) %>% summarise(p = pct(cmp, 2), .groups = "drop") %>%
      mutate(variable = "EMPREL (relation to employee)")),
    by = c("variable", "code")) %>%
  transmute(variable, code, label, n, pct_complicated = p)
show("SECONDARY EXPOSURES (labels verified against the data dictionary)", sec)
wr(sec, "paperB_secondary_exposures.csv")

# ---------------------------------------------------------------------------
# 8. Whose job is it? -- the EMPREL restriction
# ---------------------------------------------------------------------------
# The dictionary is explicit: EECLASS is "the employment classification of the
# PRIMARY BENEFICIARY, also coded on spouse and dependent claims". EMPREL gives
# the patient's relation to that beneficiary. So job class is the patient's OWN
# job only when EMPREL = 1; for a spouse or child it is a household attribute
# belonging to someone else.
#
# This is a mechanism test, not a nuisance. Unpaid leave and shift inflexibility
# constrain the person who holds the job. If the hourly gap is driven by the
# patient's own working conditions it should be widest among employees and
# attenuate among spouses and dependents, who do not work that schedule.
an <- an %>% mutate(emprel_lab = factor(EMPREL_LABELS[as.character(EMPREL)],
                                        levels = EMPREL_LABELS))

emp_tab <- an %>% group_by(emprel_lab, jobclass) %>%
  summarise(n = n(), n_cmp = sum(cmp), pct = pct(cmp, 2), .groups = "drop") %>%
  pivot_wider(names_from = jobclass, values_from = c(n, n_cmp, pct)) %>%
  mutate(gap_pp = round(pct_Hourly - pct_Salaried, 2))
show("COMPLICATED PRESENTATION BY PATIENT'S RELATION TO THE EMPLOYEE", emp_tab)
wr(emp_tab, "paperB_by_emprel.csv")

m_emp <- glm(update(F_BASE, . ~ . + hourly:emprel_lab + emprel_lab), binomial, an)
p_emp <- signif(anova(m_adj, m_emp, test = "LRT")$`Pr(>Chi)`[2], 3)

strat <- strat_fit(an %>% mutate(emprel_chr = as.character(emprel_lab)), "emprel_chr") %>%
  mutate(interaction_p = p_emp)
show("ADJUSTED HOURLY ODDS RATIO, STRATIFIED BY RELATION TO THE EMPLOYEE", strat)
wr(strat, "paperB_emprel_stratified.csv")
cat("\n  hourly x relation-to-employee interaction, likelihood-ratio P = ", p_emp, "\n", sep = "")

# primary-analysis sensitivity: patients who hold the job themselves
emp_only <- an %>% filter(EMPREL == 1) %>% mutate(across(where(is.factor), droplevels))
rd_emp   <- ame(glm(F_BASE, binomial, emp_only), emp_only)
cat(sprintf("  employees only (n = %s): adjusted risk difference = %.2f pp (95%% CI %.2f to %.2f)\n",
            format(nrow(emp_only), big.mark = " "),
            100 * rd_emp["rd"], 100 * rd_emp["lo"], 100 * rd_emp["hi"]))
cat(sprintf("  full analytic cohort:      adjusted risk difference = %.2f pp (95%% CI %.2f to %.2f)\n",
            100 * rd_adj["rd"], 100 * rd_adj["lo"], 100 * rd_adj["hi"]))

saveRDS(an, file.path(OUT, "paperB_analytic.rds"))
cat("\nWritten to", OUT, "\n")
