# 10_ipw.R -- inverse probability of selection weighting for cohort retention.
#
# Employment classification is assignable for 66 070 of 130 033 patients, and the
# unassignable half differs on geography, benefit design, and the outcome. This
# reweights the analytic cohort to resemble the full extract on observed
# characteristics.
#
# WHAT THIS DOES NOT DO: classification is unobserved in the excluded half by
# definition, so no weighting scheme can correct selection on unmeasured
# determinants of assignability. This addresses observed selection only, and the
# manuscript says so.
#
# Output: output/paperB_ipw.csv

source("01_config.R")

options(warn = 1)
suppressPackageStartupMessages({
  library(dplyr); library(splines); library(sandwich); library(lmtest)
})

show <- function(t, x) {
  cat("\n================ ", t, " ================\n", sep = "")
  print(as.data.frame(x), row.names = FALSE)
}

REGION_LABELS <- c("1" = "Northeast", "2" = "North Central", "3" = "South",
                   "4" = "West", "5" = "Unknown")

d <- readRDS(file.path(OUT, "paperA_cohort_reconciled_full.rds")) %>%
  mutate(jobclass = case_when(EECLASS %in% 1:3 ~ "Salaried",
                              EECLASS %in% 4:6 ~ "Hourly",
                              TRUE ~ NA_character_),
         assignable = as.integer(!is.na(jobclass)),
         hourly  = as.integer(jobclass == "Hourly"),
         region  = factor(REGION_LABELS[as.character(REGION)]),
         yearf   = factor(year), plan = factor(plan), sex = factor(SEX),
         cmp     = as.integer(complicated),
         setting2 = factor(ifelse(is.na(setting), "Inpatient/other", setting)),
         ind     = factor(ifelse(is.na(INDSTRY) | INDSTRY == "", "Unknown", INDSTRY)),
         emprelf = factor(EMPREL), eestatuf = factor(EESTATU))

stopifnot(nrow(d) == 130033)
cat(sprintf("assignable %s of %s (%.1f%%)\n", format(sum(d$assignable), big.mark = " "),
            format(nrow(d), big.mark = " "), 100 * mean(d$assignable)))

# ---------------------------------------------------------------------------
# 1. Selection model
# ---------------------------------------------------------------------------
# Everything observed on the FULL cohort that plausibly predicts whether the
# employer reported a usable classification. EMPREL and EESTATU are included
# because they come from the same employer-reported block as EECLASS and are the
# strongest available predictors of whether that block was completed.
F_SEL <- assignable ~ ns(AGE, 4) + sex + region + plan + yearf + approach +
  setting2 + bilateral + recurrent + ind + emprelf + eestatuf

m_sel <- glm(F_SEL, binomial, d)
d$ps <- predict(m_sel, type = "response")

cat(sprintf("\nselection model: propensity range %.3f to %.3f\n",
            min(d$ps), max(d$ps)))
cat(sprintf("  c-statistic %.3f\n",
            {r <- rank(d$ps)
             n1 <- as.numeric(sum(d$assignable))   # as.numeric: n1*n0 overflows integer
             n0 <- as.numeric(nrow(d)) - n1
             (sum(r[d$assignable == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)}))

# stabilised weights: marginal / conditional probability of selection
an <- d %>% filter(assignable == 1) %>%
  mutate(w = mean(d$assignable) / ps,
         # trim at the 1st and 99th percentile so a handful of extreme weights
         # cannot dominate; trimming point fixed in advance, not tuned
         w = pmin(pmax(w, quantile(w, .01)), quantile(w, .99)))

cat(sprintf("  stabilised weights: mean %.3f, range %.3f to %.3f\n",
            mean(an$w), min(an$w), max(an$w)))

# ---------------------------------------------------------------------------
# 2. Primary model, unweighted vs weighted
# ---------------------------------------------------------------------------
F_BASE <- cmp ~ hourly + ns(AGE, 4) + sex + region + plan + yearf

est <- function(fit, label, wtd = FALSE) {
  V  <- if (wtd) vcovHC(fit, type = "HC0") else vcov(fit)
  ct <- coeftest(fit, vcov. = V)["hourly", ]
  data.frame(model = label, or = round(exp(ct[1]), 3),
             lo = round(exp(ct[1] - 1.96 * ct[2]), 3),
             hi = round(exp(ct[1] + 1.96 * ct[2]), 3), p = signif(ct[4], 3))
}

m_unw <- glm(F_BASE, binomial, an)
m_ipw <- suppressWarnings(glm(F_BASE, binomial, an, weights = an$w))

# adjusted risk difference by g-computation, weighted where applicable
ame <- function(fit, data, w = NULL) {
  d1 <- transform(data, hourly = 1); d0 <- transform(data, hourly = 0)
  X1 <- model.matrix(delete.response(terms(fit)), d1)
  X0 <- model.matrix(delete.response(terms(fit)), d0)
  b <- coef(fit); V <- if (is.null(w)) vcov(fit) else vcovHC(fit, type = "HC0")
  p1 <- plogis(X1 %*% b); p0 <- plogis(X0 %*% b)
  ww <- if (is.null(w)) rep(1, nrow(X1)) else w
  ww <- ww / sum(ww)
  g <- colSums(ww * (as.vector(p1 * (1 - p1)) * X1 - as.vector(p0 * (1 - p0)) * X0))
  e <- sum(ww * (p1 - p0)); s <- sqrt(drop(t(g) %*% V %*% g))
  c(rd = 100 * e, lo = 100 * (e - 1.96 * s), hi = 100 * (e + 1.96 * s))
}

r_unw <- ame(m_unw, an)
r_ipw <- ame(m_ipw, an, an$w)

out <- bind_rows(est(m_unw, "Unweighted (primary)"),
                 est(m_ipw, "Selection-weighted (IPW)", wtd = TRUE)) %>%
  mutate(rd_pp = round(c(r_unw["rd"], r_ipw["rd"]), 2),
         rd_lo = round(c(r_unw["lo"], r_ipw["lo"]), 2),
         rd_hi = round(c(r_unw["hi"], r_ipw["hi"]), 2))

show("HOURLY EFFECT, UNWEIGHTED VS SELECTION-WEIGHTED", out)
write.csv(out, file.path(OUT, "paperB_ipw.csv"), row.names = FALSE)

cat(sprintf("\n  risk difference moves %.2f pp on reweighting (%.2f -> %.2f)\n",
            r_ipw["rd"] - r_unw["rd"], r_unw["rd"], r_ipw["rd"]))
cat("  A small change indicates the association is not an artefact of which\n")
cat("  patients had a classification reported, on observed characteristics.\n")
