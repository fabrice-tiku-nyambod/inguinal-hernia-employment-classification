# 07_industry_and_ddd_feasibility.R
#
# Two diagnostics that decide whether the employment-classification study can be upgraded from a
# cross-sectional association to a quasi-experiment.
#
#   PART 1  Industry gradient. INDSTRY is an employer-level attribute, so adding
#           it to the model absorbs industry-level physical strain, wage level,
#           and unionisation. If the hourly effect survives, the biomechanical
#           counter-hypothesis (manual labour raises intra-abdominal pressure and
#           drives incarceration directly) cannot account for it.
#
#   PART 2  Feasibility of a triple difference on staggered state paid-sick-leave
#           mandates. Counts cells only. No estimation, no inference.
#
# Output: output/paperB_industry_*.csv, output/paperB_ddd_*.csv

source("01_config.R")

options(warn = 1)
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(splines); library(sandwich); library(lmtest)
})

pct  <- function(x, d = 1) round(100 * mean(x), d)
wr   <- function(x, f) { write.csv(x, file.path(OUT, f), row.names = FALSE); invisible(x) }
show <- function(t, x) {
  cat("\n================ ", t, " ================\n", sep = "")
  print(as.data.frame(x), row.names = FALSE)
}

an <- readRDS(file.path(OUT, "paperB_analytic.rds"))

# INDSTRY value labels, verified 14 August 2026 against the IBM Watson Health
# CCAE/MDCR Data Dictionary (508-compliant copy, page 20).
IND_LABELS <- c(
  "1" = "Oil & gas extraction, mining", "2" = "Manufacturing, durable goods",
  "3" = "Manufacturing, nondurable goods", "4" = "Transportation, communications, utilities",
  "5" = "Retail trade", "6" = "Finance, insurance, real estate", "7" = "Services",
  "A" = "Agriculture, forestry, fishing", "C" = "Construction", "W" = "Wholesale")

an <- an %>%
  mutate(ind_raw = ifelse(is.na(INDSTRY) | INDSTRY == "", NA_character_, INDSTRY),
         industry = IND_LABELS[ind_raw])

# ===========================================================================
# PART 1 -- INDUSTRY GRADIENT
# ===========================================================================
cat("\n\n############ PART 1: INDUSTRY ############\n")

g <- an %>% filter(!is.na(industry))
cat(sprintf("industry known for %s of %s (%.1f%%)\n",
            format(nrow(g), big.mark = " "), format(nrow(an), big.mark = " "),
            100 * nrow(g) / nrow(an)))

ind_tab <- g %>% group_by(industry) %>%
  summarise(n = n(), n_hourly = sum(hourly), pct_hourly = pct(hourly),
            n_cmp = sum(cmp), pct_cmp = pct(cmp, 2),
            pct_cmp_hourly = pct(cmp[hourly == 1], 2),
            pct_cmp_salaried = pct(cmp[hourly == 0], 2), .groups = "drop") %>%
  mutate(gap_pp = round(pct_cmp_hourly - pct_cmp_salaried, 2)) %>%
  arrange(desc(pct_cmp))
show("COMPLICATED PRESENTATION BY INDUSTRY", ind_tab)
wr(ind_tab, "paperB_industry_gradient.csv")

# Ecological: does an industry's hourly share predict its complication rate?
ec <- ind_tab %>% filter(n >= 200)
ct <- cor.test(ec$pct_hourly, ec$pct_cmp)
cat(sprintf("\n  ecological correlation across %d industries (n >= 200):\n", nrow(ec)))
cat(sprintf("  r = %.3f (95%% CI %.3f to %.3f), P = %.3f\n",
            ct$estimate, ct$conf.int[1], ct$conf.int[2], ct$p.value))
cat("  NOTE ecological. An industry-level correlation is not an individual-level effect.\n")

# The discriminating contrast: physically punishing but well-paid and largely
# salaried (mining) vs light-duty but precarious and largely hourly (retail).
cn <- function(lbl) ind_tab[ind_tab$industry == lbl, ]
m <- cn("Oil & gas extraction, mining"); r <- cn("Retail trade")
cat(sprintf("\n  MINING  %% hourly %.1f | %% complicated %.2f | n %s\n",
            m$pct_hourly, m$pct_cmp, format(m$n, big.mark = " ")))
cat(sprintf("  RETAIL  %% hourly %.1f | %% complicated %.2f | n %s\n",
            r$pct_hourly, r$pct_cmp, format(r$n, big.mark = " ")))
cat("  If manual strain drove incarceration, mining would sit at the top, not the bottom.\n")

# --- the direct test: does the hourly effect survive industry adjustment? -----
F_BASE <- cmp ~ hourly + ns(AGE, 4) + sex + region + plan + yearf
or_row <- function(fit, label, term = "hourly") {
  ctb <- coeftest(fit)[term, ]
  data.frame(model = label, or = round(exp(ctb[1]), 3),
             lo = round(exp(ctb[1] - 1.96 * ctb[2]), 3),
             hi = round(exp(ctb[1] + 1.96 * ctb[2]), 3), p = signif(ctb[4], 3))
}
g <- g %>% mutate(industry = factor(industry), across(where(is.factor), droplevels))

m_base <- glm(F_BASE, binomial, g)
m_ind  <- glm(update(F_BASE, . ~ . + industry), binomial, g)
cmpm <- bind_rows(
  or_row(m_base, "Base model, industry-known subset"),
  or_row(m_ind,  "+ industry fixed effects"))
show("HOURLY ODDS RATIO, WITH AND WITHOUT INDUSTRY ADJUSTMENT", cmpm)
cat(sprintf("\n  change in the hourly log-odds on adding industry: %+.1f%%\n",
            100 * (coef(m_ind)["hourly"] - coef(m_base)["hourly"]) / coef(m_base)["hourly"]))
cat("  Industry absorbs strain, wage level, and unionisation at the employer level.\n")
cat("  A small change means none of those explains the hourly gap.\n")
wr(cmpm, "paperB_industry_adjustment.csv")

# adjusted industry effects, Finance/insurance/real estate as reference
# (largest predominantly salaried industry, so the most stable reference)
g2 <- g %>% mutate(industry = relevel(industry, ref = "Finance, insurance, real estate"))
m_lvl <- glm(update(F_BASE, . ~ . + industry), binomial, g2)
lv <- coeftest(m_lvl)
rn <- grep("^industry", rownames(lv), value = TRUE)
ind_or <- data.frame(industry = sub("^industry", "", rn),
                     or = round(exp(lv[rn, 1]), 3),
                     lo = round(exp(lv[rn, 1] - 1.96 * lv[rn, 2]), 3),
                     hi = round(exp(lv[rn, 1] + 1.96 * lv[rn, 2]), 3),
                     p = signif(lv[rn, 4], 3)) %>% arrange(desc(or))
show("ADJUSTED ODDS OF COMPLICATED PRESENTATION BY INDUSTRY (ref: finance/insurance/real estate)",
     ind_or)
wr(ind_or, "paperB_industry_or.csv")

# within-industry hourly effect -- industry held fixed by construction
wi <- bind_rows(lapply(levels(g$industry), function(l) {
  s <- g %>% filter(industry == l) %>% mutate(across(where(is.factor), droplevels))
  if (nrow(s) < 800 || n_distinct(s$hourly) < 2 || sum(s$cmp) < 40) return(NULL)
  or_row(glm(F_BASE, binomial, s), l) %>% mutate(n = nrow(s), .after = model)
}))
show("HOURLY ODDS RATIO WITHIN EACH INDUSTRY", wi)
wr(wi, "paperB_industry_within.csv")

# ===========================================================================
# PART 2 -- TRIPLE-DIFFERENCE FEASIBILITY
# ===========================================================================
cat("\n\n############ PART 2: DDD FEASIBILITY ############\n")

# STATE_LABELS is defined in 01_config.R (Data Dictionary, Attachment J).
cat("STATE_LABELS reused from the companion pipeline:", length(STATE_LABELS), "codes\n")

# Statewide private-sector paid sick leave mandates, effective dates.
# *** THESE DATES ARE NOT VERIFIED AGAINST STATUTE. They are the analyst's best
# *** knowledge and MUST be checked against A Better Balance's paid leave tracker
# *** or the statutes themselves before any estimate is reported.
PSL <- tribble(
  ~state,            ~eff,
  "Connecticut",     "2012-01-01",
  "California",      "2015-07-01",
  "Massachusetts",   "2015-07-01",
  "Oregon",          "2016-01-01",
  "Vermont",         "2017-01-01",
  "Arizona",         "2017-07-01",
  "Washington",      "2018-01-01",
  "Maryland",        "2018-02-11",
  "Rhode Island",    "2018-07-01",
  "New Jersey",      "2018-10-29",
  "Michigan",        "2019-03-29",
  "Nevada",          "2020-01-01",
  "Colorado",        "2021-01-01",
  "New York",        "2021-01-01",
  "Maine",           "2021-01-01",
  "Washington, DC",  "2008-11-13") %>%
  mutate(eff = as.Date(eff), eff_year = as.integer(format(eff, "%Y")))

an <- an %>% mutate(state = STATE_LABELS[as.character(EGEOLOC)])
cat(sprintf("\nstate assignable for %s of %s (%.1f%%); the remainder are region/division aggregates\n",
            format(sum(!is.na(an$state)), big.mark = " "),
            format(nrow(an), big.mark = " "), 100 * mean(!is.na(an$state))))

st <- an %>% filter(!is.na(state)) %>%
  left_join(PSL, by = "state") %>%
  mutate(grp = case_when(
    is.na(eff_year)        ~ "Never treated (in window)",
    eff_year <= 2015       ~ "Always treated (pre-2016)",
    TRUE                   ~ "Adopter (2016-2021)"),
    post = !is.na(eff_year) & year >= eff_year & eff_year > 2015)

show("COHORT BY TREATMENT GROUP", st %>% count(grp) %>%
       mutate(pct = round(100 * n / nrow(st), 1)))

show("ADOPTER STATES: COHORT SIZE AND MANDATE YEAR",
     st %>% filter(grp == "Adopter (2016-2021)") %>%
       group_by(state, eff_year) %>%
       summarise(n = n(), n_hourly = sum(hourly), n_post = sum(post),
                 n_hourly_post = sum(hourly == 1 & post),
                 cmp_hourly_post = sum(cmp == 1 & hourly == 1 & post),
                 .groups = "drop") %>% arrange(desc(n)))

# The cell that carries the estimate
key <- st %>% filter(grp == "Adopter (2016-2021)") %>%
  group_by(period = ifelse(post, "Post-mandate", "Pre-mandate"),
           class = ifelse(hourly == 1, "Hourly", "Salaried")) %>%
  summarise(n = n(), complicated = sum(cmp), pct = pct(cmp, 2), .groups = "drop")
show("DDD IDENTIFYING CELLS -- ADOPTER STATES ONLY", key)

ctl <- st %>% filter(grp == "Never treated (in window)") %>%
  group_by(class = ifelse(hourly == 1, "Hourly", "Salaried")) %>%
  summarise(n = n(), complicated = sum(cmp), pct = pct(cmp, 2), .groups = "drop")
show("CONTROL STATES (never treated in window)", ctl)

wr(st %>% count(state, grp, eff_year, post, hourly) %>% arrange(state),
   "paperB_ddd_cells.csv")
wr(key, "paperB_ddd_key_cells.csv")

# Blunt verdict on the smallest cell that must carry the estimate
smallest <- min(key$complicated)
cat(sprintf("\n  smallest outcome count among the four adopter cells: %d\n", smallest))
cat(if (smallest < 100)
  "  VERDICT: underpowered. A DDD on these cells will not support inference.\n" else
  "  VERDICT: cells are populated; a formal power calculation is worth running.\n")

cat("\n  CONTAMINATION WARNING: EGEOLOC resolves to state, but municipal paid sick\n")
cat("  leave ordinances (Chicago, Philadelphia, Pittsburgh, Minneapolis, St Paul,\n")
cat("  Seattle, San Francisco, New York City) treat parts of states counted here as\n")
cat("  controls. That biases any DDD toward the null and must be addressed before use.\n")
