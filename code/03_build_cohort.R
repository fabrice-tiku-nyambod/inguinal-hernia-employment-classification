source("01_config.R")

# 03_build_cohort.R
# Build the reconciled inguinal hernia repair cohort shared by all three papers
# from this MarketScan extract. Output: 130 033 patients, one index repair each.
#
# The three papers must share a denominator or reviewers and co-authors will ask
# why they differ. The rules below are the reconciled ones, adopted across all
# three:
#   1. K40 concordance on the REPAIR LINE ITSELF
#      (filter PROC %in% REP_CPT & str_detect(dx,"K40") applied line-wise),
#      not merely somewhere on the encounter.
#   2. Source tables O (professional) AND S (facility).
#   3. PLANTYP present and mappable to a label.
#
# The companion studies from this extract were rebuilt on this cohort and report
# the same 130 033. An earlier companion draft published 117 435 under a
# narrower build; that figure is superseded and is not a reconciliation target.
#
# The employment-classification study uses the FULL cohort written here, not
# cohort B: its outcome is complicated presentation, which cohort B excludes.

options(warn = 1)
suppressPackageStartupMessages({
  library(haven); library(dplyr); library(purrr); library(stringr)
})

DATA_DIR <- DATA_DIR
OUT <- OUT
YEARS <- 2016:2021
OPEN_CPT <- c("49505", "49507", "49520", "49521", "49525")
LAP_CPT  <- c("49650", "49651")
REP_CPT  <- c(OPEN_CPT, LAP_CPT)
ROBOT_HCPCS <- "S2900"
CPI <- c(`2016` = 240.007, `2017` = 245.120, `2018` = 251.107,
         `2019` = 255.657, `2020` = 258.811, `2021` = 270.970)
DEFLATOR <- CPI["2021"] / CPI
PLAN_LABELS <- c("1" = "Basic/major medical", "2" = "Comprehensive", "3" = "EPO",
                 "4" = "HMO", "5" = "POS", "6" = "PPO", "7" = "POS with capitation",
                 "8" = "CDHP", "9" = "HDHP")
clean_code <- function(x) toupper(gsub("[^A-Z0-9]", "", as.character(x)))
fmt_n <- function(x) formatC(round(x), format = "d", big.mark = " ")
sas_path <- function(tbl, yr) file.path(DATA_DIR, sprintf("fabrice_%s_%d.sas7bdat", tbl, yr))

COLS <- c("ENROLID", "SVCDATE", "PROC1", "PROCMOD", "STDPLAC", "DX1", "DX2", "DX3", "DX4",
          "PAY", "COPAY", "COINS", "DEDUCT", "AGE", "SEX", "REGION", "PLANTYP", "EGEOLOC",
          # Employment fields. Carried through the build so the SAS files are read once;
          # none of them enters a filter, so the denominator is unchanged.
          "EECLASS", "EESTATU", "EMPREL", "INDSTRY", "MSA")

# first non-missing, NA if none. dplyr::first(na.omit(x)) errors on an all-NA group.
fnm <- function(x) { x <- x[!is.na(x)]; if (length(x)) x[[1]] else NA }

message("reading O and S claim lines ...")
lines <- map_dfr(YEARS, function(y) map_dfr(c("o", "s"), function(tb) {
  x <- read_sas(sas_path(tb, y), col_select = any_of(COLS))
  for (v in setdiff(COLS, names(x))) x[[v]] <- NA
  x %>% transmute(
    ENROLID = as.character(ENROLID), svc = as.Date(SVCDATE), yr = y, tbl = tb,
    PROC = clean_code(PROC1), MOD = clean_code(PROCMOD),
    STDPLAC = as.character(STDPLAC),
    dx = paste(clean_code(DX1), clean_code(DX2), clean_code(DX3), clean_code(DX4)),
    PAY = as.numeric(PAY),
    OOP = as.numeric(COPAY) + as.numeric(COINS) + as.numeric(DEDUCT),
    CPY = as.numeric(COPAY), CNS = as.numeric(COINS), DED = as.numeric(DEDUCT),
    AGE = suppressWarnings(as.numeric(AGE)), SEX = as.character(SEX),
    REGION = suppressWarnings(as.numeric(REGION)),
    PLANTYP = suppressWarnings(as.numeric(PLANTYP)),
    EGEOLOC = suppressWarnings(as.numeric(EGEOLOC)),
    EECLASS = suppressWarnings(as.numeric(EECLASS)),
    EESTATU = suppressWarnings(as.numeric(EESTATU)),
    EMPREL  = suppressWarnings(as.numeric(EMPREL)),
    INDSTRY = as.character(INDSTRY),
    MSA = as.character(MSA))
})) %>% filter(!is.na(ENROLID), !is.na(svc))

fun <- list()
add <- function(step, n) { fun[[length(fun) + 1]] <<- data.frame(step = step, n = n); invisible() }
add("claim lines, O + S tables, 2016-2021", nrow(lines))

# --- paper 1 rule: repair CPT WITH K40 on the same line -------------------------
idx <- lines %>%
  filter(PROC %in% REP_CPT, str_detect(dx, "K40")) %>%
  group_by(ENROLID, svc) %>%
  summarise(any_open = any(PROC %in% OPEN_CPT), any_lap = any(PROC %in% LAP_CPT),
            bilateral = any(MOD == "50"),
            recurrent = any(PROC %in% c("49520", "49521", "49651")),
            complicated = any(str_detect(dx, "K40[0134]")),
            AGE = suppressWarnings(max(AGE, na.rm = TRUE)),
            SEX = dplyr::first(na.omit(SEX)), .groups = "drop")
add("repair encounters, K40 on the repair line", nrow(idx))
idx <- idx %>% filter(is.finite(AGE), AGE >= 18, AGE <= 65)
add("aged 18 to 65", nrow(idx))

robot <- lines %>% filter(PROC == ROBOT_HCPCS) %>% distinct(ENROLID, svc) %>% mutate(rob = TRUE)
idx <- idx %>% left_join(robot, by = c("ENROLID", "svc")) %>%
  mutate(rob = coalesce(rob, FALSE),
         approach = case_when(any_open ~ "Open", any_lap & rob ~ "Robotic",
                              any_lap ~ "Laparoscopic", TRUE ~ NA_character_)) %>%
  filter(!is.na(approach))
add("surgical approach assignable", nrow(idx))

day <- lines %>% semi_join(idx, by = c("ENROLID", "svc")) %>%
  group_by(ENROLID, svc) %>%
  summarise(allowed = sum(PAY, na.rm = TRUE), oop = sum(OOP, na.rm = TRUE),
            copay = sum(CPY, na.rm = TRUE), coins = sum(CNS, na.rm = TRUE),
            deduct = sum(DED, na.rm = TRUE),
            site = STDPLAC[which.max(replace(PAY, is.na(PAY), -Inf))],
            PLANTYP = dplyr::first(na.omit(PLANTYP)),
            REGION = dplyr::first(na.omit(REGION)),
            EGEOLOC = dplyr::first(na.omit(EGEOLOC)),
            # employer/benefit attributes: person-year constants, so first non-missing
            # on the encounter day is the value for the encounter
            EECLASS = fnm(EECLASS), EESTATU = fnm(EESTATU),
            EMPREL = fnm(EMPREL), INDSTRY = fnm(INDSTRY),
            MSA = fnm(MSA), .groups = "drop")

d <- idx %>% left_join(day, by = c("ENROLID", "svc")) %>%
  filter(!is.na(allowed), allowed > 0, !is.na(PLANTYP))
add("positive allowed amount and plan type present", nrow(d))
d <- d %>% arrange(ENROLID, svc) %>% distinct(ENROLID, .keep_all = TRUE)
add("one index repair per patient", nrow(d))
d <- d %>% mutate(plan = PLAN_LABELS[as.character(PLANTYP)]) %>% filter(!is.na(plan))
add("PAPER 1 ANALYTIC COHORT", nrow(d))

d <- d %>% mutate(year = as.integer(format(svc, "%Y")),
                  defl = as.numeric(DEFLATOR[as.character(year)]),
                  allowed_r = allowed * defl, oop_r = oop * defl,
                  setting = recode(site, "22" = "HOPD", "24" = "ASC", .default = NA_character_))

amb <- d %>% filter(!is.na(setting))
add("  of which ambulatory (HOPD or ASC) = PAPER A COHORT A", nrow(amb))
rest <- amb %>% filter(!complicated, !bilateral, !recurrent)
add("  elective, unilateral, initial = PAPER A COHORT B", nrow(rest))

cat("\n================ COHORT FUNNEL ================\n")
ft <- bind_rows(fun)
ft$n_fmt <- fmt_n(ft$n)
print(ft[, c("step", "n_fmt")], row.names = FALSE, right = FALSE)

cat(sprintf("\n  shared denominator across all three papers: %s\n", fmt_n(nrow(d))))
if (nrow(d) != 130033)
  cat("  *** WARNING: expected 130 033. The companion papers cite that figure.\n")

cat("\n================ SITE DISTRIBUTION IN THE RECONCILED COHORT ================\n")
print(as.data.frame(d %>% count(site, sort = TRUE) %>% head(8)), row.names = FALSE)

cat("\n================ HEADLINE ESTIMATES ON THE RECONCILED COHORT B ================\n")
r <- rest %>% group_by(setting) %>%
  summarise(n = n(), median_allowed = round(median(allowed_r)),
            median_oop = round(median(oop_r)),
            pct_full = round(100 * mean(oop_r / allowed_r >= 0.99), 1), .groups = "drop")
print(as.data.frame(r), row.names = FALSE)
cat(sprintf("  price ratio HOPD:ASC = %.2f | out-of-pocket ratio ASC:HOPD = %.2f\n",
            r$median_allowed[r$setting == "HOPD"] / r$median_allowed[r$setting == "ASC"],
            r$median_oop[r$setting == "ASC"] / r$median_oop[r$setting == "HOPD"]))

saveRDS(d,    file.path(OUT, "paperA_cohort_reconciled_full.rds"))
saveRDS(rest, file.path(OUT, "paperA_cohort_reconciled_B.rds"))
write.csv(ft[, c("step", "n")], file.path(OUT, "paperA_cohort_funnel.csv"), row.names = FALSE)
cat("\nWritten to", OUT, "\n")
