# 06_figures.R -- Figure 1, adjusted association across prespecified strata.
#
# Presentation layer, like 05: every estimate is read from a CSV written by
# 04_job_class.R. No model is specified here.
#
# Output: output/figures/*.tiff  (1200 dpi, LZW, serif)
#   Figure 1  subgroup forest
#   Figure 2  within-industry hourly odds ratio
#   Figure 3  the cost-urgency divergence
#         output/figures/figure1_forest.png  (screen proof)

source("01_config.R")

options(warn = 1)
suppressPackageStartupMessages({ library(dplyr); library(ggplot2) })

rd <- function(f) read.csv(file.path(OUT, f), check.names = FALSE)

# Okabe-Ito blue and vermillion. Validated for colour-vision deficiency:
# worst adjacent pair dE 21.9 under protanopia, 31.2 normal vision, both well
# above the dE 8 floor, and each holds >= 3:1 contrast against white.
# Colour encodes identity (job class) or magnitude (hourly share), never decoration.
COL_SAL  <- "#0072B2"   # salaried
COL_HRLY <- "#D55E00"   # hourly
ACCENT   <- "#0072B2"
RAMP_LO  <- "#6BAED6"   # sequential, light end still legible on white
RAMP_HI  <- "#08306B"   # sequential, dark

mo <- rd("paperB_models.csv")
us <- rd("paperB_union_stratified.csv")
es <- rd("paperB_emprel_stratified.csv")
an_n <- nrow(readRDS(file.path(OUT, "paperB_analytic.rds")))
ov <- mo[grepl("^Adjusted odds", mo$model), ]

# Rows are assembled top-to-bottom; `y` is reversed at plot time so the overall
# estimate sits at the top the way a reader scans it.
rows <- bind_rows(
  data.frame(label = "Overall", est = ov$estimate, lo = ov$lo, hi = ov$hi,
             n = an_n, head = FALSE),
  data.frame(label = sprintf("Union status of policy holder (interaction P = %.2f)",
                             us$interaction_p[1]),
             est = NA, lo = NA, hi = NA, n = NA, head = TRUE),
  us %>% filter(model %in% c("Non-union", "Union", "Other")) %>%
    arrange(match(model, c("Non-union", "Union", "Other"))) %>%
    transmute(label = paste0("   ", model), est = estimate, lo, hi, n, head = FALSE),
  data.frame(label = sprintf("Patient relation to policy holder (interaction P = %.2f)",
                             es$interaction_p[1]),
             est = NA, lo = NA, hi = NA, n = NA, head = TRUE),
  es %>% filter(model %in% c("Employee", "Spouse", "Child/other")) %>%
    arrange(match(model, c("Employee", "Spouse", "Child/other"))) %>%
    transmute(label = paste0("   ", model), est = estimate, lo, hi, n, head = FALSE)) %>%
  mutate(y = rev(seq_len(n())),
         txt = ifelse(head, "", sprintf("%.2f (%.2f-%.2f)", est, lo, hi)),
         ntxt = ifelse(head, "", format(n, big.mark = ",", trim = TRUE)),
         # marker area tracks stratum size, the usual forest-plot convention, with a
         # floor so the smallest stratum stays visible at print size
         sz = ifelse(head, NA, pmax(sqrt(n / max(n, na.rm = TRUE)) * 3.2, 1.7)))

# Data occupy 0.9-1.65; the numeric columns sit to the right of the panel in the
# same coordinate space, which keeps one axis and needs no second plot.
X_OR <- 2.35
X_N  <- 3.30
XMAX <- 4.10
brk  <- c(0.9, 1.0, 1.2, 1.4, 1.6)

p <- ggplot(rows, aes(y = y)) +
  # reference line first so marks sit above it; bounded to the rows so it does not
  # run through the column headers or the axis below
  annotate("segment", x = 1, xend = 1, y = -0.15, yend = max(rows$y) + 0.6,
           linewidth = 0.4, color = "grey55") +
  geom_errorbar(data = ~ filter(.x, !head),
                aes(xmin = lo, xmax = hi), orientation = "y", width = 0,
                linewidth = 0.6, color = ACCENT) +
  geom_point(data = ~ filter(.x, !head), aes(x = est, size = sz),
             shape = 22, fill = ACCENT, color = "white", stroke = 0.35) +
  scale_size_identity() +
  # row labels, estimates, and stratum sizes as text columns
  geom_text(aes(x = 0.30, label = label, fontface = ifelse(head, 2, 1)),
            hjust = 0, size = 3.1, family = "serif") +
  geom_text(aes(x = X_OR, label = txt), hjust = 1, size = 3.1, family = "serif") +
  geom_text(aes(x = X_N, label = ntxt), hjust = 1, size = 3.1, family = "serif") +
  annotate("text", x = X_OR, y = max(rows$y) + 1.15, label = "aOR (95% CI)",
           hjust = 1, size = 3.1, fontface = 2, family = "serif") +
  annotate("text", x = X_N, y = max(rows$y) + 1.15, label = "No.",
           hjust = 1, size = 3.1, fontface = 2, family = "serif") +
  # Only one directional cue is drawn. Every point estimate lies above 1, so a
  # "favors salaried" arrow would label an empty region and its caption cannot fit
  # the narrow 0.9-1.0 span without colliding with the caption on the right.
  annotate("segment", x = 1.0, xend = 1.65, y = -0.55, yend = -0.55,
           arrow = arrow(length = unit(0.10, "cm"), ends = "last"), linewidth = 0.4) +
  annotate("text", x = 1.0, y = -1.15, hjust = 0, size = 2.9, family = "serif",
           label = "Higher risk of complicated presentation among hourly workers") +
  # The panel is wider than the data so it can hold the aOR and No. columns, but a
  # themed axis would rule the whole width and underline those columns. The axis is
  # therefore drawn by hand across the data region only.
  annotate("segment", x = min(brk), xend = 1.65, y = -1.95, yend = -1.95,
           linewidth = 0.4, color = "grey30") +
  annotate("segment", x = brk, xend = brk, y = -1.95, yend = -2.12,
           linewidth = 0.4, color = "grey30") +
  annotate("text", x = brk, y = -2.42, label = sprintf("%.1f", brk),
           size = 3.0, family = "serif") +
  annotate("text", x = sqrt(min(brk) * 1.65), y = -3.15, size = 3.3, family = "serif",
           label = "Adjusted odds ratio for complicated presentation") +
  scale_x_continuous(trans = "log", limits = c(0.30, XMAX), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-3.6, max(rows$y) + 1.6), expand = c(0, 0)) +
  labs(x = NULL, y = NULL) +
  theme_classic(base_family = "serif", base_size = 10) +
  theme(axis.line = element_blank(), axis.ticks = element_blank(),
        axis.text = element_blank(),
        plot.margin = margin(10, 8, 4, 8))

# scale_x_continuous already restricts the ticks to `brk`, so the axis is drawn
# only under the data region even though the panel extends to hold the text columns
dir.create(FIGD, showWarnings = FALSE, recursive = TRUE)
DPI <- 1200          # journal line-art standard
W <- 7.2; H <- 4.3

tiff(file.path(FIGD, "figure1_forest.tiff"), width = W, height = H, units = "in",
     res = DPI, compression = "lzw", family = "serif")
print(p); invisible(dev.off())

png(file.path(FIGD, "figure1_forest.png"), width = W, height = H, units = "in",
    res = 150, family = "serif")
print(p); invisible(dev.off())

f <- file.path(FIGD, "figure1_forest.tiff")
cat(sprintf("\nWrote %s (%.1f MB, %d dpi, LZW)\n", f,
            file.size(f) / 1024^2, DPI))
cat("Screen proof:", file.path(FIGD, "figure1_forest.png"), "\n")

# ===========================================================================
# Figure 2 -- within-industry hourly odds ratio
# ===========================================================================
# This is the figure that answers the biomechanical objection, so it earns the
# space a table would not: the reader sees at a glance that every industry sits
# to the right of 1, and that the most physically punishing one sits furthest.
iw <- rd("paperB_industry_within.csv")
ig <- rd("paperB_industry_gradient.csv")

f2 <- iw %>%
  left_join(ig %>% select(model = industry, pct_hourly), by = "model") %>%
  arrange(or) %>%
  mutate(y = seq_len(n()),
         lab = sprintf("%s (%.0f%% hourly)", model, pct_hourly),
         txt = sprintf("%.2f (%.2f-%.2f)", or, lo, hi))

XT <- 6.4
p2 <- ggplot(f2, aes(y = y)) +
  annotate("segment", x = 1, xend = 1, y = 0.4, yend = nrow(f2) + 0.75,
           linewidth = 0.4, color = "grey55") +
  geom_errorbar(aes(xmin = lo, xmax = hi), orientation = "y", width = 0,
                linewidth = 0.6, color = "grey35") +
  geom_point(aes(x = or, size = pmax(sqrt(n / max(n)) * 3.4, 2.0),
                 fill = pct_hourly),
             shape = 22, color = "grey20", stroke = 0.4) +
  scale_size_identity() +
  scale_fill_gradient(low = RAMP_LO, high = RAMP_HI, guide = "none") +
  geom_text(aes(x = 0.34, label = lab), hjust = 0, size = 3.0, family = "serif") +
  geom_text(aes(x = XT, label = txt), hjust = 1, size = 3.0, family = "serif") +
  annotate("text", x = XT, y = nrow(f2) + 1.1, label = "aOR (95% CI)",
           hjust = 1, size = 3.0, fontface = 2, family = "serif") +
  annotate("segment", x = 0.9, xend = 4.0, y = -0.35, yend = -0.35,
           linewidth = 0.4, color = "grey30") +
  annotate("segment", x = c(1, 2, 3, 4), xend = c(1, 2, 3, 4),
           y = -0.35, yend = -0.55, linewidth = 0.4, color = "grey30") +
  annotate("text", x = c(1, 2, 3, 4), y = -0.85, label = c("1", "2", "3", "4"),
           size = 3.0, family = "serif") +
  annotate("text", x = 2, y = -1.5, size = 3.2, family = "serif",
           label = "Adjusted odds ratio, hourly vs salaried, within industry") +
  scale_x_continuous(trans = "log", limits = c(0.34, 7.2), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-1.9, nrow(f2) + 1.5), expand = c(0, 0)) +
  labs(x = NULL, y = NULL) +
  theme_classic(base_family = "serif", base_size = 10) +
  theme(axis.line = element_blank(), axis.ticks = element_blank(),
        axis.text = element_blank(), plot.margin = margin(10, 8, 4, 8))

tiff(file.path(FIGD, "figure2_industry.tiff"), width = 7.2, height = 3.6,
     units = "in", res = DPI, compression = "lzw", family = "serif")
print(p2); invisible(dev.off())
png(file.path(FIGD, "figure2_industry.png"), width = 7.2, height = 3.6,
    units = "in", res = 150, family = "serif")
print(p2); invisible(dev.off())

# ===========================================================================
# Figure 3 -- the cost-urgency divergence
# ===========================================================================
# The paper's whole argument in one image: the group that pays less arrives
# worse. Two panels on a shared categorical axis, opposing directions.
cs <- rd("paperB_costsharing.csv")
un <- rd("paperB_primary_unadjusted.csv")

f3 <- bind_rows(
  cs %>% transmute(jobclass, value = median_oop,
                   panel = "Median out-of-pocket cost, 2021 US$",
                   lab = sprintf("$%s", format(median_oop, big.mark = ","))),
  un %>% transmute(jobclass, value = pct,
                   panel = "Complicated presentation, %",
                   lab = sprintf("%.1f%%", pct))) %>%
  mutate(panel = factor(panel, levels = c("Median out-of-pocket cost, 2021 US$",
                                          "Complicated presentation, %")),
         jobclass = factor(jobclass, levels = c("Salaried", "Hourly")))

p3 <- ggplot(f3, aes(x = jobclass, y = value)) +
  geom_col(aes(fill = jobclass), width = 0.5) +
  scale_fill_manual(values = c(Salaried = COL_SAL, Hourly = COL_HRLY),
                    guide = "none") +
  geom_text(aes(label = lab), vjust = -0.6, size = 3.4, family = "serif",
            fontface = "bold") +
  facet_wrap(~ panel, scales = "free_y") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.22))) +
  labs(x = NULL, y = NULL) +
  theme_classic(base_family = "serif", base_size = 10) +
  theme(strip.background = element_blank(),
        strip.text = element_text(face = "bold", size = 9.5),
        axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        panel.spacing = unit(1.6, "lines"),
        plot.margin = margin(10, 10, 6, 10))

tiff(file.path(FIGD, "figure3_divergence.tiff"), width = 6.4, height = 3.0,
     units = "in", res = DPI, compression = "lzw", family = "serif")
print(p3); invisible(dev.off())
png(file.path(FIGD, "figure3_divergence.png"), width = 6.4, height = 3.0,
    units = "in", res = 150, family = "serif")
print(p3); invisible(dev.off())

for (nm in c("figure2_industry.tiff", "figure3_divergence.tiff")) {
  ff <- file.path(FIGD, nm)
  cat(sprintf("Wrote %s (%.1f MB, %d dpi, LZW)\n", nm, file.size(ff) / 1024^2, DPI))
}
