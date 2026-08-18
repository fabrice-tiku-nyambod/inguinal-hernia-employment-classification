# 08_build_submission.R -- assemble the Health Affairs submission package.
#
# The markdown draft stays the single source of truth; this renders it to Word
# rather than duplicating the text. Re-run after editing the markdown.
#
# Input : manuscript/PAPERB_healthaffairs.md, output/*.csv, output/figures/*.tif
# Output: submission - health affairs/

source("01_config.R")

options(warn = 1)
suppressPackageStartupMessages({
  library(dplyr); library(officer); library(flextable)
})

SERIF <- "Times New Roman"
SUB   <- file.path(ROOT, "submission - health affairs")
dir.create(SUB, showWarnings = FALSE, recursive = TRUE)
MD    <- file.path(ROOT, "manuscript", "PAPERB_healthaffairs.md")

source("09_wordcount.R")     # LIMITS + count_md
wc <- count_md(MD)
cat(sprintf("Health Affairs %s: %d / %d words (abstract %d + main %d), %d / %d exhibits\n",
            TYPE, wc$counted, LIMITS$words, wc$abstract, wc$main,
            wc$exhibits, LIMITS$exhibits))
if (wc$counted > LIMITS$words || wc$exhibits > LIMITS$exhibits)
  stop("manuscript exceeds a Health Affairs limit; fix before building the package",
       call. = FALSE)

# ---------------------------------------------------------------------------
# markdown -> officer
# ---------------------------------------------------------------------------
# Handles what this document actually uses: ATX headings, bold lead-ins,
# <sup> reference markers, and italic source captions. Anything else passes
# through as plain text rather than being silently mangled.
runs_for <- function(s) {
  out <- list()
  # split on <sup>..</sup> and **..**, keeping the delimiters
  pat <- "(<sup>.*?</sup>|\\*\\*.*?\\*\\*)"
  pieces <- strsplit(gsub(pat, "\001\\1\001", s), "\001", fixed = TRUE)[[1]]
  for (p in pieces) {
    if (!nzchar(p)) next
    if (grepl("^<sup>", p)) {
      txt <- gsub("</?sup>", "", p)
      out <- c(out, list(ftext(txt, fp_text(font.family = SERIF, font.size = 11,
                                            vertical.align = "superscript"))))
    } else if (grepl("^\\*\\*.*\\*\\*$", p)) {
      txt <- gsub("^\\*\\*|\\*\\*$", "", p)
      out <- c(out, list(ftext(txt, fp_text(font.family = SERIF, font.size = 11,
                                            bold = TRUE))))
    } else {
      # strip residual inline emphasis markers rather than printing them
      out <- c(out, list(ftext(gsub("\\*", "", p),
                               fp_text(font.family = SERIF, font.size = 11))))
    }
  }
  if (!length(out)) out <- list(ftext("", fp_text(font.family = SERIF)))
  out
}

md_to_docx <- function(lines, doc, stop_at = NULL) {
  # The markdown is hard-wrapped at 80 characters. Consecutive non-blank lines are
  # one paragraph and must be joined before emitting, or every wrapped line becomes
  # its own paragraph in Word.
  blocks <- list(); buf <- character(0)
  flush <- function() {
    if (length(buf)) blocks[[length(blocks) + 1]] <<- paste(buf, collapse = " ")
    buf <<- character(0)
  }
  for (ln in lines) {
    if (!is.null(stop_at) && grepl(stop_at, ln)) break
    t <- trimws(ln)
    if (grepl("^> ", t) || t == ">") { flush(); next }   # internal notes
    if (grepl("^---+$", t)) { flush(); next }
    if (!nzchar(t)) { flush(); next }
    # Headings, captions, table rows, and NUMBERED REFERENCE ENTRIES each stand
    # alone. Without the numbered rule the reference list, which has no blank
    # lines between items, collapses into a single paragraph.
    if (grepl("^#{1,3} ", t) || grepl("^[*][(]source:", t) ||
        grepl("^[|]", t) || grepl("^[0-9]{1,3}[.] ", t)) {
      flush(); blocks[[length(blocks) + 1]] <- t; next   # stand alone
    }
    buf <- c(buf, t)
  }
  flush()

  for (t in blocks) {
    if (grepl("^\\|", t)) next                     # markdown table rows: skip
    if (grepl("^\\*\\(source:", t)) {              # exhibit source caption
      doc <- body_add_fpar(doc, fpar(ftext(gsub("^\\*|\\*$", "", t),
        fp_text(font.family = SERIF, font.size = 9, italic = TRUE))))
      next
    }
    if (grepl("^## ", t) && !grepl("^## Abstract", t)) {
      doc <- body_add_break(doc)          # each major section starts a new page
    }
    if (grepl("^### ", t)) {
      doc <- body_add_fpar(doc, fpar(ftext(sub("^### ", "", t),
        fp_text(font.family = SERIF, font.size = 12, bold = TRUE)),
        fp_p = fp_par(padding.top = 10, padding.bottom = 3)))
    } else if (grepl("^## ", t)) {
      doc <- body_add_fpar(doc, fpar(ftext(sub("^## ", "", t),
        fp_text(font.family = SERIF, font.size = 14, bold = TRUE)),
        fp_p = fp_par(padding.top = 14, padding.bottom = 4)))
    } else if (grepl("^# ", t)) {
      doc <- body_add_fpar(doc, fpar(ftext(sub("^# ", "", t),
        fp_text(font.family = SERIF, font.size = 16, bold = TRUE)),
        fp_p = fp_par(padding.bottom = 8)))
    } else {
      doc <- body_add_fpar(doc, fpar(values = runs_for(t),
        fp_p = fp_par(padding.bottom = 7, line_spacing = 2)))
    }
  }
  doc
}

# ---------------------------------------------------------------------------
# 1. Manuscript
# ---------------------------------------------------------------------------
# The markdown now carries the full title page, so render it from the top and let
# the Notes section terminate the document.
# Never overwrite an existing Manuscript.docx. Word files are not byte-identical
# between renders, so any fingerprint of the output would flag every rebuild as a
# hand edit. The safe rule is simpler: if the file exists, leave it alone and put
# the fresh render beside it for the author to promote deliberately.
MAN <- file.path(SUB, "Manuscript.docx")
existing <- file.exists(MAN)
if (existing) {
  cat("
*** Manuscript.docx already exists and will NOT be overwritten.
")
  cat("*** The fresh render is Manuscript.GENERATED.docx. Compare, merge any
")
  cat("*** hand edits, then replace and delete the GENERATED copy.

")
}

md <- readLines(MD, warn = FALSE)
# title block, then a page break so the abstract stands alone
ti <- which(grepl("^## Abstract", md))[1] - 1
doc <- md_to_docx(md[seq_len(ti)], read_docx())
doc <- body_add_break(doc)
doc <- md_to_docx(md[(ti + 1):length(md)], doc)
print(doc, target = if (existing) file.path(SUB, "Manuscript.GENERATED.docx") else MAN)

# Title page as a separate file, following the companion paper's convention
tp <- read_docx() %>%
  body_add_fpar(fpar(ftext(
    "Hourly Workers Reach Surgery Later Than Salaried Workers, Despite Paying Less",
    fp_text(font.family = SERIF, font.size = 16, bold = TRUE)),
    fp_p = fp_par(padding.bottom = 12)))
for (i in 3:8) {
  t <- trimws(md[i])
  if (!nzchar(t) || grepl("^---", t)) next
  tp <- body_add_fpar(tp, fpar(values = runs_for(t),
    fp_p = fp_par(padding.bottom = 8)))
}
tp <- tp %>%
  body_add_par("") %>%
  body_add_fpar(fpar(ftext("Research Article submitted to Health Affairs",
    fp_text(font.family = SERIF, font.size = 11, italic = TRUE)))) %>%
  body_add_fpar(fpar(ftext(
    sprintf("Word count %d of 3,250 (abstract %d, main text %d). Exhibits: %d. Endnotes: %d, excluded from the count.",
            wc$counted, wc$abstract, wc$main, wc$exhibits, wc$notes),
    fp_text(font.family = SERIF, font.size = 10))))
print(tp, target = file.path(SUB, "Title Page.docx"))

# ---------------------------------------------------------------------------
# 2. Exhibits
# ---------------------------------------------------------------------------
rd <- function(f) read.csv(file.path(OUT, f), check.names = FALSE)
ft <- function(d) flextable(d) %>% theme_booktabs() %>%
  font(fontname = SERIF, part = "all") %>% fontsize(size = 9, part = "all") %>%
  bold(part = "header") %>% autofit()
ex_head <- function(doc, n, cap) body_add_fpar(doc, fpar(
  ftext(sprintf("Exhibit %s. ", n), fp_text(font.family = SERIF, font.size = 11, bold = TRUE)),
  ftext(cap, fp_text(font.family = SERIF, font.size = 11)),
  fp_p = fp_par(padding.top = 12, padding.bottom = 5)))

img <- function(doc, nm, w, h) {
  f <- file.path(FIGD, nm)
  if (file.exists(f)) body_add_img(doc, src = f, width = w, height = h) else doc
}
note8 <- function(doc, x) body_add_fpar(doc, fpar(ftext(x,
  fp_text(font.family = SERIF, font.size = 8)),
  fp_p = fp_par(padding.bottom = 6)))
SRC <- paste("Source: authors' analysis of the Merative MarketScan Commercial",
             "Claims and Encounters Database, 2016-21.")

ed <- read_docx() %>%
  body_add_fpar(fpar(ftext("Exhibits", fp_text(font.family = SERIF, font.size = 16,
                                               bold = TRUE)))) %>%
  ex_head("1", "Complicated presentation of inguinal hernia by employment classification.") %>%
  body_add_flextable(ft(rd("table2_primary.csv"))) %>%
  note8(paste("Adjusted models include age, sex, census region, health plan type,",
              "and year.", SRC)) %>%
  body_add_break() %>%

  ex_head("2", "Patient cost and clinical urgency move in opposite directions across employment classification.") %>%
  img("figure3_divergence.tiff", 6.4, 3.0) %>%
  note8(paste("Median out-of-pocket cost is for the index encounter in constant",
              "2021 dollars.", SRC)) %>%
  body_add_break() %>%

  ex_head("3", "Adjusted association overall and within strata.") %>%
  img("figure1_forest.tiff", 6.9, 4.1) %>%
  note8(paste("Squares are sized by stratum; lines are 95% CIs. Union status and",
              "relation to the policy holder describe the policy holder's",
              "employment.", SRC)) %>%
  body_add_break() %>%

  ex_head("4", "Hourly odds ratio for complicated presentation within each industry.") %>%
  img("figure2_industry.tiff", 6.9, 3.45) %>%
  note8(paste("Industries with fewer than 800 patients or 40 events were not",
              "modeled; see Appendix Exhibit A3.", SRC)) %>%
  body_add_break() %>%

  ex_head("A1", "Analytic cohort compared with patients whose classification could not be assigned. (Appendix)") %>%
  body_add_flextable(ft(rd("tableS1_excluded.csv"))) %>%
  body_add_break() %>%
  ex_head("A2", "Patient cost sharing at the index encounter. (Appendix)") %>%
  body_add_flextable(ft(rd("table4_costsharing.csv"))) %>%
  body_add_par("") %>%
  body_add_flextable(ft(rd("table4b_costsharing_models.csv"))) %>%
  body_add_break() %>%
  ex_head("A3", "Complicated presentation by employer industry, in full. (Appendix)") %>%
  body_add_flextable(ft(rd("table5_industry.csv")))

print(ed, target = file.path(SUB, "Exhibits.docx"))

# ---------------------------------------------------------------------------
# 3. Cover letter
# ---------------------------------------------------------------------------
p <- function(x, bold = FALSE, sz = 11) fpar(ftext(x,
  fp_text(font.family = SERIF, font.size = sz, bold = bold)),
  fp_p = fp_par(padding.bottom = 9, line_spacing = 1.2))

cl <- read_docx() %>%
  body_add_fpar(p("[Author names, degrees, affiliations, address]", sz = 10)) %>%
  body_add_fpar(p(format(Sys.Date(), "%d %B %Y"), sz = 10)) %>%
  body_add_fpar(p("The Editors, Health Affairs", bold = TRUE)) %>%
  body_add_fpar(p("Dear Editors,")) %>%
  body_add_fpar(p(paste(
    "We submit for your consideration “Hourly Workers Reach Surgery Later Than",
    "Salaried Workers, Despite Paying Less,” a research article examining whether",
    "delayed surgical presentation among insured workers tracks what they owe or the",
    "conditions of the job that provides their coverage."))) %>%
  body_add_fpar(p(paste(
    "Using national commercial claims for 66,070 adults undergoing inguinal hernia",
    "repair between 2016 and 2021, we find that patients whose employer classified",
    "them as hourly arrived at surgery with an obstructed or gangrenous hernia 10.9",
    "percent of the time, compared with 8.2 percent of salaried patients. The 2.7",
    "percentage-point gap was unchanged by adjustment for demography and benefit",
    "design."))) %>%
  body_add_fpar(p(paste(
    "The finding we believe will interest your readers is the direction of the cost",
    "gradient. Hourly workers paid less out of pocket, not more, and were half as",
    "likely to hold a high-deductible plan. Adjusting for what patients actually paid",
    "widened the disparity rather than narrowing it, and high-deductible enrollment",
    "bore no relation to the outcome. Financial exposure and clinical urgency moved",
    "in opposite directions across employment classification."))) %>%
  body_add_fpar(p(paste(
    "Three further analyses point toward time rather than money. Union coverage cut",
    "the gap by more than half. The association was larger among patients who were",
    "themselves the policy holder. And it persisted within every industry examined,",
    "including mining, the most physically demanding sector in the data, which argues",
    "against the alternative that manual labor simply accelerates hernia progression."))) %>%
  body_add_fpar(p(paste(
    "The policy implication is that the instruments currently aimed at delayed care",
    "act on the patient's bill, and none of them produces time away from a shift. If",
    "the binding constraint is unpaid or penalized absence, the effective levers are",
    "paid leave and scheduling policy rather than insurance design. Employment",
    "classification is recorded in commercial claims and almost never used; it",
    "identifies a population at elevated risk of late surgical presentation at no",
    "additional data cost."))) %>%
  body_add_fpar(p(paste(
    "The manuscript is not under consideration elsewhere and all authors have",
    "approved the submission. Analytic code and the accompanying regression test",
    "suite are available on request. We note two companion analyses of the same",
    "extract, addressing benefit design and site of service, which are unpublished",
    "and are not cited here."))) %>%
  body_add_fpar(p("Thank you for your consideration.")) %>%
  body_add_fpar(p("Sincerely,")) %>%
  body_add_fpar(p("[Corresponding author name, degrees]", sz = 10))
print(cl, target = file.path(SUB, "Cover Letter.docx"))

# ---------------------------------------------------------------------------
# 4. Analysis code + checklist
# ---------------------------------------------------------------------------
figdir <- file.path(SUB, "figures")
dir.create(figdir, showWarnings = FALSE, recursive = TRUE)
for (f in list.files(FIGD, pattern = "[.]tiff$", full.names = TRUE))
  file.copy(f, figdir, overwrite = TRUE)
cat("figures copied to submission:",
    paste(list.files(figdir), collapse = ", "), "
")

cdir <- file.path(SUB, "analysis code")
dir.create(file.path(cdir, "tests"), showWarnings = FALSE, recursive = TRUE)
file.copy(list.files(".", pattern = "\\.R$", full.names = TRUE), cdir, overwrite = TRUE)
file.copy("tests/test_pipeline.R", file.path(cdir, "tests"), overwrite = TRUE)

writeLines(c(
  "# Health Affairs submission — contents and checklist",
  "",
  sprintf("Assembled %s by `code/08_build_submission.R`. Re-run that script after",
          format(Sys.Date(), "%d %B %Y")),
  "editing `manuscript/PAPERB_healthaffairs.md`; do not hand-edit the Word files,",
  "as they are regenerated from the markdown.",
  "",
  "## Files",
  "",
  "| File | Notes |",
  "|---|---|",
  "| `Manuscript.docx` | Rendered from the markdown draft |",
  "| `Exhibits.docx` | Exhibits 1-4 plus Appendix Exhibit A1 |",
  "| `Cover Letter.docx` | Author block and signature are placeholders |",
  "| `analysis code/` | Full pipeline, `run_all.R` reproduces every number |",
  "",
  "## Before submitting",
  "",
  "- [ ] Author names, degrees, affiliations, corresponding author",
  "- [ ] Conflict-of-interest and funding statements",
  "- [ ] Verify formatting against current Health Affairs author guidelines:",
  "      word limit, abstract length, exhibit count, reference/endnote style",
  "- [ ] Renumber references as endnotes in one pass if required",
  "- [ ] Confirm the IRB exemption wording matches the local determination",
  "- [ ] Figure 1 supplied separately as 600 dpi TIFF if exhibits are submitted as files",
  "",
  "## Deliberately excluded",
  "",
  "- The two companion analyses from this extract are unpublished and are not cited.",
  "- No state paid-sick-leave quasi-experiment; see `output/DDD_NOT_PURSUED.md`.",
  "- Analysis timing is recorded in `output/ANALYSIS_LOG.md`, not claimed in the text."),
  file.path(SUB, "SUBMISSION_CHECKLIST.md"))

cat("\nSubmission package written to:\n  ", SUB, "\n")
print(data.frame(file = list.files(SUB, recursive = FALSE)), row.names = FALSE)
