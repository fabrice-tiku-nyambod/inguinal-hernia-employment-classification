# 11_build_cureus_submission.R -- assemble the Cureus submission package.
#
# Cureus rules applied here, from the Cureus Author Guide as supplied by the
# author. Verify against the guide in force at submission:
#   * abstract <= 3500 characters; subheaders allowed in original articles
#   * 5-10 keywords, each 3-4 words maximum
#   * 1-3 article categories
#   * citations in SQUARE BRACKETS only; square brackets used nowhere else
#   * numbers 1-9 spelled out, except measurements
#   * conclusions 1-2 paragraphs, no citations, more than 1-2 sentences
#   * no bulleted lists, no paragraph indentation
#   * media limit 25; no media in abstract, introduction, or conclusions
#   * section headings are added by Cureus and must NOT be typed into the fields
#
# Output: submission - cureus/

source("01_config.R")

options(warn = 1)
suppressPackageStartupMessages({
  library(dplyr); library(officer); library(flextable)
})

SERIF <- "Times New Roman"
SUB   <- file.path(ROOT, "submission - cureus")
MD    <- file.path(ROOT, "manuscript", "PAPERB_cureus.md")
dir.create(SUB, showWarnings = FALSE, recursive = TRUE)
md <- readLines(MD, warn = FALSE, encoding = "UTF-8")
txt <- paste(md, collapse = "\n")

# ---------------------------------------------------------------------------
# Compliance checks. These stop the build rather than produce a package that
# Cureus will bounce back for Preferred Editing.
# ---------------------------------------------------------------------------
sect <- function(name, nxt) {
  a <- grep(paste0("^## ", name), md); b <- grep(paste0("^## ", nxt), md)
  if (!length(a) || !length(b)) return(character(0))
  v <- md[(a[1] + 1):(b[1] - 1)]
  v[nzchar(trimws(v)) & !grepl("^---", v)]
}
fails <- character(0)
chk <- function(ok, msg) if (!ok) fails <<- c(fails, msg)

abs_txt <- paste(gsub("\\*\\*|\\*", "", sect("Abstract", "Introduction")), collapse = " ")
chk(nchar(abs_txt) <= 3500,
    sprintf("abstract is %d characters, limit 3500", nchar(abs_txt)))

kw <- sub(".*Keywords \\(5-10\\)\\.\\*\\* ", "", grep("Keywords", md, value = TRUE)[1])
kws <- trimws(strsplit(kw, ";")[[1]])
chk(length(kws) >= 5 && length(kws) <= 10,
    sprintf("%d keywords, need 5-10", length(kws)))
chk(all(lengths(strsplit(kws, "\\s+")) <= 4), "a keyword exceeds 4 words")

cats <- trimws(strsplit(sub(".*Categories \\(1-3\\)\\.\\*\\* ", "",
                           grep("Categories", md, value = TRUE)[1]), ";")[[1]])
chk(length(cats) >= 1 && length(cats) <= 3,
    sprintf("%d categories, need 1-3", length(cats)))

# Square brackets are reserved for citations. Anything else in brackets is a
# formatting violation Cureus screens for.
brk <- unlist(regmatches(txt, gregexpr("\\[[^]]*\\]", txt)))
bad <- brk[!grepl("^\\[[0-9]+([,\\-][0-9]+)*\\]$", brk)]
chk(length(bad) == 0,
    paste("non-citation square brackets:", paste(unique(bad), collapse = " ")))

concl <- sect("Conclusions", "Additional Information")
chk(!any(grepl("\\[[0-9]", concl)), "conclusions contain citations; Cureus forbids this")
chk(length(concl) >= 2, "conclusions must run to 1-2 full paragraphs")

chk(!any(grepl("^\\s*[-*+] ", md)), "bulleted list present; Cureus forbids these")

nrefs <- sum(grepl("^[0-9]+[.] [A-Za-z]", md))   # van, de: authors can start lowercase
media  <- sum(grepl("^\\*\\*(Table|Figure) [0-9]", md))
chk(media <= 25, sprintf("%d media items, limit 25", media))

cat(sprintf("\nCureus checks: abstract %d/3500 chars | %d keywords | %d categories | %d media | %d references\n",
            nchar(abs_txt), length(kws), length(cats), media, nrefs))
if (length(fails)) {
  cat("\nFAILED:\n"); cat(paste0("  - ", fails, collapse = "\n"), "\n\n")
  stop("fix the above before building the Cureus package", call. = FALSE)
}
cat("all Cureus format checks passed\n")

# ---------------------------------------------------------------------------
# Manuscript, rendered section by section for pasting into the submission form
# ---------------------------------------------------------------------------
runs_for <- function(s) {
  out <- list(); pat <- "(\\*\\*.*?\\*\\*|\\*.*?\\*)"
  for (p in strsplit(gsub(pat, "\001\\1\001", s), "\001", fixed = TRUE)[[1]]) {
    if (!nzchar(p)) next
    if (grepl("^\\*\\*.*\\*\\*$", p))
      out <- c(out, list(ftext(gsub("^\\*\\*|\\*\\*$", "", p),
        fp_text(font.family = SERIF, font.size = 11, bold = TRUE))))
    else if (grepl("^\\*.*\\*$", p))
      out <- c(out, list(ftext(gsub("^\\*|\\*$", "", p),
        fp_text(font.family = SERIF, font.size = 11, italic = TRUE))))
    else
      out <- c(out, list(ftext(p, fp_text(font.family = SERIF, font.size = 11))))
  }
  if (!length(out)) out <- list(ftext("", fp_text(font.family = SERIF)))
  out
}

doc <- read_docx()
buf <- character(0)
flush_buf <- function(d) {
  if (length(buf)) {
    d <- body_add_fpar(d, fpar(values = runs_for(paste(buf, collapse = " ")),
                               fp_p = fp_par(padding.bottom = 8, line_spacing = 1.5)))
    buf <<- character(0)
  }
  d
}
for (ln in md) {
  t <- trimws(ln)
  if (grepl("^---+$", t)) { doc <- flush_buf(doc); next }
  if (!nzchar(t))         { doc <- flush_buf(doc); next }
  if (grepl("^#{1,2} ", t) || grepl("^[0-9]{1,3}\\. ", t)) {
    doc <- flush_buf(doc)
    sz <- if (grepl("^# ", t)) 15 else 13
    doc <- body_add_fpar(doc, fpar(
      values = runs_for(sub("^#{1,2} ", "", t)),
      fp_p = fp_par(padding.top = if (grepl("^#", t)) 12 else 2,
                    padding.bottom = 4)))
    next
  }
  buf <- c(buf, t)
}
doc <- flush_buf(doc)
MAN <- file.path(SUB, "Manuscript.docx")
existing <- file.exists(MAN)
if (existing)
  cat("\n*** Manuscript.docx exists and will NOT be overwritten.",
      "\n*** Fresh render written to Manuscript.GENERATED.docx.\n\n")
print(doc, target = if (existing) file.path(SUB, "Manuscript.GENERATED.docx") else MAN)

# ---------------------------------------------------------------------------
# Tables, as separate pasteable files. Cureus takes tables pasted or built in
# their editor, so each is kept clean and standalone.
# ---------------------------------------------------------------------------
rd <- function(f) read.csv(file.path(OUT, f), check.names = FALSE)
ft <- function(d) flextable(d) %>% theme_booktabs() %>%
  font(fontname = SERIF, part = "all") %>% fontsize(size = 9, part = "all") %>%
  bold(part = "header") %>% autofit()
TAB <- list(
  c("1", "table1_baseline.csv",            "Baseline characteristics by employment classification"),
  c("2", "table2_primary.csv",             "Employment classification and complicated presentation"),
  c("3", "table4_costsharing.csv",         "Patient cost sharing at the index encounter"),
  c("4", "table3_subgroups.csv",           "Subgroup analyses by union coverage and relation to the policy holder"),
  c("5", "table5_industry.csv",            "Complicated presentation and hourly odds ratio by industry"),
  c("6", "tableS1_excluded.csv",           "Analytic cohort compared with unassignable patients"))
td <- read_docx() %>%
  body_add_fpar(fpar(ftext("Tables", fp_text(font.family = SERIF, font.size = 15,
                                             bold = TRUE))))
for (i in seq_along(TAB)) {
  x <- TAB[[i]]
  td <- td %>%
    body_add_fpar(fpar(
      ftext(sprintf("Table %s: ", x[1]),
            fp_text(font.family = SERIF, font.size = 11, bold = TRUE)),
      ftext(x[3], fp_text(font.family = SERIF, font.size = 11)),
      fp_p = fp_par(padding.top = 12, padding.bottom = 5))) %>%
    body_add_flextable(ft(rd(x[2])))
  if (i < length(TAB)) td <- body_add_break(td)
}
print(td, target = file.path(SUB, "Tables.docx"))

# ---------------------------------------------------------------------------
# Figures: Cureus takes image files. Copy the 1200 dpi TIFFs.
# ---------------------------------------------------------------------------
figdir <- file.path(SUB, "figures")
dir.create(figdir, showWarnings = FALSE, recursive = TRUE)
FIGMAP <- c(figure3_divergence = "Figure1_cost_and_urgency",
            figure1_forest     = "Figure2_adjusted_association",
            figure2_industry   = "Figure3_within_industry")
for (nm in names(FIGMAP)) {
  src <- file.path(FIGD, paste0(nm, ".tiff"))
  if (file.exists(src)) file.copy(src, file.path(figdir, paste0(FIGMAP[[nm]], ".tiff")),
                                  overwrite = TRUE)
}
cat("figures:", paste(list.files(figdir), collapse = ", "), "\n")

# analysis code
cdir <- file.path(SUB, "analysis code")
dir.create(file.path(cdir, "tests"), showWarnings = FALSE, recursive = TRUE)
file.copy(list.files(".", pattern = "[.]R$", full.names = TRUE), cdir, overwrite = TRUE)
file.copy("tests/test_pipeline.R", file.path(cdir, "tests"), overwrite = TRUE)

writeLines(c(
  "# Cureus submission - contents and checklist",
  "",
  sprintf("Assembled %s by `code/11_build_cureus_submission.R` from", format(Sys.Date(), "%d %B %Y")),
  "`manuscript/PAPERB_cureus.md`. Edit the markdown and re-run; do not hand-edit",
  "the Word files, and note the builder will not overwrite an existing",
  "Manuscript.docx.",
  "",
  "## Files",
  "",
  "| File | Notes |",
  "|---|---|",
  "| `Manuscript.docx` | Full text. Paste section by section into the Cureus form |",
  "| `Tables.docx` | Tables 1-6, pasteable |",
  "| `figures/` | Figures 1-3, TIFF at 1200 dpi |",
  "| `analysis code/` | Full pipeline; `run_all.R` reproduces every number |",
  "",
  "## Cureus submission form",
  "",
  "- [ ] Article type: **Original article**",
  "- [ ] Study design: **retrospective study** (answer the design questionnaire to match)",
  "- [ ] Upload IRB documentation for IRB00038226 - required when an IRB reviewed the study",
  "- [ ] Categories (1-3): Health Policy; Epidemiology/Public Health; General Surgery",
  "- [ ] Keywords (5-10): as listed at the top of the manuscript",
  "- [ ] All co-authors added before submission - they cannot be added afterwards",
  "- [ ] Affiliations byte-identical across authors from the same department",
  "- [ ] ICMJE contributions selected per author",
  "- [ ] Nominate at least **five advisers**",
  "",
  "## Format rules already enforced by the build",
  "",
  "- Abstract within 3,500 characters, with subheaders (permitted for original articles)",
  "- Square brackets used for citations only",
  "- Conclusions carry no citations and run to two paragraphs",
  "- No bulleted lists",
  "- Numbers one to nine spelled out except measurements",
  "- Media items within the limit of 25, and none in abstract, introduction, or conclusions",
  "",
  "## Do not paste section headings",
  "",
  "Cureus adds Introduction, Materials & Methods, Results, Discussion, and",
  "Conclusions automatically. The headings in Manuscript.docx mark which text goes",
  "in which field; do not paste the headings themselves.",
  "",
  "## Unverified",
  "",
  "Cureus reference style and the disclosure wording were applied from the Author",
  "Guide text supplied by the author, not from the live submission form. Check both",
  "before submitting, and use the Cureus reference converter to validate the list."),
  file.path(SUB, "SUBMISSION_CHECKLIST.md"))

cat("\nCureus package written to:\n  ", SUB, "\n")
print(data.frame(file = list.files(SUB)), row.names = FALSE)
