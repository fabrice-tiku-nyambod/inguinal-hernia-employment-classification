# 09_wordcount.R -- word counts against Health Affairs article-type limits.
#
# Source: Health Affairs "Determine Your Article Type", as supplied by the author.
# For a Research Article, effective 1 April 2024:
#
#   * limit is 3,250 words counting the ABSTRACT AND MAIN TEXT TOGETHER
#   * endnotes, exhibits, exhibit lists, and appendix material are EXCLUDED
#   * up to 4 exhibits (tables and figures)
#   * endnotes are for citations only and may not carry methods or results text,
#     which is explicitly called out as a way of evading the limit
#
# Other article types are listed for reference; switch TYPE to re-check against one.

HA_TYPES <- list(
  research    = list(words = 3250, exhibits = 4, counts_notes = FALSE),
  analysis    = list(words = 5000, exhibits = 4, counts_notes = TRUE),
  datawatch   = list(words = 2000, exhibits = 5, counts_notes = TRUE),
  review      = list(words = 5000, exhibits = 4, counts_notes = TRUE),
  commentary  = list(words = 4000, exhibits = 3, counts_notes = TRUE),
  policy_insight = list(words = 5000, exhibits = 4, counts_notes = TRUE))

TYPE   <- "research"
LIMITS <- HA_TYPES[[TYPE]]

# Sections counted toward the limit for a Research Article: abstract + main text.
# Article Information, Notes, Exhibits, and the exhibit list are excluded.
count_md <- function(path) {
  x <- readLines(path, warn = FALSE)
  cur <- "front"; sec <- character(length(x))
  for (i in seq_along(x)) {
    if (grepl("^## ", x[i])) {
      h <- tolower(trimws(sub("^## +", "", x[i])))
      cur <- if (grepl("^abstract", h)) "abstract"
             else if (grepl("^(introduction|study data|study results|discussion|conclusion)", h)) "main"
             else if (grepl("^exhibit", h)) "exhibits"
             else if (grepl("^(notes|references)", h)) "notes"
             else "excluded"
    }
    sec[i] <- cur
  }
  wc <- function(which) {
    v <- x[sec == which & !grepl("^#+ |^---|^>", x)]
    v <- gsub("<sup>.*?</sup>", "", v)                  # citation markers
    v <- gsub("\\[([^]]*)\\]\\([^)]*\\)", "\\1", v)     # keep link text, drop URL
    v <- gsub("\\*+|`[^`]*`", "", v)
    sum(lengths(strsplit(trimws(v), "\\s+")[nzchar(trimws(v))]))
  }
  a <- wc("abstract"); m <- wc("main")
  list(abstract = a, main = m, counted = a + m,
       exhibits = sum(grepl("^\\*\\*Exhibit ", x)),
       appendix_exhibits = sum(grepl("^\\*\\*Appendix Exhibit ", x)),
       notes = sum(grepl("^[0-9]+\\. ", x[sec == "notes"])))
}

report <- function(path, label) {
  r <- count_md(path)
  cat("\n================ ", label, " ================\n", sep = "")
  cat(sprintf("  article type      %s (limit %s words, %s exhibits)\n",
              TYPE, format(LIMITS$words, big.mark = ","), LIMITS$exhibits))
  cat(sprintf("    abstract        %5d\n", r$abstract))
  cat(sprintf("    main text       %5d\n", r$main))
  cat(sprintf("    COUNTED TOTAL   %5d / %s%s\n", r$counted,
              format(LIMITS$words, big.mark = ","),
              if (r$counted <= LIMITS$words)
                sprintf("   OK, %s to spare", format(LIMITS$words - r$counted, big.mark = ","))
              else sprintf("   OVER by %s", format(r$counted - LIMITS$words, big.mark = ","))))
  cat(sprintf("    exhibits        %5d / %d%s\n", r$exhibits, LIMITS$exhibits,
              if (r$exhibits <= LIMITS$exhibits) "   OK" else "   OVER"))
  cat(sprintf("    appendix exhib. %5d   (excluded from the limit)\n", r$appendix_exhibits))
  cat(sprintf("    endnotes        %5d   (excluded from the limit)\n", r$notes))
  invisible(r)
}

if (sys.nframe() == 0) {
  ROOT <- normalizePath("..", mustWork = FALSE)
  ha <- file.path(ROOT, "manuscript", "PAPERB_healthaffairs.md")
  if (file.exists(ha)) report(ha, "Health Affairs Research Article")
  cat("\n  Endnotes must carry citations only. Methods or results text moved into\n")
  cat("  the notes to save words is explicitly disallowed.\n")
}
