#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  required_packages <- c("readxl", "knitr")
  missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing_packages) > 0) {
    stop(
      paste0(
        "The following packages are required: ",
        paste(missing_packages, collapse = ", "),
        ". Install them via install.packages()."
      ),
      call. = FALSE
    )
  }
})

has_kable_extra <- requireNamespace("kableExtra", quietly = TRUE)

script_directory <- function() {
  if (!interactive()) {
    args <- commandArgs(trailingOnly = FALSE)
    file_arg <- grep("^--file=", args, value = TRUE)
    if (length(file_arg) == 1) {
      return(dirname(normalizePath(sub("^--file=", "", file_arg))))
    }
  }
  getwd()
}

format_value <- function(x) {
  if (is.na(x)) {
    return("N/A")
  }
  if (!is.numeric(x)) {
    x <- as.numeric(x)
    if (is.na(x)) {
      return("N/A")
    }
  }
  if (x == 0) {
    return("0.0000")
  }
  ax <- abs(x)
  if (ax >= 1000) {
    return(formatC(x, format = "f", digits = 1, big.mark = ","))
  }
  if (ax >= 10) {
    return(formatC(x, format = "f", digits = 2))
  }
  if (ax >= 1) {
    return(formatC(x, format = "f", digits = 3))
  }
  if (ax >= 0.1) {
    return(formatC(x, format = "f", digits = 4))
  }
  if (ax >= 0.01) {
    return(formatC(x, format = "f", digits = 5))
  }
  formatC(x, format = "e", digits = 2)
}

unique_subject_count <- function(df, subject_columns) {
  if (length(subject_columns) == 0) {
    return(NA_integer_)
  }
  column <- subject_columns[1]
  if (!column %in% names(df)) {
    return(NA_integer_)
  }
  values <- df[[column]]
  values <- values[!is.na(values)]
  if (is.character(values)) {
    values <- trimws(values)
    values <- values[nzchar(values)]
  }
  length(unique(values))
}

summarise_numeric <- function(df, exclude_columns = character()) {
  if (nrow(df) == 0) {
    return(data.frame())
  }
  columns <- names(df)
  columns <- columns[!columns %in% exclude_columns]
  summaries <- lapply(columns, function(col) {
    values <- df[[col]]
    if (inherits(values, c("POSIXt", "Date"))) {
      return(NULL)
    }
    if (is.logical(values)) {
      values <- as.numeric(values)
    }
    if (!is.numeric(values)) {
      values <- suppressWarnings(as.numeric(as.character(values)))
    }
    values <- values[!is.na(values)]
    if (!length(values)) {
      return(NULL)
    }
    data.frame(
      Measure = col,
      Count = length(values),
      Mean = mean(values),
      `Std Dev` = if (length(values) >= 2) stats::sd(values) else NA_real_,
      Min = min(values),
      Max = max(values),
      stringsAsFactors = FALSE
    )
  })
  summaries <- Filter(Negate(is.null), summaries)
  if (!length(summaries)) {
    return(data.frame())
  }
  stats <- do.call(rbind, summaries)
  rownames(stats) <- NULL
  stats
}

format_summary_for_table <- function(summary_df) {
  if (!nrow(summary_df)) {
    return(summary_df)
  }
  formatted <- summary_df
  formatted$Count <- formatC(summary_df$Count, format = "d", big.mark = ",")
  numeric_columns <- setdiff(names(summary_df), c("Measure", "Count"))
  for (col in numeric_columns) {
    formatted[[col]] <- vapply(summary_df[[col]], format_value, character(1))
  }
  formatted
}

render_section <- function(summary_df, title, subject_total) {
  if (!nrow(summary_df)) {
    return("")
  }
  formatted <- format_summary_for_table(summary_df)
  column_group <- c(" " = 1, "Descriptive statistics" = ncol(formatted) - 1)
  kbl <- knitr::kable(
    formatted,
    format = "html",
    caption = title,
    escape = FALSE,
    align = c("l", rep("r", ncol(formatted) - 1))
  )
  if (has_kable_extra) {
    kbl <- kableExtra::kable_styling(
      kbl,
      bootstrap_options = c("striped", "hover", "condensed"),
      full_width = FALSE,
      font_size = 13
    )
    kbl <- kableExtra::add_header_above(kbl, column_group, bold = TRUE)
    kbl <- kableExtra::footnote(
      kbl,
      general = paste0("Subjects included: ", format(subject_total, big.mark = ",")),
      general_title = ""
    )
    return(paste(kbl, collapse = "\n"))
  }
  table_html <- paste(kbl, collapse = "\n")
  subject_html <- paste0(
    '<p class="subject-count"><strong>Subjects included:</strong> ',
    format(subject_total, big.mark = ","),
    "</p>"
  )
  paste(table_html, subject_html, sep = "\n")
}

compose_report <- function(sections, title, introduction, css) {
  body_sections <- if (length(sections)) {
    vapply(sections, function(section) {
      paste0(
        '<section class="report-section">\n',
        '<h2>', section$title, '</h2>\n',
        section$table,
        '\n</section>'
      )
    }, character(1))
  } else {
    character(0)
  }
  paste0(
    '<!DOCTYPE html>\n<html lang="en">\n<head>\n<meta charset="UTF-8"/>\n',
    '<meta name="viewport" content="width=device-width, initial-scale=1"/>\n',
    '<title>', title, '</title>\n',
    '<style>\n', css, '\n</style>\n',
    '</head>\n<body>\n',
    '<main class="report">\n<h1>', title, '</h1>\n',
    '<p class="introduction">', introduction, '</p>\n',
    paste(body_sections, collapse = "\n\n"),
    '\n</main>\n</body>\n</html>'
  )
}

create_css <- function() {
  paste(
    "body { font-family: 'Helvetica Neue', Arial, sans-serif; margin: 2rem auto; max-width: 960px;",
    "color: #1f2933; background-color: #f9fbfd; line-height: 1.6; }",
    ".report h1 { font-size: 2rem; margin-bottom: 0.75rem; color: #102a43; }",
    ".report-section { margin-top: 2rem; }",
    ".report-section h2 { font-size: 1.35rem; margin-bottom: 0.5rem; color: #243b53; border-bottom: 2px solid #bcccdc; padding-bottom: 0.25rem; }",
    ".introduction { font-size: 1rem; margin-bottom: 1.5rem; }",
    "table { border-collapse: collapse; margin: 0.75rem 0 0.25rem; width: auto; min-width: 60%; }",
    "th, td { border: 1px solid #d9e2ec; padding: 0.55rem 0.75rem; text-align: right; font-size: 0.95rem; }",
    "th:first-child, td:first-child { text-align: left; }",
    "thead tr { background-color: #243b53; color: #f0f4f8; }",
    "tbody tr:nth-child(even) { background-color: #edf2f7; }",
    "tbody tr:hover { background-color: #d9e2ec; }",
    "caption { caption-side: top; text-align: left; font-weight: 600; font-size: 1.05rem; margin-bottom: 0.35rem; color: #102a43; }",
    ".subject-count { font-size: 0.9rem; color: #486581; margin: 0.15rem 0 0.75rem; }",
    "@media print { body { background-color: #ffffff; } table { width: 100%; } .report-section { page-break-inside: avoid; } }",
    sep = "\n"
  )
}

main <- function() {
  root <- script_directory()
  flash_path <- file.path(root, "AllSubjectsResultsFlash.xlsx")
  flicker_path <- file.path(root, "AllSubjectsResultsFlicker.xlsx")
  psych_path <- file.path(root, "analysis_psychophysical_data.csv")

  if (!file.exists(flash_path) || !file.exists(flicker_path) || !file.exists(psych_path)) {
    stop("One or more input data files could not be located in the script directory.", call. = FALSE)
  }

  flash_df <- readxl::read_excel(flash_path, na = c("", "NA", "N/A", "NaN"))
  flicker_df <- readxl::read_excel(flicker_path, na = c("", "NA", "N/A", "NaN"))
  psych_df <- utils::read.csv(psych_path, na.strings = c("", "NA", "N/A", "NaN"), stringsAsFactors = FALSE)

  sections <- list(
    list(
      title = "Flash ERG Timing and Amplitude Metrics",
      summary = summarise_numeric(flash_df, exclude_columns = c("SubjectID")),
      subjects = unique_subject_count(flash_df, "SubjectID")
    ),
    list(
      title = "Flicker ERG/VEP Amplitude, Phase, and SNR Metrics",
      summary = summarise_numeric(flicker_df, exclude_columns = c("SubjectID")),
      subjects = unique_subject_count(flicker_df, "SubjectID")
    ),
    list(
      title = "Psychophysical Performance Metrics",
      summary = summarise_numeric(psych_df, exclude_columns = c("subject")),
      subjects = unique_subject_count(psych_df, "subject")
    )
  )

  sections <- lapply(sections, function(section) {
    table_html <- render_section(section$summary, section$title, section$subjects)
    list(title = section$title, table = table_html)
  })

  sections <- Filter(function(section) nzchar(section$table), sections)

  css <- create_css()
  report_title <- "Descriptive Statistics for ERG/VEP and Psychophysics"
  introduction <- paste(
    "This report summarises the distribution of ERG/VEP measurements and psychophysical metrics.",
    "Counts refer to the number of non-missing observations for each measure, and variability is conveyed",
    "with the sample standard deviation alongside minimum and maximum values to support publication-ready reporting."
  )

  html_report <- compose_report(sections, report_title, introduction, css)
  output_path <- file.path(root, "Descriptive_Statistics_ERG_VEP_Psychophysics_Table.html")
  writeLines(html_report, output_path)

  message("Publishable-quality tables saved to: ", output_path)
  if (!has_kable_extra) {
    message("Tip: install.packages('kableExtra') for enhanced typographic styling.")
  }
}

main()
