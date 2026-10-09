# =============================================================================*
# qc_functions.R ----
# -----------------------------------------------------------------------------*
# PURPOSE
#   Shared QC helpers used by every numbered pipeline script (prep_cases and
#   rates). Each script:
#     1. opens a QC context with qc_start() in Step 0,
#     2. records printouts (qc_log) and pass/fail checks (qc_check) as it runs,
#     3. closes with qc_finish() in its QC CHECK section, which writes ONE
#        plain-text QC report (cover page + results + printouts).
#
#   FAILING OUT LOUD
#     - qc_check(..., on_fail = "stop") records a FAIL, writes the report right
#       away, then calls stop() so the R session halts.
#     - qc_check(..., on_fail = "warn") records a WARN, prints it, and raises an
#       R warning. The script keeps running.
#     - Errors that do NOT come from qc_check() (a dplyr error, a missing input
#       file, a stopifnot) are caught by a session error handler that qc_start()
#       installs. It writes every open QC report with the error message before
#       R halts, then restores your previous error setting.
#
#   FAIL vs WARN (convention used across the pipeline)
#     FAIL = structural or logical problems that make outputs wrong or unusable
#            (missing inputs/columns, duplicated join keys, row counts changing
#            unexpectedly, cases not reconciling, suppression leaks).
#     WARN = data conditions that need a human decision but can be legitimate
#            (unmatched ZIPs, demo_keys without a population, NA denominators).
#
# INPUTS  : none (sourced)
# OUTPUTS : <qc_dir>/qc_<script_stem>.txt   one report per numbered script
#
# NESTING
#   Contexts stack, so 1_prep_cases_add_pops.R can source steps 2-4 and each
#   step still writes its own report. Re-sourcing this file keeps open contexts.
# =============================================================================*

# ---- Internal state (created once per R session) ----------------------------
if (!exists(".qc_env", envir = globalenv(), inherits = FALSE)) {
  assign(".qc_env", new.env(), envir = globalenv())
  .qc_env$stack         <- list()   # open contexts, innermost last
  .qc_env$completed     <- list()   # finished contexts, keyed by script name
  .qc_env$old_error     <- NULL     # user's options(error) before we installed ours
  .qc_env$handler_on    <- FALSE
  .qc_env$fail_recorded <- FALSE    # TRUE when a qc_check FAIL already logged the stop
}

# ---- Internal helpers --------------------------------------------------------
.qc_now <- function() format(Sys.time(), "%Y-%m-%d %H:%M:%S")

# Turn anything (text, data frame, vector, list) into printable lines.
.qc_capture <- function(x, max_rows = 100) {
  if (is.null(x)) return(character(0))
  if (is.character(x) && is.null(dim(x))) {
    if (length(x) > max_rows)
      x <- c(utils::head(x, max_rows),
             sprintf("... showing first %d of %d lines", max_rows, length(x)))
    return(x)
  }
  old <- options(width = 250)
  on.exit(options(old), add = TRUE)
  if (is.data.frame(x)) {
    n <- nrow(x)
    if (n == 0) return("(0 rows)")
    df <- as.data.frame(utils::head(x, max_rows))
    # Strip SPSS/haven labels so values print plainly.
    df[] <- lapply(df, function(col) {
      if (inherits(col, "haven_labelled")) { attributes(col) <- NULL }
      col
    })
    out <- utils::capture.output(print(df, row.names = FALSE))
    if (n > max_rows) out <- c(out, sprintf("... showing first %d of %d rows", max_rows, n))
    return(out)
  }
  utils::capture.output(print(x))
}

# Interactive session? (wrapped so the behaviour can be tested from Rscript)
.qc_is_interactive <- function() interactive()

# Stop loudly when a QC call has no open report to write to.
.qc_require_context <- function() {
  if (length(.qc_env$stack) == 0)
    stop("QC: no open QC report. Re-run the script from the top so qc_start() ",
         "(the 'Start QC log' block) runs before any QC check.", call. = FALSE)
}

.qc_add <- function(entry) {
  .qc_require_context()
  n <- length(.qc_env$stack)
  k <- length(.qc_env$stack[[n]]$entries)
  .qc_env$stack[[n]]$entries[[k + 1]] <- entry
  invisible(NULL)
}

.qc_reset_handler <- function() {
  if (isTRUE(.qc_env$handler_on)) options(error = .qc_env$old_error)
  .qc_env$handler_on    <- FALSE
  .qc_env$old_error     <- NULL
  .qc_env$stack         <- list()
  .qc_env$fail_recorded <- FALSE
}

# Session error handler: write every open report, then clean up.
.qc_on_error <- function() {
  msg <- trimws(geterrmessage())
  try({
    n <- length(.qc_env$stack)
    if (n > 0) {
      inner <- .qc_env$stack[[n]]$script
      for (i in rev(seq_len(n))) {
        is_inner <- (i == n)
        if (!(is_inner && isTRUE(.qc_env$fail_recorded))) {
          title <- if (is_inner) "Script stopped with an R error"
          else paste0("Stopped because a sourced step failed: ", inner)
          k <- length(.qc_env$stack[[i]]$entries)
          .qc_env$stack[[i]]$entries[[k + 1]] <- list(
            type = "check", status = "FAIL", title = title, lines = strsplit(msg, "\n")[[1]], time = .qc_now()
          )
        }
        .qc_write_report(.qc_env$stack[[i]], finished = FALSE)
        message("QC: FAILED run written to ", .qc_env$stack[[i]]$report)
      }
    }
  }, silent = TRUE)
  if (.qc_is_interactive()) {
    # Keep the open report(s) so line-by-line work can continue. The FAIL stays
    # recorded, so the final report will still show RUN STATUS: FAILED until the
    # script is re-run cleanly from the top.
    .qc_env$fail_recorded <- FALSE
    message("QC: report kept open. Fix the problem and re-run the script from the top ",
            "for a clean report.")
  } else {
    .qc_reset_handler()
  }
  invisible(NULL)
}

.qc_write_report <- function(ctx, finished) {
  entries <- ctx$entries
  st      <- vapply(entries, function(e) e$status, "")
  n_pass  <- sum(st == "PASS"); n_warn <- sum(st == "WARN"); n_fail <- sum(st == "FAIL")
  status  <- if (!finished || n_fail > 0) "FAILED"
  else if (n_warn > 0) "PASSED WITH WARNINGS" else "PASSED"
  
  rule <- strrep("=", 90)
  sub  <- strrep("-", 90)
  
  header <- c(
    rule, paste("QC CHECKPOINT:", ctx$script), rule,
    paste("Run started    :", ctx$started_chr),
    paste("QC checkpoint file written :", .qc_now()),
    if (!is.null(ctx$data_year)) paste("Data year      :", ctx$data_year),
    paste("User / machine :", Sys.info()[["user"]], "/", Sys.info()[["nodename"]]),
    paste("R version      :", R.version.string),
    "",
    paste("RUN STATUS     :", status),
    sprintf("Checks         : %d PASS | %d WARN | %d FAIL", n_pass, n_warn, n_fail),
    ""
  )
  
  cover <- if (!finished) {
    c(sub, "WHAT TO REVIEW/CONFIRM", sub,
      "THIS RUN DID NOT FINISH. Do not use any outputs written by this run.",
      "  1. Read the FAIL entry under 'FAILURES AND WARNINGS' below.",
      "  2. Fix the cause, then re-run the script from the top.",
      "  3. The full review checklist is written once the script completes.",
      "")
  } else {
    c(sub, "WHAT TO REVIEW/CONFIRM", sub,
      "How to read this report:",
      "  - RUN STATUS must be PASSED or PASSED WITH WARNINGS. FAILED means stop.",
      "  - Each WARN needs a reviewer decision: expected (write why below) or a",
      "    problem (fix and re-run).",
      "  - PRINTOUTS do not pass or fail on their own. They are there to eyeball",
      "    against the checklist below and against last year's report.",
      "",
      "A. Confirm in this QC checkpoint file:",
      if (length(ctx$confirm)) paste0("  [ ] ", ctx$confirm) else "  (none)",
      "",
      "B. Review these output files:",
      if (length(ctx$review_files)) paste0("  [ ] ", ctx$review_files)
      else "  (nothing beyond this report)",
      "",
      "Reviewer notes on WARNs: ______________________________________________",
      "")
  }
  
  issues <- Filter(function(e) e$status %in% c("WARN", "FAIL"), entries)
  issue_lines <- c(sub, "FAILURES AND WARNINGS (review first)", sub,
                   if (length(issues) == 0) "  None." else unlist(lapply(issues, function(e)
                     c(sprintf("[%s] %s  (%s)", e$status, e$title, e$time),
                       if (length(e$lines)) paste0("    ", e$lines), ""))),
                   "")
  
  checks <- Filter(function(e) e$type == "check", entries)
  summary_lines <- c(sub, "CHECK SUMMARY (run order)", sub,
                     if (length(checks) == 0) "  No checks recorded."
                     else vapply(checks, function(e) sprintf("  [%s] %s", e$status, e$title), ""),
                     "")
  
  detail_lines <- c(sub, "DETAILED LOG: CHECKS AND PRINTOUTS (run order)", sub,
                    unlist(lapply(entries, function(e) {
                      tag <- if (e$type == "log") "PRINTOUT" else e$status
                      c(sprintf("### [%s] %s  (%s)", tag, e$title, e$time),
                        if (length(e$lines)) e$lines else "  (no detail)",
                        "")
                    })),
                    rule, "END OF QC REPORT", rule)
  
  writeLines(c(header, cover, issue_lines, summary_lines, detail_lines), ctx$report)
  status
}

# ---- Public API --------------------------------------------------------------

# Open a QC context for a script. Call once, in Step 0.
#   reset = TRUE for top-level scripts (clears stale contexts from an earlier,
#   abandoned run). Leave FALSE for scripts that are sourced by an orchestrator.
qc_start <- function(script_name, qc_dir, data_year = NULL, reset = FALSE) {
  if (reset && length(.qc_env$stack) > 0) .qc_env$stack <- list()
  if (!dir.exists(qc_dir)) dir.create(qc_dir, recursive = TRUE)
  
  # Re-running the same script from the top replaces its open context.
  open_names <- vapply(.qc_env$stack, function(s) s$script, "")
  hit <- which(open_names == script_name)
  if (length(hit)) .qc_env$stack <- .qc_env$stack[seq_len(hit[1] - 1)]
  
  stem <- tools::file_path_sans_ext(basename(script_name))
  
  # Run start time on the FILE SYSTEM clock (not this computer's clock), taken
  # from a marker file in the qc folder. Output files on the same drive are
  # compared to this, so network-drive clock differences do not cause false FAILs.
  marker <- file.path(qc_dir, paste0(".qc_run_marker_", stem))
  writeLines(.qc_now(), marker)
  fs_started <- file.mtime(marker)
  unlink(marker)
  if (is.na(fs_started)) fs_started <- Sys.time()
  
  ctx <- list(
    script      = script_name,
    qc_dir      = qc_dir,
    report      = file.path(qc_dir, paste0("qc_", stem, ".txt")),
    started     = Sys.time(),
    fs_started  = fs_started,
    started_chr = .qc_now(),
    data_year   = data_year,
    entries     = list()
  )
  .qc_env$stack[[length(.qc_env$stack) + 1]] <- ctx
  .qc_env$fail_recorded <- FALSE
  
  if (!isTRUE(.qc_env$handler_on)) {
    .qc_env$old_error  <- getOption("error")
    options(error = .qc_on_error)
    .qc_env$handler_on <- TRUE
  }
  message("QC: started ", script_name, "  (report -> ", ctx$report, ")")
  invisible(ctx$report)
}

# Record the data year once it is known (shows in the report header).
qc_set_year <- function(year) {
  n <- length(.qc_env$stack)
  if (n > 0) .qc_env$stack[[n]]$data_year <- paste(year, collapse = ", ")
  invisible(year)
}

# Record a printout (table, vector, or text). Prints to the console too.
qc_log <- function(title, x = NULL, note = NULL, max_rows = 100) {
  .qc_require_context()
  lines <- c(if (!is.null(note)) paste("NOTE:", note), .qc_capture(x, max_rows))
  cat("\n---- QC PRINTOUT:", title, "----\n")
  if (length(lines)) cat(lines, sep = "\n")
  .qc_add(list(type = "log", status = "INFO", title = title, lines = lines, time = .qc_now()))
  invisible(x)
}

# Record a pass/fail check.
#   pass     : logical; NA or any FALSE counts as not passing
#   details  : text or data frame shown when the check does not pass
#   on_fail  : "stop" (FAIL, halts R) or "warn" (WARN, keeps running)
#   pass_note: optional text shown when the check passes
qc_check <- function(title, pass, details = NULL, on_fail = c("stop", "warn"),
                     pass_note = NULL, max_rows = 100) {
  .qc_require_context()
  on_fail <- match.arg(on_fail)
  ok      <- isTRUE(all(pass))
  status  <- if (ok) "PASS" else if (on_fail == "stop") "FAIL" else "WARN"
  lines   <- if (ok) (if (is.null(pass_note)) character(0) else pass_note)
  else .qc_capture(details, max_rows)
  
  cat(sprintf("[QC %s] %s\n", status, title))
  if (!ok && length(lines)) cat(paste0("    ", lines), sep = "\n")
  .qc_add(list(type = "check", status = status, title = title, lines = lines, time = .qc_now()))
  
  if (status == "WARN") warning("QC WARNING: ", title, call. = FALSE)
  if (status == "FAIL") {
    n <- length(.qc_env$stack)
    if (n > 0) .qc_write_report(.qc_env$stack[[n]], finished = FALSE)
    .qc_env$fail_recorded <- TRUE
    stop("QC FAIL: ", title, call. = FALSE)
  }
  invisible(ok)
}

# Close the current context and write the finished report.
#   confirm      : checklist items to confirm inside the QC report
#   review_files : output files (with what to look for) to open and review
qc_finish <- function(confirm = character(0), review_files = character(0)) {
  n <- length(.qc_env$stack)
  if (n == 0) {
    message("QC: no open QC context (was qc_start() run?). Nothing written.")
    return(invisible(NULL))
  }
  ctx <- .qc_env$stack[[n]]
  ctx$confirm      <- confirm
  ctx$review_files <- review_files
  status <- .qc_write_report(ctx, finished = TRUE)
  
  st <- vapply(ctx$entries, function(e) e$status, "")
  .qc_env$completed[[ctx$script]] <- list(
    status = status, report = ctx$report, finished = Sys.time(),
    n_warn = sum(st == "WARN"), n_fail = sum(st == "FAIL")
  )
  .qc_env$stack <- .qc_env$stack[seq_len(n - 1)]
  if (length(.qc_env$stack) == 0) .qc_reset_handler()
  
  message("QC: ", ctx$script, " -> ", status, "  (", ctx$report, ")")
  invisible(status)
}

# The current report's entries as a data frame, for scripts that put their QC
# results into a workbook as well as the text report. Call it after the checks
# and before qc_finish().
qc_report_table <- function(max_detail_chars = 2000) {
  .qc_require_context()
  entries <- .qc_env$stack[[length(.qc_env$stack)]]$entries
  if (length(entries) == 0)
    return(data.frame(order = integer(0), type = character(0), status = character(0),
                      item = character(0), time = character(0), detail = character(0)))
  data.frame(
    order  = seq_along(entries),
    type   = vapply(entries, function(e) if (e$type == "log") "PRINTOUT" else "CHECK", ""),
    status = vapply(entries, function(e) e$status, ""),
    item   = vapply(entries, function(e) e$title, ""),
    time   = vapply(entries, function(e) e$time, ""),
    detail = vapply(entries, function(e) {
      txt <- paste(e$lines, collapse = " | ")
      if (nchar(txt) > max_detail_chars)
        paste0(substr(txt, 1, max_detail_chars), " ... (see the text report for the rest)")
      else txt
    }, ""),
    stringsAsFactors = FALSE
  )
}

# Run start time of the current context (used to confirm files were written this run).
# Uses the file system clock captured by qc_start(); stops if no report is open.
qc_run_started <- function() {
  .qc_require_context()
  .qc_env$stack[[length(.qc_env$stack)]]$fs_started
}

# ---- Reusable checks ---------------------------------------------------------

# FAIL if an input file is missing.
qc_input_exists <- function(path, what = basename(path)) {
  qc_check(paste0("Input exists: ", what), file.exists(path),
           details = paste("Not found:", path))
}

# FAIL if required columns are missing from a data frame.
qc_required_columns <- function(df, cols, what) {
  missing <- setdiff(cols, names(df))
  qc_check(paste0(what, ": required columns present"), length(missing) == 0,
           details = paste("Missing:", paste(missing, collapse = ", ")))
}

# FAIL unless datayear.csv produced exactly one usable Year.
qc_data_year <- function(year_value) {
  qc_check("datayear.csv holds exactly one non-missing Year",
           length(year_value) == 1 && !is.na(year_value) && nzchar(year_value),
           details = paste("Values read:", paste(year_value, collapse = ", ")),
           pass_note = paste("Year =", year_value))
  qc_set_year(year_value)
}

# FAIL if any output file is missing or was not updated during this run.
qc_files_written <- function(paths, title = "Output files written this run") {
  since  <- qc_run_started() - 2   # 2-second tolerance for drive timestamp rounding
  exists <- file.exists(paths)
  mtime  <- file.mtime(paths)
  ok     <- exists & !is.na(mtime) & mtime >= since
  info   <- data.frame(
    file     = paths,
    exists   = exists,
    size_kb  = round(file.size(paths) / 1024, 1),
    modified = format(mtime, "%Y-%m-%d %H:%M:%S")
  )
  qc_check(title, all(ok), details = info[!ok, , drop = FALSE])
  qc_log(paste0(title, " (listing)"), info)
}

# Compare a raw variable with the recoded variable it produced, so the recode can
# be confirmed cell by cell: one row per raw value x recoded value, with the
# value labels attached (SPSS/haven labels when the column carries them).
#   df    : data frame
#   from  : name of the raw/original column (e.g. "CORErace5")
#   to    : name of the recoded/derived column (e.g. "RACE_ETH")
#   title : printout title; defaults to "<from> -> <to>"
#   note  : optional text (e.g. the intended mapping) shown above the table
#   check_unmapped: WARN when a non-missing raw value produced a missing recoded
#                   value. Set FALSE for printout-only comparisons.
qc_recode_crosstab <- function(df, from, to, title = NULL, note = NULL,
                               check_unmapped = TRUE, max_rows = 200) {
  qc_required_columns(df, c(from, to), paste0("Recode check ", from, " -> ", to))
  
  codes <- function(x) {
    if (is.numeric(unclass(x))) suppressWarnings(as.numeric(unclass(x))) else as.character(x)
  }
  label_map <- function(x) {
    l <- attr(x, "labels")
    if (is.null(l)) return(NULL)
    stats::setNames(names(l), as.character(unname(l)))
  }
  add_labels <- function(values, x) {
    m <- label_map(x)
    if (is.null(m)) return(rep(NA_character_, length(values)))
    unname(m[as.character(values)])
  }
  
  fv <- codes(df[[from]])
  tv <- codes(df[[to]])
  tab <- as.data.frame(table(from_value = fv, to_value = tv, useNA = "ifany"),
                       stringsAsFactors = FALSE)
  tab <- tab[tab$Freq > 0, , drop = FALSE]
  tab$from_label <- add_labels(tab$from_value, df[[from]])
  tab$to_label   <- add_labels(tab$to_value,   df[[to]])
  tab <- tab[, c("from_value", "from_label", "to_value", "to_label", "Freq")]
  names(tab) <- c(from, paste0(from, "_label"), to, paste0(to, "_label"), "n_rows")
  ord <- order(suppressWarnings(as.numeric(tab[[1]])), tab[[1]], na.last = TRUE)
  tab <- tab[ord, , drop = FALSE]
  
  qc_log(if (is.null(title)) paste0("Recode check: ", from, " -> ", to) else title,
         tab, note = note, max_rows = max_rows)
  if (check_unmapped) {
    unmapped <- tab[!is.na(tab[[1]]) & is.na(tab[[3]]), , drop = FALSE]
    qc_check(paste0("Recode check ", from, " -> ", to, ": no raw value left unrecoded"),
             nrow(unmapped) == 0, on_fail = "warn",
             details = c("These raw values produced a missing recoded value:",
                         .qc_capture(unmapped)))
  }
  invisible(tab)
}

# Write a SEPARATE, short report (for example a file handed to a data checker).
# This does not touch the script's own QC report.
#   path     : file to write
#   title    : report title
#   intro    : character vector of instructions for the reader
#   sections : list of list(title =, note =, content =) where content is text or
#              a data frame
qc_side_report <- function(path, title, intro = character(0), sections = list(),
                           max_rows = 500) {
  rule <- strrep("=", 90)
  sub  <- strrep("-", 90)
  out <- c(rule, title, rule,
           paste("Written       :", .qc_now()),
           paste("Prepared by   :", Sys.info()[["user"]]),
           "",
           sub, "WHAT TO CHECK", sub,
           if (length(intro)) intro else "(no instructions provided)",
           "",
           "Reviewed by: ______________________   Date: ______________",
           "")
  for (sec in sections) {
    out <- c(out, sub, sec$title, sub,
             if (!is.null(sec$note)) c(paste("NOTE:", sec$note), "") else NULL,
             .qc_capture(sec$content, max_rows), "")
  }
  out <- c(out, rule, "END OF DATA CHECK FILE", rule)
  dir <- dirname(path)
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  writeLines(out, path)
  message("Data check file written: ", path)
  invisible(path)
}

# ---- prep_cases helpers --------------------------------------------------------

# Consistency of the demographic *_case counters added by the demographics script.
#   race_counters_gated: TRUE for HCAI (every counter requires CountyCases == 1),
#   FALSE for VRBIS (several race counters are not gated; open item, left as is).
qc_demographic_counters <- function(df, race_counters_gated) {
  age10 <- c("age0_9_case", "age10_19_case", "age20_29_case", "age30_39_case",
             "age40_49_case", "age50_59_case", "age60_69_case", "age70_79_case",
             "age80plus_case")
  aa    <- c("age0_4_case", "age5_14_case", "age15_24_case", "age25_34_case",
             "age35_44_case", "age45_54_case", "age55_64_case", "age65_74_case",
             "age75_84_case", "age85plus_case")
  race  <- c("hisp_total_case", "white_total_case", "black_total_case",
             "api_total_case", "other_total_case")
  core  <- c("total_case", "male_total_case", "female_total_case", age10, aa, race)
  
  qc_required_columns(df, core, "Demographic counters")
  
  s   <- function(col) sum(suppressWarnings(as.numeric(df[[col]])), na.rm = TRUE)
  age <- suppressWarnings(as.numeric(df$Age))
  cc  <- !is.na(df$CountyCases) & df$CountyCases == 1
  
  n_case_cols <- length(grep("_case$", names(df), value = TRUE))
  tot     <- s("total_case")
  n_cc    <- sum(cc)
  sex_sum <- s("male_total_case") + s("female_total_case")
  age10_s <- sum(vapply(age10, s, 0))
  aa_s    <- sum(vapply(aa, s, 0))
  race_s  <- sum(vapply(race, s, 0))
  n_age_ok <- sum(cc & !is.na(age) & age >= 0)
  
  qc_log("Demographic counter totals (county cases)", data.frame(
    measure = c("*_case counter columns", "CountyCases == 1 rows", "total_case",
                "male + female", "unknown/other sex (total - male - female)",
                "county cases with valid Age (>= 0)", "sum of 10-year age counters",
                "sum of age-adjustment counters", "sum of race/ethnicity totals",
                "county cases with Age > 110"),
    value   = c(n_case_cols, n_cc, tot, sex_sum, tot - sex_sum, n_age_ok, age10_s,
                aa_s, race_s, sum(cc & !is.na(age) & age > 110))
  ))
  
  qc_check("total_case equals the number of CountyCases rows", tot == n_cc,
           details = sprintf("total_case = %s, CountyCases rows = %s", tot, n_cc))
  qc_check("male_total_case + female_total_case <= total_case", sex_sum <= tot,
           details = sprintf("male + female = %s, total_case = %s", sex_sum, tot))
  qc_check("10-year age counters sum to county cases with a valid Age",
           age10_s == n_age_ok, on_fail = "warn",
           details = sprintf("sum of 10-year counters = %s, county cases with Age >= 0 = %s. A gap usually means non-integer or out-of-range ages.",
                             age10_s, n_age_ok))
  qc_check("Age-adjustment counters sum to the same total as 10-year counters",
           aa_s == age10_s, on_fail = "warn",
           details = sprintf("age-adjustment sum = %s, 10-year sum = %s. age0_4 counts Age < 5 (including negatives); age0_9 requires Age >= 0.",
                             aa_s, age10_s))
  qc_check("No county cases with Age > 110 (possible unknown-age code)",
           sum(cc & !is.na(age) & age > 110) == 0, on_fail = "warn",
           details = "Ages above 110 are counted in age80plus / age85plus. Confirm they are real ages, not an unknown code such as 999.")
  
  if (race_counters_gated) {
    qc_check("Race/ethnicity totals sum to no more than total_case", race_s <= tot,
             details = sprintf("race/ethnicity sum = %s, total_case = %s", race_s, tot))
  } else {
    qc_log("Race/ethnicity totals vs total_case (VRBIS)",
           sprintf("race/ethnicity sum = %s, total_case = %s, difference = %s",
                   race_s, tot, race_s - tot),
           note = "Several VRBIS race counters are not gated by CountyCases (known open item). A race sum above total_case reflects non-final-resident records in those counters.")
  }
  invisible(NULL)
}

# ZIP crosswalk checks run BEFORE distinct(Zipcode): conflicting mappings would
# otherwise be resolved silently by keeping whichever row comes first.
qc_zip_crosswalk_conflicts <- function(xwalk, standardize_zip) {
  qc_required_columns(xwalk, c("Zipcode", "ENCLOSINGZIP"), "ezc.csv crosswalk")
  pairs <- unique(data.frame(Zipcode      = standardize_zip(xwalk$Zipcode),
                             ENCLOSINGZIP = standardize_zip(xwalk$ENCLOSINGZIP)))
  dup_zips <- unique(pairs$Zipcode[duplicated(pairs$Zipcode)])
  conflicts <- pairs[pairs$Zipcode %in% dup_zips, , drop = FALSE]
  conflicts <- conflicts[order(conflicts$Zipcode), , drop = FALSE]
  qc_check("Crosswalk maps each ZIP to exactly one ENCLOSINGZIP", length(dup_zips) == 0,
           details = conflicts)
}

# Residence ZIP -> ENCLOSINGZIP results for county cases.
qc_zip_join_results <- function(core_df, xwalk, n_before) {
  qc_check("Crosswalk join did not change the row count", nrow(core_df) == n_before,
           details = sprintf("rows before = %s, rows after = %s", n_before, nrow(core_df)))
  # CountyCases must already exist, otherwise every count below would read as 0.
  qc_required_columns(core_df, c("CountyCases", "Zipcode", "ENCLOSINGZIP"),
                      "ZIP match summary")
  cc <- !is.na(core_df$CountyCases) & core_df$CountyCases == 1
  unmatched <- cc & !(core_df$Zipcode %in% xwalk$Zipcode)
  unk_enc   <- cc & core_df$ENCLOSINGZIP == 99999L
  qc_log("County cases: ZIP match summary", data.frame(
    measure = c(
      "rows in the file (all records)",
      "county cases (CountyCases == 1): the only rows counted in the *_case counters",
      "rows that are NOT county cases (kept in the core file, never counted)",
      "county cases whose residence ZIP is NOT in the ezc.csv crosswalk",
      "county cases assigned ENCLOSINGZIP = 99999 (unknown ZIP)",
      "percent of county cases with ENCLOSINGZIP = 99999"
    ),
    value = as.character(c(
      nrow(core_df), sum(cc), sum(!cc), sum(unmatched), sum(unk_enc),
      round(100 * sum(unk_enc) / max(sum(cc), 1), 2)
    ))
  ), note = paste(
    "Counts are ROWS (records), not ZIPs. 'County cases' are the rows flagged",
    "CountyCases == 1 earlier in this script (the residency / facility-county rule).",
    "A 0 on the 'NOT in the ezc.csv crosswalk' row means every county case matched the",
    "crosswalk. A 0 on the 'county cases' row would mean the residency rule matched",
    "nothing, and the script would already have failed."
  ))
  if (any(unmatched)) {
    tab <- as.data.frame(table(Zipcode = core_df$Zipcode[unmatched], useNA = "ifany"),
                         stringsAsFactors = FALSE)
    tab <- tab[order(-tab$Freq), , drop = FALSE]
    names(tab)[2] <- "county_cases"
    tab$what_this_is <- ifelse(
      is.na(tab$Zipcode), "residence ZIP missing and not filled",
      ifelse(tab$Zipcode == "99999", "residence ZIP was missing; filled with 99999 earlier in this script",
             "a real ZIP on the record that is not in ezc.csv (out-of-county or invalid residence ZIP)"))
    qc_log("County-case residence ZIPs with no crosswalk match (all go to ENCLOSINGZIP 99999)", tab,
           note = paste("These are the residence ZIPs recorded on county cases. Anything other than",
                        "99999 is a ZIP that exists on the record but is not in the ezc.csv crosswalk:",
                        "usually an out-of-county mailing or residence address, or a bad ZIP. The case",
                        "still counts in county totals, but it has no geography, so it drops out when",
                        "cases are allocated to geographies in the rates project unless 99999 is in the",
                        "allocation crosswalk."))
  }
  invisible(NULL)
}

# Condition columns listed in the condition CSV must exist in the core file.
qc_condition_columns <- function(df, cond_vars, cond_csv, cond_scripts) {
  qc_check("Condition scripts found in the condition folder", length(cond_scripts) > 0,
           details = "No .R/.r files found. Check cond_scripts_dir.")
  qc_log("Condition scripts sourced", basename(cond_scripts))
  missing <- setdiff(cond_vars, names(df))
  qc_check(paste0("Every variable in ", cond_csv, " exists in the core file"),
           length(missing) == 0,
           details = c(paste("Missing:", paste(missing, collapse = ", ")),
                       "Usually a condition script was not sourced, or the CSV name does not match the column the script creates."))
}

# Reconcile aggregate_conditions() output against the core file, per condition var.
#   Expected = sum of total_case over rows for this OUTCOME with a valid condition value.
qc_aggregation_reconcile <- function(core_df, merged, outcome_label, outcome_tag,
                                     cond_vars, year) {
  base <- core_df[as.character(core_df$OUTCOME) == outcome_label, , drop = FALSE]
  tc   <- suppressWarnings(as.numeric(base$total_case))
  invalid <- function(x) { y <- tolower(trimws(as.character(x))); is.na(y) | y == "" | y == "condition" }
  
  expected <- vapply(cond_vars, function(v) sum(tc[!invalid(base[[v]])], na.rm = TRUE), 0)
  merged$condition_var <- sub(paste0("_", outcome_tag, "_\\d{4}\\.rds$"), "", merged$source_file)
  observed <- tapply(suppressWarnings(as.numeric(merged$total_case_sum)),
                     merged$condition_var, sum, na.rm = TRUE)
  
  tbl <- data.frame(
    condition_var         = cond_vars,
    core_total_case       = expected,
    aggregated_total_case = ifelse(is.na(observed[cond_vars]), 0, observed[cond_vars]),
    row.names = NULL
  )
  tbl$difference <- tbl$aggregated_total_case - tbl$core_total_case
  tbl <- tbl[order(-tbl$core_total_case), , drop = FALSE]
  
  qc_log(paste0("[", outcome_tag, "] total_case per condition: core file vs aggregated output"),
         tbl, max_rows = 500)
  qc_check(paste0("[", outcome_tag, "] Aggregated total_case matches the core file for every condition"),
           all(abs(tbl$difference) < 1e-6), details = tbl[abs(tbl$difference) >= 1e-6, , drop = FALSE])
  
  stale <- setdiff(unique(merged$condition_var), cond_vars)
  qc_check(paste0("[", outcome_tag, "] Merged file contains only conditions from the current condition list"),
           length(stale) == 0,
           details = c(paste("Extra condition files merged:", paste(stale, collapse = ", ")),
                       "Leftover condfiles from an earlier run are being merged. Delete them from the condfiles folder and re-run."))
  
  yrs <- unique(merged$Year)
  qc_check(paste0("[", outcome_tag, "] Merged file Year matches datayear.csv"),
           length(yrs) == 1 && as.character(yrs) == as.character(year),
           details = paste("Years in merged file:", paste(yrs, collapse = ", ")))
  
  zero <- tbl$condition_var[tbl$core_total_case == 0]
  qc_check(paste0("[", outcome_tag, "] Every condition has at least one county case"),
           length(zero) == 0, on_fail = "warn",
           details = paste("Zero county cases:", paste(zero, collapse = ", ")))
  invisible(tbl)
}