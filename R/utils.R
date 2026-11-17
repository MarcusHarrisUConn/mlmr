`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

first_or <- function(x, y = NULL) {
  if (is.null(x) || !length(x)) y else x[[1]]
}

numeric_vars <- function(data) {
  names(data)[vapply(data, function(x) is.numeric(x) && !inherits(x, "haven_labelled"), logical(1)) |
    vapply(data, function(x) is.numeric(x) && is.null(attr(x, "labels", exact = TRUE)), logical(1))]
}

factor_vars <- function(data) {
  names(data)[vapply(data, function(x) is.factor(x) || is.character(x), logical(1))]
}

candidate_group_vars <- function(data) {
  if (!nrow(data)) return(character())
  names(data)[vapply(data, function(x) {
    is.factor(x) || is.character(x) || length(unique(x[!is.na(x)])) <= max(12, floor(nrow(data) / 8))
  }, logical(1))]
}

variable_labels_map <- function(data) {
  stored <- attr(data, "variable_labels", exact = TRUE)
  out <- stats::setNames(rep("", ncol(data)), names(data))
  if (is.character(stored) && !is.null(names(stored))) {
    shared <- intersect(names(out), names(stored))
    out[shared] <- stored[shared]
  }
  for (nm in names(data)) {
    lbl <- attr(data[[nm]], "label", exact = TRUE)
    if (is.character(lbl) && length(lbl) == 1 && nzchar(lbl) && !nzchar(out[[nm]])) {
      out[[nm]] <- lbl
    }
  }
  out
}

format_apa_p <- function(p) {
  out <- rep(NA_character_, length(p))
  ok <- !is.na(p) & is.finite(p)
  out[!ok] <- ""
  small <- ok & p < .001
  large <- ok & p >= .9995
  mid <- ok & !small & !large
  out[small] <- "< .001"
  out[large] <- "> .999"
  out[mid] <- sub("^0\\.", ".", sprintf("%.3f", p[mid]))
  out
}

format_apa_p_prose <- function(p_str) {
  vapply(p_str, function(s) {
    s <- trimws(as.character(s))
    if (!nzchar(s)) return("p = NA")
    if (grepl("^[<>]", s)) paste("p", s) else paste("p =", s)
  }, character(1), USE.NAMES = FALSE)
}

is_significant_p_string <- function(p_str, alpha = 0.05) {
  p_str <- trimws(as.character(p_str))
  num <- suppressWarnings(as.numeric(p_str))
  p_str == "< .001" | (!is.na(num) & num < alpha)
}

html_escape <- function(x) {
  x <- as.character(x)
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x
}

latex_escape <- function(x) {
  x <- gsub("\\", "/", as.character(x), fixed = TRUE)
  x <- gsub("([{}$#%&^~`])", "", x, perl = TRUE)
  gsub("_", "\\_", x, fixed = TRUE)
}

latex_var <- function(x) {
  paste0("\\mathrm{", latex_escape(x), "}")
}

latex_table_escape <- function(x) {
  x <- as.character(x)
  x <- gsub("\\", "\001", x, fixed = TRUE)
  x <- gsub("([{}])", "\\\\\\1", x, perl = TRUE)
  x <- gsub("\001", "\\textbackslash{}", x, fixed = TRUE)
  x <- gsub("_", "\\_", x, fixed = TRUE)
  x <- gsub("%", "\\%", x, fixed = TRUE)
  x <- gsub("&", "\\&", x, fixed = TRUE)
  x <- gsub("#", "\\#", x, fixed = TRUE)
  x <- gsub("$", "\\$", x, fixed = TRUE)
  x <- gsub("^", "\\textasciicircum{}", x, fixed = TRUE)
  x <- gsub("~", "\\textasciitilde{}", x, fixed = TRUE)
  x <- gsub("<", "$<$", x, fixed = TRUE)
  x <- gsub(">", "$>$", x, fixed = TRUE)
  x
}

md_escape_cell <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- gsub("\r?\n", " ", x)
  gsub("|", "\\|", x, fixed = TRUE)
}

md_pipe_table <- function(tab) {
  if (!is.data.frame(tab) || !nrow(tab)) return("_No rows available._")
  headers <- md_escape_cell(names(tab))
  divider <- rep("---", ncol(tab))
  escaped <- as.data.frame(lapply(tab, md_escape_cell), stringsAsFactors = FALSE, check.names = FALSE)
  rows <- apply(escaped, 1, paste, collapse = " | ")
  paste(
    c(
      paste(headers, collapse = " | "),
      paste(divider, collapse = " | "),
      rows
    ),
    collapse = "\n"
  )
}

r_string_literal <- function(x) {
  paste(deparse(as.character(x), width.cutoff = 500L), collapse = "")
}

r_char_vector_lines <- function(var_name, values) {
  values <- as.character(values)
  if (!length(values)) {
    return(sprintf("%s <- character()", var_name))
  }
  if (length(values) == 1L) {
    return(sprintf("%s <- %s", var_name, r_string_literal(values[[1]])))
  }
  encoded <- vapply(values, r_string_literal, character(1))
  c(
    sprintf("%s <- c(", var_name),
    paste0("  ", encoded, ifelse(seq_along(encoded) < length(encoded), ",", "")),
    ")"
  )
}
