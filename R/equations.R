mlm_equations <- function(result) {
  spec <- result$spec
  coef_names <- names(lme4::fixef(result$fit))
  slopes <- setdiff(coef_names, "(Intercept)")
  beta_terms <- sprintf("b%s*%s", seq_along(slopes), slopes)
  fixed_rhs <- paste(c(if ("(Intercept)" %in% coef_names) "b0", beta_terms), collapse = " + ")
  lhs <- if (is_gaussian_fit(result$fit)) {
    sprintf("E[%s]", spec$outcome)
  } else {
    sprintf("%s(E[%s])", stats::family(result$fit)$link, spec$outcome)
  }
  cnms <- lme4::getME(result$fit, "cnms")
  random_lines <- vapply(unique(names(cnms)), function(group) {
    components <- cnms[names(cnms) == group]
    labels <- unlist(components, use.names = FALSE)
    structure_text <- if (length(labels) < 2) {
      "a single variance"
    } else if (length(components) == 1) {
      "correlated effects"
    } else if (all(lengths(components) == 1)) {
      "independent effects"
    } else {
      "partly correlated effects"
    }
    sprintf(
      "Random effects by %s: %s (%s).",
      group, paste(sub("(Intercept)", "intercept", labels, fixed = TRUE), collapse = ", "), structure_text
    )
  }, character(1))
  c(
    sprintf("Fixed component: %s = %s", lhs, fixed_rhs),
    unname(random_lines),
    sprintf("Fitted formula: %s", paste(deparse(result$formula, width.cutoff = 500), collapse = " "))
  )
}

# Subscript indices are concatenated (gamma_10) unless an index
# needs two digits, in which case they are comma separated.
equation_index <- function(index) {
  if (any(index > 9)) paste(index, collapse = ",") else paste(index, collapse = "")
}

constant_within <- function(x, group) {
  x <- as.character(x)
  group_chr <- as.character(group)
  non_missing <- !is.na(x) & !is.na(group_chr)
  if (!any(non_missing)) return(TRUE)
  x_sub <- x[non_missing]
  g_sub <- group_chr[non_missing]
  first <- x_sub[match(g_sub, g_sub)]
  all(x_sub == first)
}

is_nested_in <- function(lower, higher) {
  constant_within(higher, lower)
}

# Orders grouping factors from the lowest level (most units) upward and reports
# whether they form a strict hierarchy in the fitted data.
equation_groups <- function(fit) {
  flist <- lme4::getME(fit, "flist")
  groups <- unique(names(lme4::getME(fit, "cnms")))
  n_units <- vapply(groups, function(g) nlevels(droplevels(flist[[g]])), integer(1))
  groups <- groups[order(-n_units)]
  nested <- TRUE
  if (length(groups) > 1) {
    for (i in seq_len(length(groups) - 1)) {
      if (!is_nested_in(flist[[groups[[i]]]], flist[[groups[[i + 1]]]])) nested <- FALSE
    }
  }
  list(groups = groups, nested = nested, flist = flist)
}

# Splits every fixed-effect coefficient into the parts that vary at each level
# of a nested design. Returns NULL when the model cannot be written in
# level-by-level form (crossed factors, four or more levels, or coefficient
# names that cannot be matched to model terms).
equation_levels <- function(fit) {
  grp <- equation_groups(fit)
  groups <- grp$groups
  if (!grp$nested || length(groups) > 2) return(NULL)
  n_levels <- length(groups) + 1L
  X <- lme4::getME(fit, "X")
  coef_names <- colnames(X)
  assign <- attr(X, "assign")
  term_labels <- attr(stats::terms(fit), "term.labels")
  if (is.null(assign) || length(assign) != length(coef_names)) return(NULL)
  frame <- stats::model.frame(fit)
  variable_level <- function(variable) {
    variable <- gsub("`", "", variable, fixed = TRUE)
    if (!variable %in% names(frame)) return(NA_integer_)
    level <- 1L
    for (group in groups) {
      if (!constant_within(frame[[variable]], grp$flist[[group]])) break
      level <- level + 1L
    }
    level
  }
  parts <- matrix("", nrow = length(coef_names), ncol = n_levels)
  for (j in seq_along(coef_names)) {
    if (assign[[j]] == 0) next
    variables <- strsplit(term_labels[[assign[[j]]]], ":", fixed = TRUE)[[1]]
    components <- strsplit(coef_names[[j]], ":", fixed = TRUE)[[1]]
    if (length(variables) != length(components)) return(NULL)
    levels <- vapply(variables, variable_level, integer(1))
    if (anyNA(levels)) return(NULL)
    for (level in seq_len(n_levels)) {
      parts[j, level] <- paste(components[levels == level], collapse = ":")
    }
  }
  index <- matrix(0L, nrow = length(coef_names), ncol = n_levels)
  for (level in seq_len(n_levels)) {
    parent <- if (level == 1) rep("", length(coef_names)) else apply(index[, seq_len(level - 1), drop = FALSE], 1, paste, collapse = ".")
    for (key in unique(parent)) {
      rows <- which(parent == key)
      labels <- unique(parts[rows, level])
      labels <- labels[nzchar(labels)]
      index[rows, level] <- match(parts[rows, level], labels, nomatch = 0L)
    }
  }
  cnms <- lme4::getME(fit, "cnms")
  random <- list()
  for (depth in seq_along(groups)) {
    components <- cnms[names(cnms) == groups[[depth]]]
    labels <- unlist(components, use.names = FALSE)
    prefixes <- matrix(0L, nrow = length(labels), ncol = depth)
    for (i in seq_along(labels)) {
      if (identical(labels[[i]], "(Intercept)")) next
      row <- match(labels[[i]], coef_names)
      if (is.na(row) || any(index[row, seq_len(n_levels) > depth] != 0L)) return(NULL)
      prefixes[i, ] <- index[row, seq_len(depth)]
    }
    random[[groups[[depth]]]] <- list(
      labels = labels,
      prefixes = prefixes,
      component = rep(seq_along(components), lengths(components))
    )
  }
  order_rows <- do.call(order, as.data.frame(index))
  list(
    n_levels = n_levels, groups = groups, coef_names = coef_names,
    parts = parts, index = index, random = random, order = order_rows
  )
}

equation_link_line <- function(result, eta, outcome_tex) {
  link <- stats::family(result$fit)$link
  mean_tex <- if (identical(result$spec$distribution, "binomial")) {
    sprintf("P(%s = 1)", outcome_tex)
  } else {
    sprintf("E(%s)", outcome_tex)
  }
  rhs <- switch(
    link,
    logit = sprintf("\\mathrm{logit}\\left[%s\\right]", mean_tex),
    probit = sprintf("\\mathrm{probit}\\left[%s\\right]", mean_tex),
    log = sprintf("\\mathrm{log}\\left[%s\\right]", mean_tex),
    inverse = sprintf("1 / %s", mean_tex),
    sprintf("\\mathrm{%s}\\left[%s\\right]", latex_escape(link), mean_tex)
  )
  paste(eta, "=", rhs)
}

tau_block_latex <- function(symbols, cells, group, labels) {
  sprintf(
    "\\begin{bmatrix}%s\\end{bmatrix}_{\\mathrm{%s}} \\sim MVN\\left(\\begin{bmatrix}%s\\end{bmatrix},\\begin{bmatrix}%s\\end{bmatrix}\\right)\\quad \\text{%s: %s}",
    paste(symbols, collapse = "\\\\ "),
    latex_escape(group),
    paste(rep("0", length(symbols)), collapse = "\\\\ "),
    paste(apply(cells, 1, paste, collapse = " & "), collapse = "\\\\ "),
    latex_escape(group),
    paste(latex_escape(labels), collapse = ", ")
  )
}

equation_plain <- function(x) {
  x <- gsub("\\\\(beta|gamma|pi|tau|eta)", "\\1", x)
  x <- gsub("[{}]", "", x)
  gsub("\\", "", x, fixed = TRUE)
}

# Level-by-level (hierarchical) notation for two- and three-level nested models.
equations_hierarchical <- function(result, lev) {
  fit <- result$fit
  n_levels <- lev$n_levels
  gaussian <- is_gaussian_fit(fit)
  coef_symbol <- if (n_levels == 2) c("\\beta", "\\gamma") else c("\\pi", "\\beta", "\\gamma")
  random_symbol <- if (n_levels == 2) "u" else c("r", "u")
  residual_symbol <- if (n_levels == 2) "r" else "e"
  subs <- if (n_levels == 2) c("ij", "j") else c("ijk", "jk", "k")
  tails <- substring(subs, 2)
  index <- lev$index
  parts <- lev$parts
  symbol <- function(base, prefix, tail) sprintf("%s_{%s%s}", base, equation_index(prefix), tail)
  variable_tex <- function(part, sub) {
    if (!nzchar(part)) return("")
    components <- strsplit(part, ":", fixed = TRUE)[[1]]
    paste(sprintf("%s_{%s}", latex_var(components), sub), collapse = "\\,")
  }
  key_of <- function(prefix) paste(prefix, collapse = ".")
  part_for <- function(prefix) {
    depth <- length(prefix)
    rows <- which(apply(index[, seq_len(depth), drop = FALSE], 1, key_of) == key_of(prefix))
    parts[rows[[1]], depth]
  }
  has_random <- function(depth, prefix) {
    rand <- lev$random[[lev$groups[[depth]]]]
    any(apply(rand$prefixes, 1, key_of) == key_of(prefix))
  }
  outcome_tex <- sprintf("%s_{%s}", latex_var(result$spec$outcome), subs[[1]])
  eta <- sprintf("\\eta_{%s}", subs[[1]])
  lhs <- if (gaussian) outcome_tex else eta

  # Level 1.
  level1_children <- sort(unique(index[, 1]))
  level1_terms <- vapply(level1_children, function(p) {
    paste0(symbol(coef_symbol[[1]], p, tails[[1]]), variable_tex(part_for(p), subs[[1]]))
  }, character(1))
  level1 <- paste(lhs, "=", paste(c(level1_terms, if (gaussian) sprintf("%s_{%s}", residual_symbol, subs[[1]])), collapse = " + "))
  if (!gaussian) level1 <- paste(equation_link_line(result, eta, outcome_tex), level1, sep = "\\\\ ")

  # Higher levels: one equation per coefficient of the level below.
  higher <- lapply(seq_len(n_levels - 1), function(depth) {
    prefixes <- unique(index[lev$order, seq_len(depth), drop = FALSE])
    lines <- vapply(seq_len(nrow(prefixes)), function(i) {
      prefix <- prefixes[i, ]
      rows <- which(apply(index[, seq_len(depth), drop = FALSE], 1, key_of) == key_of(prefix))
      children <- sort(unique(index[rows, depth + 1]))
      terms <- vapply(children, function(child) {
        paste0(
          symbol(coef_symbol[[depth + 1]], c(prefix, child), tails[[depth + 1]]),
          variable_tex(part_for(c(prefix, child)), subs[[depth + 1]])
        )
      }, character(1))
      if (has_random(depth, prefix)) terms <- c(terms, symbol(random_symbol[[depth]], prefix, tails[[depth]]))
      paste(symbol(coef_symbol[[depth]], prefix, tails[[depth]]), "=", paste(terms, collapse = " + "))
    }, character(1))
    paste(lines, collapse = "\\\\ ")
  })

  # Combined (mixed-model) equation.
  fixed_terms <- vapply(lev$order, function(row) {
    variables <- vapply(rev(seq_len(n_levels)), function(level) variable_tex(parts[row, level], subs[[level]]), character(1))
    paste(c(symbol("\\gamma", index[row, ], ""), variables[nzchar(variables)]), collapse = "")
  }, character(1))
  random_terms <- unlist(lapply(seq_along(lev$groups), function(depth) {
    rand <- lev$random[[lev$groups[[depth]]]]
    prefixes <- unique(rand$prefixes[do.call(order, as.data.frame(rand$prefixes)), , drop = FALSE])
    vapply(seq_len(nrow(prefixes)), function(i) {
      prefix <- prefixes[i, ]
      variables <- vapply(rev(seq_len(depth)), function(level) {
        if (prefix[[level]] == 0L) "" else variable_tex(part_for(prefix[seq_len(level)]), subs[[level]])
      }, character(1))
      paste(c(symbol(random_symbol[[depth]], prefix, tails[[depth]]), variables[nzchar(variables)]), collapse = "")
    }, character(1))
  }))
  combined <- paste(lhs, "=", paste(c(fixed_terms, random_terms, if (gaussian) sprintf("%s_{%s}", residual_symbol, subs[[1]])), collapse = " + "))

  # Tau matrices, one per grouping factor.
  tau_tag <- if (n_levels == 2) "" else c("\\pi ", "\\beta ")
  blocks <- lapply(seq_along(lev$groups), function(depth) {
    group <- lev$groups[[depth]]
    rand <- lev$random[[group]]
    labels_index <- apply(rand$prefixes, 1, equation_index)
    symbols <- vapply(seq_along(rand$labels), function(i) symbol(random_symbol[[depth]], rand$prefixes[i, ], tails[[depth]]), character(1))
    n <- length(symbols)
    cells <- matrix("0", nrow = n, ncol = n)
    for (i in seq_len(n)) {
      for (j in seq_len(n)) {
        if (rand$component[[i]] != rand$component[[j]]) next
        pair <- c(labels_index[[max(i, j)]], labels_index[[min(i, j)]])
        cells[i, j] <- sprintf("\\tau_{%s%s}", tau_tag[[depth]], paste(pair, collapse = if (depth > 1 || any(nchar(pair) > 1)) "," else ""))
      }
    }
    list(group = group, labels = rand$labels, symbols = symbols, cells = cells)
  })

  role_for_row <- function(row) {
    idx <- index[row, ]
    random_notes <- character()
    for (depth in seq_along(lev$groups)) {
      prefix <- idx[seq_len(depth)]
      if (all(idx[seq_len(n_levels) > depth] == 0L) && has_random(depth, prefix)) {
        random_notes <- c(random_notes, sprintf("Random (%s)", equation_plain(symbol(random_symbol[[depth]], prefix, tails[[depth]]))))
      } else if (any(idx[seq_len(n_levels) > depth] != 0L) && has_random(depth, prefix)) {
        random_notes <- c(random_notes, sprintf("Moderates random %s", equation_plain(symbol(coef_symbol[[depth]], prefix, tails[[depth]]))))
      }
    }
    if (length(random_notes)) paste(random_notes, collapse = "; ") else "Fixed only"
  }

  key <- data.frame(
    Symbol = equation_plain(vapply(lev$order, function(row) symbol("\\gamma", index[row, ], ""), character(1))),
    Coefficient = lev$coef_names[lev$order],
    Estimate = unname(lme4::fixef(fit)[lev$coef_names[lev$order]]),
    Role = vapply(lev$order, role_for_row, character(1)),
    stringsAsFactors = FALSE
  )
  level_labels <- c("Level 1", sprintf("Level %s (%s)", seq_along(lev$groups) + 1L, lev$groups))
  level_notes <- c(
    if (gaussian) {
      sprintf("Observation-level model for %s with within-cluster residual %s_%s.", result$spec$outcome, residual_symbol, subs[[1]])
    } else {
      sprintf("Link-scale observation model (%s link); no additive Level-1 residual is included.", stats::family(fit)$link)
    },
    vapply(seq_along(lev$groups), function(depth) {
      grp <- lev$groups[[depth]]
      rand <- lev$random[[grp]]
      sprintf(
        "Cluster-level equations across %s units; random terms (%s) allow intercepts or slopes to vary across %s.",
        grp,
        paste(equation_plain(vapply(seq_along(rand$labels), function(i) symbol(random_symbol[[depth]], rand$prefixes[i, ], tails[[depth]]), character(1))), collapse = ", "),
        grp
      )
    }, character(1))
  )
  list(
    level = level_labels,
    level_notes = level_notes,
    equations = c(level1, unlist(higher)),
    combined = combined,
    blocks = blocks,
    key = key,
    notation = if (n_levels == 2) "two-level hierarchical" else "three-level hierarchical"
  )
}

# Single mixed-model equation for crossed or otherwise non-hierarchical designs.
equations_mixed <- function(result) {
  fit <- result$fit
  gaussian <- is_gaussian_fit(fit)
  coef_names <- names(lme4::fixef(fit))
  slopes <- setdiff(coef_names, "(Intercept)")
  grp <- equation_groups(fit)
  cnms <- lme4::getME(fit, "cnms")
  unit_letters <- c("j", "k", "l", "m", "n", "o")
  random_letters <- c("u", "v", "w", "z", "a", "d")
  variable_tex <- function(name) {
    components <- strsplit(name, ":", fixed = TRUE)[[1]]
    paste(sprintf("%s_{i}", latex_var(components)), collapse = "\\,")
  }
  outcome_tex <- sprintf("%s_{i}", latex_var(result$spec$outcome))
  lhs <- if (gaussian) outcome_tex else "\\eta_{i}"
  fixed_terms <- c(
    if ("(Intercept)" %in% coef_names) "\\beta_{0}",
    sprintf("\\beta_{%s}%s", seq_along(slopes), vapply(slopes, variable_tex, character(1)))
  )
  blocks <- lapply(seq_along(grp$groups), function(g) {
    group <- grp$groups[[g]]
    unit <- unit_letters[[(g - 1) %% length(unit_letters) + 1]]
    letter <- random_letters[[(g - 1) %% length(random_letters) + 1]]
    components <- cnms[names(cnms) == group]
    labels <- unlist(components, use.names = FALSE)
    component <- rep(seq_along(components), lengths(components))
    label_index <- match(labels, c("(Intercept)", slopes)) - 1L
    if (anyNA(label_index)) label_index <- seq_along(labels) - 1L
    symbols <- sprintf("%s_{%s%s}", letter, label_index, unit)
    n <- length(labels)
    cells <- matrix("0", nrow = n, ncol = n)
    for (i in seq_len(n)) {
      for (j in seq_len(n)) {
        if (component[[i]] != component[[j]]) next
        pair <- c(label_index[[max(i, j)]], label_index[[min(i, j)]])
        cells[i, j] <- sprintf("\\tau_{%s %s}", letter, paste(pair, collapse = if (any(pair > 9)) "," else ""))
      }
    }
    terms <- paste0(
      sprintf("%s_{%s%s[i]}", letter, label_index, unit),
      vapply(labels, function(label) if (identical(label, "(Intercept)")) "" else variable_tex(label), character(1))
    )
    list(group = group, labels = labels, symbols = symbols, cells = cells, terms = terms)
  })
  equation <- paste(lhs, "=", paste(c(fixed_terms, unlist(lapply(blocks, `[[`, "terms")), if (gaussian) "e_{i}"), collapse = " + "))
  equations <- if (gaussian) equation else paste(equation_link_line(result, "\\eta_{i}", outcome_tex), equation, sep = "\\\\ ")
  all_coefs <- c(if ("(Intercept)" %in% coef_names) "(Intercept)", slopes)
  role_vec <- vapply(all_coefs, function(cf) {
    hits <- unlist(lapply(blocks, function(b) {
      idx <- which(b$labels == cf)
      if (length(idx)) sprintf("%s (%s)", equation_plain(b$symbols[idx]), b$group) else character()
    }))
    if (length(hits)) paste("Random:", paste(hits, collapse = ", ")) else "Fixed only"
  }, character(1))
  key <- data.frame(
    Symbol = c(if ("(Intercept)" %in% coef_names) "beta_0", sprintf("beta_%s", seq_along(slopes))),
    Coefficient = all_coefs,
    Estimate = unname(lme4::fixef(fit)[all_coefs]),
    Role = unname(role_vec),
    stringsAsFactors = FALSE
  )
  list(
    level = "Mixed-model form",
    level_notes = sprintf("Single mixed-model equation with random-effect blocks across %s.", paste(grp$groups, collapse = " and ")),
    equations = equations,
    combined = equation,
    blocks = lapply(blocks, function(block) block[c("group", "labels", "symbols", "cells")]),
    key = key,
    notation = "mixed-model"
  )
}

equation_structure <- function(result) {
  lev <- tryCatch(equation_levels(result$fit), error = function(e) NULL)
  if (is.null(lev)) equations_mixed(result) else equations_hierarchical(result, lev)
}

mlm_latex_equations <- function(result) {
  eq <- equation_structure(result)
  note <- switch(
    eq$notation,
    `mixed-model` = "The grouping factors are crossed or exceed three levels, so the model is written as a single mixed-model equation. Index i denotes observations and j[i], k[i] denote the units that observation i belongs to.",
    "Predictors are assigned to levels from the fitted data: a predictor that is constant within every unit of a grouping factor is treated as measured at that level. Cross-level interactions appear in the equation for the lower-level coefficient they moderate."
  )
  list(
    level = eq$level,
    level_notes = eq$level_notes,
    equations = eq$equations,
    combined = eq$combined,
    tau = tau_latex(result, eq),
    key = eq$key,
    notation = eq$notation,
    note = note
  )
}

tau_blocks <- function(result, eq = equation_structure(result)) {
  eq$blocks
}

tau_latex <- function(result, eq = equation_structure(result)) {
  vapply(eq$blocks, function(block) {
    tau_block_latex(block$symbols, block$cells, block$group, block$labels)
  }, character(1))
}

tau_label_table <- function(result) {
  rows <- lapply(tau_blocks(result), function(block) {
    diag_params <- equation_plain(diag(block$cells))
    data.frame(
      Group = block$group,
      `Matrix Index` = equation_plain(block$symbols),
      Variance = diag_params,
      Coefficient = block$labels,
      Estimated = "Yes",
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  })
  if (!length(rows)) {
    return(data.frame(
      Group = character(),
      `Matrix Index` = character(),
      Variance = character(),
      Coefficient = character(),
      Estimated = character(),
      check.names = FALSE
    ))
  }
  out <- do.call(rbind, rows)
  row.names(out) <- NULL
  out
}

equation_key_table <- function(result) {
  key <- equation_structure(result)$key
  data.frame(
    Symbol = key$Symbol,
    Coefficient = key$Coefficient,
    Estimate = sprintf("%.3f", key$Estimate),
    Role = key$Role %||% rep("Fixed", nrow(key)),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

wrap_single_equation_rows <- function(line, terms_per_line = 3) {
  pieces <- strsplit(line, " \\+ ", fixed = FALSE)[[1]]
  parts <- strsplit(pieces[[1]], " = ", fixed = TRUE)[[1]]
  if (length(parts) < 2) {
    return(sprintf("\\multicolumn{3}{l}{%s}", line))
  }
  lhs <- parts[[1]]
  rhs_start <- paste(parts[-1], collapse = " = ")
  rest <- pieces[-1]
  if (length(pieces) <= terms_per_line + 1) {
    rhs_full <- if (length(rest)) paste(c(rhs_start, rest), collapse = " + ") else rhs_start
    return(sprintf("%s & = & %s", lhs, rhs_full))
  }
  groups <- split(rest, ceiling(seq_along(rest) / terms_per_line))
  if (identical(as.integer(terms_per_line), 1L)) {
    c(
      sprintf("%s & = & %s", lhs, rhs_start),
      vapply(groups, function(g) sprintf("  &   & {} + %s", paste(g, collapse = " + ")), character(1))
    )
  } else {
    c(
      sprintf("%s & = & %s + %s", lhs, rhs_start, paste(groups[[1]], collapse = " + ")),
      vapply(groups[-1], function(g) sprintf("  &   & {} + %s", paste(g, collapse = " + ")), character(1))
    )
  }
}

latex_wrapped_equation <- function(equation, terms_per_line = 3) {
  equation <- paste(equation, collapse = "\\\\ ")
  pieces <- strsplit(equation, " \\+ ", fixed = FALSE)[[1]]
  if (length(pieces) <= terms_per_line + 1 && !grepl("\\\\\\\\", equation)) {
    return(paste0("\\[", equation, "\\]"))
  }
  rows <- wrap_single_equation_rows(equation, terms_per_line = terms_per_line)
  paste0("\\[\n\\begin{array}{rcl}\n", paste(rows, collapse = "\\\\\n"), "\n\\end{array}\n\\]")
}

latex_display_equation <- function(equation, terms_per_line = 3) {
  equation <- paste(equation, collapse = "\\\\ ")
  if (!grepl("\\\\\\\\", equation)) {
    return(latex_wrapped_equation(equation, terms_per_line = terms_per_line))
  }
  lines <- trimws(strsplit(equation, "\\\\\\\\")[[1]])
  lines <- lines[nzchar(lines)]
  row_blocks <- lapply(lines, wrap_single_equation_rows, terms_per_line = terms_per_line)
  joined <- vapply(row_blocks, paste, character(1), collapse = "\\\\\n")
  paste0("\\[\n\\begin{array}{rcl}\n", paste(joined, collapse = "\\\\[0.45em]\n"), "\n\\end{array}\n\\]")
}

quarto_math_block <- function(equation) {
  equation <- paste(equation, collapse = "\\\\ ")
  if (!grepl("\\\\\\\\", equation)) {
    return(sprintf("$$\n%s\n$$", equation))
  }
  lines <- trimws(strsplit(equation, "\\\\\\\\")[[1]])
  lines <- lines[nzchar(lines)]
  aligned <- vapply(lines, function(ln) {
    sub(" = ", " &= ", ln, fixed = TRUE)
  }, character(1))
  sprintf("$$\n\\begin{aligned}\n%s\n\\end{aligned}\n$$", paste(aligned, collapse = " \\\\\n"))
}

render_latex_piece <- function(x) {
  x <- html_escape(x)
  x <- gsub("\\\\_", "_", x, fixed = FALSE)
  x <- gsub("\\\\left", "", x, fixed = FALSE)
  x <- gsub("\\\\right", "", x, fixed = FALSE)
  x <- gsub("\\\\quad", " ", x, fixed = FALSE)
  x <- gsub("\\\\,", " ", x, fixed = FALSE)
  x <- gsub("\\\\sim", "<span class='eq-op'>&sim;</span>", x, fixed = FALSE)
  x <- gsub("\\\\cdot", "&middot;", x, fixed = FALSE)
  x <- gsub("\\\\eta", "&eta;", x, fixed = FALSE)
  x <- gsub("\\\\beta", "&beta;", x, fixed = FALSE)
  x <- gsub("\\\\gamma", "&gamma;", x, fixed = FALSE)
  x <- gsub("\\\\tau", "&tau;", x, fixed = FALSE)
  x <- gsub("\\\\pi", "&pi;", x, fixed = FALSE)
  x <- gsub("\\\\mathrm\\{([^{}]+)\\}", "<span class='eq-var'>\\1</span>", x, perl = TRUE)
  x <- gsub("\\\\text\\{([^{}]+)\\}", "<span class='eq-text'>\\1</span>", x, perl = TRUE)
  # Support nested subscripts such as _{<span class='eq-var'>schoolid</span>}
  x <- gsub("_\\{((?:[^{}]|<[^>]+>)+)\\}", "<sub>\\1</sub>", x, perl = TRUE)
  x <- gsub("\\\\", "", x, fixed = TRUE)
  x
}

render_bmatrix_html <- function(content) {
  rows <- strsplit(content, "\\\\\\\\", perl = TRUE)[[1]]
  rows <- rows[nzchar(trimws(rows))]
  cells <- lapply(rows, function(row) {
    cols <- strsplit(row, "\\s*&\\s*", perl = TRUE)[[1]]
    paste(sprintf("<td>%s</td>", vapply(cols, render_latex_piece, character(1))), collapse = "")
  })
  paste0(
    "<span class='eq-matrix-frame'><table class='eq-matrix' aria-label='Matrix'><tbody>",
    paste(sprintf("<tr>%s</tr>", unlist(cells, use.names = FALSE)), collapse = ""),
    "</tbody></table></span>"
  )
}

render_latex_with_matrices <- function(x) {
  pattern <- "\\\\begin\\{bmatrix\\}(.+?)\\\\end\\{bmatrix\\}"
  out <- character()
  rest <- x
  repeat {
    match <- regexpr(pattern, rest, perl = TRUE)
    if (match[[1]] < 0) {
      out <- c(out, render_latex_piece(rest))
      break
    }
    start <- match[[1]]
    end <- start + attr(match, "match.length") - 1
    before <- substr(rest, 1, start - 1)
    full <- substr(rest, start, end)
    content <- sub(pattern, "\\1", full, perl = TRUE)
    out <- c(out, render_latex_piece(before), render_bmatrix_html(content))
    rest <- substr(rest, end + 1, nchar(rest))
  }
  paste(out, collapse = "")
}

equation_html <- function(latex) {
  parts <- unlist(lapply(latex, function(item) {
    if (grepl("\\\\begin\\{bmatrix\\}", item)) return(item)
    strsplit(item, "\\\\\\\\", perl = TRUE)[[1]]
  }), use.names = FALSE)
  parts <- parts[nzchar(trimws(parts))]
  rendered <- vapply(parts, render_latex_with_matrices, character(1))
  paste0("<div class='equation-rendered'>", paste(sprintf("<div class='equation-line'>%s</div>", rendered), collapse = ""), "</div>")
}

# Labeled HTML Tau matrix display with row and column parameter headers (Issue #2).
render_tau_labeled_html <- function(result) {
  blocks <- tau_blocks(result)
  if (!length(blocks)) return("")
  cards <- vapply(blocks, function(block) {
    sym_html <- vapply(block$symbols, render_latex_piece, character(1))
    short_labels <- html_escape(sub("^\\(Intercept\\)$", "Intercept", block$labels))
    col_headers <- paste(
      sprintf("<th>%s<div class='tau-sub-label'>%s</div></th>", sym_html, short_labels),
      collapse = ""
    )
    body_rows <- vapply(seq_len(nrow(block$cells)), function(i) {
      row_cells <- paste(
        vapply(block$cells[i, ], function(cell) sprintf("<td>%s</td>", render_latex_piece(cell)), character(1)),
        collapse = ""
      )
      sprintf(
        "<tr><th scope='row'>%s <span class='tau-sub-label'>(%s)</span></th>%s</tr>",
        sym_html[[i]], short_labels[[i]], row_cells
      )
    }, character(1))
    dist_html <- equation_html(tau_block_latex(block$symbols, block$cells, block$group, block$labels))
    paste0(
      "<div class='equation-card'>",
      sprintf("<strong>Grouping factor: %s</strong>", html_escape(block$group)),
      dist_html,
      "<div class='tau-labeled-wrap'><table class='tau-labeled-grid' aria-label='Labeled Tau covariance matrix'>",
      sprintf("<thead><tr><th>Random Effect</th>%s</tr></thead>", col_headers),
      sprintf("<tbody>%s</tbody></table></div>", paste(body_rows, collapse = "")),
      "</div>"
    )
  }, character(1))
  paste0("<div class='equation-stack'>", paste(cards, collapse = ""), "</div>")
}


# Pre-fit symbolic multilevel equation builder directly from an `mlm_spec` object
# (< 1 ms via model.matrix without calling lmer/glmer), powering live equation
# previews on the Home Page and Guided Model Builder before fitting.
mlm_spec_equations <- function(spec, data = spec$data) {
  if (!inherits(spec, "mlm_spec")) {
    stop("`spec` must be an \"mlm_spec\" object created with mlm_spec().", call. = FALSE)
  }
  gaussian <- identical(spec$distribution, "gaussian")
  fallback_eq <- function() {
    outcome_tex <- sprintf("%s_{ij}", latex_var(spec$outcome))
    lhs <- if (gaussian) outcome_tex else sprintf("\\eta_{ij}")
    f_terms <- names(spec$fixed)
    rhs <- if (length(f_terms)) {
      paste(c("\\beta_{0j}", sprintf("\\beta_{%sj}%s_{ij}", seq_along(f_terms), vapply(f_terms, latex_var, character(1)))), collapse = " + ")
    } else {
      "\\beta_{0j}"
    }
    eq1 <- paste(lhs, "=", paste(c(rhs, if (gaussian) "r_{ij}"), collapse = " + "))
    list(
      level = "Symbolic Preview",
      level_notes = "Select grouping factors and valid predictors to preview exact multi-level equations.",
      equations = eq1,
      combined = eq1,
      tau = character(),
      notation = "preview",
      note = "Symbolic equation preview prior to model estimation."
    )
  }

  if (!is.data.frame(data) || !nrow(data)) return(fallback_eq())
  groups_raw <- unique(unlist(spec$grouping[names(spec$random)], use.names = FALSE))
  if (!length(groups_raw)) groups_raw <- unique(unlist(spec$grouping, use.names = FALSE))
  groups_raw <- intersect(groups_raw, names(data))
  if (!length(groups_raw)) return(fallback_eq())

  rows <- analysis_rows(data, model_variables(spec))
  if (!any(rows)) return(fallback_eq())
  centered <- center_predictors(data, spec$fixed, spec$grouping, rows = rows)$data
  sub_dat <- centered[rows, , drop = FALSE]

  # Build fixed-effects design matrix X directly without fitting lmer()
  interaction_terms <- vapply(spec$interactions, function(x) {
    paste(formula_name(vapply(x, effective_term, character(1), fixed = spec$fixed)), collapse = ":")
  }, character(1))
  rhs_fixed <- unique(c(formula_name(unname(effective_fixed_terms(spec$fixed))), interaction_terms))
  if (!length(rhs_fixed)) rhs_fixed <- "1"
  fixed_form <- tryCatch(stats::as.formula(paste("~", paste(rhs_fixed, collapse = " + "))), error = function(e) NULL)
  if (is.null(fixed_form)) return(fallback_eq())

  mf <- tryCatch(stats::model.frame(fixed_form, data = sub_dat), error = function(e) NULL)
  X <- tryCatch(stats::model.matrix(fixed_form, data = mf), error = function(e) NULL)
  if (is.null(X)) return(fallback_eq())

  coef_names <- colnames(X)
  assign_vec <- attr(X, "assign")
  term_labels <- attr(stats::terms(fixed_form), "term.labels")

  # Order grouping factors from lowest cluster level (most units) upward
  flist <- lapply(stats::setNames(groups_raw, groups_raw), function(g) droplevels(factor(sub_dat[[g]])))
  n_units <- vapply(groups_raw, function(g) nlevels(flist[[g]]), integer(1))
  groups <- groups_raw[order(-n_units)]
  nested <- !identical(spec$structure, "crossed") && length(groups) <= 2L
  if (nested && length(groups) > 1L) {
    for (i in seq_len(length(groups) - 1L)) {
      if (!is_nested_in(flist[[groups[[i]]]], flist[[groups[[i + 1L]]]])) nested <- FALSE
    }
  }

  # Reconstruct lme4-compatible cnms list for random effects
  cnms <- list()
  cnms_names <- character()
  expand_slope_cols <- function(slope_var) {
    eff_name <- effective_term(spec$fixed, slope_var)
    if (eff_name %in% coef_names) return(eff_name)
    idx_term <- match(formula_name(eff_name), term_labels)
    if (!is.na(idx_term)) return(coef_names[assign_vec == idx_term])
    character()
  }
  for (g in groups) {
    rand_block <- spec$random[[g]]
    if (is.null(rand_block)) next
    sl <- rand_block$slopes %||% character()
    sl_cols <- unique(unlist(lapply(sl, expand_slope_cols)))
    inc_int <- isTRUE(rand_block$intercept)
    if (!inc_int && !length(sl_cols)) next
    if (isTRUE(rand_block$correlation)) {
      cnms[[length(cnms) + 1L]] <- c(if (inc_int) "(Intercept)", sl_cols)
      cnms_names <- c(cnms_names, g)
    } else {
      if (inc_int) {
        cnms[[length(cnms) + 1L]] <- "(Intercept)"
        cnms_names <- c(cnms_names, g)
      }
      for (sc in sl_cols) {
        cnms[[length(cnms) + 1L]] <- sc
        cnms_names <- c(cnms_names, g)
      }
    }
  }
  if (!length(cnms)) return(fallback_eq())
  names(cnms) <- cnms_names

  # Attempt hierarchical level decomposition when nested (2 or 3 levels)
  lev <- NULL
  if (nested && length(groups) <= 2L && !is.null(assign_vec) && length(assign_vec) == length(coef_names)) {
    n_levels <- length(groups) + 1L
    variable_level <- function(variable) {
      variable <- gsub("`", "", variable, fixed = TRUE)
      if (!variable %in% names(sub_dat)) return(NA_integer_)
      lvl <- 1L
      for (grp_nm in groups) {
        if (!constant_within(sub_dat[[variable]], flist[[grp_nm]])) break
        lvl <- lvl + 1L
      }
      lvl
    }
    ok <- TRUE
    parts <- matrix("", nrow = length(coef_names), ncol = n_levels)
    for (j in seq_along(coef_names)) {
      if (assign_vec[[j]] == 0L) next
      variables <- strsplit(term_labels[[assign_vec[[j]]]], ":", fixed = TRUE)[[1]]
      components <- strsplit(coef_names[[j]], ":", fixed = TRUE)[[1]]
      if (length(variables) != length(components)) { ok <- FALSE; break }
      lvls <- vapply(variables, variable_level, integer(1))
      if (anyNA(lvls)) { ok <- FALSE; break }
      for (lvl in seq_len(n_levels)) {
        parts[j, lvl] <- paste(components[lvls == lvl], collapse = ":")
      }
    }
    if (ok) {
      index <- matrix(0L, nrow = length(coef_names), ncol = n_levels)
      for (lvl in seq_len(n_levels)) {
        parent <- if (lvl == 1L) rep("", length(coef_names)) else apply(index[, seq_len(lvl - 1L), drop = FALSE], 1, paste, collapse = ".")
        for (k in unique(parent)) {
          r_idx <- which(parent == k)
          lbls <- unique(parts[r_idx, lvl])
          lbls <- lbls[nzchar(lbls)]
          index[r_idx, lvl] <- match(parts[r_idx, lvl], lbls, nomatch = 0L)
        }
      }
      random_map <- list()
      for (depth in seq_along(groups)) {
        comps <- cnms[names(cnms) == groups[[depth]]]
        lbls <- unlist(comps, use.names = FALSE)
        prefixes <- matrix(0L, nrow = length(lbls), ncol = depth)
        for (i in seq_along(lbls)) {
          if (identical(lbls[[i]], "(Intercept)")) next
          row_m <- match(lbls[[i]], coef_names)
          if (is.na(row_m) || any(index[row_m, seq_len(n_levels) > depth] != 0L)) { ok <- FALSE; break }
          prefixes[i, ] <- index[row_m, seq_len(depth)]
        }
        if (!ok) break
        random_map[[groups[[depth]]]] <- list(
          labels = lbls,
          prefixes = prefixes,
          component = rep(seq_along(comps), lengths(comps))
        )
      }
      if (ok) {
        lev <- list(
          n_levels = n_levels,
          groups = groups,
          coef_names = coef_names,
          parts = parts,
          index = index,
          random = random_map,
          order = do.call(order, as.data.frame(index))
        )
      }
    }
  }

  # Create lightweight mock result shell for equation formatting helpers
  mock_fit <- structure(
    list(link = spec$link),
    class = if (gaussian) "lmerMod_preview" else "glmerMod_preview"
  )
  link_line_spec <- function(eta_sym, out_tex) {
    mean_tex <- if (identical(spec$distribution, "binomial")) sprintf("P(%s = 1)", out_tex) else sprintf("E(%s)", out_tex)
    rhs <- switch(
      spec$link,
      logit = sprintf("\\mathrm{logit}\\left[%s\\right]", mean_tex),
      probit = sprintf("\\mathrm{probit}\\left[%s\\right]", mean_tex),
      log = sprintf("\\mathrm{log}\\left[%s\\right]", mean_tex),
      inverse = sprintf("1 / %s", mean_tex),
      sprintf("\\mathrm{%s}\\left[%s\\right]", latex_escape(spec$link), mean_tex)
    )
    paste(eta_sym, "=", rhs)
  }

  if (!is.null(lev)) {
    n_levels <- lev$n_levels
    coef_symbol <- if (n_levels == 2L) c("\\beta", "\\gamma") else c("\\pi", "\\beta", "\\gamma")
    random_symbol <- if (n_levels == 2L) "u" else c("r", "u")
    residual_symbol <- if (n_levels == 2L) "r" else "e"
    subs <- if (n_levels == 2L) c("ij", "j") else c("ijk", "jk", "k")
    tails <- substring(subs, 2)
    index <- lev$index
    parts <- lev$parts
    symbol <- function(base, prefix, tail) sprintf("%s_{%s%s}", base, equation_index(prefix), tail)
    variable_tex <- function(part, sub) {
      if (!nzchar(part)) return("")
      comps <- strsplit(part, ":", fixed = TRUE)[[1]]
      paste(sprintf("%s_{%s}", latex_var(comps), sub), collapse = "\\,")
    }
    key_of <- function(prefix) paste(prefix, collapse = ".")
    part_for <- function(prefix) {
      depth <- length(prefix)
      r_idx <- which(apply(index[, seq_len(depth), drop = FALSE], 1, key_of) == key_of(prefix))
      parts[r_idx[[1]], depth]
    }
    has_random <- function(depth, prefix) {
      rand <- lev$random[[lev$groups[[depth]]]]
      any(apply(rand$prefixes, 1, key_of) == key_of(prefix))
    }
    outcome_tex <- sprintf("%s_{%s}", latex_var(spec$outcome), subs[[1]])
    eta <- sprintf("\\eta_{%s}", subs[[1]])
    lhs <- if (gaussian) outcome_tex else eta
    l1_children <- sort(unique(index[, 1]))
    l1_terms <- vapply(l1_children, function(p) paste0(symbol(coef_symbol[[1]], p, tails[[1]]), variable_tex(part_for(p), subs[[1]])), character(1))
    level1 <- paste(lhs, "=", paste(c(l1_terms, if (gaussian) sprintf("%s_{%s}", residual_symbol, subs[[1]])), collapse = " + "))
    if (!gaussian) level1 <- paste(link_line_spec(eta, outcome_tex), level1, sep = "\\\\ ")

    higher <- lapply(seq_len(n_levels - 1L), function(depth) {
      prefixes <- unique(index[lev$order, seq_len(depth), drop = FALSE])
      lines <- vapply(seq_len(nrow(prefixes)), function(i) {
        prefix <- prefixes[i, ]
        r_idx <- which(apply(index[, seq_len(depth), drop = FALSE], 1, key_of) == key_of(prefix))
        children <- sort(unique(index[r_idx, depth + 1L]))
        terms <- vapply(children, function(child) {
          paste0(symbol(coef_symbol[[depth + 1L]], c(prefix, child), tails[[depth + 1L]]), variable_tex(part_for(c(prefix, child)), subs[[depth + 1L]]))
        }, character(1))
        if (has_random(depth, prefix)) terms <- c(terms, symbol(random_symbol[[depth]], prefix, tails[[depth]]))
        paste(symbol(coef_symbol[[depth]], prefix, tails[[depth]]), "=", paste(terms, collapse = " + "))
      }, character(1))
      paste(lines, collapse = "\\\\ ")
    })

    fixed_terms <- vapply(lev$order, function(row) {
      vars_s <- vapply(rev(seq_len(n_levels)), function(lvl) variable_tex(parts[row, lvl], subs[[lvl]]), character(1))
      paste(c(symbol("\\gamma", index[row, ], ""), vars_s[nzchar(vars_s)]), collapse = "")
    }, character(1))
    random_terms <- unlist(lapply(seq_along(lev$groups), function(depth) {
      rand <- lev$random[[lev$groups[[depth]]]]
      prefixes <- unique(rand$prefixes[do.call(order, as.data.frame(rand$prefixes)), , drop = FALSE])
      vapply(seq_len(nrow(prefixes)), function(i) {
        prefix <- prefixes[i, ]
        vars_s <- vapply(rev(seq_len(depth)), function(lvl) if (prefix[[lvl]] == 0L) "" else variable_tex(part_for(prefix[seq_len(lvl)]), subs[[lvl]]), character(1))
        paste(c(symbol(random_symbol[[depth]], prefix, tails[[depth]]), vars_s[nzchar(vars_s)]), collapse = "")
      }, character(1))
    }))
    combined <- paste(lhs, "=", paste(c(fixed_terms, random_terms, if (gaussian) sprintf("%s_{%s}", residual_symbol, subs[[1]])), collapse = " + "))
    tau_tag <- if (n_levels == 2L) "" else c("\\pi ", "\\beta ")
    blocks <- lapply(seq_along(lev$groups), function(depth) {
      grp_nm <- lev$groups[[depth]]
      rand <- lev$random[[grp_nm]]
      labels_index <- apply(rand$prefixes, 1, equation_index)
      syms <- vapply(seq_along(rand$labels), function(i) symbol(random_symbol[[depth]], rand$prefixes[i, ], tails[[depth]]), character(1))
      n <- length(syms)
      cells <- matrix("0", nrow = n, ncol = n)
      for (i in seq_len(n)) {
        for (j in seq_len(n)) {
          if (rand$component[[i]] != rand$component[[j]]) next
          pair <- c(labels_index[[max(i, j)]], labels_index[[min(i, j)]])
          cells[i, j] <- sprintf("\\tau_{%s%s}", tau_tag[[depth]], paste(pair, collapse = if (depth > 1L || any(nchar(pair) > 1L)) "," else ""))
        }
      }
      list(group = grp_nm, labels = rand$labels, symbols = syms, cells = cells)
    })
    tau_strs <- vapply(blocks, function(b) tau_block_latex(b$symbols, b$cells, b$group, b$labels), character(1))
    return(list(
      level = c("Level 1", sprintf("Level %s (%s)", seq_along(lev$groups) + 1L, lev$groups)),
      level_notes = c(
        if (gaussian) sprintf("Observation-level outcome model for %s.", spec$outcome) else sprintf("Link-scale model (%s link) for %s.", spec$link, spec$outcome),
        sprintf("Cluster-level equations across %s.", lev$groups)
      ),
      equations = c(level1, unlist(higher)),
      combined = combined,
      tau = tau_strs,
      notation = if (n_levels == 2L) "two-level hierarchical" else "three-level hierarchical",
      note = "Live multilevel equation preview generated from your current variable, centering, random-slope, and moderator selections."
    ))
  }

  # Mixed-model fallback for crossed / non-hierarchical specifications
  slopes <- setdiff(coef_names, "(Intercept)")
  unit_letters <- c("j", "k", "l", "m", "n", "o")
  random_letters <- c("u", "v", "w", "z", "a", "d")
  var_tex_mixed <- function(nm) {
    comps <- strsplit(nm, ":", fixed = TRUE)[[1]]
    paste(sprintf("%s_{i}", latex_var(comps)), collapse = "\\,")
  }
  outcome_tex <- sprintf("%s_{i}", latex_var(spec$outcome))
  lhs <- if (gaussian) outcome_tex else "\\eta_{i}"
  f_terms <- c(if ("(Intercept)" %in% coef_names) "\\beta_{0}", sprintf("\\beta_{%s}%s", seq_along(slopes), vapply(slopes, var_tex_mixed, character(1))))
  blocks <- lapply(seq_along(groups), function(g_idx) {
    grp_nm <- groups[[g_idx]]
    unit <- unit_letters[[(g_idx - 1L) %% length(unit_letters) + 1L]]
    letter <- random_letters[[(g_idx - 1L) %% length(random_letters) + 1L]]
    comps <- cnms[names(cnms) == grp_nm]
    lbls <- unlist(comps, use.names = FALSE)
    comp_id <- rep(seq_along(comps), lengths(comps))
    lbl_idx <- match(lbls, c("(Intercept)", slopes)) - 1L
    if (anyNA(lbl_idx)) lbl_idx <- seq_along(lbls) - 1L
    syms <- sprintf("%s_{%s%s}", letter, lbl_idx, unit)
    n <- length(lbls)
    cells <- matrix("0", nrow = n, ncol = n)
    for (i in seq_len(n)) {
      for (j in seq_len(n)) {
        if (comp_id[[i]] != comp_id[[j]]) next
        pair <- c(lbl_idx[[max(i, j)]], lbl_idx[[min(i, j)]])
        cells[i, j] <- sprintf("\\tau_{%s %s}", letter, paste(pair, collapse = if (any(pair > 9L)) "," else ""))
      }
    }
    terms_str <- paste0(
      sprintf("%s_{%s%s[i]}", letter, lbl_idx, unit),
      vapply(lbls, function(lbl) if (identical(lbl, "(Intercept)")) "" else var_tex_mixed(lbl), character(1))
    )
    list(group = grp_nm, labels = lbls, symbols = syms, cells = cells, terms = terms_str)
  })
  eq_str <- paste(lhs, "=", paste(c(f_terms, unlist(lapply(blocks, `[[`, "terms")), if (gaussian) "e_{i}"), collapse = " + "))
  eqs <- if (gaussian) eq_str else paste(link_line_spec("\\eta_{i}", outcome_tex), eq_str, sep = "\\\\ ")
  tau_strs <- vapply(blocks, function(b) tau_block_latex(b$symbols, b$cells, b$group, b$labels), character(1))
  list(
    level = "Mixed-model form",
    level_notes = sprintf("Cross-classified / mixed-model equation across %s.", paste(groups, collapse = " and ")),
    equations = eqs,
    combined = eq_str,
    tau = tau_strs,
    notation = "mixed-model",
    note = "Cross-classified design preview written in single-equation mixed-model notation."
  )
}
