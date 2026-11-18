# Publication-ready multilevel visualizations and side-by-side multi-model APA tables:
# - mlm_coef_plot(): Fixed-effects forest plot comparing model-based and cluster-robust (CR1) 95% CIs
# - mlm_interaction_plot(): Model-implied simple-slopes / cross-level interaction plot with 95% CIs
# - mlm_compare_table(): Side-by-side multi-model APA coefficient, variance, and fit table

resolve_plot_var <- function(var_name, result, candidates) {
  if (is.null(var_name) || !nzchar(var_name) || identical(var_name, "none")) return(NULL)
  if (var_name %in% candidates) return(var_name)
  if (inherits(result, "mlm_result") && !is.null(result$spec$fixed)) {
    eff_map <- effective_fixed_terms(result$spec$fixed)
    if (var_name %in% names(eff_map) && eff_map[[var_name]] %in% candidates) {
      return(unname(eff_map[[var_name]]))
    }
  }
  NULL
}

mlm_coef_plot <- function(result, include_intercept = FALSE) {
  fit <- unwrap_fit(result)
  if (!inherits(fit, "merMod")) {
    stop("`result` must be a fitted \"mlm_result\" or `lme4` model object.", call. = FALSE)
  }
  mod_tab <- fixed_effects_table(result)
  if (!isTRUE(include_intercept) && nrow(mod_tab) > 1L) {
    mod_tab <- mod_tab[mod_tab$term != "(Intercept)", , drop = FALSE]
  }
  rob_tab <- tryCatch(mlm_robust_se(result, type = "CR1"), error = function(e) NULL)

  rows <- list(data.frame(
    Term = mod_tab$term,
    Method = "Model-based",
    Estimate = mod_tab$Estimate,
    Low = mod_tab$CI_low,
    High = mod_tab$CI_high,
    stringsAsFactors = FALSE
  ))

  if (!is.null(rob_tab) && nrow(rob_tab)) {
    rob_sub <- rob_tab[rob_tab$Predictor %in% mod_tab$term, , drop = FALSE]
    if (nrow(rob_sub)) {
      beta_all <- lme4::fixef(fit)[rob_sub$Predictor]
      crit <- if (is_gaussian_fit(fit)) stats::qt(.975, df = pmax(1, rob_sub$Clusters - 1)) else rep(stats::qnorm(.975), nrow(rob_sub))
      rows[[2L]] <- data.frame(
        Term = rob_sub$Predictor,
        Method = "Cluster-robust (CR1)",
        Estimate = unname(beta_all),
        Low = unname(beta_all) - crit * rob_sub$SE_Robust,
        High = unname(beta_all) + crit * rob_sub$SE_Robust,
        stringsAsFactors = FALSE
      )
    }
  }

  plot_df <- do.call(rbind, rows)
  term_levels <- rev(unique(mod_tab$term))
  plot_df$Term <- factor(plot_df$Term, levels = term_levels)
  plot_df$Method <- factor(plot_df$Method, levels = c("Model-based", "Cluster-robust (CR1)"))

  x_lab <- if (is_gaussian_fit(fit)) {
    sprintf("Fixed-effect coefficient on %s (with 95%% CI)", if (inherits(result, "mlm_result")) result$spec$outcome else "outcome")
  } else {
    sprintf("Link-scale coefficient (%s link, with 95%% CI)", stats::family(fit)$link)
  }

  ggplot2::ggplot(
    plot_df,
    ggplot2::aes(
      x = .data[["Estimate"]],
      y = .data[["Term"]],
      xmin = .data[["Low"]],
      xmax = .data[["High"]],
      color = .data[["Method"]],
      shape = .data[["Method"]]
    )
  ) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "#7c8b95", linewidth = 0.6) +
    ggplot2::geom_errorbar(
      position = ggplot2::position_dodge(width = 0.48),
      width = 0.22,
      linewidth = 0.75,
      orientation = "y"
    ) +
    ggplot2::geom_point(
      position = ggplot2::position_dodge(width = 0.48),
      size = 2.7
    ) +
    ggplot2::scale_color_manual(values = c("Model-based" = "#203a43", "Cluster-robust (CR1)" = "#b05b3b")) +
    ggplot2::labs(
      x = x_lab,
      y = NULL,
      color = "95% CI Method",
      shape = "95% CI Method"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      legend.position = "top",
      panel.grid.minor = ggplot2::element_blank()
    )
}

interaction_plot_candidates <- function(result) {
  fit <- unwrap_fit(result)
  tt_rhs <- stats::delete.response(stats::terms(fit, fixed.only = TRUE))
  mf <- stats::model.frame(fit)
  intersect(all.vars(tt_rhs), names(mf))
}

default_interaction_pair <- function(result) {
  cands <- interaction_plot_candidates(result)
  if (!length(cands)) return(list(focal = NULL, moderator = "none"))
  if (inherits(result, "mlm_result") && length(result$spec$interactions)) {
    pair <- result$spec$interactions[[1]]
    f1 <- resolve_plot_var(pair[[1]], result, cands)
    f2 <- if (length(pair) >= 2L) resolve_plot_var(pair[[2]], result, cands) else NULL
    if (!is.null(f1) && !is.null(f2) && !identical(f1, f2)) {
      mf <- stats::model.frame(result$fit)
      # Prefer continuous variable on X axis and categorical/Level-2 variable as moderator
      if (!is.numeric(mf[[f1]]) && is.numeric(mf[[f2]])) {
        return(list(focal = f2, moderator = f1))
      }
      return(list(focal = f1, moderator = f2))
    }
  }
  focal <- cands[[1]]
  mod <- if (length(cands) >= 2L) cands[[2]] else "none"
  list(focal = focal, moderator = mod)
}

mlm_interaction_plot <- function(result, focal = NULL, moderator = NULL, vcov_type = c("CR1", "model")) {
  vcov_type <- match.arg(vcov_type)
  fit <- unwrap_fit(result)
  if (!inherits(fit, "merMod")) {
    stop("`result` must be a fitted \"mlm_result\" or `lme4` model object.", call. = FALSE)
  }
  tt_rhs <- stats::delete.response(stats::terms(fit, fixed.only = TRUE))
  mf <- stats::model.frame(fit)
  cands <- intersect(all.vars(tt_rhs), names(mf))
  if (!length(cands)) {
    return(
      ggplot2::ggplot() +
        ggplot2::annotate("text", x = 0, y = 0, label = "Add at least one fixed predictor to plot model-implied simple slopes.") +
        ggplot2::theme_void()
    )
  }

  defs <- default_interaction_pair(result)
  focal_col <- resolve_plot_var(focal, result, cands) %||% defs$focal %||% cands[[1]]
  mod_col <- if (identical(moderator, "none")) {
    NULL
  } else {
    resolve_plot_var(moderator, result, setdiff(cands, focal_col))
  }
  if (is.null(moderator) && !identical(defs$moderator, "none") && !identical(defs$moderator, focal_col)) {
    mod_col <- defs$moderator
  }

  is_num_focal <- is.numeric(mf[[focal_col]])
  focal_vals <- if (is_num_focal) {
    rng <- stats::quantile(mf[[focal_col]], probs = c(0.02, 0.98), na.rm = TRUE)
    if (!all(is.finite(rng)) || diff(rng) <= 1e-12) rng <- range(mf[[focal_col]], na.rm = TRUE)
    seq(rng[[1]], rng[[2]], length.out = 60L)
  } else {
    lv <- levels(factor(mf[[focal_col]]))
    factor(lv, levels = lv)
  }

  mod_info <- if (is.null(mod_col)) {
    list(values = 1L, labels = "Overall marginal trajectory")
  } else if (is.numeric(mf[[mod_col]])) {
    m_val <- mean(mf[[mod_col]], na.rm = TRUE)
    sd_val <- stats::sd(mf[[mod_col]], na.rm = TRUE)
    if (!is.finite(sd_val) || sd_val <= 1e-12) {
      list(values = m_val, labels = sprintf("%s = %.2f", mod_col, m_val))
    } else {
      vals <- c(m_val - sd_val, m_val, m_val + sd_val)
      labs <- c(
        sprintf("-1 SD (%.2f)", vals[[1]]),
        sprintf("Mean (%.2f)", vals[[2]]),
        sprintf("+1 SD (%.2f)", vals[[3]])
      )
      list(values = vals, labels = labs)
    }
  } else {
    lv <- levels(factor(mf[[mod_col]]))
    list(values = factor(lv, levels = lv), labels = lv)
  }

  grid_idx <- expand.grid(
    f_i = seq_along(focal_vals),
    m_i = seq_along(mod_info$values),
    KEEP.OUT.ATTRS = FALSE
  )
  n_grid <- nrow(grid_idx)
  grid <- data.frame(row_id = seq_len(n_grid))
  for (v in cands) {
    col_mf <- mf[[v]]
    if (identical(v, focal_col)) {
      grid[[v]] <- focal_vals[grid_idx$f_i]
    } else if (!is.null(mod_col) && identical(v, mod_col)) {
      grid[[v]] <- mod_info$values[grid_idx$m_i]
    } else if (is.numeric(col_mf)) {
      grid[[v]] <- rep(mean(col_mf, na.rm = TRUE), n_grid)
    } else {
      lv <- levels(factor(col_mf))
      grid[[v]] <- factor(rep(lv[[1]], n_grid), levels = lv)
    }
  }
  grid$row_id <- NULL

  X_orig <- lme4::getME(fit, "X")
  X_grid <- stats::model.matrix(tt_rhs, data = grid, contrasts.arg = attr(X_orig, "contrasts"))
  beta <- lme4::fixef(fit)
  common_cols <- intersect(names(beta), colnames(X_grid))
  X_sub <- X_grid[, common_cols, drop = FALSE]
  b_sub <- beta[common_cols]

  V_full <- if (identical(vcov_type, "CR1")) {
    tryCatch(cluster_sandwich_vcov(fit, type = "CR1")$vcov, error = function(e) as.matrix(stats::vcov(fit)))
  } else {
    as.matrix(stats::vcov(fit))
  }
  dimnames(V_full) <- list(names(beta), names(beta))
  V_sub <- V_full[common_cols, common_cols, drop = FALSE]

  eta <- as.numeric(X_sub %*% b_sub)
  se_eta <- sqrt(pmax(0, rowSums((X_sub %*% V_sub) * X_sub)))
  crit <- stats::qnorm(.975)

  if (is_gaussian_fit(fit)) {
    pred <- eta
    low <- eta - crit * se_eta
    high <- eta + crit * se_eta
    y_lab <- sprintf("Predicted %s", if (inherits(result, "mlm_result")) result$spec$outcome else "outcome")
  } else {
    inv <- stats::family(fit)$linkinv
    pred <- inv(eta)
    low <- inv(eta - crit * se_eta)
    high <- inv(eta + crit * se_eta)
    y_lab <- if (identical(stats::family(fit)$family, "binomial")) {
      sprintf("Predicted P(%s = 1)", if (inherits(result, "mlm_result")) result$spec$outcome else "event")
    } else {
      sprintf("Predicted %s (response scale)", if (inherits(result, "mlm_result")) result$spec$outcome else "response")
    }
  }

  plot_df <- data.frame(
    Focal = focal_vals[grid_idx$f_i],
    Moderator = factor(mod_info$labels[grid_idx$m_i], levels = unique(mod_info$labels)),
    Predicted = pred,
    Low = low,
    High = high,
    stringsAsFactors = FALSE
  )

  palette_vals <- c("#1f4e5f", "#b05b3b", "#5c7d47", "#7a5195", "#c59b45", "#3d6b82")
  mod_title <- if (is.null(mod_col)) NULL else mod_col

  if (is_num_focal) {
    p <- ggplot2::ggplot(
      plot_df,
      ggplot2::aes(
        x = .data[["Focal"]],
        y = .data[["Predicted"]],
        color = .data[["Moderator"]],
        fill = .data[["Moderator"]]
      )
    ) +
      ggplot2::geom_ribbon(
        ggplot2::aes(ymin = .data[["Low"]], ymax = .data[["High"]]),
        alpha = 0.16,
        color = NA
      ) +
      ggplot2::geom_line(linewidth = 1.05) +
      ggplot2::scale_color_manual(values = rep(palette_vals, length.out = nlevels(plot_df$Moderator))) +
      ggplot2::scale_fill_manual(values = rep(palette_vals, length.out = nlevels(plot_df$Moderator)))
  } else {
    p <- ggplot2::ggplot(
      plot_df,
      ggplot2::aes(
        x = .data[["Focal"]],
        y = .data[["Predicted"]],
        ymin = .data[["Low"]],
        ymax = .data[["High"]],
        color = .data[["Moderator"]],
        group = .data[["Moderator"]]
      )
    ) +
      ggplot2::geom_errorbar(position = ggplot2::position_dodge(width = 0.35), width = 0.18, linewidth = 0.8) +
      ggplot2::geom_point(position = ggplot2::position_dodge(width = 0.35), size = 2.8) +
      ggplot2::scale_color_manual(values = rep(palette_vals, length.out = nlevels(plot_df$Moderator)))
  }

  p +
    ggplot2::labs(
      x = focal_col,
      y = y_lab,
      color = mod_title,
      fill = mod_title
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      legend.position = if (is.null(mod_col)) "none" else "top",
      panel.grid.minor = ggplot2::element_blank()
    )
}

sig_stars <- function(p_val) {
  if (!length(p_val) || is.na(p_val)) return("")
  if (p_val < 0.001) return("***")
  if (p_val < 0.01) return("**")
  if (p_val < 0.05) return("*")
  ""
}

# Side-by-side multi-model APA table (Fixed Effects + Variance Components + Fit Statistics)
mlm_compare_table <- function(models, digits = 2) {
  if (inherits(models, "mlm_result")) {
    models <- list("Model 1" = models)
  }
  valid <- Filter(function(m) inherits(m, "mlm_result") && inherits(m$fit, "merMod"), models)
  if (!length(valid)) {
    return(data.frame(Section = character(), Term = character(), stringsAsFactors = FALSE))
  }
  if (is.null(names(valid)) || any(!nzchar(names(valid)))) {
    names(valid) <- sprintf("Model %d", seq_along(valid))
  }
  model_names <- names(valid)

  fmt_num <- paste0("%.", digits, "f")

  # 1. Fixed effects union in natural order
  fe_tabs <- lapply(valid, fixed_effects_table)
  all_fe <- unique(unlist(lapply(fe_tabs, function(t) t$term)))
  fe_rows <- lapply(all_fe, function(term) {
    vals <- vapply(fe_tabs, function(tab) {
      idx <- match(term, tab$term)
      if (is.na(idx)) return("")
      est <- sprintf(fmt_num, tab$Estimate[[idx]])
      se <- sprintf(fmt_num, tab[["Std. Error"]][[idx]])
      st <- sig_stars(tab$p[[idx]])
      sprintf("%s%s (%s)", est, st, se)
    }, character(1))
    as.data.frame(c(list(Section = "Fixed Effects", Term = term), as.list(stats::setNames(vals, model_names))), check.names = FALSE, stringsAsFactors = FALSE)
  })

  # 2. Variance components union (diagonal variances + residual variance)
  vc_lists <- lapply(valid, function(m) {
    vc <- as.data.frame(lme4::VarCorr(m$fit))
    vc <- vc[is.na(vc$var2), , drop = FALSE]
    labels <- ifelse(
      vc$grp == "Residual",
      "Level-1 Residual Variance",
      sprintf("%s: %s Variance", sub("\\.[0-9]+$", "", vc$grp), vc$var1)
    )
    stats::setNames(vc$vcov, labels)
  })
  all_vc <- unique(unlist(lapply(vc_lists, names)))
  vc_rows <- lapply(all_vc, function(v_lbl) {
    vals <- vapply(vc_lists, function(v_map) {
      if (!v_lbl %in% names(v_map) || is.na(v_map[[v_lbl]])) return("")
      sprintf(fmt_num, v_map[[v_lbl]])
    }, character(1))
    as.data.frame(c(list(Section = "Variance Components", Term = v_lbl), as.list(stats::setNames(vals, model_names))), check.names = FALSE, stringsAsFactors = FALSE)
  })

  # 3. Fit & sample summary rows
  stat_row <- function(label, fn) {
    vals <- vapply(valid, fn, character(1))
    as.data.frame(c(list(Section = "Model Fit", Term = label), as.list(stats::setNames(vals, model_names))), check.names = FALSE, stringsAsFactors = FALSE)
  }

  fit_rows <- list(
    stat_row("Observations (N)", function(m) as.character(stats::nobs(m$fit))),
    stat_row("Top-Level Clusters (J)", function(m) as.character(nlevels(top_level_cluster_vector(m$fit)$factor))),
    stat_row("Estimator", function(m) if (is_gaussian_fit(m$fit)) (if (lme4::isREML(m$fit)) "REML" else "ML") else "Laplace ML"),
    stat_row("Log-Likelihood", function(m) sprintf("%.2f", as.numeric(stats::logLik(m$fit)))),
    stat_row("Deviance (-2LL)", function(m) sprintf("%.2f", -2 * as.numeric(stats::logLik(m$fit)))),
    stat_row("AIC", function(m) sprintf("%.2f", stats::AIC(m$fit))),
    stat_row("BIC", function(m) sprintf("%.2f", stats::BIC(m$fit))),
    stat_row("Marginal R2", function(m) {
      r2 <- tryCatch(mlm_r2(m)$R2_marginal[[1]], error = function(e) NA_real_)
      if (is.na(r2)) "" else sprintf("%.3f", r2)
    }),
    stat_row("Conditional R2", function(m) {
      r2 <- tryCatch(mlm_r2(m)$R2_conditional[[1]], error = function(e) NA_real_)
      if (is.na(r2)) "" else sprintf("%.3f", r2)
    })
  )

  out <- do.call(rbind, c(fe_rows, vc_rows, fit_rows))
  row.names(out) <- NULL
  out
}

apa_compare_models_html <- function(models, table_number = "6", title = "Side-by-Side Multilevel Model Comparison") {
  tab <- mlm_compare_table(models)
  if (!nrow(tab)) {
    return(apa_html_table(
      data.frame(Term = character(), check.names = FALSE),
      table_number = table_number,
      title = title,
      note = "Fit one or more models (or click Auto-Build 3-Step Sequence) to generate a side-by-side comparison table."
    ))
  }
  apa_html_table(
    tab,
    table_number = table_number,
    title = title,
    note = "Fixed-effect entries report unstandardized b (SE). * p < .05, ** p < .01, *** p < .001.",
    italic_columns = character()
  )
}
