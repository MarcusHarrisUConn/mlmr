compute_ddf_summary <- function(fit, df_method = "none") {
  if (!is_gaussian_fit(fit) || identical(df_method, "none")) {
    return(list(method = "none", table = NULL))
  }
  if (!requireNamespace("lmerTest", quietly = TRUE)) {
    warning(
      "Package `lmerTest` is required for ", df_method,
      " degrees of freedom; falling back to large-sample normal approximations.",
      call. = FALSE
    )
    return(list(method = "none", table = NULL))
  }
  if (identical(df_method, "kenward-roger") && !requireNamespace("pbkrtest", quietly = TRUE)) {
    warning(
      "Package `pbkrtest` is required for Kenward-Roger degrees of freedom; falling back to Satterthwaite.",
      call. = FALSE
    )
    df_method <- "satterthwaite"
  }
  if (identical(df_method, "kenward-roger") && !isTRUE(lme4::isREML(fit))) {
    warning(
      "Kenward-Roger degrees of freedom require a REML-fitted model; falling back to Satterthwaite.",
      call. = FALSE
    )
    df_method <- "satterthwaite"
  }
  ddf_arg <- if (identical(df_method, "kenward-roger")) "Kenward-Roger" else "Satterthwaite"
  tab <- tryCatch(
    {
      lt <- if (inherits(fit, "lmerModLmerTest")) fit else lmerTest::as_lmerModLmerTest(fit)
      stats::coef(summary(lt, ddf = ddf_arg))
    },
    error = function(e) {
      warning("Denominator-df calculation failed (", conditionMessage(e), "); using large-sample normal approximations.", call. = FALSE)
      NULL
    }
  )
  if (is.null(tab) || !"df" %in% colnames(tab)) {
    return(list(method = "none", table = NULL))
  }
  list(method = df_method, table = tab)
}

mlm_fit <- function(spec, REML = TRUE, optimizer = "bobyqa", maxfun = 20000,
                    df_method = c("none", "satterthwaite", "kenward-roger")) {
  if (!inherits(spec, "mlm_spec")) {
    stop("`spec` must be an \"mlm_spec\" object created with mlm_spec().", call. = FALSE)
  }
  df_method <- match.arg(df_method)
  if (!is.data.frame(spec$data)) {
    stop("The specification has no data. Supply `data` to mlm_spec() before fitting.", call. = FALSE)
  }
  if (!family_link_supported(spec$distribution, spec$link)) {
    stop("Unsupported distribution/link pair. ", family_link_detail(spec$distribution), call. = FALSE)
  }
  missing_vars <- setdiff(model_variables(spec), names(spec$data))
  if (length(missing_vars)) {
    stop("The following selected variables are missing from `data`: ", paste(missing_vars, collapse = ", "), call. = FALSE)
  }
  problems <- spec_problems(spec)
  if (nrow(problems)) {
    stop("The model specification cannot be fit:\n", paste0("  - ", problems$Detail, collapse = "\n"), call. = FALSE)
  }
  rows <- analysis_rows(spec$data, model_variables(spec))
  if (!any(rows)) {
    stop("No rows are complete on the selected model variables.", call. = FALSE)
  }
  centered <- center_predictors(spec$data, spec$fixed, spec$grouping, rows = rows)
  spec$data <- centered$data
  formula <- build_formula(spec)
  if (identical(spec$distribution, "gaussian")) {
    control <- lme4::lmerControl(optimizer = optimizer, optCtrl = list(maxfun = maxfun))
    dat_fit <- spec$data
    if (!identical(df_method, "none") && requireNamespace("lmerTest", quietly = TRUE)) {
      fit <- suppressMessages(lmerTest::lmer(formula, data = dat_fit, REML = REML, control = control))
    } else {
      fit <- suppressMessages(lme4::lmer(formula, data = dat_fit, REML = REML, control = control))
    }
  } else if (identical(spec$distribution, "negative binomial")) {
    glmm_control <- lme4::glmerControl(optimizer = optimizer, optCtrl = list(maxfun = maxfun))
    fit <- suppressMessages(lme4::glmer.nb(formula, data = spec$data, control = glmm_control))
  } else {
    glmm_control <- lme4::glmerControl(optimizer = optimizer, optCtrl = list(maxfun = maxfun))
    fam <- switch(
      spec$distribution,
      binomial = stats::binomial(link = spec$link),
      poisson = stats::poisson(link = spec$link),
      Gamma = stats::Gamma(link = spec$link),
      stop("Unsupported distribution: ", spec$distribution, call. = FALSE)
    )
    fit <- suppressMessages(lme4::glmer(formula, data = spec$data, family = fam, control = glmm_control))
  }
  ddf_info <- compute_ddf_summary(fit, df_method = df_method)
  structure(
    list(
      fit = fit,
      formula = formula,
      data = spec$data,
      centering_code = centered$code,
      spec = spec,
      ddf_table = ddf_info$table,
      settings = list(
        REML = identical(spec$distribution, "gaussian") && isTRUE(REML),
        optimizer = optimizer,
        maxfun = maxfun,
        df_method = ddf_info$method
      )
    ),
    class = "mlm_result"
  )
}

print.mlm_result <- function(x, ...) {
  fit <- x$fit
  estimator <- if (is_gaussian_fit(fit)) {
    if (lme4::isREML(fit)) "REML" else "ML"
  } else {
    "ML (Laplace approximation)"
  }
  group_sizes <- lme4::ngrps(fit)
  cat("<mlm_result>\n")
  cat("  Formula:      ", paste(deparse(x$formula, width.cutoff = 500), collapse = " "), "\n", sep = "")
  cat("  Distribution: ", x$spec$distribution, " (", x$spec$link, " link), estimated by ", estimator, "\n", sep = "")
  cat("  Observations: ", stats::nobs(fit), " of ", nrow(x$data), " rows\n", sep = "")
  cat("  Groups:       ", paste(sprintf("%s = %s", names(group_sizes), group_sizes), collapse = ", "), "\n", sep = "")
  cat("  AIC:          ", sprintf("%.2f", stats::AIC(fit)), "\n", sep = "")
  cat("  Singular fit: ", if (isTRUE(lme4::isSingular(fit, tol = 1e-4))) "yes" else "no", "\n\n", sep = "")
  print(apa_fixed_table(x), row.names = FALSE)
  cat("\nUse mlm_apa_tables(), mlm_latex_equations(), mlm_r2(), or mlm_publication_bundle() for reporting output.\n")
  invisible(x)
}

result_settings <- function(result, REML = NULL, optimizer = NULL, maxfun = NULL, df_method = NULL) {
  stored <- result$settings %||% list()
  fit_reml <- tryCatch(is_gaussian_fit(result$fit) && lme4::isREML(result$fit), error = function(e) NULL)
  list(
    REML = isTRUE(fit_reml %||% stored$REML %||% REML %||% TRUE),
    optimizer = stored$optimizer %||% optimizer %||% "bobyqa",
    maxfun = stored$maxfun %||% maxfun %||% 20000,
    df_method = stored$df_method %||% df_method %||% "none"
  )
}

unwrap_fit <- function(x) {
  if (inherits(x, "mlm_result")) x$fit else x
}

fixed_effects_note <- function(x) {
  df_method <- if (inherits(x, "mlm_result")) x$settings$df_method %||% "none" else "none"
  fit <- unwrap_fit(x)
  if (is_gaussian_fit(fit)) {
    if (identical(df_method, "satterthwaite")) {
      return("For this Gaussian mixed model, p values and 95% confidence intervals use Satterthwaite denominator degrees of freedom via lmerTest.")
    }
    if (identical(df_method, "kenward-roger")) {
      return("For this Gaussian mixed model, p values and 95% confidence intervals use Kenward-Roger denominator degrees of freedom via lmerTest and pbkrtest.")
    }
    return("For Gaussian mixed models fit with lme4, p values use large-sample normal approximations; set df_method = \"satterthwaite\" (with lmerTest) when small-sample denominator degrees of freedom are needed.")
  }
  "For generalized mixed models, p values and intervals are Wald large-sample approximations on the link scale."
}

icc_note <- function(fit) {
  fit <- unwrap_fit(fit)
  if (is_gaussian_fit(fit)) {
    "ICCs represent the proportion of model-implied variance attributable to each grouping level for this Gaussian mixed model."
  } else {
    "Classical Gaussian ICCs are not computed for generalized mixed models; use mlm_r2() or model-family-specific latent- or observation-scale ICC methods before reporting ICCs."
  }
}

fixed_effects_table <- function(x, df_method = NULL) {
  fit <- unwrap_fit(x)
  method <- df_method %||% (if (inherits(x, "mlm_result")) x$settings$df_method else NULL) %||% "none"
  cached_ddf <- if (inherits(x, "mlm_result") && identical(method, x$settings$df_method %||% "none")) x$ddf_table else NULL
  if (is.null(cached_ddf) && !identical(method, "none") && is_gaussian_fit(fit)) {
    ddf_res <- compute_ddf_summary(fit, df_method = method)
    method <- ddf_res$method
    cached_ddf <- ddf_res$table
  }
  if (!is.null(cached_ddf) && "df" %in% colnames(cached_ddf)) {
    estimate <- cached_ddf[, "Estimate"]
    se <- cached_ddf[, "Std. Error"]
    df_val <- cached_ddf[, "df"]
    stat <- cached_ddf[, "t value"]
    p_val <- if ("Pr(>|t|)" %in% colnames(cached_ddf)) cached_ddf[, "Pr(>|t|)"] else 2 * stats::pt(abs(stat), df = df_val, lower.tail = FALSE)
    crit <- stats::qt(.975, df = pmax(df_val, 1))
    out <- data.frame(
      term = rownames(cached_ddf),
      Estimate = estimate,
      `Std. Error` = se,
      df = df_val,
      `t value` = stat,
      p = p_val,
      CI_low = estimate - crit * se,
      CI_high = estimate + crit * se,
      check.names = FALSE,
      row.names = NULL,
      stringsAsFactors = FALSE
    )
    return(out)
  }

  coefs <- stats::coef(summary(fit))
  stat_col <- intersect(c("t value", "z value"), colnames(coefs))
  estimate <- coefs[, "Estimate"]
  se <- coefs[, "Std. Error"]
  stat <- if (length(stat_col)) coefs[, stat_col[[1]]] else estimate / se
  out <- data.frame(term = rownames(coefs), coefs, check.names = FALSE, row.names = NULL, stringsAsFactors = FALSE)
  out$p <- 2 * stats::pnorm(abs(stat), lower.tail = FALSE)
  out$CI_low <- estimate - stats::qnorm(.975) * se
  out$CI_high <- estimate + stats::qnorm(.975) * se
  out
}

variance_table <- function(fit) {
  fit <- unwrap_fit(fit)
  vc <- as.data.frame(lme4::VarCorr(fit))
  vc[, intersect(c("grp", "var1", "var2", "vcov", "sdcor"), names(vc)), drop = FALSE]
}

icc_table <- function(fit, include_latent = FALSE) {
  fit <- unwrap_fit(fit)
  if (!is_gaussian_fit(fit) && !isTRUE(include_latent)) {
    return(data.frame(group = character(), variance = numeric(), icc = numeric()))
  }
  vc <- as.data.frame(lme4::VarCorr(fit))
  is_intercept <- vc$grp != "Residual" & is.na(vc$var2) & !is.na(vc$var1) & vc$var1 == "(Intercept)"
  intercepts <- vc[is_intercept, c("grp", "vcov"), drop = FALSE]
  residual <- if (is_gaussian_fit(fit)) {
    attr(lme4::VarCorr(fit), "sc")^2
  } else {
    glmm_distribution_variance(fit)$variance
  }
  total <- sum(intercepts$vcov, na.rm = TRUE) + residual
  if (!nrow(intercepts) || !is.finite(total) || total <= 0) {
    return(data.frame(group = character(), variance = numeric(), icc = numeric()))
  }
  data.frame(
    group = intercepts$grp,
    variance = round(intercepts$vcov, 4),
    icc = round(intercepts$vcov / total, 4),
    stringsAsFactors = FALSE
  )
}

glmm_distribution_variance <- function(fit) {
  if (is_gaussian_fit(fit)) {
    return(list(variance = stats::sigma(fit)^2, scale = "Gaussian residual variance"))
  }
  fam <- stats::family(fit)
  if (identical(fam$family, "binomial")) {
    if (identical(fam$link, "logit")) {
      return(list(variance = (pi^2) / 3, scale = "Latent logistic scale (pi^2 / 3)"))
    }
    if (identical(fam$link, "probit")) {
      return(list(variance = 1, scale = "Latent probit scale (1.0)"))
    }
  }
  if (grepl("^poisson", fam$family, ignore.case = TRUE) && identical(fam$link, "log")) {
    mu <- mean(stats::fitted(fit), na.rm = TRUE)
    return(list(
      variance = log(1 + 1 / max(mu, 1e-6)),
      scale = "Log-normal count approximation (Nakagawa et al., 2017)"
    ))
  }
  if (grepl("^Negative Binomial", fam$family, ignore.case = TRUE) && identical(fam$link, "log")) {
    mu <- mean(stats::fitted(fit), na.rm = TRUE)
    theta <- tryCatch(lme4::getME(fit, "glmer.nb.theta"), error = function(e) 1)
    return(list(
      variance = log(1 + 1 / max(mu, 1e-6) + 1 / max(theta, 1e-6)),
      scale = "Log-normal negative-binomial approximation (Nakagawa et al., 2017)"
    ))
  }
  if (identical(fam$family, "Gamma") && identical(fam$link, "log")) {
    phi <- stats::sigma(fit)^2
    return(list(
      variance = log(1 + max(phi, 0)),
      scale = "Log-normal Gamma approximation (Nakagawa et al., 2017)"
    ))
  }
  list(variance = NA_real_, scale = sprintf("Unsupported scale for %s (%s)", fam$family, fam$link))
}

# Marginal and conditional R^2 following Nakagawa & Schielzeth (2013),
# Johnson (2014) for random slopes, and Nakagawa et al. (2017) for GLMMs.
mlm_r2 <- function(result) {
  fit <- unwrap_fit(result)
  if (!inherits(fit, "merMod")) {
    stop("`result` must be a fitted \"mlm_result\" or `lme4` model object.", call. = FALSE)
  }
  X <- lme4::getME(fit, "X")
  beta <- lme4::fixef(fit)
  eta_fixed <- as.numeric(X %*% beta)
  var_fixed <- if (length(eta_fixed) > 1) stats::var(eta_fixed) else 0

  vc_list <- lme4::VarCorr(fit)
  var_random <- sum(vapply(vc_list, function(sigma_mat) {
    sigma <- as.matrix(sigma_mat)
    rn <- rownames(sigma)
    if (all(rn %in% colnames(X))) {
      Z_sub <- X[, rn, drop = FALSE]
      mean(rowSums((Z_sub %*% sigma) * Z_sub))
    } else {
      sum(diag(sigma))
    }
  }, numeric(1)))

  dist_info <- glmm_distribution_variance(fit)
  var_dist <- dist_info$variance
  total_var <- var_fixed + var_random + var_dist
  if (!is.finite(total_var) || total_var <= 0) {
    return(data.frame(
      R2_marginal = NA_real_,
      R2_conditional = NA_real_,
      Fixed_Variance = round(var_fixed, 4),
      Random_Variance = round(var_random, 4),
      Residual_Variance = NA_real_,
      Scale = dist_info$scale,
      stringsAsFactors = FALSE
    ))
  }
  data.frame(
    R2_marginal = round(var_fixed / total_var, 4),
    R2_conditional = round((var_fixed + var_random) / total_var, 4),
    Fixed_Variance = round(var_fixed, 4),
    Random_Variance = round(var_random, 4),
    Residual_Variance = round(var_dist, 4),
    Scale = dist_info$scale,
    stringsAsFactors = FALSE
  )
}

# Empirical Bayes random-coefficient reliability (Raudenbush & Bryk, 2002, Eq. 3.45):
# Lambda_j = I_q - (I_q + sigma^-2 * T_g * Z_j' Z_j)^-1, averaged across clusters,
# covering both random intercepts and random slopes.
mlm_reliability <- function(result) {
  fit <- unwrap_fit(result)
  if (!inherits(fit, "merMod")) {
    stop("`result` must be a fitted \"mlm_result\" or `lme4` model object.", call. = FALSE)
  }
  empty_df <- data.frame(
    Group = character(),
    Term = character(),
    Units = integer(),
    Variance = numeric(),
    Mean_Reliability = numeric(),
    Min_Reliability = numeric(),
    Max_Reliability = numeric(),
    stringsAsFactors = FALSE
  )
  if (!is_gaussian_fit(fit)) {
    return(empty_df)
  }
  vc_list <- lme4::VarCorr(fit)
  flist <- lme4::getME(fit, "flist")
  X <- lme4::getME(fit, "X")
  sigma2 <- stats::sigma(fit)^2
  if (!is.finite(sigma2) || sigma2 <= 0) return(empty_df)

  rows <- list()
  for (vc_name in names(vc_list)) {
    T_mat <- as.matrix(vc_list[[vc_name]])
    base_grp <- sub("\\.[0-9]+$", "", vc_name)
    if (!base_grp %in% names(flist)) next
    terms <- rownames(T_mat)
    if (!all(terms %in% colnames(X))) next
    q <- nrow(T_mat)
    I_q <- diag(q)
    grp_fac <- droplevels(flist[[base_grp]])
    idx_by_grp <- split(seq_along(grp_fac), grp_fac)
    lambda_mat <- matrix(NA_real_, nrow = length(idx_by_grp), ncol = q)
    for (j in seq_along(idx_by_grp)) {
      idx <- idx_by_grp[[j]]
      Zj <- X[idx, terms, drop = FALSE]
      ZtZ <- crossprod(Zj)
      M <- I_q + (1 / sigma2) * (T_mat %*% ZtZ)
      inv_M <- tryCatch(solve(M), error = function(e) diag(1, q))
      Lam_j <- I_q - inv_M
      lambda_mat[j, ] <- pmin(1, pmax(0, diag(Lam_j)))
    }
    for (k in seq_len(q)) {
      vals <- lambda_mat[, k]
      rows[[length(rows) + 1L]] <- data.frame(
        Group = base_grp,
        Term = terms[[k]],
        Units = nrow(lambda_mat),
        Variance = round(T_mat[k, k], 4),
        Mean_Reliability = round(mean(vals, na.rm = TRUE), 3),
        Min_Reliability = round(min(vals, na.rm = TRUE), 3),
        Max_Reliability = round(max(vals, na.rm = TRUE), 3),
        stringsAsFactors = FALSE
      )
    }
  }
  if (!length(rows)) return(empty_df)
  out <- do.call(rbind, rows)
  row.names(out) <- NULL
  out
}

mlm_diagnostics <- function(fit) {
  fit <- unwrap_fit(fit)
  messages <- fit@optinfo$conv$lme4$messages
  gradient <- fit@optinfo$derivs$gradient
  overdispersion <- if (inherits(fit, "glmerMod")) {
    ratio <- sum(stats::resid(fit, type = "pearson")^2, na.rm = TRUE) / stats::df.residual(fit)
    format(ratio, digits = 4)
  } else {
    "Not applicable"
  }
  data.frame(
    check = c("Singular fit", "Convergence messages", "Maximum absolute gradient", "Overdispersion ratio"),
    value = c(
      as.character(lme4::isSingular(fit, tol = 1e-4)),
      if (is.null(messages)) "None reported" else paste(messages, collapse = "; "),
      if (is.null(gradient)) "Not available" else format(max(abs(gradient)), digits = 4),
      overdispersion
    ),
    stringsAsFactors = FALSE
  )
}

# Builds an observation-level data frame aligned strictly to the complete-case
# rows used by `fit`, preventing row-count crashes when `result$data` has NAs
# and mapping binary factor outcomes to 0/1 numeric probabilities for plots.
diagnostic_frame <- function(result) {
  fit <- result$fit
  fitted_vals <- as.numeric(stats::fitted(fit))
  pearson_vals <- as.numeric(stats::resid(fit, type = "pearson"))
  resid_vals <- if (is_gaussian_fit(fit)) {
    as.numeric(stats::resid(fit))
  } else {
    pearson_vals
  }
  rows <- analysis_rows(result$data, model_variables(result$spec))
  sub_data <- result$data[rows, , drop = FALSE]
  if (nrow(sub_data) != length(fitted_vals)) {
    mf <- stats::model.frame(fit)
    matched <- match(rownames(mf), rownames(result$data))
    if (!anyNA(matched) && length(matched) == length(fitted_vals)) {
      sub_data <- result$data[matched, , drop = FALSE]
    } else {
      sub_data <- as.data.frame(mf, check.names = FALSE)
    }
  }
  obs_raw <- if (result$spec$outcome %in% names(sub_data)) {
    sub_data[[result$spec$outcome]]
  } else {
    stats::model.response(stats::model.frame(fit))
  }
  obs_num <- if (is.numeric(obs_raw)) {
    as.numeric(obs_raw)
  } else if (is.factor(obs_raw) || is.character(obs_raw)) {
    fac <- factor(obs_raw)
    if (nlevels(fac) == 2L) as.numeric(fac == levels(fac)[[2L]]) else as.numeric(fac)
  } else {
    suppressWarnings(as.numeric(obs_raw))
  }
  data.frame(
    observed = obs_num,
    fitted = fitted_vals,
    residual = resid_vals,
    pearson = pearson_vals,
    sub_data,
    check.names = FALSE,
    row.names = NULL
  )
}

compare_models_lrt <- function(history) {
  if (length(history) < 2) {
    return(list(
      ok = FALSE,
      note = "Fit at least two models on the same dataset and outcome to run a likelihood-ratio comparison.",
      table = NULL
    ))
  }
  results <- lapply(history, function(item) if (inherits(item, "mlm_result")) item else item$result)
  fits <- lapply(results, `[[`, "fit")
  outcomes <- unique(vapply(results, function(r) r$spec$outcome, character(1)))
  dists <- unique(vapply(results, function(r) r$spec$distribution, character(1)))
  n_obs <- unique(vapply(fits, stats::nobs, integer(1)))
  if (length(outcomes) > 1) {
    return(list(
      ok = FALSE,
      note = sprintf("Stored models use different outcomes (%s); likelihood-ratio tests require the same outcome and analysis sample.", paste(outcomes, collapse = ", ")),
      table = data.frame(Message = "Likelihood-ratio comparison skipped because stored models use different outcome variables.", check.names = FALSE)
    ))
  }
  if (length(dists) > 1) {
    return(list(
      ok = FALSE,
      note = sprintf("Stored models use different distributions (%s); likelihood-ratio tests require comparable model families.", paste(dists, collapse = ", ")),
      table = data.frame(Message = "Likelihood-ratio comparison skipped because stored models use different outcome distributions.", check.names = FALSE)
    ))
  }
  if (length(n_obs) > 1) {
    return(list(
      ok = FALSE,
      note = sprintf("Stored models have different complete-case sample sizes (%s rows); restrict to a common complete sample before running LRT.", paste(n_obs, collapse = " vs. ")),
      table = data.frame(Message = "Likelihood-ratio comparison skipped because models were fit to different numbers of complete observations.", check.names = FALSE)
    ))
  }
  all_gaussian <- all(vapply(fits, is_gaussian_fit, logical(1)))
  model_names <- names(history) %||% paste0("Model_", seq_along(fits))
  anova_args <- c(unname(fits), list(model.names = model_names))
  if (all_gaussian) anova_args$refit <- TRUE
  out <- tryCatch(
    do.call(stats::anova, anova_args),
    error = function(e) e
  )
  if (inherits(out, "error")) {
    return(list(
      ok = FALSE,
      note = "Likelihood-ratio comparison could not be computed for the stored models.",
      table = data.frame(Message = conditionMessage(out), check.names = FALSE)
    ))
  }
  tab <- as.data.frame(out)
  tab <- cbind(Model = rownames(tab), tab, stringsAsFactors = FALSE)
  rownames(tab) <- NULL
  list(
    ok = TRUE,
    note = "Likelihood-ratio comparisons are valid for nested models fit to the exact same observations. Gaussian REML models are automatically refit with ML for fixed-effect comparisons.",
    table = tab
  )
}

model_summary_cards <- function(result, optimizer = NULL) {
  spec <- result$spec
  fit <- result$fit
  fixed <- apa_fixed_table(result)
  fixed_non_intercept <- fixed[fixed$Predictor != "(Intercept)", , drop = FALSE]
  sig <- fixed_non_intercept[is_significant_p_string(fixed_non_intercept$p), , drop = FALSE]
  diag <- mlm_diagnostics(fit)
  r2 <- tryCatch(mlm_r2(result), error = function(e) NULL)
  r2_text <- if (!is.null(r2) && is.finite(r2$R2_marginal)) {
    sprintf("Marginal = %.3f; Conditional = %.3f", r2$R2_marginal, r2$R2_conditional)
  } else {
    "Not available"
  }
  data.frame(
    Metric = c(
      "Outcome",
      "Distribution",
      "Grouping structure",
      "Fixed effects",
      "Random-effect blocks",
      "Significant fixed effects",
      "R-squared (Nakagawa)",
      "AIC",
      "Singular fit",
      "Optimizer"
    ),
    Value = c(
      spec$outcome,
      if (identical(spec$distribution, "gaussian")) "Gaussian" else sprintf("%s (%s link)", spec$distribution, spec$link),
      spec$structure %||% "nested",
      as.character(length(names(spec$fixed))),
      paste(names(spec$random), collapse = ", "),
      if (nrow(sig)) paste(sig$Predictor, collapse = ", ") else "None at alpha = .05",
      r2_text,
      sprintf("%.2f", stats::AIC(fit)),
      diag$value[diag$check == "Singular fit"],
      optimizer %||% result$settings$optimizer %||% "Not recorded"
    ),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

model_guidance <- function(result, REML = NULL) {
  REML <- result_settings(result, REML = REML)$REML
  spec <- result$spec
  fit <- result$fit
  diag <- mlm_diagnostics(fit)
  groups <- unlist(spec$grouping, use.names = FALSE)
  warnings <- character()
  if (isTRUE(lme4::isSingular(fit, tol = 1e-4))) {
    warnings <- c(warnings, "The model is singular; at least one random-effect variance or covariance may be estimated near zero. Consider simplifying random slopes or correlations.")
  }
  messages <- diag$value[diag$check == "Convergence messages"]
  if (length(messages) && !identical(messages, "None reported")) {
    warnings <- c(warnings, paste("Convergence message:", messages))
  }
  if (identical(spec$distribution, "gaussian") && isTRUE(REML)) {
    warnings <- c(warnings, "REML is appropriate for estimating variance components, but use ML when comparing nested fixed-effect models.")
  }
  if (!identical(spec$distribution, "gaussian")) {
    warnings <- c(warnings, "This is a generalized mixed model workflow. Review link-scale interpretation, sparse cells or overdispersion, and family-specific reporting before publication.")
  }
  for (group in groups) {
    if (group %in% names(result$data) && length(unique(stats::na.omit(result$data[[group]]))) < 10) {
      warnings <- c(warnings, sprintf("%s has fewer than 10 groups; random effects for this grouping factor may be unstable.", group))
    }
  }
  if (!length(warnings)) warnings <- "No major convergence or design warnings were detected."
  unique(warnings)
}

model_template_explanation <- function(template) {
  switch(
    template %||% "custom",
    null = "Null model: estimates unconditional clustering and ICCs before adding predictors.",
    random_intercept = "Random intercept model: allows average outcome levels to vary across groups while keeping predictor effects fixed.",
    random_slope = "Random slope model: allows selected Level-1 associations to vary across groups and estimates their variance.",
    cross_level = "Cross-level interaction model: tests whether a higher-level predictor moderates a lower-level predictor slope.",
    three_level = "Three-level nested model: represents students within schools within districts, with variance partitioned across two clustering levels.",
    longitudinal = "Longitudinal growth model: models repeated measurement waves nested within individuals (and optionally schools), with random intercepts and growth slopes.",
    crossed = "Crossed model: estimates separate random-effect blocks when units belong to non-nested classifications, such as schools and teachers.",
    logistic = "Logistic HGLM: fits a multilevel model for a binary outcome using a binomial distribution and logit link, reporting odds ratios.",
    poisson = "Poisson HGLM: fits a multilevel model for a count outcome using a Poisson distribution and log link, reporting incidence rate ratios.",
    "Custom model: uses the current selections for fixed effects, interactions, centering, and random effects."
  )
}

interpretation_points <- function(result) {
  spec <- result$spec
  fixed <- apa_fixed_table(result)
  icc <- icc_table(result$fit)
  r2 <- tryCatch(mlm_r2(result), error = function(e) NULL)
  rel <- tryCatch(mlm_reliability(result), error = function(e) NULL)
  random_slopes <- unique(unlist(lapply(spec$random, function(x) x$slopes %||% character())))
  interactions <- vapply(spec$interactions, paste, character(1), collapse = " x ")
  sig <- fixed[!fixed$Predictor %in% "(Intercept)" & is_significant_p_string(fixed$p), , drop = FALSE]
  pts <- c(
    sprintf("The outcome is %s, modeled with a %s multilevel specification.", spec$outcome, spec$distribution),
    sprintf(
      "Fixed effects represent average associations after the selected centering and dummy-coding choices. %s %s",
      if (nrow(sig)) paste("Statistically detectable fixed effects include", paste(sig$Predictor, collapse = ", "), ".") else "No non-intercept fixed effects are below alpha = .05 in the current fit.",
      fixed_effects_note(result)
    ),
    sprintf(
      "Random effects are estimated for %s. Random slopes are included for %s.",
      paste(names(spec$random), collapse = ", "),
      if (length(random_slopes)) paste(random_slopes, collapse = ", ") else "no predictors"
    ),
    sprintf("Interactions in the current model: %s.", if (length(interactions)) paste(interactions, collapse = ", ") else "none"),
    sprintf(
      "%s %s",
      icc_note(result$fit),
      if (nrow(icc)) paste("Current ICC estimates:", paste(sprintf("%s = %.3f", icc$group, icc$icc), collapse = "; ")) else ""
    )
  )
  if (!is.null(r2) && is.finite(r2$R2_marginal)) {
    pts <- c(pts, sprintf(
      "Variance explained (%s): marginal R^2 = %.3f (fixed effects alone), conditional R^2 = %.3f (fixed plus random effects).",
      r2$Scale, r2$R2_marginal, r2$R2_conditional
    ))
  }
  if (!is.null(rel) && nrow(rel)) {
    pts <- c(pts, sprintf(
      "Mean empirical Bayes random-coefficient reliability: %s.",
      paste(sprintf("%s (%s) = %.3f", rel$Group, rel$Term, rel$Mean_Reliability), collapse = "; ")
    ))
  }
  pts
}

assumption_points <- function(result) {
  diag <- mlm_diagnostics(result$fit)
  c(
    "Inspect residual plots for nonlinearity, heteroskedasticity, and influential patterns before treating the fixed-effect table as final.",
    "A singular fit means one or more random-effect variance or covariance parameters are estimated near zero; simplify the random-effects structure if this persists.",
    "Use ML, not REML, when comparing nested models that differ in fixed effects.",
    "Cluster-mean centering supports within-group interpretation for Level-1 predictors; grand-mean centering supports contextual or between-group interpretation.",
    sprintf("Current diagnostics: %s.", paste(sprintf("%s = %s", diag$check, diag$value), collapse = "; "))
  )
}
