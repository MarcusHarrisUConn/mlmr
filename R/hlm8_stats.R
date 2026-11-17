# HLM 8 statistical parity helpers:
# - Cluster-robust Huber-White sandwich standard errors (Raudenbush & Bryk, 2002, Eq. 9.15)
# - Variance-component likelihood-ratio tests with boundary-mixture p-values (Stram & Lee, 1994)
# - Bartlett H test of homogeneity of Level-1 variance across clusters (Raudenbush & Bryk, 2002, p. 263)
# - Unit-specific vs. population-average HGLM estimates for logit models (Zeger et al., 1988; R&B Ch. 10)

top_level_cluster_vector <- function(fit) {
  flist <- lme4::getME(fit, "flist")
  cnms_groups <- unique(names(lme4::getME(fit, "cnms")))
  groups <- intersect(cnms_groups, names(flist))
  if (!length(groups)) groups <- names(flist)
  # Highest nesting level has the fewest distinct clusters.
  n_units <- vapply(groups, function(g) nlevels(droplevels(flist[[g]])), integer(1))
  top_grp <- groups[[which.min(n_units)]]
  list(name = top_grp, factor = droplevels(flist[[top_grp]]))
}

mlm_robust_se <- function(result, type = c("CR1", "CR0")) {
  type <- match.arg(type)
  fit <- unwrap_fit(result)
  if (!inherits(fit, "merMod")) {
    stop("`result` must be a fitted \"mlm_result\" or `lme4` model object.", call. = FALSE)
  }
  X <- lme4::getME(fit, "X")
  beta <- lme4::fixef(fit)
  p <- ncol(X)
  cl_info <- top_level_cluster_vector(fit)
  grp <- cl_info$factor
  J <- nlevels(grp)
  if (J < 2L) {
    stop("At least two clusters are required to compute cluster-robust standard errors.", call. = FALSE)
  }
  idx_by_grp <- split(seq_along(grp), grp)
  B <- as.matrix(stats::vcov(fit))

  meat <- matrix(0, nrow = p, ncol = p)
  if (is_gaussian_fit(fit)) {
    y <- lme4::getME(fit, "y")
    e <- as.numeric(y - X %*% beta)
    A <- lme4::getME(fit, "Lambdat") %*% lme4::getME(fit, "Zt")
    sig2 <- stats::sigma(fit)^2
    for (idx in idx_by_grp) {
      Xj <- X[idx, , drop = FALSE]
      Aj <- as.matrix(A[, idx, drop = FALSE])
      Vj <- sig2 * (crossprod(Aj) + diag(length(idx)))
      Vinv_e <- tryCatch(solve(Vj, e[idx]), error = function(err) qr.solve(Vj, e[idx]))
      uj <- as.numeric(crossprod(Xj, Vinv_e))
      meat <- meat + tcrossprod(uj)
    }
  } else {
    # Working-score cluster sandwich on the linear predictor scale for GLMMs.
    w <- as.numeric(stats::weights(fit, type = "working"))
    r_work <- as.numeric(stats::resid(fit, type = "working"))
    for (idx in idx_by_grp) {
      Xj <- X[idx, , drop = FALSE]
      uj <- as.numeric(crossprod(Xj, w[idx] * r_work[idx]))
      meat <- meat + tcrossprod(uj)
    }
  }

  scale_factor <- if (identical(type, "CR1")) J / (J - 1) else 1
  V_rob <- scale_factor * (B %*% meat %*% B)
  se_model <- sqrt(pmax(0, diag(B)))
  se_rob <- sqrt(pmax(0, diag(V_rob)))
  stat_rob <- ifelse(se_rob > 0, beta / se_rob, NA_real_)

  if (is_gaussian_fit(fit)) {
    df_rob <- rep(max(1, J - 1), p)
    p_rob <- 2 * stats::pt(abs(stat_rob), df = df_rob, lower.tail = FALSE)
    crit <- stats::qt(.975, df = df_rob)
  } else {
    df_rob <- rep(NA_real_, p)
    p_rob <- 2 * stats::pnorm(abs(stat_rob), lower.tail = FALSE)
    crit <- rep(stats::qnorm(.975), p)
  }

  ci_low <- beta - crit * se_rob
  ci_high <- beta + crit * se_rob

  out <- data.frame(
    Predictor = colnames(X),
    Estimate = round(unname(beta), 4),
    SE_Model = round(unname(se_model), 4),
    SE_Robust = round(unname(se_rob), 4),
    df = if (is_gaussian_fit(fit)) df_rob else rep(NA_real_, p),
    Statistic = round(unname(stat_rob), 3),
    p = format_apa_p(p_rob),
    `95% CI` = sprintf("[%.2f, %.2f]", ci_low, ci_high),
    Cluster = cl_info$name,
    Clusters = J,
    check.names = FALSE,
    row.names = NULL,
    stringsAsFactors = FALSE
  )
  label <- exponentiated_label(fit)
  if (!is.null(label)) {
    out[[label]] <- sprintf("%.2f", exp(beta))
    out[[sprintf("%s 95%% CI", label)]] <- sprintf("[%.2f, %.2f]", exp(ci_low), exp(ci_high))
  }
  out
}

apa_robust_html <- function(result, table_number = "1b", title = "Fixed Effects With HLM Cluster-Robust Standard Errors") {
  rob <- tryCatch(mlm_robust_se(result, type = "CR1"), error = function(e) NULL)
  if (is.null(rob) || !nrow(rob)) {
    return(apa_html_table(
      data.frame(Predictor = character(), check.names = FALSE),
      table_number = table_number,
      title = title,
      note = "Cluster-robust standard errors could not be computed for this specification."
    ))
  }
  cl_name <- rob$Cluster[[1]]
  n_cl <- rob$Clusters[[1]]
  has_df <- !all(is.na(rob$df))
  disp <- data.frame(
    Predictor = rob$Predictor,
    b = sprintf("%.2f", rob$Estimate),
    `SE (Model)` = sprintf("%.2f", rob$SE_Model),
    `SE (Robust)` = sprintf("%.2f", rob$SE_Robust),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  if (has_df) disp$df <- sprintf("%.0f", rob$df)
  disp$t <- sprintf("%.2f", rob$Statistic)
  disp$p <- rob$p
  disp$`95% CI (Robust)` <- rob$`95% CI`
  extra_cols <- setdiff(names(rob), c("Predictor", "Estimate", "SE_Model", "SE_Robust", "df", "Statistic", "p", "95% CI", "Cluster", "Clusters"))
  for (col in extra_cols) disp[[col]] <- rob[[col]]
  apa_html_table(
    disp,
    table_number = table_number,
    title = title,
    note = html_escape(sprintf(
      "Huber-White CR1 sandwich standard errors clustered by %s (J = %s; Raudenbush & Bryk, 2002). Large divergences between model-based and robust SEs can indicate covariance or distributional misspecification.",
      cl_name, n_cl
    )),
    italic_columns = c("b", "df", "t", "p")
  )
}

# Single-term random-effect likelihood-ratio deletion tests matching HLM 8's
# variance-component hypothesis testing table.
mlm_variance_tests <- function(result) {
  if (!inherits(result, "mlm_result")) {
    stop("`result` must be an \"mlm_result\" object returned by mlm_fit().", call. = FALSE)
  }
  fit <- result$fit
  spec <- result$spec
  vc <- as.data.frame(lme4::VarCorr(fit))
  var_rows <- vc[vc$grp != "Residual" & is.na(vc$var2) & !is.na(vc$var1), , drop = FALSE]
  if (!nrow(var_rows)) {
    return(data.frame(
      Group = character(),
      Term = character(),
      SD = numeric(),
      Variance = numeric(),
      Chisq = numeric(),
      df = integer(),
      p = character(),
      stringsAsFactors = FALSE
    ))
  }

  ll_full <- as.numeric(stats::logLik(fit))
  df_full <- attr(stats::logLik(fit), "df")
  settings <- result_settings(result)

  rows <- lapply(seq_len(nrow(var_rows)), function(i) {
    grp <- var_rows$grp[[i]]
    # lme4 suffixes duplicate group blocks as schoolid.1 when uncorrelated; strip suffix to find spec block.
    base_grp <- sub("\\.[0-9]+$", "", grp)
    term <- var_rows$var1[[i]]
    sd_est <- round(var_rows$sdcor[[i]], 4)
    var_est <- round(var_rows$vcov[[i]], 4)

    reduced_spec <- spec
    if (base_grp %in% names(reduced_spec$random)) {
      if (identical(term, "(Intercept)")) {
        reduced_spec$random[[base_grp]]$intercept <- FALSE
      } else {
        # Match centered or raw slope name back to spec$fixed name
        orig_slopes <- reduced_spec$random[[base_grp]]$slopes %||% character()
        eff_map <- stats::setNames(orig_slopes, unname(effective_fixed_terms(spec$fixed[orig_slopes])))
        target_orig <- if (term %in% names(eff_map)) eff_map[[term]] else term
        reduced_spec$random[[base_grp]]$slopes <- setdiff(orig_slopes, target_orig)
      }
    }

    rem_terms <- random_formula_terms(reduced_spec)
    chisq_val <- NA_real_
    df_diff <- NA_integer_
    p_mix <- NA_real_

    if (length(rem_terms) > 0L) {
      red_fit <- tryCatch(
        {
          f_red <- build_formula(reduced_spec)
          if (is_gaussian_fit(fit)) {
            ctrl <- lme4::lmerControl(optimizer = settings$optimizer, optCtrl = list(maxfun = settings$maxfun))
            suppressWarnings(suppressMessages(lme4::lmer(f_red, data = result$data, REML = settings$REML, control = ctrl)))
          } else if (identical(spec$distribution, "negative binomial")) {
            ctrl <- lme4::glmerControl(optimizer = settings$optimizer, optCtrl = list(maxfun = settings$maxfun))
            suppressWarnings(suppressMessages(lme4::glmer.nb(f_red, data = result$data, control = ctrl)))
          } else {
            ctrl <- lme4::glmerControl(optimizer = settings$optimizer, optCtrl = list(maxfun = settings$maxfun))
            fam <- switch(
              spec$distribution,
              binomial = stats::binomial(link = spec$link),
              poisson = stats::poisson(link = spec$link),
              Gamma = stats::Gamma(link = spec$link)
            )
            suppressWarnings(suppressMessages(lme4::glmer(f_red, data = result$data, family = fam, control = ctrl)))
          }
        },
        error = function(e) NULL
      )
      if (!is.null(red_fit)) {
        ll_red <- as.numeric(stats::logLik(red_fit))
        df_red <- attr(stats::logLik(red_fit), "df")
        chisq_val <- max(0, 2 * (ll_full - ll_red))
        df_diff <- as.integer(max(1L, df_full - df_red))
        # Boundary mixture p-value (Stram & Lee, 1994): 0.5*Chisq(df-1) + 0.5*Chisq(df)
        p_upper <- stats::pchisq(chisq_val, df = df_diff, lower.tail = FALSE)
        p_lower <- if (df_diff == 1L) {
          if (chisq_val <= 0) 1 else 0
        } else {
          stats::pchisq(chisq_val, df = df_diff - 1L, lower.tail = FALSE)
        }
        p_mix <- 0.5 * p_lower + 0.5 * p_upper
      }
    } else if (is_gaussian_fit(fit)) {
      # Compare single random-effect block against OLS lm() via ML log-likelihood
      fit_ml <- if (isTRUE(lme4::isREML(fit))) {
        suppressWarnings(suppressMessages(stats::update(fit, REML = FALSE, data = result$data)))
      } else {
        fit
      }
      rhs <- unique(c(
        formula_name(unname(effective_fixed_terms(spec$fixed))),
        vapply(spec$interactions, function(x) paste(formula_name(vapply(x, effective_term, character(1), fixed = spec$fixed)), collapse = ":"), character(1))
      ))
      if (!length(rhs)) rhs <- "1"
      lm_form <- stats::as.formula(paste(formula_name(spec$outcome), "~", paste(rhs, collapse = " + ")))
      rows <- analysis_rows(result$data, model_variables(spec))
      lm_fit <- tryCatch(stats::lm(lm_form, data = result$data[rows, , drop = FALSE]), error = function(e) NULL)
      if (!is.null(lm_fit)) {
        chisq_val <- max(0, 2 * (as.numeric(stats::logLik(fit_ml)) - as.numeric(stats::logLik(lm_fit))))
        df_diff <- as.integer(max(1L, attr(stats::logLik(fit_ml), "df") - attr(stats::logLik(lm_fit), "df")))
        p_upper <- stats::pchisq(chisq_val, df = df_diff, lower.tail = FALSE)
        p_lower <- if (df_diff == 1L) (if (chisq_val <= 0) 1 else 0) else stats::pchisq(chisq_val, df = df_diff - 1L, lower.tail = FALSE)
        p_mix <- 0.5 * p_lower + 0.5 * p_upper
      }
    }

    data.frame(
      Group = base_grp,
      Term = term,
      SD = sd_est,
      Variance = var_est,
      Chisq = if (is.na(chisq_val)) NA_real_ else round(chisq_val, 2),
      df = df_diff,
      p = if (is.na(p_mix)) "" else format_apa_p(p_mix),
      stringsAsFactors = FALSE
    )
  })

  out <- do.call(rbind, rows)
  row.names(out) <- NULL
  out
}

# Bartlett-style H statistic testing homogeneity of Level-1 residual variance
# across clusters (Raudenbush & Bryk, 2002, pp. 263-265).
mlm_homogeneity_test <- function(result) {
  fit <- unwrap_fit(result)
  if (!inherits(fit, "merMod") || !is_gaussian_fit(fit)) {
    return(data.frame(
      Cluster = character(),
      Eligible_Clusters = integer(),
      H_Statistic = numeric(),
      df = integer(),
      p = character(),
      Interpretation = character(),
      stringsAsFactors = FALSE
    ))
  }
  flist <- lme4::getME(fit, "flist")
  cnms_groups <- unique(names(lme4::getME(fit, "cnms")))
  groups <- intersect(cnms_groups, names(flist))
  # Lowest cluster level has the most units
  n_units <- vapply(groups, function(g) nlevels(droplevels(flist[[g]])), integer(1))
  l2_name <- groups[[which.max(n_units)]]
  grp <- droplevels(flist[[l2_name]])
  X <- lme4::getME(fit, "X")
  y <- lme4::getME(fit, "y")

  idx_list <- split(seq_along(grp), grp)
  s2_vec <- numeric()
  df_vec <- integer()
  for (idx in idx_list) {
    Xj <- X[idx, , drop = FALSE]
    # Keep columns that vary within this cluster plus intercept
    keep_cols <- vapply(seq_len(ncol(Xj)), function(k) {
      col <- Xj[, k]
      all(col == 1) || length(unique(col)) > 1L
    }, logical(1))
    Xj_sub <- Xj[, keep_cols, drop = FALSE]
    qr_x <- qr(Xj_sub)
    df_j <- length(idx) - qr_x$rank
    if (df_j >= 2L) {
      res_j <- qr.resid(qr_x, y[idx])
      s2_j <- sum(res_j^2) / df_j
      if (is.finite(s2_j) && s2_j > 1e-12) {
        s2_vec <- c(s2_vec, s2_j)
        df_vec <- c(df_vec, df_j)
      }
    }
  }
  J_elig <- length(s2_vec)
  if (J_elig < 2L) {
    return(data.frame(
      Cluster = l2_name,
      Eligible_Clusters = J_elig,
      H_Statistic = NA_real_,
      df = NA_integer_,
      p = "",
      Interpretation = "Too few clusters with sufficient within-cluster degrees of freedom (df >= 2) to test Level-1 variance homogeneity.",
      stringsAsFactors = FALSE
    ))
  }
  log_s2 <- log(s2_vec)
  weighted_mean_log_s2 <- sum(df_vec * log_s2) / sum(df_vec)
  d_j <- (log_s2 - weighted_mean_log_s2) / sqrt(2 / df_vec)
  H_stat <- sum(d_j^2)
  df_H <- J_elig - 1L
  p_H <- stats::pchisq(H_stat, df = df_H, lower.tail = FALSE)
  interp <- if (p_H < 0.05) {
    sprintf("Statistically detectable heterogeneity in Level-1 residual variance across %s units (H = %.2f, df = %s, %s); inspect residual plots by cluster.", l2_name, H_stat, df_H, format_apa_p_prose(format_apa_p(p_H)))
  } else {
    sprintf("No statistically detectable departure from homogeneous Level-1 variance across %s units (H = %.2f, df = %s, %s).", l2_name, H_stat, df_H, format_apa_p_prose(format_apa_p(p_H)))
  }
  data.frame(
    Cluster = l2_name,
    Eligible_Clusters = J_elig,
    H_Statistic = round(H_stat, 2),
    df = df_H,
    p = format_apa_p(p_H),
    Interpretation = interp,
    stringsAsFactors = FALSE
  )
}

# Unit-specific vs. approximate population-average coefficients and odds ratios
# for logistic mixed models (Zeger et al., 1988; Raudenbush & Bryk, 2002, Ch. 10).
mlm_population_average <- function(result) {
  fit <- unwrap_fit(result)
  if (!inherits(fit, "glmerMod") || !identical(stats::family(fit)$family, "binomial") || !identical(stats::family(fit)$link, "logit")) {
    return(data.frame(
      Predictor = character(),
      b_UnitSpecific = numeric(),
      OR_UnitSpecific = character(),
      b_PopAverage = numeric(),
      OR_PopAverage = character(),
      stringsAsFactors = FALSE
    ))
  }
  vc <- as.data.frame(lme4::VarCorr(fit))
  int_rows <- vc[vc$grp != "Residual" & is.na(vc$var2) & vc$var1 == "(Intercept)", , drop = FALSE]
  tau00_sum <- sum(int_rows$vcov, na.rm = TRUE)
  c_logit <- (16 * sqrt(3)) / (15 * pi)
  shrink <- 1 / sqrt(1 + (c_logit^2) * tau00_sum)
  tab <- fixed_effects_table(result)
  b_us <- tab$Estimate
  b_pa <- b_us * shrink
  ci_pa_low <- tab$CI_low * shrink
  ci_pa_high <- tab$CI_high * shrink
  data.frame(
    Predictor = tab$term,
    b_UnitSpecific = round(b_us, 3),
    OR_UnitSpecific = sprintf("%.2f [%.2f, %.2f]", exp(b_us), exp(tab$CI_low), exp(tab$CI_high)),
    b_PopAverage = round(b_pa, 3),
    OR_PopAverage = sprintf("%.2f [%.2f, %.2f]", exp(b_pa), exp(ci_pa_low), exp(ci_pa_high)),
    stringsAsFactors = FALSE
  )
}
