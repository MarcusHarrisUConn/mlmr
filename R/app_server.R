validation_report_text <- function(report) {
  table_text <- function(title, tab) {
    if (!is.data.frame(tab) || !nrow(tab)) return(c(paste("##", title), "", "_No rows available._", ""))
    c(paste("##", title), "", md_pipe_table(tab), "")
  }
  c(
    "# mlmr Validation Report",
    "",
    sprintf("Generated: %s", report$generated_at),
    sprintf("Seed: %s", report$seed),
    sprintf("Tolerance: %s", report$tolerance),
    "",
    table_text("Benchmark Summary", report$benchmarks),
    table_text("Fixed Effects", report$fixed_effects),
    table_text("Variance Components", report$variance_components),
    table_text("Intraclass Correlations", report$icc),
    table_text("Model R-Squared", report$r2),
    table_text("Diagnostics", report$diagnostics)
  )
}

render_equations_preview_ui <- function(eq, compact = FALSE) {
  cards <- lapply(seq_along(eq$level), function(i) {
    sub_note <- if (!compact && length(eq$level_notes) >= i) {
      shiny::p(class = "equation-subnote", eq$level_notes[[i]])
    } else {
      NULL
    }
    shiny::div(
      class = "equation-card",
      shiny::tags$strong(eq$level[[i]]),
      shiny::HTML(equation_html(eq$equations[[i]])),
      sub_note
    )
  })
  if (!compact && length(eq$level) > 1L) {
    cards[[length(cards) + 1L]] <- shiny::div(
      class = "equation-card",
      shiny::tags$strong("Combined Mixed-Model Form"),
      shiny::HTML(equation_html(eq$combined))
    )
  }
  shiny::div(class = "equation-stack", cards)
}

app_server <- function(input, output, session) {
  fit_state <- shiny::reactiveVal(list(result = NULL, error = NULL, optimizer = NULL))
  model_history <- shiny::reactiveVal(list())
  benchmark_validation_run <- shiny::reactiveVal(FALSE)
  per_group_random <- shiny::reactiveVal(list(
    schoolid = list(slopes = "ses", correlation = TRUE)
  ))
  slope_moderators <- shiny::reactiveVal(list(
    ses = "sector",
    homework = "climate"
  ))
  extra_interactions <- shiny::reactiveVal(list())
  dataset_signature <- shiny::reactiveVal("")

  shiny::observeEvent(input$go_customize_model, {
    shiny::updateNavbarPage(session, "main_nav", selected = "Model")
  })

  # Bidirectional Outcome (DV) and Distribution/Link sync between Data tab and Model tab
  shiny::observeEvent(input$outcome_data, {
    if (!is.null(input$outcome_data) && !identical(input$outcome_data, input$outcome)) {
      shiny::updateSelectInput(session, "outcome", selected = input$outcome_data)
    }
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$outcome, {
    if (!is.null(input$outcome) && !identical(input$outcome, input$outcome_data)) {
      shiny::updateSelectInput(session, "outcome_data", selected = input$outcome)
    }
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$distribution_data, {
    if (!is.null(input$distribution_data) && !identical(input$distribution_data, input$distribution)) {
      shiny::updateSelectInput(session, "distribution", selected = input$distribution_data)
    }
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$distribution, {
    if (!is.null(input$distribution) && !identical(input$distribution, input$distribution_data)) {
      shiny::updateSelectInput(session, "distribution_data", selected = input$distribution)
    }
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$link_data, {
    if (!is.null(input$link_data) && !identical(input$link_data, input$link)) {
      shiny::updateSelectInput(session, "link", selected = input$link_data)
    }
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$link, {
    if (!is.null(input$link) && !identical(input$link, input$link_data)) {
      shiny::updateSelectInput(session, "link_data", selected = input$link)
    }
  }, ignoreInit = TRUE)

  # File upload resets stale HSB defaults cleanly
  shiny::observeEvent(input$data_file, {
    shiny::updateCheckboxInput(session, "use_example_data", value = FALSE)
    shiny::updateTextInput(session, "interactions", value = "")
    slope_moderators(list())
    extra_interactions(list())
    model_history(list())
    fit_state(list(result = NULL, error = NULL, optimizer = NULL))
  }, ignoreInit = TRUE)

  # Sync design_mode <-> built-in longitudinal dataset smoothly
  shiny::observeEvent(input$example_dataset, {
    if (!isTRUE(input$use_example_data)) return()
    ds <- input$example_dataset %||% "hsb"
    model_history(list())
    fit_state(list(result = NULL, error = NULL, optimizer = NULL))
    if (identical(ds, "longitudinal")) {
      per_group_random(list(
        studentid = list(slopes = "wave", correlation = TRUE),
        schoolid = list(slopes = character(), correlation = TRUE)
      ))
      slope_moderators(list(wave = "treatment"))
      extra_interactions(list())
      shiny::updateSelectInput(session, "design_mode", selected = "longitudinal")
      shiny::updateTextInput(session, "interactions", value = "wave:treatment")
      shiny::updateSelectInput(session, "model_preset", selected = "longitudinal")
    } else {
      per_group_random(list(
        schoolid = list(slopes = "ses", correlation = TRUE),
        districtid = list(slopes = character(), correlation = TRUE)
      ))
      slope_moderators(list(ses = "sector", homework = "climate"))
      extra_interactions(list())
      if (identical(input$design_mode, "longitudinal")) {
        shiny::updateSelectInput(session, "design_mode", selected = "two_level")
      }
      shiny::updateTextInput(session, "interactions", value = "ses:sector, homework:climate")
      shiny::updateSelectInput(session, "model_preset", selected = "custom")
    }
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$design_mode, {
    dm <- input$design_mode %||% "two_level"
    if (identical(dm, "longitudinal") && isTRUE(input$use_example_data) && !identical(input$example_dataset, "longitudinal")) {
      shiny::updateSelectInput(session, "example_dataset", selected = "longitudinal")
    } else if (dm %in% c("two_level", "three_level", "crossed") && isTRUE(input$use_example_data) && identical(input$example_dataset, "longitudinal")) {
      shiny::updateSelectInput(session, "example_dataset", selected = "hsb")
    }
  }, ignoreInit = TRUE)

  data_reactive <- shiny::reactive({
    sf <- if (is.null(input$strings_as_factors)) TRUE else isTRUE(input$strings_as_factors)
    use_ex <- if (is.null(input$use_example_data)) TRUE else isTRUE(input$use_example_data)
    if (!use_ex && !is.null(input$data_file)) {
      dat <- read_uploaded_datafile(
        path = input$data_file$datapath,
        filename = input$data_file$name,
        strings_as_factors = sf
      )
    } else if (identical(input$example_dataset %||% "hsb", "longitudinal")) {
      dat <- clean_imported_dataframe(example_longitudinal(), strings_as_factors = sf)
    } else {
      dat <- clean_imported_dataframe(example_hsb(), strings_as_factors = sf)
    }
    sig <- paste(c(nrow(dat), names(dat)), collapse = "|")
    if (!identical(shiny::isolate(dataset_signature()), sig)) {
      dataset_signature(sig)
      cur_rand <- shiny::isolate(per_group_random())
      per_group_random(cur_rand[intersect(names(cur_rand), names(dat))])
      cur_mods <- shiny::isolate(slope_moderators())
      valid_mods <- lapply(cur_mods[intersect(names(cur_mods), names(dat))], function(m) intersect(m, names(dat)))
      slope_moderators(Filter(length, valid_mods))
      extra_interactions(Filter(function(p) all(p %in% names(dat)), shiny::isolate(extra_interactions())))
    }
    dat
  })

  output$data_source_note <- shiny::renderUI({
    use_ex <- if (is.null(input$use_example_data)) TRUE else isTRUE(input$use_example_data)
    if (use_ex) {
      ds_label <- if (identical(input$example_dataset %||% "hsb", "longitudinal")) {
        "built-in longitudinal growth dataset (400 observations across 80 students)"
      } else {
        "built-in HSB educational dataset (students nested in 24 schools and 6 districts)"
      }
      return(shiny::div(class = "mode-note", sprintf("Loaded %s. Upload a file above to analyze your own data.", ds_label)))
    }
    if (is.null(input$data_file)) {
      return(shiny::div(class = "mode-note", "Upload a CSV, TSV/TXT, Excel, SPSS, SAS, or Stata file to use your own data."))
    }
    shiny::div(class = "mode-note", sprintf("Active uploaded file: %s (%s rows, %s columns)", input$data_file$name, nrow(data_reactive()), ncol(data_reactive())))
  })

  preferred_outcome_for <- function(dat) {
    if ("mathscore" %in% names(dat)) return("mathscore")
    if ("reading" %in% names(dat)) return("reading")
    first_or(numeric_vars(dat), first_or(names(dat)))
  }

  active_outcome <- shiny::reactive({
    dat <- data_reactive()
    if (!is.null(input$outcome) && input$outcome %in% names(dat)) return(input$outcome)
    if (!is.null(input$outcome_data) && input$outcome_data %in% names(dat)) return(input$outcome_data)
    preferred_outcome_for(dat)
  })

  output$data_outcome_selector <- shiny::renderUI({
    dat <- data_reactive()
    sel <- active_outcome()
    shiny::selectInput("outcome_data", "Dependent Variable (Outcome Y)", choices = names(dat), selected = sel)
  })

  output$outcome_selector <- shiny::renderUI({
    dat <- data_reactive()
    sel <- active_outcome()
    shiny::selectInput("outcome", "Dependent Variable (Outcome Y)", choices = names(dat), selected = sel)
  })

  # Dynamic Cluster Selectors based on Design Mode
  output$cluster_selectors <- shiny::renderUI({
    dat <- data_reactive()
    choices <- candidate_group_vars(dat)
    choices <- setdiff(choices, c(active_outcome(), "studentid"[!"wave" %in% names(dat)], "obsid"))
    if (!length(choices)) choices <- setdiff(names(dat), active_outcome())
    dm <- input$design_mode %||% "two_level"

    grp_label_choices <- stats::setNames(choices, vapply(choices, function(g) {
      sprintf("%s (%s units)", g, length(unique(stats::na.omit(dat[[g]]))))
    }, character(1)))

    default_l2 <- if ("schoolid" %in% choices && !"wave" %in% names(dat)) {
      "schoolid"
    } else if ("studentid" %in% choices && "wave" %in% names(dat)) {
      "studentid"
    } else {
      first_or(choices)
    }
    cur_l2 <- shiny::isolate(input$cluster_l2)
    sel_l2 <- if (!is.null(cur_l2) && cur_l2 %in% choices) cur_l2 else default_l2

    if (identical(dm, "two_level")) {
      return(shiny::selectInput("cluster_l2", "Level-2 Cluster ID (e.g., school, clinic, team)", choices = grp_label_choices, selected = sel_l2))
    }
    if (identical(dm, "longitudinal")) {
      return(shiny::selectInput("cluster_l2", "Person / Subject ID (Level-2 repeated-measures unit)", choices = grp_label_choices, selected = if ("studentid" %in% choices) "studentid" else sel_l2))
    }
    rem_choices <- setdiff(choices, sel_l2)
    if (!length(rem_choices)) rem_choices <- choices
    rem_label_choices <- grp_label_choices[grp_label_choices %in% rem_choices]
    default_l3 <- if (identical(dm, "crossed") && "teacherid" %in% rem_choices) {
      "teacherid"
    } else if ("districtid" %in% rem_choices) {
      "districtid"
    } else if ("schoolid" %in% rem_choices) {
      "schoolid"
    } else {
      first_or(rem_choices)
    }
    cur_l3 <- shiny::isolate(input$cluster_l3)
    sel_l3 <- if (!is.null(cur_l3) && cur_l3 %in% rem_choices) cur_l3 else default_l3

    if (identical(dm, "three_level")) {
      return(shiny::tagList(
        shiny::selectInput("cluster_l2", "Level-2 Cluster ID (inner group, e.g., school)", choices = grp_label_choices, selected = sel_l2),
        shiny::selectInput("cluster_l3", "Level-3 Cluster ID (outer group, e.g., district)", choices = rem_label_choices, selected = sel_l3)
      ))
    }
    shiny::tagList(
      shiny::selectInput("cluster_l2", "Classification Factor A (e.g., school)", choices = grp_label_choices, selected = sel_l2),
      shiny::selectInput("cluster_l3", "Classification Factor B (e.g., teacher)", choices = rem_label_choices, selected = sel_l3)
    )
  })

  active_structure_type <- shiny::reactive({
    # Support programmatic `structure_type` override in testServer if supplied
    dm <- input$design_mode %||% "two_level"
    if (identical(dm, "crossed") || identical(input$structure_type, "crossed")) return("crossed")
    if (identical(input$structure_type, "multiple membership")) return("multiple membership")
    "nested"
  })

  active_groups <- shiny::reactive({
    dat <- data_reactive()
    # Support programmatic `grouping_vars` in testServer when explicitly set
    if (!is.null(input$grouping_vars)) {
      valid_prog <- intersect(input$grouping_vars, names(dat))
      if (length(valid_prog)) return(unique(valid_prog))
    }
    dm <- input$design_mode %||% "two_level"
    l2 <- if (!is.null(input$cluster_l2) && input$cluster_l2 %in% names(dat)) {
      input$cluster_l2
    } else if ("schoolid" %in% names(dat) && !"wave" %in% names(dat)) {
      "schoolid"
    } else if ("studentid" %in% names(dat) && "wave" %in% names(dat)) {
      "studentid"
    } else {
      first_or(setdiff(candidate_group_vars(dat), active_outcome()), character())
    }
    if (dm %in% c("three_level", "crossed")) {
      l3 <- if (!is.null(input$cluster_l3) && input$cluster_l3 %in% names(dat)) {
        input$cluster_l3
      } else if (identical(dm, "crossed") && "teacherid" %in% names(dat)) {
        "teacherid"
      } else if ("districtid" %in% names(dat)) {
        "districtid"
      } else {
        first_or(setdiff(candidate_group_vars(dat), c(active_outcome(), l2)), character())
      }
      return(unique(c(l2, l3)[nzchar(c(l2, l3))]))
    }
    unique(l2[nzchar(l2)])
  })

  active_nesting_note <- shiny::reactive({
    grps <- active_groups()
    dm <- input$design_mode %||% "two_level"
    if (!length(grps)) return("No cluster ID selected")
    if (identical(dm, "crossed") || identical(active_structure_type(), "crossed")) {
      return(sprintf("observations cross-classified by %s", paste(grps, collapse = " and ")))
    }
    if (identical(dm, "longitudinal")) {
      return(sprintf("repeated waves nested within %s", paste(grps, collapse = " within ")))
    }
    sprintf("observations nested within %s", paste(grps, collapse = " within "))
  })

  output$hierarchy_status_badge <- shiny::renderUI({
    dat <- data_reactive()
    grps <- active_groups()
    if (!length(grps)) return(shiny::div(class = "hierarchy-badge warn", "Select at least one cluster ID."))
    if (length(grps) == 1L) {
      g <- grps[[1]]
      counts <- table(dat[[g]])
      cls <- if (length(counts) >= 10) "hierarchy-badge ok" else "hierarchy-badge warn"
      return(shiny::div(
        class = cls,
        sprintf("Level 1: %s rows -> Level 2 (%s): %s clusters (%.0f-%.0f rows/cluster).", nrow(dat), g, length(counts), min(counts), max(counts))
      ))
    }
    nc <- nesting_check(dat, grps, active_structure_type())
    if (is.null(nc)) {
      return(shiny::div(
        class = "hierarchy-badge ok",
        sprintf("Cross-classified factors: %s (%s units) x %s (%s units).", grps[[1]], length(unique(dat[[grps[[1]]]])), grps[[2]], length(unique(dat[[grps[[2]]]])))
      ))
    }
    cls <- if (identical(nc$status, "OK")) "hierarchy-badge ok" else "hierarchy-badge warn"
    shiny::div(class = cls, nc$detail)
  })

  # Empirical variable classification helper for annotated choices
  annotated_predictor_choices <- shiny::reactive({
    dat <- data_reactive()
    out_var <- active_outcome()
    grps <- active_groups()
    # Exclude row-level pure string IDs (studentid/obsid) only from default recommendations, keep selectable if desired
    pool <- setdiff(names(dat), c(out_var, grps))
    l2_grp <- first_or(grps)
    labels <- vapply(pool, function(v) {
      col <- dat[[v]]
      kind <- if (is.numeric(col)) "num" else "factor"
      if (is.null(l2_grp) || !l2_grp %in% names(dat)) return(sprintf("%s (%s)", v, kind))
      varies <- !constant_within(col, dat[[l2_grp]])
      tag <- if (varies) sprintf("varies in %s", l2_grp) else sprintf("constant in %s", l2_grp)
      sprintf("%s [%s, %s]", v, kind, tag)
    }, character(1))
    stats::setNames(pool, labels)
  })

  output$level1_selector_ui <- shiny::renderUI({
    dat <- data_reactive()
    choices <- annotated_predictor_choices()
    pool <- unname(choices)
    is_hsb <- all(c("mathscore", "ses", "schoolid") %in% names(dat))
    is_long <- all(c("reading", "wave", "studentid") %in% names(dat))
    default_l1 <- if (is_hsb) {
      intersect(c("ses", "priorachieve", "homework", "female", "minority"), pool)
    } else if (is_long) {
      intersect(c("wave", "studyhours"), pool)
    } else {
      character()
    }
    cur <- shiny::isolate(input$level1_terms)
    sel <- if (!is.null(cur)) intersect(cur, pool) else default_l1
    shiny::selectizeInput(
      "level1_terms",
      "Select Level-1 predictors",
      choices = choices,
      selected = sel,
      multiple = TRUE,
      options = list(plugins = list("remove_button"), placeholder = "Click to add within-cluster predictors...")
    )
  })

  output$level2_selector_ui <- shiny::renderUI({
    dat <- data_reactive()
    choices <- annotated_predictor_choices()
    pool <- unname(choices)
    is_hsb <- all(c("mathscore", "ses", "schoolid") %in% names(dat))
    is_long <- all(c("reading", "wave", "studentid") %in% names(dat))
    default_l2 <- if (is_hsb) {
      intersect(c("meanses", "climate", "schoolsize", "sector"), pool)
    } else if (is_long) {
      intersect(c("treatment", "baseses"), pool)
    } else {
      character()
    }
    cur <- shiny::isolate(input$level2_terms)
    sel <- if (!is.null(cur)) intersect(cur, pool) else default_l2
    shiny::selectizeInput(
      "level2_terms",
      "Select Level-2 predictors (for Intercept)",
      choices = choices,
      selected = sel,
      multiple = TRUE,
      options = list(plugins = list("remove_button"), placeholder = "Click to add cluster-level predictors...")
    )
  })

  output$level3_builder_step_ui <- shiny::renderUI({
    dm <- input$design_mode %||% "two_level"
    has_3_groups <- length(active_groups()) >= 2L && identical(active_structure_type(), "nested")
    if (!identical(dm, "three_level") && !has_3_groups) return(NULL)
    dat <- data_reactive()
    choices <- annotated_predictor_choices()
    pool <- unname(choices)
    cur <- shiny::isolate(input$level3_terms)
    sel <- if (!is.null(cur)) intersect(cur, pool) else intersect("districtses", pool)
    l3_ctr <- lapply(sel, function(term) {
      if (!term %in% numeric_vars(dat)) return(NULL)
      cur_c <- shiny::isolate(input[[paste0("center_", term)]]) %||% default_center_for(term)
      shiny::selectInput(paste0("center_", term), sprintf("Center %s", term), c("Grand-mean (GMC)" = "GMC", "Uncentered" = "none"), selected = cur_c)
    })
    shiny::div(
      class = "builder-step",
      step_header_ui("3b", "Level-3 Predictors (Outer Cluster IVs)"),
      shiny::selectizeInput(
        "level3_terms",
        "Select Level-3 predictors",
        choices = choices,
        selected = sel,
        multiple = TRUE,
        options = list(plugins = list("remove_button"), placeholder = "Click to add Level-3 predictors...")
      ),
      l3_ctr
    )
  })

  default_center_for <- function(term) {
    if (term %in% c("ses", "priorachieve", "homework", "studyhours")) return("CWC")
    if (term %in% c("meanses", "climate", "schoolsize", "districtses", "baseses")) return("GMC")
    "none"
  }

  # Inline Level-1 rows: centering + Random Slope (u_qj) checkbox per variable
  output$l1_inline_controls <- shiny::renderUI({
    dat <- data_reactive()
    l1 <- intersect(input$level1_terms %||% character(), names(dat))
    if (!length(l1)) return(shiny::p(class = "equation-subnote", "No Level-1 predictors selected (unconditional Level-1 intercept only)."))
    l2_grp <- first_or(active_groups(), "cluster")
    saved_slopes <- shiny::isolate(per_group_random())[[l2_grp]]$slopes %||% character()
    shiny::tagList(lapply(l1, function(term) {
      is_num <- term %in% numeric_vars(dat)
      cur_ctr <- shiny::isolate(input[[paste0("center_", term)]]) %||% default_center_for(term)
      has_slope <- term %in% saved_slopes
      shiny::div(
        class = "mlm-var-row",
        shiny::div(
          class = "mlm-var-header",
          shiny::span(term),
          shiny::span(class = "tau-sub-label", if (is_num) "Numeric" else "Factor")
        ),
        if (is_num) {
          shiny::tagList(
            shiny::selectInput(
              paste0("center_", term),
              label = NULL,
              choices = c(
                "Group-mean centered (CWC)" = "CWC",
                "Grand-mean centered (GMC)" = "GMC",
                "Uncentered (raw metric)" = "none"
              ),
              selected = cur_ctr
            ),
            shiny::checkboxInput(
              paste0("slope_l1_", term),
              label = sprintf("Random slope across %s (+ u)", l2_grp),
              value = has_slope
            )
          )
        } else {
          shiny::div(
            class = "tau-sub-label",
            sprintf("Dummy-coded factor (reference: %s)", first_or(levels(factor(dat[[term]])), ""))
          )
        }
      )
    }))
  })

  # Sync inline Level-1 slope checkboxes -> per_group_random for the primary Level-2 cluster
  shiny::observe({
    dat <- data_reactive()
    l1 <- intersect(input$level1_terms %||% character(), numeric_vars(dat))
    l2_grp <- first_or(active_groups())
    if (is.null(l2_grp) || !nzchar(l2_grp) || !length(l1)) return()
    vals <- lapply(l1, function(term) input[[paste0("slope_l1_", term)]])
    if (any(vapply(vals, is.null, logical(1)))) return()
    checked_slopes <- l1[vapply(vals, isTRUE, logical(1))]
    cur <- shiny::isolate(per_group_random())
    prev_corr <- if (!is.null(cur[[l2_grp]])) isTRUE(cur[[l2_grp]]$correlation %||% TRUE) else TRUE
    if (!identical(cur[[l2_grp]]$slopes, checked_slopes)) {
      cur[[l2_grp]] <- list(slopes = checked_slopes, correlation = prev_corr)
      per_group_random(cur)
      if (identical(shiny::isolate(input$random_group_focus) %||% l2_grp, l2_grp)) {
        shiny::updateSelectInput(session, "random_slopes", selected = checked_slopes)
      }
    }
  })

  output$l2_inline_controls <- shiny::renderUI({
    dat <- data_reactive()
    l2 <- intersect(input$level2_terms %||% character(), names(dat))
    if (!length(l2)) return(NULL)
    shiny::tagList(lapply(l2, function(term) {
      is_num <- term %in% numeric_vars(dat)
      cur_ctr <- shiny::isolate(input[[paste0("center_", term)]]) %||% default_center_for(term)
      shiny::div(
        class = "mlm-var-row",
        shiny::div(
          class = "mlm-var-header",
          shiny::span(term),
          shiny::span(class = "tau-sub-label", if (is_num) "Numeric" else "Factor")
        ),
        if (is_num) {
          shiny::selectInput(
            paste0("center_", term),
            label = NULL,
            choices = c(
              "Grand-mean centered (GMC)" = "GMC",
              "Uncentered (raw metric)" = "none",
              "Group-mean centered (CWC)" = "CWC"
            ),
            selected = cur_ctr
          )
        } else {
          shiny::div(
            class = "tau-sub-label",
            sprintf("Dummy-coded factor (reference: %s)", first_or(levels(factor(dat[[term]])), ""))
          )
        }
      )
    }))
  })

  # Cross-Level Slope Moderator Selectors (Slope-as-Outcome Equations)
  output$cross_level_moderator_controls <- shiny::renderUI({
    dat <- data_reactive()
    l1 <- intersect(input$level1_terms %||% character(), names(dat))
    higher <- unique(c(
      intersect(input$level2_terms %||% character(), names(dat)),
      intersect(input$level3_terms %||% character(), names(dat))
    ))
    if (!length(l1) || !length(higher)) return(NULL)
    saved_mods <- shiny::isolate(slope_moderators())
    shiny::div(
      style = "margin-top: 8px;",
      shiny::tags$strong(style = "font-size: 0.85rem;", "Cross-Level Slope Moderators (Predictors of Level-1 Slopes)"),
      shiny::p(class = "equation-subnote", "Select higher-level predictors that moderate a Level-1 slope (creates cross-level interactions in the slope equation):"),
      lapply(l1, function(term) {
        sel_m <- intersect(saved_mods[[term]] %||% character(), higher)
        shiny::selectizeInput(
          paste0("mod_l1_", term),
          label = sprintf("Moderators of %s slope", term),
          choices = higher,
          selected = sel_m,
          multiple = TRUE,
          options = list(plugins = list("remove_button"), placeholder = "Fixed / unmoderated slope")
        )
      })
    )
  })

  # Sync cross-level moderator dropdowns -> slope_moderators & interactions text box
  shiny::observe({
    dat <- data_reactive()
    l1 <- intersect(input$level1_terms %||% character(), names(dat))
    if (!length(l1)) return()
    mod_vals <- lapply(l1, function(t) input[[paste0("mod_l1_", t)]])
    # Wait until rendered inputs exist
    rendered_flags <- vapply(l1, function(t) paste0("mod_l1_", t) %in% names(input), logical(1))
    if (!any(rendered_flags)) return()
    new_map <- list()
    for (i in seq_along(l1)) {
      if (rendered_flags[[i]] && length(mod_vals[[i]])) {
        new_map[[l1[[i]]]] <- mod_vals[[i]]
      }
    }
    slope_moderators(new_map)
    cross_pairs <- unlist(lapply(names(new_map), function(t) {
       lapply(new_map[[t]], function(m) c(t, m))
    }), recursive = FALSE)
    all_pairs <- unique(c(cross_pairs, shiny::isolate(extra_interactions())))
    txt <- if (length(all_pairs)) paste(vapply(all_pairs, paste, character(1), collapse = ":"), collapse = ", ") else ""
    shiny::updateTextInput(session, "interactions", value = txt)
  })

  # Within-level / general interaction builder
  output$within_interaction_picker_ui <- shiny::renderUI({
    terms <- selected_fixed_terms()
    if (length(terms) < 2L) return(NULL)
    cur_pairs <- interaction_spec()
    active_badges <- if (length(cur_pairs)) {
      paste(vapply(cur_pairs, paste, character(1), collapse = " x "), collapse = "; ")
    } else {
      "None"
    }
    shiny::div(
      style = "margin-top:8px;",
      shiny::tags$strong(style = "font-size:0.85rem;", "Add Within-Level or Custom Interaction"),
      bslib::layout_columns(
        col_widths = c(6, 6),
        shiny::selectInput("inter_var_a", NULL, choices = terms, selected = terms[[1]]),
        shiny::selectInput("inter_var_b", NULL, choices = terms, selected = terms[[min(2L, length(terms))]])
      ),
      shiny::div(
        class = "preset-row",
        shiny::actionButton("add_interaction_btn", "+ Add Interaction", class = "btn-outline-primary btn-sm"),
        shiny::actionButton("clear_interactions_btn", "Clear All Interactions", class = "btn-outline-secondary btn-sm")
      ),
      shiny::div(class = "tau-sub-label", sprintf("Active interactions: %s", active_badges))
    )
  })

  shiny::observeEvent(input$add_interaction_btn, {
    a <- input$inter_var_a
    b <- input$inter_var_b
    if (is.null(a) || is.null(b) || identical(a, b)) {
      shiny::showNotification("Choose two distinct predictors to form an interaction.", type = "warning")
      return()
    }
    pair <- c(a, b)
    cur_extra <- extra_interactions()
    already <- any(vapply(cur_extra, function(p) setequal(p, pair), logical(1)))
    if (!already) {
      cur_extra[[length(cur_extra) + 1L]] <- pair
      extra_interactions(cur_extra)
    }
    cur_txt <- trimws(input$interactions %||% "")
    new_tok <- paste(pair, collapse = ":")
    updated_txt <- if (nzchar(cur_txt)) paste(cur_txt, new_tok, sep = ", ") else new_tok
    shiny::updateTextInput(session, "interactions", value = updated_txt)
  })

  shiny::observeEvent(input$clear_interactions_btn, {
    slope_moderators(list())
    extra_interactions(list())
    shiny::updateTextInput(session, "interactions", value = "")
    dat <- data_reactive()
    for (t in intersect(input$level1_terms %||% character(), names(dat))) {
      shiny::updateSelectizeInput(session, paste0("mod_l1_", t), selected = character())
    }
  })

  selected_fixed_terms <- shiny::reactive({
    dat <- data_reactive()
    out_var <- active_outcome()
    grps <- active_groups()
    valid_pool <- setdiff(names(dat), c(out_var, grps))
    has_ui_inputs <- !is.null(input$level1_terms) || !is.null(input$level2_terms) || !is.null(input$level3_terms)
    raw <- unique(c(input$level1_terms %||% character(), input$level2_terms %||% character(), input$level3_terms %||% character()))
    valid <- intersect(raw, valid_pool)
    if (has_ui_inputs) return(valid)
    if (all(c("mathscore", "ses", "schoolid") %in% names(dat))) {
      return(intersect(c("ses", "priorachieve", "homework", "female", "minority", "meanses", "climate", "schoolsize", "sector"), valid_pool))
    }
    if (all(c("reading", "wave", "studentid") %in% names(dat))) {
      return(intersect(c("wave", "studyhours", "treatment", "baseses"), valid_pool))
    }
    character()
  })

  predictor_level_spec <- shiny::reactive({
    dat <- data_reactive()
    has_ui <- !is.null(input$level1_terms) || !is.null(input$level2_terms) || !is.null(input$level3_terms)
    if (!has_ui && all(c("mathscore", "ses", "schoolid") %in% names(dat))) {
      return(list(
        `Level 1` = c("ses", "priorachieve", "homework", "female", "minority"),
        `Level 2` = c("meanses", "climate", "schoolsize", "sector"),
        `Level 3` = character()
      ))
    }
    if (!has_ui && all(c("reading", "wave", "studentid") %in% names(dat))) {
      return(list(
        `Level 1` = c("wave", "studyhours"),
        `Level 2` = c("treatment", "baseses"),
        `Level 3` = character()
      ))
    }
    list(
      `Level 1` = intersect(input$level1_terms %||% character(), names(dat)),
      `Level 2` = intersect(input$level2_terms %||% character(), names(dat)),
      `Level 3` = intersect(input$level3_terms %||% character(), names(dat))
    )
  })

  update_level_terms <- function(level1 = character(), level2 = character(), level3 = character()) {
    shiny::updateSelectizeInput(session, "level1_terms", selected = level1)
    shiny::updateSelectizeInput(session, "level2_terms", selected = level2)
    shiny::updateSelectizeInput(session, "level3_terms", selected = level3)
  }

  link_choices <- function(distribution) {
    switch(
      distribution %||% "gaussian",
      gaussian = "identity",
      binomial = c("logit", "probit"),
      poisson = "log",
      `negative binomial` = "log",
      Gamma = c("log", "inverse"),
      "identity"
    )
  }

  active_distribution <- shiny::reactive({
    input$distribution %||% input$distribution_data %||% "gaussian"
  })

  active_link <- shiny::reactive({
    dist <- active_distribution()
    valid_links <- link_choices(dist)
    cand <- input$link %||% input$link_data %||% valid_links[[1]]
    first_or(intersect(cand, valid_links), valid_links[[1]])
  })

  shiny::observeEvent(input$distribution, {
    choices <- link_choices(input$distribution)
    sel <- first_or(intersect(input$link %||% choices[[1]], choices), choices[[1]])
    shiny::updateSelectInput(session, "link", choices = choices, selected = sel)
    shiny::updateSelectInput(session, "link_data", choices = choices, selected = sel)
  }, ignoreInit = FALSE)

  shiny::observeEvent(input$distribution_data, {
    choices <- link_choices(input$distribution_data)
    sel <- first_or(intersect(input$link_data %||% choices[[1]], choices), choices[[1]])
    shiny::updateSelectInput(session, "link_data", choices = choices, selected = sel)
    shiny::updateSelectInput(session, "link", choices = choices, selected = sel)
  }, ignoreInit = TRUE)

  set_group_slopes <- function(group_map) {
    per_group_random(group_map)
    focus <- input$random_group_focus %||% first_or(names(group_map))
    if (!is.null(focus) && focus %in% names(group_map)) {
      shiny::updateSelectInput(session, "random_slopes", selected = group_map[[focus]]$slopes %||% character())
      shiny::updateCheckboxInput(session, "random_correlation", value = isTRUE(group_map[[focus]]$correlation %||% TRUE))
    }
    l2_grp <- first_or(names(group_map))
    if (!is.null(l2_grp)) {
      sl <- group_map[[l2_grp]]$slopes %||% character()
      for (t in c("ses", "priorachieve", "homework", "wave", "studyhours")) {
        shiny::updateCheckboxInput(session, paste0("slope_l1_", t), value = t %in% sl)
      }
    }
  }

  apply_binary_preset <- function() {
    shiny::updateCheckboxInput(session, "use_example_data", value = TRUE)
    shiny::updateSelectInput(session, "example_dataset", selected = "hsb")
    shiny::updateSelectInput(session, "design_mode", selected = "two_level")
    shiny::updateSelectInput(session, "cluster_l2", selected = "schoolid")
    shiny::updateSelectInput(session, "outcome", selected = "passmath")
    shiny::updateSelectInput(session, "outcome_data", selected = "passmath")
    shiny::updateSelectInput(session, "distribution", selected = "binomial")
    shiny::updateSelectInput(session, "distribution_data", selected = "binomial")
    shiny::updateSelectInput(session, "link", choices = c("logit", "probit"), selected = "logit")
    shiny::updateSelectInput(session, "link_data", choices = c("logit", "probit"), selected = "logit")
    update_level_terms(
      level1 = c("ses", "priorachieve", "homework", "female", "minority"),
      level2 = c("meanses", "climate", "sector")
    )
    slope_moderators(list())
    extra_interactions(list())
    shiny::updateTextInput(session, "interactions", value = "")
    set_group_slopes(list(schoolid = list(slopes = "ses", correlation = TRUE)))
    shiny::showNotification("Binary logistic mixed-model preset loaded (Outcome: passmath, Binomial logit).", type = "message")
  }

  apply_poisson_preset <- function() {
    shiny::updateCheckboxInput(session, "use_example_data", value = TRUE)
    shiny::updateSelectInput(session, "example_dataset", selected = "hsb")
    shiny::updateSelectInput(session, "design_mode", selected = "two_level")
    shiny::updateSelectInput(session, "cluster_l2", selected = "schoolid")
    shiny::updateSelectInput(session, "outcome", selected = "absences")
    shiny::updateSelectInput(session, "outcome_data", selected = "absences")
    shiny::updateSelectInput(session, "distribution", selected = "poisson")
    shiny::updateSelectInput(session, "distribution_data", selected = "poisson")
    shiny::updateSelectInput(session, "link", choices = "log", selected = "log")
    shiny::updateSelectInput(session, "link_data", choices = "log", selected = "log")
    update_level_terms(
      level1 = c("ses", "priorachieve", "homework", "female", "minority"),
      level2 = c("meanses", "climate", "sector")
    )
    slope_moderators(list())
    extra_interactions(list())
    shiny::updateTextInput(session, "interactions", value = "")
    set_group_slopes(list(schoolid = list(slopes = "ses", correlation = TRUE)))
    shiny::showNotification("Poisson count mixed-model preset loaded (Outcome: absences, Poisson log).", type = "message")
  }

  shiny::observeEvent(input$binary_preset, apply_binary_preset())
  shiny::observeEvent(input$binary_preset_data, apply_binary_preset())
  shiny::observeEvent(input$poisson_preset, apply_poisson_preset())
  shiny::observeEvent(input$poisson_preset_data, apply_poisson_preset())

  shiny::observeEvent(input$apply_model_preset, {
    preset <- input$model_preset %||% "custom"
    if (identical(preset, "custom")) {
      shiny::showNotification("Keep editing the current custom model.", type = "message")
      return()
    }
    if (identical(preset, "logistic")) {
      apply_binary_preset()
      return()
    }
    if (identical(preset, "poisson")) {
      apply_poisson_preset()
      return()
    }
    if (identical(preset, "longitudinal")) {
      shiny::updateCheckboxInput(session, "use_example_data", value = TRUE)
      shiny::updateSelectInput(session, "example_dataset", selected = "longitudinal")
      shiny::updateCheckboxInput(session, "advanced_mode", value = FALSE)
      shiny::updateSelectInput(session, "design_mode", selected = "longitudinal")
      shiny::updateSelectInput(session, "cluster_l2", selected = "studentid")
      shiny::updateSelectInput(session, "outcome", selected = "reading")
      shiny::updateSelectInput(session, "outcome_data", selected = "reading")
      shiny::updateSelectInput(session, "distribution", selected = "gaussian")
      shiny::updateSelectInput(session, "distribution_data", selected = "gaussian")
      shiny::updateSelectInput(session, "link", choices = "identity", selected = "identity")
      shiny::updateSelectInput(session, "link_data", choices = "identity", selected = "identity")
      shiny::updateCheckboxInput(session, "reml", value = TRUE)
      update_level_terms(level1 = c("wave", "studyhours"), level2 = c("treatment", "baseses"))
      slope_moderators(list(wave = "treatment"))
      extra_interactions(list())
      shiny::updateTextInput(session, "interactions", value = "wave:treatment")
      set_group_slopes(list(studentid = list(slopes = "wave", correlation = TRUE)))
      shiny::showNotification("Longitudinal growth model template applied.", type = "message")
      return()
    }

    shiny::updateCheckboxInput(session, "use_example_data", value = TRUE)
    shiny::updateSelectInput(session, "example_dataset", selected = "hsb")
    common_level1 <- c("ses", "priorachieve", "homework", "female", "minority")
    common_level2 <- c("meanses", "climate", "schoolsize", "sector")
    shiny::updateCheckboxInput(session, "advanced_mode", value = FALSE)
    shiny::updateSelectInput(session, "outcome", selected = "mathscore")
    shiny::updateSelectInput(session, "outcome_data", selected = "mathscore")
    shiny::updateSelectInput(session, "distribution", selected = "gaussian")
    shiny::updateSelectInput(session, "distribution_data", selected = "gaussian")
    shiny::updateSelectInput(session, "link", choices = "identity", selected = "identity")
    shiny::updateSelectInput(session, "link_data", choices = "identity", selected = "identity")
    shiny::updateCheckboxInput(session, "reml", value = TRUE)
    shiny::updateSelectInput(session, "design_mode", selected = "two_level")
    shiny::updateSelectInput(session, "cluster_l2", selected = "schoolid")
    shiny::updateCheckboxGroupInput(session, "random_intercept_groups", selected = "schoolid")
    update_level_terms(level1 = common_level1, level2 = common_level2)
    slope_moderators(list())
    extra_interactions(list())
    shiny::updateTextInput(session, "interactions", value = "")
    set_group_slopes(list(schoolid = list(slopes = character(), correlation = TRUE)))

    if (identical(preset, "null")) {
      update_level_terms()
    } else if (identical(preset, "random_intercept")) {
      update_level_terms(level1 = "ses", level2 = c("meanses", "sector"))
    } else if (identical(preset, "random_slope")) {
      update_level_terms(level1 = c("ses", "priorachieve", "homework"), level2 = c("meanses", "sector"))
      set_group_slopes(list(schoolid = list(slopes = "ses", correlation = TRUE)))
    } else if (identical(preset, "cross_level")) {
      update_level_terms(level1 = common_level1, level2 = common_level2)
      set_group_slopes(list(schoolid = list(slopes = "ses", correlation = TRUE)))
      slope_moderators(list(ses = "sector", homework = "climate"))
      shiny::updateTextInput(session, "interactions", value = "ses:sector, homework:climate")
    } else if (identical(preset, "three_level")) {
      shiny::updateSelectInput(session, "design_mode", selected = "three_level")
      shiny::updateSelectInput(session, "cluster_l2", selected = "schoolid")
      shiny::updateSelectInput(session, "cluster_l3", selected = "districtid")
      shiny::updateCheckboxGroupInput(session, "random_intercept_groups", selected = c("schoolid", "districtid"))
      update_level_terms(level1 = common_level1, level2 = c("meanses", "climate", "schoolsize", "sector"), level3 = "districtses")
      set_group_slopes(list(
        schoolid = list(slopes = "ses", correlation = TRUE),
        districtid = list(slopes = character(), correlation = TRUE)
      ))
    } else if (identical(preset, "crossed")) {
      shiny::updateCheckboxInput(session, "advanced_mode", value = TRUE)
      shiny::updateSelectInput(session, "design_mode", selected = "crossed")
      shiny::updateSelectInput(session, "cluster_l2", selected = "schoolid")
      shiny::updateSelectInput(session, "cluster_l3", selected = "teacherid")
      shiny::updateCheckboxGroupInput(session, "random_intercept_groups", selected = c("schoolid", "teacherid"))
      update_level_terms(level1 = c("ses", "priorachieve", "homework", "female", "minority"), level2 = c("meanses", "sector"))
      set_group_slopes(list(
        schoolid = list(slopes = "ses", correlation = TRUE),
        teacherid = list(slopes = character(), correlation = TRUE)
      ))
    }

    preset_labels <- c(
      "Null / unconditional model" = "null",
      "Random intercept model" = "random_intercept",
      "Random slope model" = "random_slope",
      "Cross-level interaction model" = "cross_level",
      "Three-level nested model" = "three_level",
      "Crossed school + teacher model" = "crossed"
    )
    shiny::showNotification(sprintf("%s template applied.", names(which(preset_labels == preset))), type = "message")
  })

  output$variable_palette <- shiny::renderUI({
    dat <- data_reactive()
    shiny::tags$div(
      class = "variable-palette",
      lapply(names(dat), function(nm) {
        shiny::tags$span(class = if (is.numeric(dat[[nm]])) "var-pill numeric" else "var-pill factor", nm)
      })
    )
  })

  fixed_spec <- shiny::reactive({
    dat <- data_reactive()
    terms <- selected_fixed_terms()
    out <- lapply(terms, function(term) {
      center <- if (term %in% numeric_vars(dat)) input[[paste0("center_", term)]] %||% default_center_for(term) else "none"
      list(center = center)
    })
    names(out) <- terms
    out
  })

  parsed_interactions <- shiny::reactive({
    txt <- trimws(input$interactions %||% "")
    if (!nzchar(txt)) return(list())
    pieces <- strsplit(txt, ",", fixed = TRUE)[[1]]
    Filter(function(x) length(x) > 1, lapply(pieces, function(piece) trimws(strsplit(piece, ":", fixed = TRUE)[[1]])))
  })

  interaction_spec <- shiny::reactive({
    dat <- data_reactive()
    active_f <- names(fixed_spec())
    sm <- slope_moderators()
    mod_pairs <- unlist(lapply(intersect(names(sm), active_f), function(t) {
      mods <- intersect(sm[[t]] %||% character(), active_f)
      lapply(mods, function(m) c(t, m))
    }), recursive = FALSE)
    ext_pairs <- Filter(function(pair) all(pair %in% active_f), extra_interactions())

    raw_txt <- trimws(input$interactions %||% "")
    is_default_hsb_txt <- identical(raw_txt, "ses:sector, homework:climate")
    raw_pairs <- if (!nzchar(raw_txt) || is_default_hsb_txt) {
      list()
    } else {
      Filter(function(pair) all(pair %in% names(dat)), parsed_interactions())
    }

    combined <- c(mod_pairs, ext_pairs, raw_pairs)
    if (!length(combined)) return(list())
    keys <- vapply(combined, function(p) paste(sort(p), collapse = "::"), character(1))
    combined[!duplicated(keys)]
  })

  invalid_interaction_vars <- shiny::reactive({
    dat <- data_reactive()
    raw_txt <- trimws(input$interactions %||% "")
    if (!nzchar(raw_txt)) return(character())
    is_hsb <- all(c("mathscore", "ses", "schoolid") %in% names(dat))
    if (!is_hsb && identical(raw_txt, "ses:sector, homework:climate")) return(character())
    raw_vars <- unique(unlist(parsed_interactions(), use.names = FALSE))
    setdiff(raw_vars, names(dat))
  })

  # Per-grouping-factor random-effects persistence
  shiny::observeEvent(list(input$random_slopes, input$random_correlation), {
    focus <- input$random_group_focus
    if (is.null(focus) || !nzchar(focus)) return()
    cur <- per_group_random()
    prev_slopes <- if (!is.null(cur[[focus]])) cur[[focus]]$slopes %||% character() else character()
    new_slopes <- if (!is.null(input$random_slopes) || isTRUE(input$advanced_mode)) {
      input$random_slopes %||% character()
    } else {
      prev_slopes
    }
    cur[[focus]] <- list(
      slopes = new_slopes,
      correlation = isTRUE(input$random_correlation)
    )
    per_group_random(cur)
  }, ignoreNULL = FALSE, ignoreInit = TRUE)

  output$random_intercept_selector <- shiny::renderUI({
    groups <- active_groups()
    if (!length(groups)) return(NULL)
    cur <- shiny::isolate(input$random_intercept_groups)
    sel <- if (is.null(cur)) groups else intersect(cur, groups)
    if (!length(sel)) sel <- groups
    shiny::checkboxGroupInput("random_intercept_groups", "Include random intercepts for", choices = groups, selected = sel)
  })

  output$random_group_selector <- shiny::renderUI({
    groups <- active_groups()
    if (length(groups) <= 1L && !isTRUE(input$advanced_mode)) return(NULL)
    if (!length(groups)) return(shiny::p("Select a cluster ID on the Data tab first."))
    cur_focus <- shiny::isolate(input$random_group_focus)
    sel <- if (!is.null(cur_focus) && cur_focus %in% groups) cur_focus else groups[[1]]
    shiny::selectInput("random_group_focus", "Configure Tau covariance / slopes for", choices = groups, selected = sel)
  })

  output$random_slope_selector <- shiny::renderUI({
    dat <- data_reactive()
    focus <- input$random_group_focus %||% first_or(active_groups())
    choices <- intersect(names(fixed_spec()), numeric_vars(dat))
    saved <- shiny::isolate(per_group_random())[[focus %||% ""]]
    default_slopes <- if (!is.null(saved)) {
      intersect(saved$slopes %||% character(), choices)
    } else if (identical(focus, "schoolid") && "ses" %in% choices) {
      "ses"
    } else if (identical(focus, "studentid") && "wave" %in% choices) {
      "wave"
    } else {
      character()
    }
    shiny::selectInput("random_slopes", sprintf("Random slopes (%s)", focus %||% "group"), choices = choices, selected = default_slopes, multiple = TRUE)
  })

  shiny::observeEvent(input$random_group_focus, {
    focus <- input$random_group_focus
    if (is.null(focus) || !nzchar(focus)) return()
    saved <- per_group_random()[[focus]]
    if (!is.null(saved)) {
      shiny::updateCheckboxInput(session, "random_correlation", value = isTRUE(saved$correlation %||% TRUE))
      shiny::updateSelectInput(session, "random_slopes", selected = saved$slopes %||% character())
    }
  }, ignoreInit = TRUE)

  random_spec <- shiny::reactive({
    dat <- data_reactive()
    groups <- active_groups()
    if (!length(groups)) return(list())
    intercept_groups <- if (is.null(input$random_intercept_groups)) groups else intersect(input$random_intercept_groups, groups)
    if (!length(intercept_groups) && is.null(input$random_intercept_groups)) intercept_groups <- groups
    saved_map <- per_group_random()
    focus <- input$random_group_focus %||% first_or(groups)
    valid_slopes <- intersect(names(fixed_spec()), numeric_vars(dat))
    out <- lapply(groups, function(group_name) {
      grp_entry <- saved_map[[group_name]]
      slopes <- if (identical(group_name, focus) && !is.null(input$random_slopes) && (isTRUE(input$advanced_mode) || length(groups) > 1L)) {
        intersect(input$random_slopes, valid_slopes)
      } else if (!is.null(grp_entry)) {
        intersect(grp_entry$slopes %||% character(), valid_slopes)
      } else if (identical(group_name, "schoolid") && "ses" %in% valid_slopes) {
        "ses"
      } else if (identical(group_name, "studentid") && "wave" %in% valid_slopes) {
        "wave"
      } else {
        character()
      }
      corr <- if (identical(group_name, focus) && !is.null(input$random_correlation)) {
        isTRUE(input$random_correlation)
      } else if (!is.null(grp_entry)) {
        isTRUE(grp_entry$correlation %||% TRUE)
      } else {
        TRUE
      }
      list(
        intercept = group_name %in% intercept_groups,
        slopes = slopes,
        correlation = corr
      )
    })
    names(out) <- groups
    out
  })

  grouping_spec <- shiny::reactive({
    groups <- active_groups()
    out <- as.list(groups)
    names(out) <- groups
    out
  })

  spec_reactive <- shiny::reactive({
    dat <- data_reactive()
    out_var <- active_outcome()
    shiny::req(out_var, out_var %in% names(dat))
    mlm_spec(
      outcome = out_var,
      distribution = active_distribution(),
      link = active_link(),
      fixed = fixed_spec(),
      grouping = grouping_spec(),
      nesting = active_nesting_note(),
      structure = active_structure_type(),
      random = random_spec(),
      interactions = interaction_spec(),
      predictor_levels = predictor_level_spec(),
      data = dat
    )
  })

  # Home Page (Data > Overview) DV, Hierarchy, IVs, and Live Equation Outputs
  output$home_blueprint_tiles <- shiny::renderUI({
    dat <- data_reactive()
    spec <- spec_reactive()
    out_var <- spec$outcome
    out_col <- dat[[out_var]]
    dv_sub <- if (is.numeric(out_col)) {
      sprintf("Numeric (M = %.2f, SD = %.2f) | %s (%s)", mean(out_col, na.rm = TRUE), stats::sd(out_col, na.rm = TRUE), spec$distribution, spec$link)
    } else {
      sprintf("Categorical (%s levels) | %s (%s)", length(unique(stats::na.omit(out_col))), spec$distribution, spec$link)
    }
    grps <- active_groups()
    hier_val <- if (!length(grps)) {
      "No cluster ID selected"
    } else if (length(grps) == 1L) {
      sprintf("Level 1 (N = %s) -> Level 2: %s (J = %s)", nrow(dat), grps[[1]], length(unique(stats::na.omit(dat[[grps[[1]]]]))))
    } else if (identical(spec$structure, "crossed")) {
      sprintf("%s (%s) x %s (%s)", grps[[1]], length(unique(stats::na.omit(dat[[grps[[1]]]]))), grps[[2]], length(unique(stats::na.omit(dat[[grps[[2]]]]))))
    } else {
      sprintf("L1 (N = %s) -> L2: %s (%s) -> L3: %s (%s)", nrow(dat), grps[[1]], length(unique(stats::na.omit(dat[[grps[[1]]]]))), grps[[2]], length(unique(stats::na.omit(dat[[grps[[2]]]]))))
    }
    n_iv <- length(names(spec$fixed))
    sl_all <- unique(unlist(lapply(spec$random, function(r) r$slopes %||% character())))
    n_inter <- length(spec$interactions)
    shiny::div(
      class = "blueprint-grid",
      shiny::div(
        class = "blueprint-tile dv-tile",
        shiny::div(class = "blueprint-label", "Dependent Variable (Outcome Y)"),
        shiny::div(class = "blueprint-value", out_var),
        shiny::div(class = "blueprint-sub", dv_sub)
      ),
      shiny::div(
        class = "blueprint-tile cluster-tile",
        shiny::div(class = "blueprint-label", "Multilevel Clustering Structure"),
        shiny::div(class = "blueprint-value", hier_val),
        shiny::div(class = "blueprint-sub", sprintf("Design: %s", spec$structure))
      ),
      shiny::div(
        class = "blueprint-tile iv-tile",
        shiny::div(class = "blueprint-label", "Independent Variables & Slopes"),
        shiny::div(class = "blueprint-value", sprintf("%s Predictor%s | %s Random Slope%s", n_iv, ifelse(n_iv == 1, "", "s"), length(sl_all), ifelse(length(sl_all) == 1, "", "s"))),
        shiny::div(class = "blueprint-sub", sprintf("%s interaction / moderation term%s", n_inter, ifelse(n_inter == 1, "", "s")))
      )
    )
  })

  output$home_active_ivs_table <- shiny::renderTable({
    active_predictors_blueprint_table(data_reactive(), spec_reactive())
  }, striped = TRUE, bordered = TRUE)

  live_spec_equations_reactive <- shiny::reactive({
    mlm_spec_equations(spec_reactive(), data_reactive())
  })

  output$home_live_equations <- shiny::renderUI({
    render_equations_preview_ui(live_spec_equations_reactive(), compact = TRUE)
  })

  output$live_model_equations_ui <- shiny::renderUI({
    eq <- live_spec_equations_reactive()
    shiny::tagList(
      render_equations_preview_ui(eq, compact = FALSE),
      shiny::div(class = "mode-note", eq$note)
    )
  })

  output$auto_variable_guide_table <- shiny::renderTable({
    auto_classify_variables_table(data_reactive(), spec_reactive())
  }, striped = TRUE, bordered = TRUE)

  output$data_preview <- shiny::renderTable(utils::head(data_reactive(), 10), striped = TRUE, bordered = TRUE)

  output$missingness_table <- shiny::renderTable({
    missingness_table(data_reactive())
  }, striped = TRUE, bordered = TRUE)

  output$feature_matrix_table <- shiny::renderTable({
    mlm_feature_matrix()
  }, striped = TRUE, bordered = TRUE)

  output$validation_benchmarks_table <- shiny::renderTable({
    shiny::withProgress(message = "Preparing validation benchmarks", value = 0.25, {
      mlm_validation_benchmarks(run = isTRUE(benchmark_validation_run()))
    })
  }, striped = TRUE, bordered = TRUE)

  shiny::observeEvent(input$run_benchmark_validation, {
    benchmark_validation_run(TRUE)
    shiny::showNotification("Benchmark fit and code round-trip validation complete.", type = "message")
  })

  output$model_template_explanation <- shiny::renderUI({
    shiny::div(class = "template-note", model_template_explanation(input$model_preset %||% "custom"))
  })

  output$group_summary <- shiny::renderUI({
    dat <- data_reactive()
    groups <- active_groups()
    if (!length(groups)) return(shiny::p("Select one or more cluster IDs."))
    rows <- lapply(groups, function(g) {
      shiny::tags$li(sprintf("%s: %s clusters", g, length(unique(stats::na.omit(dat[[g]])))))
    })
    shiny::tags$ul(rows)
  })

  output$group_structure_table <- shiny::renderTable({
    group_structure_table(data_reactive(), active_groups(), active_structure_type())
  }, striped = TRUE, bordered = TRUE)

  output$group_structure_note <- shiny::renderUI({
    shiny::div(class = "mode-note", group_structure_note(active_groups(), active_structure_type()))
  })

  output$group_plot <- shiny::renderPlot({
    dat <- data_reactive()
    g <- first_or(active_groups())
    shiny::req(g, g %in% names(dat))
    counts <- sort(table(dat[[g]]), decreasing = TRUE)
    if (length(counts) > 40) counts <- counts[seq_len(40)]
    plot_df <- data.frame(group = factor(names(counts), levels = rev(names(counts))), n = as.numeric(counts))
    ggplot2::ggplot(plot_df, ggplot2::aes(x = .data[["group"]], y = .data[["n"]])) +
      ggplot2::geom_col(fill = "#245a73") +
      ggplot2::coord_flip() +
      ggplot2::labs(x = NULL, y = sprintf("Rows per %s cluster", g)) +
      ggplot2::theme_minimal(base_size = 12)
  })

  readiness_reactive <- shiny::reactive({
    tab <- model_readiness_table(data_reactive(), spec_reactive())
    bad_inter <- invalid_interaction_vars()
    if (length(bad_inter)) {
      extra <- data.frame(
        Status = "Stop",
        Check = "Interactions",
        Detail = sprintf("Unknown variable(s) in interactions box: %s.", paste(bad_inter, collapse = ", ")),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
      tab <- rbind(extra, tab)
    }
    tab
  })

  output$variable_role_table <- shiny::renderTable({
    variable_role_table(data_reactive(), spec_reactive())
  }, striped = TRUE, bordered = TRUE)

  output$model_builder_summary <- shiny::renderTable({
    model_builder_summary(spec_reactive())
  }, striped = TRUE, bordered = TRUE)

  output$predictor_level_table <- shiny::renderTable({
    predictor_level_table(data_reactive(), spec_reactive())
  }, striped = TRUE, bordered = TRUE)

  output$formula_preview <- shiny::renderText({
    paste(deparse(build_formula(spec_reactive()), width.cutoff = 500), collapse = "\n")
  })

  output$vcov_template <- shiny::renderPlot({
    rand <- random_spec()
    focus <- input$random_group_focus %||% first_or(names(rand))
    block <- if (is.null(focus)) NULL else rand[[focus]] %||% first_or(rand)
    shiny::req(!is.null(block))
    slopes <- block$slopes %||% character()
    labels <- c(if (isTRUE(block$intercept)) "Intercept", slopes)
    shiny::req(length(labels) > 0)
    grid <- expand.grid(row = labels, col = labels, stringsAsFactors = FALSE)
    grid$row <- factor(grid$row, levels = rev(labels))
    grid$col <- factor(grid$col, levels = labels)
    grid$value <- if (isTRUE(block$correlation)) 1 else as.numeric(as.character(grid$row) == as.character(grid$col))
    grid$label <- mapply(function(r_lbl, c_lbl) {
      i <- match(as.character(r_lbl), labels) - 1L
      j <- match(as.character(c_lbl), labels) - 1L
      if (i == j) return(sprintf("tau[%s%s]", i, j))
      if (isTRUE(block$correlation)) sprintf("tau[%s%s]", max(i, j), min(i, j)) else "0"
    }, grid$row, grid$col, USE.NAMES = FALSE)
    ggplot2::ggplot(grid, ggplot2::aes(x = .data[["col"]], y = .data[["row"]], fill = .data[["value"]])) +
      ggplot2::geom_tile(color = "white", linewidth = 0.8) +
      ggplot2::geom_text(ggplot2::aes(label = .data[["label"]]), size = 4.2, color = "#17232f", parse = TRUE) +
      ggplot2::scale_fill_gradient(low = "#eef3f5", high = "#6f9fa6", guide = "none") +
      ggplot2::labs(x = NULL, y = NULL) +
      ggplot2::theme_minimal(base_size = 13)
  })

  output$vcov_template_note <- shiny::renderUI({
    rand <- random_spec()
    focus <- input$random_group_focus %||% first_or(names(rand))
    block <- if (is.null(focus)) NULL else rand[[focus]] %||% first_or(rand)
    if (is.null(block)) return(NULL)
    shiny::div(
      class = "mode-note",
      sprintf(
        "Tau matrix preview for %s. Shaded cells are estimated variances/covariances; 0 cells are constrained to zero when independent random effects are selected.",
        focus %||% "the selected grouping factor"
      )
    )
  })

  output$estimation_preview <- shiny::renderTable({
    spec <- spec_reactive()
    miss_df <- model_missingness(data_reactive(), spec)
    df_label <- if (identical(spec$distribution, "gaussian")) input$df_method %||% "none" else "Wald (link scale)"
    rbind(
      data.frame(
        setting = c("Outcome (DV)", "Distribution", "Grouping structure", "REML", "Degrees of freedom", "Optimizer", "Max function evaluations"),
        value = c(
          spec$outcome,
          spec$distribution,
          spec$structure %||% "nested",
          as.character(isTRUE(input$reml)),
          df_label,
          input$optimizer %||% "bobyqa",
          as.character(input$maxfun %||% 30000)
        ),
        stringsAsFactors = FALSE
      ),
      data.frame(
        setting = names(miss_df),
        value = as.character(unlist(miss_df, use.names = FALSE)),
        stringsAsFactors = FALSE
      )
    )
  }, striped = TRUE, bordered = TRUE)

  output$model_readiness_table <- shiny::renderTable({
    readiness_reactive()
  }, striped = TRUE, bordered = TRUE)

  output$model_missingness_table <- shiny::renderTable({
    model_missingness_table(data_reactive(), spec_reactive())
  }, striped = TRUE, bordered = TRUE)

  output$complete_case_impact <- shiny::renderTable({
    model_missingness(data_reactive(), spec_reactive())
  }, striped = TRUE, bordered = TRUE)

  output$missing_pattern_table <- shiny::renderTable({
    missing_pattern_table(data_reactive(), spec_reactive())
  }, striped = TRUE, bordered = TRUE)

  output$imputation_code_panel <- shiny::renderText({
    current_formula <- tryCatch(build_formula(spec_reactive()), error = function(e) NULL)
    paste(imputation_repro_code(spec_reactive(), current_formula), collapse = "\n")
  })

  active_df_method <- shiny::reactive({
    m <- input$df_method %||% (if (requireNamespace("lmerTest", quietly = TRUE)) "satterthwaite" else "none")
    if (m %in% c("none", "satterthwaite", "kenward-roger")) m else "none"
  })

  run_fit <- function(optimizer) {
    readiness <- readiness_reactive()
    if (model_readiness_has_stops(readiness)) {
      fit_state(list(result = NULL, error = "Model readiness checks found issues that must be fixed before fitting.", optimizer = optimizer))
      shiny::showNotification("Fix Model Readiness stop items before fitting.", type = "error")
      shiny::updateNavbarPage(session, "main_nav", selected = "Estimate")
      return(invisible(NULL))
    }
    shiny::withProgress(message = "Fitting multilevel model", value = 0.4, {
      tryCatch(
        {
          reml_flag <- if (is.null(input$reml)) TRUE else isTRUE(input$reml)
          result <- mlm_fit(
            spec_reactive(),
            REML = reml_flag,
            optimizer = optimizer %||% "bobyqa",
            maxfun = input$maxfun %||% 30000,
            df_method = active_df_method()
          )
          fit_state(list(result = result, error = NULL, optimizer = optimizer))
          history <- model_history()
          label <- sprintf("Model %s", length(history) + 1)
          history[[label]] <- list(result = result, optimizer = optimizer, REML = result$settings$REML, maxfun = result$settings$maxfun)
          model_history(history)
          shiny::showNotification("Model fit completed. Results are ready.", type = "message")
          shiny::updateNavbarPage(session, "main_nav", selected = "Results")
        },
        error = function(e) {
          fit_state(list(result = NULL, error = conditionMessage(e), optimizer = optimizer))
          shiny::showNotification("Model fit failed. See Fit Status for details.", type = "error")
        }
      )
    })
  }

  shiny::observeEvent(input$fit_model, run_fit(input$optimizer %||% "bobyqa"))
  shiny::observeEvent(input$fit_current_from_home, run_fit(input$optimizer %||% "bobyqa"))
  shiny::observeEvent(input$fit_from_model_tab, run_fit(input$optimizer %||% "bobyqa"))

  run_example_fit <- function() {
    shiny::withProgress(message = "Fitting HSB example model", value = 0.4, {
      tryCatch(
        {
          shiny::updateCheckboxInput(session, "use_example_data", value = TRUE)
          shiny::updateSelectInput(session, "example_dataset", selected = "hsb")
          shiny::updateCheckboxInput(session, "advanced_mode", value = FALSE)
          shiny::updateSelectInput(session, "design_mode", selected = "two_level")
          shiny::updateSelectInput(session, "cluster_l2", selected = "schoolid")
          shiny::updateSelectInput(session, "outcome", selected = "mathscore")
          shiny::updateSelectInput(session, "outcome_data", selected = "mathscore")
          shiny::updateSelectInput(session, "distribution", selected = "gaussian")
          shiny::updateSelectInput(session, "distribution_data", selected = "gaussian")
          shiny::updateSelectInput(session, "link", choices = "identity", selected = "identity")
          shiny::updateSelectInput(session, "link_data", choices = "identity", selected = "identity")
          update_level_terms(
            level1 = c("ses", "priorachieve", "homework", "female", "minority"),
            level2 = c("meanses", "climate", "schoolsize", "sector"),
            level3 = character()
          )
          slope_moderators(list(ses = "sector", homework = "climate"))
          extra_interactions(list())
          shiny::updateTextInput(session, "interactions", value = "ses:sector, homework:climate")
          set_group_slopes(list(schoolid = list(slopes = "ses", correlation = TRUE)))

          dat <- clean_imported_dataframe(example_hsb(), strings_as_factors = TRUE)
          spec <- mlm_spec(
            outcome = "mathscore",
            distribution = "gaussian",
            link = "identity",
            fixed = list(
              ses = list(center = "CWC"),
              priorachieve = list(center = "CWC"),
              homework = list(center = "CWC"),
              meanses = list(center = "GMC"),
              climate = list(center = "GMC"),
              schoolsize = list(center = "GMC"),
              sector = list(center = "none"),
              female = list(center = "none"),
              minority = list(center = "none")
            ),
            grouping = list(schoolid = "schoolid"),
            nesting = "observations nested within schoolid",
            structure = "nested",
            random = list(
              schoolid = list(intercept = TRUE, slopes = "ses", correlation = TRUE)
            ),
            interactions = list(c("ses", "sector"), c("homework", "climate")),
            predictor_levels = list(
              `Level 1` = c("ses", "priorachieve", "homework", "female", "minority"),
              `Level 2` = c("meanses", "climate", "schoolsize", "sector")
            ),
            data = dat
          )
          opt <- input$optimizer %||% "bobyqa"
          mf <- input$maxfun %||% 30000
          reml_flag <- if (is.null(input$reml)) TRUE else isTRUE(input$reml)
          result <- mlm_fit(spec, REML = reml_flag, optimizer = opt, maxfun = mf, df_method = active_df_method())
          fit_state(list(result = result, error = NULL, optimizer = opt))
          history <- model_history()
          label <- sprintf("Example Model %s", length(history) + 1)
          history[[label]] <- list(result = result, optimizer = opt, REML = result$settings$REML, maxfun = mf)
          model_history(history)
          shiny::showNotification("HSB demo model fit completed. Results are ready.", type = "message")
          shiny::updateNavbarPage(session, "main_nav", selected = "Results")
          shiny::updateTabsetPanel(session, "results_section", selected = "Dashboard")
        },
        error = function(e) {
          fit_state(list(result = NULL, error = conditionMessage(e), optimizer = input$optimizer %||% "bobyqa"))
          shiny::showNotification("Example model fit failed. See Fit Status for details.", type = "error")
        }
      )
    })
  }

  shiny::observeEvent(input$fit_example, run_example_fit())
  shiny::observeEvent(input$fit_demo_from_data, run_example_fit())
  shiny::observeEvent(input$refit_model, run_fit("Nelder_Mead"))

  shiny::observeEvent(input$clear_model_history, {
    model_history(list())
    shiny::showNotification("Comparison history cleared.", type = "message")
  })

  active_result <- shiny::reactive(fit_state()$result)
  active_error <- shiny::reactive(fit_state()$error)

  output$fit_status <- shiny::renderText({
    res <- active_result()
    if (is.null(res)) {
      if (!is.null(active_error())) return("Model fit failed.")
      return("Model has not been fit yet. Click Fit Current Model, then the app will switch to Results when estimation completes.")
    }
    diag <- mlm_diagnostics(res$fit)
    paste(c(
      sprintf("Model fit completed with optimizer: %s", fit_state()$optimizer %||% res$settings$optimizer %||% "bobyqa"),
      sprintf("Formula: %s", paste(deparse(res$formula, width.cutoff = 500), collapse = " ")),
      paste(diag$check, diag$value, sep = ": ")
    ), collapse = "\n")
  })

  output$fit_error <- shiny::renderUI({
    err <- active_error()
    if (is.null(err)) return(NULL)
    shiny::div(class = "error-box", shiny::strong("Fit error"), shiny::p(err))
  })

  output$results_status <- shiny::renderText({
    res <- active_result()
    if (is.null(res)) {
      if (!is.null(active_error())) return("The last fit failed. The error is shown below.")
      return("No model has been fit yet. Go to Data or Model and click Fit Current Model.")
    }
    sprintf("Showing results for: %s", paste(deparse(res$formula, width.cutoff = 500), collapse = " "))
  })

  output$results_error <- shiny::renderUI({
    err <- active_error()
    if (is.null(err)) return(NULL)
    shiny::div(class = "error-box", shiny::strong("Fit error"), shiny::p(err))
  })

  output$dashboard_status <- shiny::renderUI({
    res <- active_result()
    err <- active_error()
    if (!is.null(res)) {
      return(shiny::div(
        class = "dashboard-status success",
        shiny::strong("Model fit complete"),
        shiny::span(sprintf(" %s", paste(deparse(res$formula, width.cutoff = 500), collapse = " ")))
      ))
    }
    if (!is.null(err)) {
      return(shiny::div(class = "dashboard-status danger", shiny::strong("Last fit failed"), shiny::span(" Review the Fit Status card below.")))
    }
    shiny::div(class = "dashboard-status waiting", shiny::strong("No fitted model yet"), shiny::span(" Review the blueprint on Data or customize on Model, then click Fit Current Model."))
  })

  output$dashboard_actions <- shiny::renderUI({
    has_res <- !is.null(active_result())
    shiny::div(
      class = "dashboard-nav-row",
      shiny::actionButton("go_estimation", "Fit or Refit Model", class = "btn-primary"),
      if (has_res) shiny::actionButton("go_tables", "Open APA & Robust Tables", class = "btn-outline-primary"),
      if (has_res) shiny::actionButton("go_equations", "Open Equations & Tau", class = "btn-outline-primary"),
      if (has_res) shiny::actionButton("go_diagnostics", "Open Diagnostics", class = "btn-outline-primary"),
      if (has_res) shiny::actionButton("go_exports", "Export Report & Bundle", class = "btn-outline-primary")
    )
  })

  shiny::observeEvent(input$go_estimation, shiny::updateNavbarPage(session, "main_nav", selected = "Estimate"))
  shiny::observeEvent(input$go_tables, shiny::updateTabsetPanel(session, "results_section", selected = "Tables"))
  shiny::observeEvent(input$go_equations, shiny::updateTabsetPanel(session, "results_section", selected = "Equations"))
  shiny::observeEvent(input$go_diagnostics, shiny::updateTabsetPanel(session, "results_section", selected = "Diagnostics"))
  shiny::observeEvent(input$go_exports, shiny::updateNavbarPage(session, "main_nav", selected = "Report & Code"))

  output$dashboard_summary <- shiny::renderTable({
    res <- active_result()
    if (is.null(res)) {
      return(data.frame(
        Metric = c("Outcome (DV)", "Formula preview", "Grouping structure"),
        Value = c(
          active_outcome() %||% "",
          paste(deparse(build_formula(spec_reactive()), width.cutoff = 500), collapse = " "),
          active_structure_type()
        ),
        check.names = FALSE
      ))
    }
    model_summary_cards(res, fit_state()$optimizer %||% res$settings$optimizer)
  }, striped = TRUE, bordered = TRUE)

  output$dashboard_guidance <- shiny::renderUI({
    res <- active_result()
    if (is.null(res)) {
      return(shiny::tags$ul(
        shiny::tags$li("Review the active Outcome (DV) and Independent Variables (IVs) on Data > Overview."),
        shiny::tags$li("Use Model to toggle Group-Mean (CWC) vs Grand-Mean (GMC) centering, random slopes, and cross-level moderators."),
        shiny::tags$li("Use ML rather than REML when comparing nested fixed-effect models.")
      ))
    }
    shiny::tags$ul(lapply(model_guidance(res, res$settings$REML), shiny::tags$li))
  })

  output$interpretation_panel <- shiny::renderUI({
    res <- active_result()
    if (is.null(res)) return(shiny::p("Fit a model to generate interpretation guidance."))
    shiny::tags$ul(lapply(interpretation_points(res), shiny::tags$li))
  })

  output$assumptions_panel <- shiny::renderUI({
    res <- active_result()
    if (is.null(res)) return(shiny::p("Fit a model to generate assumption checks."))
    shiny::tags$ul(lapply(assumption_points(res), shiny::tags$li))
  })

  output$latex_equations_results <- shiny::renderUI({
    res <- active_result()
    if (is.null(res)) return(shiny::p("Fit a model to generate equations."))
    eq <- mlm_latex_equations(res)
    shiny::tags$div(class = "equation-stack", lapply(seq_along(eq$level), function(i) {
      sub_note <- if (length(eq$level_notes) >= i) shiny::p(class = "equation-subnote", eq$level_notes[[i]]) else NULL
      shiny::tags$div(
        class = "equation-card",
        shiny::tags$strong(eq$level[[i]]),
        shiny::HTML(equation_html(eq$equations[[i]])),
        sub_note
      )
    }))
  })

  output$equation_note_results <- shiny::renderUI({
    res <- active_result()
    if (is.null(res)) return(NULL)
    shiny::div(class = "mode-note", mlm_latex_equations(res)$note)
  })

  output$equation_key_results <- shiny::renderTable({
    res <- active_result()
    shiny::req(res)
    equation_key_table(res)
  }, striped = TRUE, bordered = TRUE)

  output$combined_equation_results <- shiny::renderUI({
    res <- active_result()
    if (is.null(res)) return(shiny::p("Fit a model to generate a combined equation."))
    eq <- mlm_latex_equations(res)
    shiny::tags$div(
      class = "equation-card",
      shiny::HTML(equation_html(eq$combined)),
      shiny::p(class = "equation-subnote", "Combined mixed-model form substituting higher-level equations into the outcome model.")
    )
  })

  output$tau_equations_results <- shiny::renderUI({
    res <- active_result()
    if (is.null(res)) return(NULL)
    html <- render_tau_labeled_html(res)
    if (!nzchar(html)) return(NULL)
    shiny::HTML(html)
  })

  output$tau_label_table_results <- shiny::renderTable({
    res <- active_result()
    shiny::req(res)
    tau_label_table(res)
  }, striped = TRUE, bordered = TRUE)

  output$raw_latex_results <- shiny::renderText({
    res <- active_result()
    if (is.null(res)) return("Fit a model to generate raw LaTeX.")
    eq <- mlm_latex_equations(res)
    paste(
      "% Level equations:",
      paste(vapply(eq$equations, latex_display_equation, character(1)), collapse = "\n\n"),
      "",
      "% Combined equation:",
      latex_wrapped_equation(eq$combined),
      "",
      "% Tau matrices:",
      paste(eq$tau, collapse = "\n\n"),
      sep = "\n"
    )
  })

  output$apa_table_html <- shiny::renderUI({
    res <- active_result()
    if (is.null(res)) return(shiny::p("Fit a model to generate an APA-style fixed-effects table."))
    shiny::HTML(apa_fixed_html(res, table_number = "1a", exponentiate = TRUE))
  })

  output$robust_table_html <- shiny::renderUI({
    res <- active_result()
    if (is.null(res)) return(shiny::p("Fit a model to compute cluster-robust sandwich standard errors."))
    shiny::HTML(apa_robust_html(res, table_number = "1b"))
  })

  output$inference_notice <- shiny::renderUI({
    res <- active_result()
    if (is.null(res)) return(NULL)
    shiny::tags$div(
      class = "mode-badge advanced",
      shiny::tags$strong("Inference Summary"),
      shiny::tags$span(paste(" ", fixed_effects_note(res), " Both model-based SEs (Table 1a) and Huber-White cluster-robust sandwich SEs (Table 1b) are shown below. ", icc_note(res$fit)))
    )
  })

  output$dummy_table_html <- shiny::renderUI({
    res <- active_result()
    if (is.null(res)) return(shiny::p("Fit a model to generate a dummy-coding table."))
    shiny::HTML(apa_dummy_html(res$data, names(res$spec$fixed)))
  })

  output$variance_table_html <- shiny::renderUI({
    res <- active_result()
    if (is.null(res)) return(shiny::p("Fit a model to generate variance components."))
    shiny::HTML(apa_variance_html(res$fit))
  })

  output$variance_lrt_table <- shiny::renderTable({
    res <- active_result()
    shiny::req(res)
    mlm_variance_tests(res)
  }, striped = TRUE, bordered = TRUE)

  output$reliability_table_html <- shiny::renderUI({
    res <- active_result()
    if (is.null(res)) return(shiny::p("Fit a model to compute random-coefficient reliabilities."))
    shiny::HTML(apa_reliability_html(res))
  })

  output$icc_table_html <- shiny::renderUI({
    res <- active_result()
    if (is.null(res)) return(shiny::p("Fit a model to generate ICCs."))
    shiny::HTML(apa_icc_html(res$fit))
  })

  output$r2_table_html <- shiny::renderUI({
    res <- active_result()
    if (is.null(res)) return(shiny::p("Fit a model to compute Nakagawa R2."))
    shiny::HTML(apa_r2_html(res))
  })

  output$wald_terms_selector <- shiny::renderUI({
    res <- active_result()
    if (is.null(res)) return(shiny::p("Fit a model to select fixed effects for joint hypothesis testing."))
    choices <- setdiff(names(lme4::fixef(res$fit)), "(Intercept)")
    if (!length(choices)) choices <- names(lme4::fixef(res$fit))
    sel <- choices[seq_len(min(2L, length(choices)))]
    shiny::selectizeInput(
      "wald_terms",
      "Fixed-effect coefficients tested equal to 0",
      choices = choices,
      selected = sel,
      multiple = TRUE,
      options = list(plugins = list("remove_button"))
    )
  })

  output$wald_test_table <- shiny::renderTable({
    res <- active_result()
    shiny::req(res)
    terms <- input$wald_terms
    if (is.null(terms) || !length(terms)) {
      all_f <- setdiff(names(lme4::fixef(res$fit)), "(Intercept)")
      terms <- all_f[seq_len(min(2L, length(all_f)))]
    }
    mlm_wald_test(res, terms = terms, vcov_type = input$wald_vcov_type %||% "CR1")
  }, striped = TRUE, bordered = TRUE)

  output$pop_average_card_ui <- shiny::renderUI({
    res <- active_result()
    if (is.null(res)) return(NULL)
    pa <- mlm_population_average(res)
    if (!nrow(pa)) return(NULL)
    names(pa) <- c("Predictor", "b (Unit-Specific)", "OR [95% CI] (Unit-Specific)", "b (Pop.-Average)", "OR [95% CI] (Pop.-Average)")
    bslib::card(
      bslib::card_header("Unit-Specific vs. Population-Average (Marginal) Logistic Odds Ratios"),
      shiny::HTML(apa_html_table(
        pa,
        table_number = "1c",
        title = "Unit-Specific and Population-Average Logistic Mixed-Model Estimates",
        note = "Unit-specific estimates condition on cluster random effects; population-average approximations marginalize across cluster heterogeneity (Zeger et al., 1988; Raudenbush & Bryk, 2002, Ch. 10).",
        italic_columns = character()
      ))
    )
  })

  output$software_table_html <- shiny::renderUI({
    pkgs <- active_packages_for_result(active_result())
    shiny::HTML(apa_software_html(packages = pkgs))
  })

  output$software_statement <- shiny::renderText({
    pkgs <- active_packages_for_result(active_result())
    mlm_software_apa(packages = pkgs)
  })

  output$papaja_code <- shiny::renderText({
    pkgs <- active_packages_for_result(active_result())
    paste(mlm_papaja_code(packages = pkgs), collapse = "\n")
  })

  output$raw_latex_bundle <- shiny::renderText({
    res <- active_result()
    if (is.null(res)) return("Fit a model to generate copy/paste LaTeX.")
    raw_latex_bundle(res)
  })

  output$diagnostics_table <- shiny::renderTable({
    res <- active_result()
    shiny::req(res)
    shiny::withProgress(message = "Preparing diagnostics", value = 0.35, {
      mlm_diagnostics(res$fit)
    })
  }, striped = TRUE, bordered = TRUE)

  output$homogeneity_table <- shiny::renderTable({
    res <- active_result()
    shiny::req(res)
    hom <- mlm_homogeneity_test(res)
    if (!nrow(hom)) {
      return(data.frame(Note = "Level-1 variance homogeneity H test applies to Gaussian mixed models.", check.names = FALSE))
    }
    hom
  }, striped = TRUE, bordered = TRUE)

  output$diagnostic_x_selector <- shiny::renderUI({
    res <- active_result()
    if (is.null(res)) return(shiny::p("Fit a model to choose graph variables."))
    choices <- numeric_vars(res$data)
    selected <- first_or(intersect(c("ses_CWC", "wave", "priorachieve_CWC", "homework_CWC", "ses"), choices), first_or(choices))
    shiny::selectInput("diagnostic_x", "X variable for model lines", choices = choices, selected = selected)
  })

  output$diagnostic_group_selector <- shiny::renderUI({
    res <- active_result()
    if (is.null(res)) return(NULL)
    groups <- unlist(res$spec$grouping, use.names = FALSE)
    shiny::selectInput("diagnostic_group", "Grouping factor for model lines", choices = groups, selected = first_or(groups))
  })

  output$ranef_group_selector <- shiny::renderUI({
    res <- active_result()
    if (is.null(res)) return(NULL)
    groups <- names(lme4::ranef(res$fit))
    shiny::selectInput("ranef_group", "Caterpillar grouping factor", choices = groups, selected = first_or(groups))
  })

  output$ranef_term_selector <- shiny::renderUI({
    res <- active_result()
    if (is.null(res)) return(NULL)
    re_list <- lme4::ranef(res$fit)
    grp <- if (!is.null(input$ranef_group) && input$ranef_group %in% names(re_list)) input$ranef_group else first_or(names(re_list))
    shiny::req(grp)
    terms <- colnames(re_list[[grp]])
    shiny::selectInput("ranef_term", "Caterpillar random effect", choices = terms, selected = first_or(terms))
  })

  diagnostic_df <- shiny::reactive({
    res <- active_result()
    shiny::req(res)
    diagnostic_frame(res)
  })

  output$residual_fitted_plot <- shiny::renderPlot({
    shiny::withProgress(message = "Drawing residual diagnostics", value = 0.4, {
      res <- active_result()
      shiny::req(res)
      df <- diagnostic_df()
      y_lab <- if (is_gaussian_fit(res$fit)) "Residuals" else "Pearson residuals"
      ggplot2::ggplot(df, ggplot2::aes(x = .data[["fitted"]], y = .data[["residual"]])) +
        ggplot2::geom_hline(yintercept = 0, color = "#8a969c") +
        ggplot2::geom_point(alpha = 0.62, color = "#245a73") +
        ggplot2::geom_smooth(method = "loess", formula = y ~ x, se = FALSE, color = "#8b3f2f", linewidth = 0.8) +
        ggplot2::labs(x = "Fitted values (response scale)", y = y_lab) +
        ggplot2::theme_minimal(base_size = 12)
    })
  })

  output$qq_plot <- shiny::renderPlot({
    shiny::withProgress(message = "Drawing QQ plot", value = 0.4, {
      df <- diagnostic_df()
      ggplot2::ggplot(df, ggplot2::aes(sample = .data[["residual"]])) +
        ggplot2::stat_qq(color = "#245a73", alpha = 0.7) +
        ggplot2::stat_qq_line(color = "#8b3f2f", linewidth = 0.8) +
        ggplot2::labs(x = "Theoretical quantiles", y = "Sample residual quantiles") +
        ggplot2::theme_minimal(base_size = 12)
    })
  })

  output$observed_fitted_plot <- shiny::renderPlot({
    shiny::withProgress(message = "Drawing observed versus fitted plot", value = 0.4, {
      res <- active_result()
      shiny::req(res)
      df <- diagnostic_df()
      rng <- range(c(df$observed, df$fitted), na.rm = TRUE)
      y_lab <- if (identical(res$spec$distribution, "binomial")) "Observed outcome (0 / 1)" else "Observed values"
      p <- ggplot2::ggplot(df, ggplot2::aes(x = .data[["fitted"]], y = .data[["observed"]])) +
        ggplot2::geom_abline(intercept = 0, slope = 1, color = "#8a969c") +
        ggplot2::geom_point(alpha = 0.62, color = "#245a73") +
        ggplot2::labs(x = "Fitted values", y = y_lab) +
        ggplot2::theme_minimal(base_size = 12)
      if (all(is.finite(rng)) && diff(rng) > 0) {
        p <- p + ggplot2::coord_equal(xlim = rng, ylim = rng)
      }
      p
    })
  })

  output$random_effects_interval_plot <- shiny::renderPlot({
    shiny::withProgress(message = "Estimating random-effect intervals", value = 0.25, {
      res <- active_result()
      shiny::req(res)
      re <- as.data.frame(lme4::ranef(res$fit, condVar = TRUE))
      shiny::incProgress(0.55)
      shiny::req(nrow(re) > 0)
      grpvar <- if (!is.null(input$ranef_group) && input$ranef_group %in% re$grpvar) {
        input$ranef_group
      } else {
        first_or(unique(re$grpvar))
      }
      plot_df <- re[re$grpvar == grpvar, , drop = FALSE]
      term <- if (!is.null(input$ranef_term) && input$ranef_term %in% plot_df$term) {
        input$ranef_term
      } else if ("(Intercept)" %in% plot_df$term) {
        "(Intercept)"
      } else {
        first_or(unique(plot_df$term))
      }
      plot_df <- plot_df[plot_df$term == term, , drop = FALSE]
      shiny::req(nrow(plot_df) > 0)
      if (nrow(plot_df) > 50) {
        ord <- order(plot_df$condval)
        keep_idx <- unique(c(ord[seq_len(25)], ord[seq(nrow(plot_df) - 24, nrow(plot_df))]))
        plot_df <- plot_df[keep_idx, , drop = FALSE]
      }
      plot_df <- plot_df[order(plot_df$condval), , drop = FALSE]
      unique_labels <- make.unique(as.character(plot_df$grp))
      plot_df$grp_label <- factor(unique_labels, levels = unique_labels)
      plot_df$low <- plot_df$condval - 1.96 * plot_df$condsd
      plot_df$high <- plot_df$condval + 1.96 * plot_df$condsd
      ggplot2::ggplot(plot_df, ggplot2::aes(x = .data[["condval"]], y = .data[["grp_label"]])) +
        ggplot2::geom_vline(xintercept = 0, color = "#8a969c") +
        ggplot2::geom_segment(ggplot2::aes(x = .data[["low"]], xend = .data[["high"]], y = .data[["grp_label"]], yend = .data[["grp_label"]]), color = "#6b7a83") +
        ggplot2::geom_point(color = "#245a73", size = 2) +
        ggplot2::labs(x = sprintf("Empirical Bayes estimate (%s: %s)", grpvar, term), y = NULL) +
        ggplot2::theme_minimal(base_size = 12)
    })
  })

  output$model_lines_plot <- shiny::renderPlot({
    shiny::withProgress(message = "Preparing group-specific model lines", value = 0.35, {
      res <- active_result()
      shiny::req(res)
      df <- diagnostic_df()
      num_choices <- intersect(names(df), numeric_vars(res$data))
      x_var <- if (!is.null(input$diagnostic_x) && input$diagnostic_x %in% names(df)) {
        input$diagnostic_x
      } else {
        first_or(intersect(c("ses_CWC", "wave", "ses"), num_choices), first_or(num_choices))
      }
      groups <- unlist(res$spec$grouping, use.names = FALSE)
      g_var <- if (!is.null(input$diagnostic_group) && input$diagnostic_group %in% names(df)) {
        input$diagnostic_group
      } else {
        first_or(intersect(groups, names(df)))
      }
      shiny::req(x_var, g_var, x_var %in% names(df), g_var %in% names(df))
      group_counts <- sort(table(df[[g_var]]), decreasing = TRUE)
      keep <- names(group_counts)[seq_len(min(8, length(group_counts)))]
      plot_df <- df[as.character(df[[g_var]]) %in% keep, , drop = FALSE]
      plot_df[[g_var]] <- factor(as.character(plot_df[[g_var]]), levels = keep)
      plot_df <- plot_df[order(plot_df[[g_var]], plot_df[[x_var]]), , drop = FALSE]
      ggplot2::ggplot(plot_df, ggplot2::aes(x = .data[[x_var]], y = .data[["observed"]], color = .data[[g_var]])) +
        ggplot2::geom_point(alpha = 0.45) +
        ggplot2::geom_line(ggplot2::aes(y = .data[["fitted"]], group = .data[[g_var]]), linewidth = 0.9) +
        ggplot2::labs(x = x_var, y = res$spec$outcome, color = g_var) +
        ggplot2::theme_minimal(base_size = 12)
    })
  })

  output$model_compare_table <- shiny::renderTable({
    history <- model_history()
    if (!length(history)) {
      return(data.frame(Message = "Fit one or more models to populate model comparison."))
    }
    rows <- lapply(names(history), function(label) {
      res_item <- history[[label]]$result
      fit <- res_item$fit
      criterion <- if (inherits(fit, "lmerMod") && isTRUE(lme4::isREML(fit))) {
        lme4::REMLcrit(fit)
      } else {
        stats::deviance(fit)
      }
      r2 <- tryCatch(mlm_r2(res_item), error = function(e) NULL)
      data.frame(
        Model = label,
        Outcome = res_item$spec$outcome,
        N = stats::nobs(fit),
        REML = history[[label]]$REML,
        Optimizer = history[[label]]$optimizer,
        Deviance_or_REMLcrit = round(criterion, 2),
        Parameters = attr(stats::logLik(fit), "df"),
        AIC = round(stats::AIC(fit), 2),
        BIC = round(stats::BIC(fit), 2),
        logLik = round(as.numeric(stats::logLik(fit)), 2),
        `Marginal R2` = if (!is.null(r2) && is.finite(r2$R2_marginal)) round(r2$R2_marginal, 3) else NA_real_,
        Singular = lme4::isSingular(fit, tol = 1e-4),
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
    })
    do.call(rbind, rows)
  }, striped = TRUE, bordered = TRUE)

  lrt_comparison <- shiny::reactive({
    compare_models_lrt(model_history())
  })

  output$lrt_note <- shiny::renderUI({
    cmp <- lrt_comparison()
    if (length(model_history()) < 2) return(shiny::p(cmp$note))
    shiny::div(class = "mode-note", cmp$note)
  })

  output$lrt_table <- shiny::renderTable({
    shiny::req(length(model_history()) >= 2)
    lrt_comparison()$table
  }, striped = TRUE, bordered = TRUE, digits = 4)

  output$model_formula_history <- shiny::renderText({
    history <- model_history()
    if (!length(history)) return("No models stored yet.")
    paste(vapply(names(history), function(label) {
      formula_text <- paste(deparse(history[[label]]$result$formula, width.cutoff = 500), collapse = " ")
      sprintf("%s: %s", label, formula_text)
    }, character(1)), collapse = "\n\n")
  })

  output$code_panel <- shiny::renderText({
    res <- active_result()
    if (is.null(res)) return("Fit a model to generate reproducible R code.")
    paste(generate_repro_code(res), collapse = "\n")
  })

  output$publication_checklist <- shiny::renderTable({
    res <- active_result()
    if (is.null(res)) {
      return(data.frame(
        Status = "Waiting",
        Item = "Fit a model",
        Evidence = "No fitted model is available yet.",
        `Suggested Review` = "Click Fit Current Model before preparing publication materials.",
        check.names = FALSE
      ))
    }
    mlm_reproducibility_check(res)
  }, striped = TRUE, bordered = TRUE)

  output$methods_text <- shiny::renderText({
    res <- active_result()
    if (is.null(res)) return("Fit a model to generate methods text.")
    mlm_methods_text(res)
  })

  output$download_r_code <- shiny::downloadHandler(
    filename = function() "mlmr_reproducible_analysis.R",
    content = function(file) {
      res <- active_result()
      text <- if (is.null(res)) {
        "# Fit a model in mlmr before exporting reproducible R code."
      } else {
        paste(generate_repro_code(res), collapse = "\n")
      }
      writeLines(text, file, useBytes = TRUE)
    }
  )

  output$download_latex <- shiny::downloadHandler(
    filename = function() "mlmr_tables_and_equations.tex",
    content = function(file) {
      res <- active_result()
      text <- if (is.null(res)) {
        "% Fit a model in mlmr before exporting LaTeX."
      } else {
        raw_latex_bundle(res)
      }
      writeLines(text, file, useBytes = TRUE)
    }
  )

  output$download_report <- shiny::downloadHandler(
    filename = function() "mlmr_model_report.qmd",
    content = function(file) {
      res <- active_result()
      text <- if (is.null(res)) {
        c("# mlmr Model Report", "", "Fit a model to generate a report.")
      } else {
        manuscript_report_markdown(res)
      }
      writeLines(text, file, useBytes = TRUE)
    }
  )

  output$download_publication_bundle <- shiny::downloadHandler(
    filename = function() "mlmr_publication_bundle.zip",
    content = function(file) {
      res <- active_result()
      if (is.null(res)) {
        stage <- tempfile("mlmr_empty_")
        dir.create(stage)
        on.exit(unlink(stage, recursive = TRUE), add = TRUE)
        readme <- file.path(stage, "README.txt")
        writeLines("Fit a model in mlmr before downloading the publication bundle.", readme, useBytes = TRUE)
        old_wd <- getwd()
        on.exit(setwd(old_wd), add = TRUE)
        setwd(stage)
        utils::zip(zipfile = file, files = "README.txt", flags = "-9Xq")
        return(invisible(NULL))
      }
      mlm_write_publication_bundle(res, file)
    }
  )

  output$download_validation_report <- shiny::downloadHandler(
    filename = function() "mlmr_validation_report.md",
    content = function(file) {
      report <- mlm_validation_report()
      writeLines(validation_report_text(report), file, useBytes = TRUE)
    }
  )

  output$download_tables_html <- shiny::downloadHandler(
    filename = function() "mlmr_apa_tables.html",
    content = function(file) {
      res <- active_result()
      text <- if (is.null(res)) "<p>Fit a model to generate APA tables.</p>" else apa_tables_html_document(res, exponentiate = TRUE)
      writeLines(text, file, useBytes = TRUE)
    }
  )

  output$download_tables_doc <- shiny::downloadHandler(
    filename = function() "mlmr_apa_tables.doc",
    content = function(file) {
      res <- active_result()
      text <- if (is.null(res)) "<p>Fit a model to generate APA tables.</p>" else apa_tables_html_document(res, exponentiate = TRUE)
      writeLines(text, file, useBytes = TRUE)
    }
  )

  output$download_tables_tex <- shiny::downloadHandler(
    filename = function() "mlmr_apa_tables.tex",
    content = function(file) {
      res <- active_result()
      text <- if (is.null(res)) "% Fit a model to generate APA tables." else apa_tables_latex_document(res, exponentiate = TRUE)
      writeLines(text, file, useBytes = TRUE)
    }
  )

  output$download_level1_residuals <- shiny::downloadHandler(
    filename = function() "mlmr_level1_residuals.csv",
    content = function(file) {
      res <- active_result()
      df <- if (is.null(res)) data.frame(Note = "Fit a model before exporting residuals.") else mlm_residual_files(res)$level1
      utils::write.csv(df, file, row.names = FALSE)
    }
  )

  output$download_cluster_residuals <- shiny::downloadHandler(
    filename = function() "mlmr_cluster_eb_residuals.csv",
    content = function(file) {
      res <- active_result()
      df <- if (is.null(res)) data.frame(Note = "Fit a model before exporting residuals.") else mlm_residual_files(res)$cluster
      utils::write.csv(df, file, row.names = FALSE)
    }
  )
}
