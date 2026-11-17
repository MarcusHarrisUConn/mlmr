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

app_server <- function(input, output, session) {
  fit_state <- shiny::reactiveVal(list(result = NULL, error = NULL, optimizer = NULL))
  model_history <- shiny::reactiveVal(list())
  benchmark_validation_run <- shiny::reactiveVal(FALSE)
  per_group_random <- shiny::reactiveVal(list(
    schoolid = list(slopes = "ses", correlation = TRUE)
  ))
  dataset_signature <- shiny::reactiveVal("")

  shiny::observeEvent(input$data_file, {
    shiny::updateCheckboxInput(session, "use_example_data", value = FALSE)
    shiny::updateTextInput(session, "interactions", value = "")
    model_history(list())
    fit_state(list(result = NULL, error = NULL, optimizer = NULL))
  }, ignoreInit = TRUE)

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
      shiny::updateSelectInput(session, "structure_type", selected = "nested")
      shiny::updateTextInput(session, "nesting_text", value = "waves %in% students")
      shiny::updateTextInput(session, "interactions", value = "wave:treatment")
      shiny::updateSelectInput(session, "model_preset", selected = "longitudinal")
    } else {
      per_group_random(list(
        schoolid = list(slopes = "ses", correlation = TRUE),
        districtid = list(slopes = character(), correlation = TRUE)
      ))
      shiny::updateSelectInput(session, "structure_type", selected = "nested")
      shiny::updateTextInput(session, "nesting_text", value = "students %in% schools")
      shiny::updateTextInput(session, "interactions", value = "ses:sector, homework:climate")
      shiny::updateSelectInput(session, "model_preset", selected = "custom")
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
      keep_groups <- intersect(names(cur_rand), names(dat))
      per_group_random(cur_rand[keep_groups])
    }
    dat
  })

  output$data_source_note <- shiny::renderUI({
    use_ex <- if (is.null(input$use_example_data)) TRUE else isTRUE(input$use_example_data)
    if (use_ex) {
      ds_label <- if (identical(input$example_dataset %||% "hsb", "longitudinal")) {
        "built-in longitudinal growth dataset"
      } else {
        "built-in HSB educational dataset"
      }
      return(shiny::div(class = "mode-note", sprintf("Using the %s. Upload a file or uncheck this box to analyze your own data.", ds_label)))
    }
    if (is.null(input$data_file)) {
      return(shiny::div(class = "mode-note", "Upload a CSV, TSV/TXT, Excel, SPSS, SAS, or Stata file to use your own data."))
    }
    shiny::div(class = "mode-note", sprintf("Using uploaded file: %s", input$data_file$name))
  })

  output$data_preview <- shiny::renderTable(utils::head(data_reactive(), 10), striped = TRUE, bordered = TRUE)

  output$missingness_table <- shiny::renderTable({
    missingness_table(data_reactive())
  }, striped = TRUE, bordered = TRUE)

  output$mode_badge <- shiny::renderUI({
    if (isTRUE(input$advanced_mode)) {
      shiny::div(class = "mode-badge advanced", shiny::strong("Advanced Mode"), shiny::span(" GLMMs, crossed structures, optimizer controls, diagnostics, comparison, and missing-data scaffolds."))
    } else {
      shiny::div(class = "mode-badge standard", shiny::strong("HLM Mode"), shiny::span(" Nested Gaussian HLM workflow with centering, degrees of freedom, equations, APA tables, and reproducible lme4 code."))
    }
  })

  output$hlm8_parity_table <- shiny::renderTable({
    mlm_hlm8_parity()
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

  output$group_selector <- shiny::renderUI({
    dat <- data_reactive()
    choices <- candidate_group_vars(dat)
    default_grp <- if ("schoolid" %in% choices && "mathscore" %in% names(dat)) {
      "schoolid"
    } else if ("studentid" %in% choices && "wave" %in% names(dat)) {
      "studentid"
    } else {
      first_or(choices, character())
    }
    shiny::tagList(
      shiny::selectInput("grouping_vars", "Grouping factors / ID variables", choices = choices, selected = default_grp, multiple = TRUE),
      shiny::selectInput("primary_group", "Primary higher-level group", choices = choices, selected = first_or(default_grp, first_or(choices)))
    )
  })

  active_groups <- shiny::reactive({
    dat <- data_reactive()
    raw <- input$grouping_vars
    valid <- intersect(raw %||% character(), names(dat))
    if (length(valid) || !is.null(raw)) return(valid)
    choices <- candidate_group_vars(dat)
    if ("schoolid" %in% choices && "mathscore" %in% names(dat)) return("schoolid")
    if ("studentid" %in% choices && "wave" %in% names(dat)) return("studentid")
    first_or(choices, character())
  })

  output$group_summary <- shiny::renderUI({
    dat <- data_reactive()
    groups <- active_groups()
    if (!length(groups)) return(shiny::p("Select one or more grouping factors."))
    rows <- lapply(groups, function(g) {
      shiny::tags$li(sprintf("%s: %s groups", g, length(unique(stats::na.omit(dat[[g]])))))
    })
    shiny::tags$ul(rows)
  })

  output$group_structure_table <- shiny::renderTable({
    group_structure_table(data_reactive(), active_groups(), input$structure_type %||% "nested")
  }, striped = TRUE, bordered = TRUE)

  output$group_structure_note <- shiny::renderUI({
    shiny::div(class = "mode-note", group_structure_note(active_groups(), input$structure_type %||% "nested"))
  })

  output$group_plot <- shiny::renderPlot({
    dat <- data_reactive()
    g <- if (!is.null(input$primary_group) && input$primary_group %in% names(dat)) {
      input$primary_group
    } else {
      first_or(active_groups())
    }
    shiny::req(g, g %in% names(dat))
    counts <- sort(table(dat[[g]]), decreasing = TRUE)
    if (length(counts) > 40) counts <- counts[seq_len(40)]
    plot_df <- data.frame(group = factor(names(counts), levels = rev(names(counts))), n = as.numeric(counts))
    ggplot2::ggplot(plot_df, ggplot2::aes(x = .data[["group"]], y = .data[["n"]])) +
      ggplot2::geom_col(fill = "#245a73") +
      ggplot2::coord_flip() +
      ggplot2::labs(x = NULL, y = "Rows per group") +
      ggplot2::theme_minimal(base_size = 12)
  })

  output$outcome_selector <- shiny::renderUI({
    dat <- data_reactive()
    preferred <- if ("mathscore" %in% names(dat)) {
      "mathscore"
    } else if ("reading" %in% names(dat)) {
      "reading"
    } else {
      first_or(numeric_vars(dat), first_or(names(dat)))
    }
    shiny::selectInput("outcome", "Outcome", choices = names(dat), selected = preferred)
  })

  active_outcome <- shiny::reactive({
    dat <- data_reactive()
    if (!is.null(input$outcome) && input$outcome %in% names(dat)) return(input$outcome)
    if ("mathscore" %in% names(dat)) return("mathscore")
    if ("reading" %in% names(dat)) return("reading")
    first_or(numeric_vars(dat), first_or(names(dat)))
  })

  output$fixed_selector <- shiny::renderUI({
    dat <- data_reactive()
    out_var <- active_outcome()
    grps <- active_groups()
    choices <- setdiff(names(dat), c(out_var, grps))
    is_hsb <- all(c("mathscore", "ses", "schoolid") %in% names(dat))
    is_long <- all(c("reading", "wave", "studentid") %in% names(dat))
    sel_l1 <- if (is_hsb) {
      intersect(c("ses", "priorachieve", "homework", "female", "minority"), choices)
    } else if (is_long) {
      intersect(c("wave", "studyhours"), choices)
    } else {
      character()
    }
    sel_l2 <- if (is_hsb) {
      intersect(c("meanses", "climate", "schoolsize", "sector"), choices)
    } else if (is_long) {
      intersect(c("treatment", "baseses"), choices)
    } else {
      character()
    }
    shiny::tagList(
      shiny::div(class = "mode-note", "Assign predictors to the level where they belong in the design. These selections become the fixed effects in the model."),
      shiny::selectizeInput("level1_terms", "Level 1 predictors", choices = choices, selected = sel_l1, multiple = TRUE, options = list(plugins = list("remove_button"))),
      shiny::selectizeInput("level2_terms", "Level 2 predictors", choices = choices, selected = sel_l2, multiple = TRUE, options = list(plugins = list("remove_button"))),
      shiny::selectizeInput("level3_terms", "Level 3 predictors", choices = choices, selected = character(), multiple = TRUE, options = list(plugins = list("remove_button")))
    )
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

  shiny::observeEvent(input$distribution, {
    choices <- link_choices(input$distribution)
    shiny::updateSelectInput(session, "link", choices = choices, selected = first_or(intersect(input$link %||% choices[[1]], choices), choices[[1]]))
  }, ignoreInit = FALSE)

  set_group_slopes <- function(group_map) {
    per_group_random(group_map)
    focus <- input$random_group_focus %||% first_or(names(group_map))
    if (!is.null(focus) && focus %in% names(group_map)) {
      shiny::updateSelectInput(session, "random_slopes", selected = group_map[[focus]]$slopes %||% character())
      shiny::updateCheckboxInput(session, "random_correlation", value = isTRUE(group_map[[focus]]$correlation %||% TRUE))
    }
  }

  shiny::observeEvent(input$binary_preset, {
    shiny::updateCheckboxInput(session, "use_example_data", value = TRUE)
    shiny::updateSelectInput(session, "example_dataset", selected = "hsb")
    shiny::updateSelectInput(session, "outcome", selected = "passmath")
    shiny::updateSelectInput(session, "distribution", selected = "binomial")
    shiny::updateSelectInput(session, "link", selected = "logit")
    shiny::updateSelectInput(session, "grouping_vars", selected = "schoolid")
    update_level_terms(
      level1 = c("ses", "priorachieve", "homework", "female", "minority"),
      level2 = c("meanses", "climate", "sector")
    )
    shiny::updateTextInput(session, "interactions", value = "")
    set_group_slopes(list(schoolid = list(slopes = "ses", correlation = TRUE)))
    shiny::showNotification("Binary logistic mixed-model preset selected.", type = "message")
  })

  shiny::observeEvent(input$poisson_preset, {
    shiny::updateCheckboxInput(session, "use_example_data", value = TRUE)
    shiny::updateSelectInput(session, "example_dataset", selected = "hsb")
    shiny::updateSelectInput(session, "outcome", selected = "absences")
    shiny::updateSelectInput(session, "distribution", selected = "poisson")
    shiny::updateSelectInput(session, "link", selected = "log")
    shiny::updateSelectInput(session, "grouping_vars", selected = "schoolid")
    update_level_terms(
      level1 = c("ses", "priorachieve", "homework", "female", "minority"),
      level2 = c("meanses", "climate", "sector")
    )
    shiny::updateTextInput(session, "interactions", value = "")
    set_group_slopes(list(schoolid = list(slopes = "ses", correlation = TRUE)))
    shiny::showNotification("Poisson count mixed-model preset selected.", type = "message")
  })

  shiny::observeEvent(input$apply_model_preset, {
    preset <- input$model_preset %||% "custom"
    if (identical(preset, "custom")) {
      shiny::showNotification("Keep editing the current custom model.", type = "message")
      return()
    }
    if (identical(preset, "longitudinal")) {
      shiny::updateCheckboxInput(session, "use_example_data", value = TRUE)
      shiny::updateSelectInput(session, "example_dataset", selected = "longitudinal")
      shiny::updateCheckboxInput(session, "advanced_mode", value = FALSE)
      shiny::updateSelectInput(session, "outcome", selected = "reading")
      shiny::updateSelectInput(session, "distribution", selected = "gaussian")
      shiny::updateSelectInput(session, "link", selected = "identity")
      shiny::updateCheckboxInput(session, "reml", value = TRUE)
      shiny::updateSelectInput(session, "structure_type", selected = "nested")
      shiny::updateTextInput(session, "nesting_text", value = "waves %in% students")
      shiny::updateSelectInput(session, "grouping_vars", selected = "studentid")
      update_level_terms(level1 = c("wave", "studyhours"), level2 = c("treatment", "baseses"))
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
    shiny::updateSelectInput(session, "distribution", selected = "gaussian")
    shiny::updateSelectInput(session, "link", selected = "identity")
    shiny::updateCheckboxInput(session, "reml", value = TRUE)
    shiny::updateSelectInput(session, "structure_type", selected = "nested")
    shiny::updateTextInput(session, "nesting_text", value = "students %in% schools")
    shiny::updateSelectInput(session, "grouping_vars", selected = "schoolid")
    shiny::updateCheckboxGroupInput(session, "random_intercept_groups", selected = "schoolid")
    update_level_terms(level1 = common_level1, level2 = common_level2)
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
      shiny::updateTextInput(session, "interactions", value = "ses:sector, homework:climate")
    } else if (identical(preset, "three_level")) {
      shiny::updateSelectInput(session, "structure_type", selected = "nested")
      shiny::updateTextInput(session, "nesting_text", value = "students %in% schools %in% districts")
      shiny::updateSelectInput(session, "grouping_vars", selected = c("schoolid", "districtid"))
      shiny::updateCheckboxGroupInput(session, "random_intercept_groups", selected = c("schoolid", "districtid"))
      update_level_terms(level1 = common_level1, level2 = c("meanses", "climate", "schoolsize", "sector"), level3 = "districtses")
      set_group_slopes(list(
        schoolid = list(slopes = "ses", correlation = TRUE),
        districtid = list(slopes = character(), correlation = TRUE)
      ))
    } else if (identical(preset, "crossed")) {
      shiny::updateSelectInput(session, "structure_type", selected = "crossed")
      shiny::updateTextInput(session, "nesting_text", value = "students crossed by schools and teachers")
      shiny::updateSelectInput(session, "grouping_vars", selected = c("schoolid", "teacherid"))
      shiny::updateCheckboxGroupInput(session, "random_intercept_groups", selected = c("schoolid", "teacherid"))
      update_level_terms(level1 = c("ses", "priorachieve", "homework", "female", "minority"), level2 = c("meanses", "sector"))
      set_group_slopes(list(
        schoolid = list(slopes = "ses", correlation = TRUE),
        teacherid = list(slopes = character(), correlation = TRUE)
      ))
    } else if (identical(preset, "logistic")) {
      shiny::updateCheckboxInput(session, "advanced_mode", value = TRUE)
      shiny::updateSelectInput(session, "outcome", selected = "passmath")
      shiny::updateSelectInput(session, "distribution", selected = "binomial")
      shiny::updateSelectInput(session, "link", selected = "logit")
      update_level_terms(level1 = c("ses", "priorachieve", "homework", "female", "minority"), level2 = c("meanses", "climate", "sector"))
      set_group_slopes(list(schoolid = list(slopes = "ses", correlation = TRUE)))
    } else if (identical(preset, "poisson")) {
      shiny::updateCheckboxInput(session, "advanced_mode", value = TRUE)
      shiny::updateSelectInput(session, "outcome", selected = "absences")
      shiny::updateSelectInput(session, "distribution", selected = "poisson")
      shiny::updateSelectInput(session, "link", selected = "log")
      update_level_terms(level1 = c("ses", "priorachieve", "homework", "female", "minority"), level2 = c("meanses", "climate", "sector"))
      set_group_slopes(list(schoolid = list(slopes = "ses", correlation = TRUE)))
    }

    preset_labels <- c(
      "Null / unconditional model" = "null",
      "Random intercept model" = "random_intercept",
      "Random slope model" = "random_slope",
      "Cross-level interaction model" = "cross_level",
      "Three-level nested model" = "three_level",
      "Crossed school + teacher model" = "crossed",
      "Logistic HGLM" = "logistic",
      "Poisson HGLM" = "poisson"
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

  default_center_for <- function(term) {
    if (term %in% c("ses", "priorachieve", "homework", "studyhours")) return("CWC")
    if (term %in% c("meanses", "climate", "schoolsize", "districtses", "baseses")) return("GMC")
    "none"
  }

  output$centering_controls <- shiny::renderUI({
    dat <- data_reactive()
    terms <- selected_fixed_terms()
    numeric_terms <- terms[terms %in% numeric_vars(dat)]
    if (!length(numeric_terms)) return(shiny::p("No numeric predictors selected for centering."))
    shiny::tagList(lapply(numeric_terms, function(term) {
      cur <- shiny::isolate(input[[paste0("center_", term)]]) %||% default_center_for(term)
      shiny::selectInput(paste0("center_", term), paste("Center", term), c("none", "GMC", "CWC"), selected = cur)
    }))
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
    raw_pairs <- parsed_interactions()
    Filter(function(pair) all(pair %in% names(dat)), raw_pairs)
  })

  invalid_interaction_vars <- shiny::reactive({
    dat <- data_reactive()
    raw_txt <- trimws(input$interactions %||% "")
    if (!nzchar(raw_txt)) return(character())
    # Ignore stale initial HSB defaults when user switches to an uploaded dataset
    # before typing in the interactions field.
    is_hsb <- all(c("mathscore", "ses", "schoolid") %in% names(dat))
    if (!is_hsb && identical(raw_txt, "ses:sector, homework:climate")) return(character())
    raw_vars <- unique(unlist(parsed_interactions(), use.names = FALSE))
    setdiff(raw_vars, names(dat))
  })

  # Per-grouping-factor random-effects persistence.
  shiny::observeEvent(list(input$random_slopes, input$random_correlation), {
    focus <- input$random_group_focus
    if (is.null(focus) || !nzchar(focus)) return()
    cur <- per_group_random()
    cur[[focus]] <- list(
      slopes = input$random_slopes %||% character(),
      correlation = isTRUE(input$random_correlation)
    )
    per_group_random(cur)
  }, ignoreNULL = FALSE, ignoreInit = TRUE)

  output$random_intercept_selector <- shiny::renderUI({
    groups <- active_groups()
    if (!length(groups)) return(NULL)
    cur <- shiny::isolate(input$random_intercept_groups)
    sel <- if (is.null(cur)) groups else intersect(cur, groups)
    if (!length(sel) && is.null(cur)) sel <- groups
    shiny::checkboxGroupInput("random_intercept_groups", "Random intercepts for", choices = groups, selected = sel)
  })

  output$random_group_selector <- shiny::renderUI({
    groups <- active_groups()
    if (!length(groups)) return(shiny::p("Choose grouping factors in the Data tab first."))
    cur_focus <- shiny::isolate(input$random_group_focus)
    sel <- if (!is.null(cur_focus) && cur_focus %in% groups) cur_focus else first_or(intersect(input$primary_group, groups), groups[[1]])
    shiny::selectInput("random_group_focus", "Edit random slopes / correlation for", choices = groups, selected = sel)
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
    }
  }, ignoreInit = TRUE)

  random_spec <- shiny::reactive({
    dat <- data_reactive()
    groups <- active_groups()
    if (!length(groups)) return(list())
    intercept_groups <- if (is.null(input$random_intercept_groups)) groups else intersect(input$random_intercept_groups, groups)
    saved_map <- per_group_random()
    focus <- input$random_group_focus %||% first_or(groups)
    valid_slopes <- intersect(names(fixed_spec()), numeric_vars(dat))
    out <- lapply(groups, function(group_name) {
      grp_entry <- saved_map[[group_name]]
      slopes <- if (identical(group_name, focus) && !is.null(input$random_slopes)) {
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
      distribution = if (isTRUE(input$advanced_mode)) input$distribution %||% "gaussian" else "gaussian",
      link = if (isTRUE(input$advanced_mode)) input$link %||% "identity" else "identity",
      fixed = fixed_spec(),
      grouping = grouping_spec(),
      nesting = input$nesting_text,
      structure = input$structure_type %||% "nested",
      random = random_spec(),
      interactions = interaction_spec(),
      predictor_levels = predictor_level_spec(),
      data = dat
    )
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
    focus <- input$random_group_focus %||% input$primary_group %||% first_or(names(rand))
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
    focus <- input$random_group_focus %||% input$primary_group %||% first_or(names(rand))
    block <- if (is.null(focus)) NULL else rand[[focus]] %||% first_or(rand)
    if (is.null(block)) return(NULL)
    shiny::div(
      class = "mode-note",
      sprintf(
        "Preview for %s. Shaded cells are estimated random-effect variances/covariances; 0 cells are fixed to zero when independent random effects are selected.",
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
        setting = c("Distribution", "Grouping structure", "REML", "Degrees of freedom", "Optimizer", "Max function evaluations"),
        value = c(
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
          result <- mlm_fit(
            spec_reactive(),
            REML = isTRUE(input$reml),
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

  shiny::observeEvent(input$fit_model, {
    run_fit(input$optimizer %||% "bobyqa")
  })

  run_example_fit <- function() {
    shiny::withProgress(message = "Fitting example model", value = 0.4, {
      tryCatch(
        {
          # Sync UI state to the flagship non-singular HSB random-slope model.
          shiny::updateCheckboxInput(session, "use_example_data", value = TRUE)
          shiny::updateSelectInput(session, "example_dataset", selected = "hsb")
          shiny::updateCheckboxInput(session, "advanced_mode", value = FALSE)
          shiny::updateSelectInput(session, "structure_type", selected = "nested")
          shiny::updateTextInput(session, "nesting_text", value = "students %in% schools")
          shiny::updateSelectInput(session, "grouping_vars", selected = "schoolid")
          shiny::updateSelectInput(session, "outcome", selected = "mathscore")
          update_level_terms(
            level1 = c("ses", "priorachieve", "homework", "female", "minority"),
            level2 = c("meanses", "climate", "schoolsize", "sector"),
            level3 = character()
          )
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
            nesting = "students %in% schools",
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
          shiny::showNotification("Example model fit completed. Results are ready.", type = "message")
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

  shiny::observeEvent(input$fit_example, {
    run_example_fit()
  })

  shiny::observeEvent(input$fit_demo_from_data, {
    run_example_fit()
  })

  shiny::observeEvent(input$refit_model, {
    run_fit("Nelder_Mead")
  })

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
      return("Model has not been fit yet. Click Fit Model, then the app will switch to Results when the fit is complete.")
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
      return("No model has been fit yet. Go to Estimate and click Fit Model.")
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
    shiny::div(class = "dashboard-status waiting", shiny::strong("No fitted model yet"), shiny::span(" Build the model, then click Fit Model on the Estimate tab."))
  })

  output$dashboard_actions <- shiny::renderUI({
    has_res <- !is.null(active_result())
    shiny::div(
      class = "dashboard-nav-row",
      shiny::actionButton("go_estimation", "Fit or Refit Model", class = "btn-primary"),
      if (has_res) shiny::actionButton("go_tables", "Open APA Tables", class = "btn-outline-primary"),
      if (has_res) shiny::actionButton("go_equations", "Open Equations & Tau", class = "btn-outline-primary"),
      if (has_res) shiny::actionButton("go_diagnostics", "Open Diagnostics", class = "btn-outline-primary"),
      if (has_res) shiny::actionButton("go_exports", "Export Report & Code", class = "btn-outline-primary")
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
        Metric = c("Outcome", "Formula preview", "Grouping structure"),
        Value = c(
          active_outcome() %||% "",
          paste(deparse(build_formula(spec_reactive()), width.cutoff = 500), collapse = " "),
          input$structure_type %||% "nested"
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
        shiny::tags$li("Choose a model template if you want a fast starting point."),
        shiny::tags$li("Check centering choices before fitting random slopes."),
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
    if (is.null(res)) return(shiny::p("Fit a model to generate an APA-style table."))
    shiny::HTML(apa_fixed_html(res, exponentiate = TRUE))
  })

  output$inference_notice <- shiny::renderUI({
    res <- active_result()
    if (is.null(res)) return(NULL)
    shiny::tags$div(
      class = "mode-badge advanced",
      shiny::tags$strong("Inference Review"),
      shiny::tags$span(paste(" ", fixed_effects_note(res), " ", icc_note(res$fit)))
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

  output$icc_table_html <- shiny::renderUI({
    res <- active_result()
    if (is.null(res)) return(shiny::p("Fit a model to generate ICCs."))
    shiny::HTML(apa_icc_html(res$fit))
  })

  output$r2_table_html <- shiny::renderUI({
    res <- active_result()
    if (is.null(res)) return(shiny::p("Fit a model to compute R2 and reliability."))
    shiny::HTML(apa_r2_html(res))
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
        # Keep extremes readable for very large cluster counts.
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
        AIC = round(stats::AIC(fit), 2),
        BIC = round(stats::BIC(fit), 2),
        logLik = round(as.numeric(stats::logLik(fit)), 2),
        Criterion = round(criterion, 2),
        `Marginal R2` = if (!is.null(r2) && is.finite(r2$R2_marginal)) round(r2$R2_marginal, 3) else NA_real_,
        Parameters = attr(stats::logLik(fit), "df"),
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
        `Suggested Review` = "Go to Estimate and fit a model before preparing publication materials.",
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
}
