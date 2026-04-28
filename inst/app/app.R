required_packages <- c("shiny", "bslib", "lme4", "ggplot2")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages)) {
  stop(
    "Missing required package(s): ",
    paste(missing_packages, collapse = ", "),
    ". Run source('install_packages.R') from this folder, then relaunch the app.",
    call. = FALSE
  )
}

library(shiny)
library(bslib)
library(ggplot2)

source(file.path("R", "mlm_core.R"), local = TRUE)

theme <- bs_theme(
  version = 5,
  bootswatch = "flatly",
  primary = "#3f6f78",
  secondary = "#6e7f8d",
  base_font = font_collection("Source Sans Pro", "Segoe UI", "Arial", "sans-serif"),
  heading_font = font_collection("Source Serif Pro", "Georgia", "Times New Roman", "serif")
)

ui <- page_navbar(
  title = "mlmr",
  id = "main_nav",
  theme = theme,
  header = tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
    tags$script(src = "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js"),
    tags$script(HTML("
      function copyOutputText(id) {
        const el = document.getElementById(id);
        if (!el) return;
        const text = el.innerText || el.textContent || '';
        navigator.clipboard.writeText(text);
      }
    "))
  ),
  nav_spacer(),
  nav_item(div(class = "theme-toggle", input_dark_mode(id = "theme_mode", mode = "light"))),
  nav_panel(
    "Data",
    layout_sidebar(
      sidebar = sidebar(
        radioButtons("data_source", "Data source", c("Example HSB-style data" = "example", "Upload CSV" = "upload")),
        conditionalPanel(
          "input.data_source == 'upload'",
          fileInput("csv_file", "CSV file", accept = c(".csv")),
          checkboxInput("strings_as_factors", "Convert text columns to factors", TRUE)
        ),
        uiOutput("group_selector"),
        selectInput(
          "structure_type",
          "Grouping structure",
          choices = c("Strict nested" = "nested", "Crossed / non-nested" = "crossed", "Multiple membership scaffold" = "multiple membership"),
          selected = "nested"
        ),
        textInput("nesting_text", "Nesting or membership note", "students %in% schools"),
        checkboxInput("advanced_mode", "Advanced mode", FALSE),
        uiOutput("mode_badge"),
        conditionalPanel(
          "input.advanced_mode == true",
          div(
            class = "mode-note",
            strong("Advanced mode"),
            p("Adds GLMM distribution/link controls and exposes settings intended for non-Gaussian outcomes. The HLM replacement workflow can stay in standard mode.")
          )
        )
      ),
      tabsetPanel(
        id = "data_section",
        tabPanel(
          "Overview",
          br(),
          card(
            card_header("Try the Built-In Demo"),
            p("Fit the preset example model to see tables, equations, diagnostics, and reproducible code in under a minute."),
            actionButton("fit_demo_from_data", "Fit Example Model", class = "btn-primary")
          ),
          card(card_header("Preview"), div(class = "scroll-table", tableOutput("data_preview")))
        ),
        tabPanel(
          "Structure",
          br(),
          layout_columns(
            col_widths = c(5, 7),
            card(card_header("Grouping Summary"), uiOutput("group_summary"), tableOutput("group_structure_table"), uiOutput("group_structure_note")),
            card(card_header("Rows Per Group"), plotOutput("group_plot", height = "360px"))
          )
        ),
        tabPanel(
          "Missingness",
          br(),
          layout_columns(
            col_widths = c(6, 6),
            card(card_header("All Variables"), div(class = "scroll-table", tableOutput("missingness_table"))),
            card(card_header("Model Variables"), div(class = "scroll-table", tableOutput("model_missingness_table"))),
            card(card_header("Complete-Case Impact"), tableOutput("complete_case_impact")),
            card(card_header("Missing-Data Patterns"), div(class = "scroll-table", tableOutput("missing_pattern_table")))
          ),
          card(
            card_header("Multiple Imputation Code"),
            p("This scaffold follows the current model specification and is ready to adapt once you choose an imputation strategy for a manuscript."),
            verbatimTextOutput("imputation_code_panel")
          )
        ),
        tabPanel(
          "Roles",
          br(),
          card(card_header("Variable Roles"), div(class = "scroll-table", tableOutput("variable_role_table")))
        )
      )
    )
  ),
  nav_panel(
    "Model",
    layout_sidebar(
      sidebar = sidebar(
        uiOutput("outcome_selector"),
        conditionalPanel(
          "input.advanced_mode == true",
          div(
            class = "preset-row",
            actionButton("binary_preset", "Binary Example"),
            actionButton("poisson_preset", "Count Example")
          ),
          selectInput("distribution", "Outcome distribution", c("gaussian", "binomial", "poisson", "negative binomial", "Gamma"), selected = "gaussian"),
          selectInput("link", "Link", c("identity", "logit", "probit", "log", "inverse"), selected = "identity"),
          div(class = "mode-note", "For the HLM-style Gaussian models, leave Advanced mode off.")
        ),
        selectInput(
          "model_preset",
          "Model template",
          choices = c(
            "Current custom model" = "custom",
            "Null / unconditional model" = "null",
            "Random intercept model" = "random_intercept",
            "Random slope model" = "random_slope",
            "Cross-level interaction model" = "cross_level",
            "Three-level nested model" = "three_level",
            "Crossed school + teacher model" = "crossed",
            "Logistic HGLM" = "logistic",
            "Poisson HGLM" = "poisson"
          ),
          selected = "custom"
        ),
        actionButton("apply_model_preset", "Apply Template", class = "btn-outline-primary"),
        uiOutput("model_template_explanation"),
        uiOutput("fixed_selector"),
        uiOutput("centering_controls"),
        textInput("interactions", "Interactions, comma separated", value = "ses:sector, homework:climate", placeholder = "ses:sector, ses:meanses"),
        hr(),
        uiOutput("random_group_selector"),
        uiOutput("random_intercept_selector"),
        uiOutput("random_slope_selector"),
        checkboxInput("random_intercept", "Random intercept", TRUE),
        checkboxInput("random_correlation", "Correlated intercept/slope effects", TRUE)
      ),
      tabsetPanel(
        id = "model_section",
        tabPanel(
          "Summary",
          br(),
          layout_columns(
            col_widths = c(5, 7),
            card(card_header("Model Builder Summary"), tableOutput("model_builder_summary")),
            card(card_header("Working Formula"), verbatimTextOutput("formula_preview"))
          )
        ),
        tabPanel(
          "Fixed Effects",
          br(),
          card(card_header("Fixed Effects Preview"), tableOutput("fixed_preview")),
          card(card_header("Available Variables"), uiOutput("variable_palette"))
        ),
        tabPanel(
          "Random Effects",
          br(),
          layout_columns(
            col_widths = c(5, 7),
            card(card_header("Random Effects Structure"), verbatimTextOutput("random_preview")),
            card(card_header("Variance-Covariance Template"), plotOutput("vcov_template", height = "360px"))
          )
        )
      )
    )
  ),
  nav_panel(
    "Estimate",
    layout_sidebar(
      sidebar = sidebar(
        checkboxInput("reml", "Use REML for Gaussian models", TRUE),
        selectInput("optimizer", "Optimizer", c("bobyqa", "Nelder_Mead", "nloptwrap", "optimx"), selected = "bobyqa"),
        numericInput("maxfun", "Maximum function evaluations", value = 30000, min = 1000, step = 1000),
        actionButton("fit_example", "Fit Example Model", class = "btn-outline-primary"),
        actionButton("fit_model", "Fit Model", class = "btn-primary"),
        actionButton("refit_model", "Refit With Nelder_Mead", class = "btn-outline-secondary")
      ),
      card(
        card_header("Estimation Choices"),
        tableOutput("estimation_preview")
      ),
      card(
        card_header("Model Readiness"),
        p("Review these checks before fitting uploaded data or a custom random-effects structure."),
        div(class = "scroll-table", tableOutput("model_readiness_table"))
      ),
      card(
        card_header("Fit Status"),
        verbatimTextOutput("fit_status"),
        uiOutput("fit_error")
      )
    )
  ),
  nav_panel(
    "Results",
    div(
      class = "results-page",
      tabsetPanel(
        id = "results_section",
        tabPanel(
          "Dashboard",
          br(),
          card(
            class = "dashboard-hero",
            card_header("Model Dashboard"),
            uiOutput("dashboard_status"),
            uiOutput("dashboard_actions")
          ),
          layout_columns(
            col_widths = c(6, 6),
            card(card_header("Model Summary"), tableOutput("dashboard_summary")),
            card(card_header("Guidance"), uiOutput("dashboard_guidance"))
          ),
          card(card_header("Fit Status"), verbatimTextOutput("results_status"), uiOutput("results_error"))
        ),
        tabPanel(
          "Tables",
          br(),
          layout_columns(
            col_widths = c(6, 6),
            card(card_header("APA Fixed Effects Table"), uiOutput("apa_table_html")),
            card(card_header("Dummy Coding"), uiOutput("dummy_table_html")),
            card(card_header("Variance Components"), uiOutput("variance_table_html")),
            card(card_header("ICC"), uiOutput("icc_table_html"))
          )
        ),
        tabPanel(
          "Equations",
          br(),
          card(card_header("Level-by-Level Equations"), uiOutput("latex_equations_results")),
          card(card_header("Combined Full Equation"), uiOutput("combined_equation_results")),
          card(card_header("Tau Variance-Covariance Structure"), uiOutput("tau_equations_results"), div(class = "scroll-table", tableOutput("tau_label_table_results"))),
          card(card_header("Raw LaTeX"), verbatimTextOutput("raw_latex_results"))
        ),
        tabPanel(
          "Interpretation",
          br(),
          layout_columns(
            col_widths = c(6, 6),
            card(card_header("Model Interpretation"), uiOutput("interpretation_panel")),
            card(card_header("Assumptions and Checks"), uiOutput("assumptions_panel"))
          )
        ),
        tabPanel(
          "Diagnostics",
          br(),
          layout_sidebar(
            sidebar = sidebar(
              uiOutput("diagnostic_x_selector"),
              uiOutput("diagnostic_group_selector")
            ),
            div(
              class = "results-page",
              card(card_header("Diagnostics"), tableOutput("diagnostics_table")),
              layout_columns(
                col_widths = c(6, 6),
                card(card_header("Residuals vs Fitted"), plotOutput("residual_fitted_plot", height = "340px")),
                card(card_header("Normal QQ Plot"), plotOutput("qq_plot", height = "340px")),
                card(card_header("Observed vs Fitted"), plotOutput("observed_fitted_plot", height = "340px")),
                card(card_header("Random Effects With Intervals"), plotOutput("random_effects_interval_plot", height = "340px"))
              ),
              card(card_header("Model-Implied Lines by Group"), plotOutput("model_lines_plot", height = "460px"))
            )
          )
        ),
        tabPanel(
          "Compare",
          br(),
          card(card_header("Model Fit Comparison"), tableOutput("model_compare_table")),
          card(card_header("Likelihood-Ratio Comparison"), uiOutput("lrt_note"), tableOutput("lrt_table")),
          card(card_header("Stored Model Formulas"), verbatimTextOutput("model_formula_history"))
        )
      )
    )
  ),
  nav_panel(
    "Report & Code",
    div(
      class = "results-page",
      card(
        card_header("Quarto Report"),
        p("Download a Quarto-ready report with the model summary, tables, equations, diagnostics, R code, and LaTeX."),
        downloadButton("download_report", "Download .qmd Report")
      ),
      card(
        full_screen = TRUE,
        card_header("Reproducible R Code"),
        div(
          class = "code-actions",
          tags$button(type = "button", class = "btn btn-outline-secondary copy-button", onclick = "copyOutputText('code_panel')", "Copy R Code"),
          downloadButton("download_r_code", "Download .R")
        ),
        verbatimTextOutput("code_panel")
      ),
      card(
        card_header("Copy/Paste Raw LaTeX"),
        div(
          class = "code-actions",
          tags$button(type = "button", class = "btn btn-outline-secondary copy-button", onclick = "copyOutputText('raw_latex_bundle')", "Copy LaTeX"),
          downloadButton("download_latex", "Download .tex")
        ),
        verbatimTextOutput("raw_latex_bundle")
      ),
      card(
        card_header("APA Table Exports"),
        p("Export manuscript tables separately for Word, HTML, or LaTeX workflows."),
        div(
          class = "code-actions",
          downloadButton("download_tables_doc", "Download Word Tables"),
          downloadButton("download_tables_html", "Download HTML Tables"),
          downloadButton("download_tables_tex", "Download LaTeX Tables")
        )
      )
    )
  ),
  nav_panel(
    "About",
    div(
      class = "results-page",
      card(
        card_header("Purpose"),
        p("mlmr is a free, open-source Shiny interface for fitting, teaching, and reporting multilevel models in R. The goal is to provide an HLM-style guided workflow while producing transparent lme4 code that users can inspect, copy, rerun, and publish."),
        p("The app is designed for researchers who want the conceptual clarity of level-by-level model building without being locked into private software. It supports uploaded data, a built-in HSB-style example, centering decisions, fixed and random effects, interactions, model diagnostics, APA-style tables, equations, and reproducible exports.")
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("Current Strengths"),
          tags$ul(
            tags$li("Two- and three-level nested Gaussian multilevel models."),
            tags$li("Random intercepts, random slopes, and correlated or independent random-effects structures."),
            tags$li("Grand-mean and cluster-mean centering controls."),
            tags$li("APA-style fixed effects, variance components, ICC, and dummy-coding tables."),
            tags$li("Level-by-level equations, combined equations, and Tau variance-covariance matrices."),
            tags$li("Quarto, R script, and LaTeX export for reproducible workflows.")
          )
        ),
        card(
          card_header("Advanced Direction"),
          tags$ul(
            tags$li("GLMM presets for binary and count outcomes."),
            tags$li("Crossed and non-nested random-effect structures using lme4 syntax."),
            tags$li("Missing-data review and multiple-imputation scaffold code."),
            tags$li("Model comparison and likelihood-ratio workflows."),
            tags$li("Future work: full multiple imputation pooling, richer cross-classified models, longitudinal templates, and exportable Word/HTML reports.")
          )
        )
      ),
      card(
        card_header("Recommended Workflow"),
        tags$ol(
          tags$li("Start on Data to confirm the dataset, grouping factors, structure, missingness, and variable roles."),
          tags$li("Use Model to choose the outcome, predictors, centering, interactions, and random-effects structure."),
          tags$li("Use Estimate to fit the model or fit the built-in example model."),
          tags$li("Use Results for dashboard summaries, APA tables, equations, diagnostics, and model comparison."),
          tags$li("Use Report & Code to export a Quarto report, raw LaTeX, or a reproducible R script.")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  fit_state <- reactiveVal(list(result = NULL, error = NULL, optimizer = NULL))
  model_history <- reactiveVal(list())

  data_reactive <- reactive({
    if (identical(input$data_source, "upload") && !is.null(input$csv_file)) {
      dat <- read.csv(input$csv_file$datapath, stringsAsFactors = isTRUE(input$strings_as_factors), check.names = TRUE)
    } else {
      dat <- example_hsb()
    }
    for (nm in names(dat)) {
      if (is.character(dat[[nm]]) && (identical(input$data_source, "example") || isTRUE(input$strings_as_factors))) {
        dat[[nm]] <- factor(dat[[nm]])
      }
    }
    dat
  })

  output$data_preview <- renderTable(head(data_reactive(), 10), striped = TRUE, bordered = TRUE)

  output$missingness_table <- renderTable({
    missingness_table(data_reactive())
  }, striped = TRUE, bordered = TRUE)

  output$mode_badge <- renderUI({
    if (isTRUE(input$advanced_mode)) {
      div(class = "mode-badge advanced", strong("Advanced Mode"), span(" GLMMs, crossed structures, optimizer controls, diagnostics, comparison, and missing-data scaffolds."))
    } else {
      div(class = "mode-badge standard", strong("HLM Mode"), span(" Nested Gaussian HLM workflow with centering, equations, APA tables, and reproducible lme4 code."))
    }
  })

  output$model_template_explanation <- renderUI({
    div(class = "template-note", model_template_explanation(input$model_preset %||% "custom"))
  })

  output$group_selector <- renderUI({
    dat <- data_reactive()
    choices <- candidate_group_vars(dat)
    selected <- intersect(c("schoolid", "districtid"), choices)
    tagList(
      selectInput("grouping_vars", "Grouping factors / ID variables", choices = choices, selected = selected, multiple = TRUE),
      selectInput("primary_group", "Primary higher-level group", choices = choices, selected = first_or(selected, first_or(choices)))
    )
  })

  output$group_summary <- renderUI({
    dat <- data_reactive()
    groups <- input$grouping_vars %||% character()
    if (!length(groups)) return(p("Select one or more grouping factors."))
    rows <- lapply(groups, function(g) {
      tags$li(sprintf("%s: %s groups", g, length(unique(dat[[g]]))))
    })
    tags$ul(rows)
  })

  output$group_structure_table <- renderTable({
    group_structure_table(data_reactive(), input$grouping_vars %||% character(), input$structure_type %||% "nested")
  }, striped = TRUE, bordered = TRUE)

  output$group_structure_note <- renderUI({
    groups <- input$grouping_vars %||% character()
    div(class = "mode-note", group_structure_note(groups, input$structure_type %||% "nested"))
  })

  output$group_plot <- renderPlot({
    dat <- data_reactive()
    g <- input$primary_group
    req(g, g %in% names(dat))
    counts <- sort(table(dat[[g]]), decreasing = TRUE)
    plot_df <- data.frame(group = factor(names(counts), levels = names(counts)), n = as.numeric(counts))
    ggplot(plot_df, aes(group, n)) +
      geom_col(fill = "#245a73") +
      coord_flip() +
      labs(x = NULL, y = "Rows per group") +
      theme_minimal(base_size = 12)
  })

  output$outcome_selector <- renderUI({
    dat <- data_reactive()
    selectInput("outcome", "Outcome", choices = names(dat), selected = first_or(intersect("mathscore", names(dat)), first_or(numeric_vars(dat), first_or(names(dat)))))
  })

  output$fixed_selector <- renderUI({
    dat <- data_reactive()
    choices <- setdiff(names(dat), c(input$outcome, input$grouping_vars))
    selected <- intersect(c("ses", "priorachieve", "homework", "meanses", "climate", "schoolsize", "sector", "female", "minority"), choices)
    selectizeInput("fixed_terms", "Predictors", choices = choices, selected = selected, multiple = TRUE, options = list(plugins = list("remove_button")))
  })

  observeEvent(input$binary_preset, {
    updateSelectInput(session, "outcome", selected = "passmath")
    updateSelectInput(session, "distribution", selected = "binomial")
    updateSelectInput(session, "link", selected = "logit")
    updateSelectizeInput(session, "fixed_terms", selected = c("ses", "priorachieve", "homework", "meanses", "climate", "sector", "female", "minority"))
    updateSelectInput(session, "random_slopes", selected = "ses")
    showNotification("Binary logistic mixed-model preset selected.", type = "message")
  })

  observeEvent(input$poisson_preset, {
    updateSelectInput(session, "outcome", selected = "absences")
    updateSelectInput(session, "distribution", selected = "poisson")
    updateSelectInput(session, "link", selected = "log")
    updateSelectizeInput(session, "fixed_terms", selected = c("ses", "priorachieve", "homework", "meanses", "climate", "sector", "female", "minority"))
    updateSelectInput(session, "random_slopes", selected = "ses")
    showNotification("Poisson count mixed-model preset selected.", type = "message")
  })

  observeEvent(input$apply_model_preset, {
    preset <- input$model_preset %||% "custom"
    if (identical(preset, "custom")) {
      showNotification("Keep editing the current custom model.", type = "message")
      return()
    }
    common_fixed <- c("ses", "priorachieve", "homework", "meanses", "climate", "schoolsize", "sector", "female", "minority")
    updateSelectInput(session, "outcome", selected = "mathscore")
    updateSelectInput(session, "distribution", selected = "gaussian")
    updateSelectInput(session, "link", selected = "identity")
    updateCheckboxInput(session, "reml", value = TRUE)
    updateSelectInput(session, "structure_type", selected = "nested")
    updateTextInput(session, "nesting_text", value = "students %in% schools")
    updateSelectizeInput(session, "fixed_terms", selected = common_fixed)
    updateTextInput(session, "interactions", value = "")
    updateSelectInput(session, "random_slopes", selected = character())
    updateCheckboxInput(session, "random_correlation", value = TRUE)
    updateCheckboxGroupInput(session, "random_intercept_groups", selected = input$grouping_vars %||% c("schoolid", "districtid"))
    if (identical(preset, "null")) {
      updateSelectizeInput(session, "fixed_terms", selected = character())
    } else if (identical(preset, "random_intercept")) {
      updateSelectizeInput(session, "fixed_terms", selected = c("ses", "meanses", "sector"))
    } else if (identical(preset, "random_slope")) {
      updateSelectizeInput(session, "fixed_terms", selected = c("ses", "priorachieve", "homework", "meanses", "sector"))
      updateSelectInput(session, "random_slopes", selected = c("ses"))
    } else if (identical(preset, "cross_level")) {
      updateSelectizeInput(session, "fixed_terms", selected = common_fixed)
      updateSelectInput(session, "random_slopes", selected = c("ses", "homework"))
      updateTextInput(session, "interactions", value = "ses:sector, homework:climate")
    } else if (identical(preset, "three_level")) {
      updateSelectInput(session, "structure_type", selected = "nested")
      updateTextInput(session, "nesting_text", value = "students %in% schools %in% districts")
      updateSelectInput(session, "grouping_vars", selected = c("schoolid", "districtid"))
      updateSelectizeInput(session, "fixed_terms", selected = common_fixed)
      updateSelectInput(session, "random_slopes", selected = c("ses", "homework"))
    } else if (identical(preset, "crossed")) {
      updateSelectInput(session, "structure_type", selected = "crossed")
      updateTextInput(session, "nesting_text", value = "students crossed by schools and teachers")
      updateSelectInput(session, "grouping_vars", selected = c("schoolid", "teacherid"))
      updateSelectizeInput(session, "fixed_terms", selected = c("ses", "priorachieve", "homework", "meanses", "sector", "female", "minority"))
      updateSelectInput(session, "random_slopes", selected = c("ses"))
    } else if (identical(preset, "logistic")) {
      updateCheckboxInput(session, "advanced_mode", value = TRUE)
      updateSelectInput(session, "outcome", selected = "passmath")
      updateSelectInput(session, "distribution", selected = "binomial")
      updateSelectInput(session, "link", selected = "logit")
      updateSelectizeInput(session, "fixed_terms", selected = c("ses", "priorachieve", "homework", "meanses", "climate", "sector", "female", "minority"))
      updateSelectInput(session, "random_slopes", selected = "ses")
    } else if (identical(preset, "poisson")) {
      updateCheckboxInput(session, "advanced_mode", value = TRUE)
      updateSelectInput(session, "outcome", selected = "absences")
      updateSelectInput(session, "distribution", selected = "poisson")
      updateSelectInput(session, "link", selected = "log")
      updateSelectizeInput(session, "fixed_terms", selected = c("ses", "priorachieve", "homework", "meanses", "climate", "sector", "female", "minority"))
      updateSelectInput(session, "random_slopes", selected = "ses")
    }
    showNotification(sprintf("%s template applied.", names(which(c(
      "Null / unconditional model" = "null",
      "Random intercept model" = "random_intercept",
      "Random slope model" = "random_slope",
      "Cross-level interaction model" = "cross_level",
      "Three-level nested model" = "three_level",
      "Crossed school + teacher model" = "crossed",
      "Logistic HGLM" = "logistic",
      "Poisson HGLM" = "poisson"
    ) == preset))), type = "message")
  })

  output$variable_palette <- renderUI({
    dat <- data_reactive()
    tags$div(
      class = "variable-palette",
      lapply(names(dat), function(nm) {
        tags$span(class = if (is.numeric(dat[[nm]])) "var-pill numeric" else "var-pill factor", nm)
      })
    )
  })

  output$centering_controls <- renderUI({
    dat <- data_reactive()
    terms <- input$fixed_terms %||% character()
    numeric_terms <- terms[terms %in% numeric_vars(dat)]
    if (!length(numeric_terms)) return(p("No numeric predictors selected for centering."))
    tagList(lapply(numeric_terms, function(term) {
      selectInput(paste0("center_", term), paste("Center", term), c("none", "GMC", "CWC"), selected = if (term %in% c("ses", "priorachieve", "homework")) "CWC" else if (term %in% c("meanses", "climate", "schoolsize")) "GMC" else "none")
    }))
  })

  fixed_spec <- reactive({
    dat <- data_reactive()
    terms <- input$fixed_terms %||% character()
    out <- lapply(terms, function(term) {
      center <- if (term %in% numeric_vars(dat)) input[[paste0("center_", term)]] %||% "none" else "none"
      list(center = center)
    })
    names(out) <- terms
    out
  })

  interaction_spec <- reactive({
    txt <- trimws(input$interactions %||% "")
    if (!nzchar(txt)) return(list())
    pieces <- strsplit(txt, ",", fixed = TRUE)[[1]]
    Filter(function(x) length(x) > 1, lapply(pieces, function(piece) trimws(strsplit(piece, ":", fixed = TRUE)[[1]])))
  })

  random_spec <- reactive({
    groups <- input$grouping_vars %||% character()
    intercept_groups <- input$random_intercept_groups %||% groups
    slope_group <- input$random_group_focus %||% input$primary_group %||% first_or(groups)
    if (!length(groups)) return(list())
    out <- lapply(groups, function(group_name) {
      list(
        intercept = group_name %in% intercept_groups,
        slopes = if (identical(group_name, slope_group)) input$random_slopes %||% character() else character(),
        correlation = isTRUE(input$random_correlation)
      )
    })
    names(out) <- groups
    out
  })

  grouping_spec <- reactive({
    groups <- input$grouping_vars %||% character()
    out <- as.list(groups)
    names(out) <- groups
    out
  })

  spec_reactive <- reactive({
    req(input$outcome)
    mlm_spec(
      outcome = input$outcome,
      distribution = if (isTRUE(input$advanced_mode)) input$distribution %||% "gaussian" else "gaussian",
      link = if (isTRUE(input$advanced_mode)) input$link %||% "identity" else "identity",
      fixed = fixed_spec(),
      grouping = grouping_spec(),
      nesting = input$nesting_text,
      structure = input$structure_type %||% "nested",
      random = random_spec(),
      interactions = interaction_spec(),
      data = data_reactive()
    )
  })

  output$fixed_preview <- renderTable({
    fixed <- fixed_spec()
    if (!length(fixed)) return(data.frame(term = character(), centering = character()))
    data.frame(term = names(fixed), centering = vapply(fixed, function(x) x$center, character(1)), row.names = NULL)
  }, striped = TRUE, bordered = TRUE)

  output$variable_role_table <- renderTable({
    variable_role_table(data_reactive(), spec_reactive())
  }, striped = TRUE, bordered = TRUE)

  output$model_builder_summary <- renderTable({
    model_builder_summary(spec_reactive())
  }, striped = TRUE, bordered = TRUE)

  output$formula_preview <- renderText({
    paste(deparse(build_formula(spec_reactive())), collapse = "\n")
  })

  output$random_group_selector <- renderUI({
    groups <- input$grouping_vars %||% character()
    if (!length(groups)) return(p("Choose grouping factors in the Data tab first."))
    selectInput("random_group_focus", "Edit random effects for", choices = groups, selected = input$primary_group %||% groups[[1]])
  })

  output$random_intercept_selector <- renderUI({
    groups <- input$grouping_vars %||% character()
    if (!length(groups)) return(NULL)
    checkboxGroupInput("random_intercept_groups", "Random intercepts for", choices = groups, selected = groups)
  })

  output$random_slope_selector <- renderUI({
    choices <- names(fixed_spec())
    selectInput("random_slopes", "Random slopes", choices = choices, selected = intersect(c("ses", "priorachieve", "homework"), choices), multiple = TRUE)
  })

  output$random_preview <- renderText({
    formula <- build_formula(spec_reactive())
    rand <- random_spec()
    paste(
      paste("Formula:", deparse(formula)),
      paste("Grouping note:", input$nesting_text %||% ""),
      paste("Structure:", input$structure_type %||% "nested"),
      paste("Random effect blocks:", paste(names(rand), collapse = ", ")),
      sep = "\n"
    )
  })

  output$vcov_template <- renderPlot({
    rand <- random_spec()
    slopes <- if (length(rand)) rand[[1]]$slopes else character()
    labels <- c(if (isTRUE(input$random_intercept)) "Intercept", slopes)
    req(length(labels))
    grid <- expand.grid(row = labels, col = labels)
    grid$value <- if (isTRUE(input$random_correlation)) 1 else as.numeric(grid$row == grid$col)
    ggplot(grid, aes(col, row, fill = value)) +
      geom_tile(color = "white") +
      scale_fill_gradient(low = "#eef3f5", high = "#245a73", guide = "none") +
      labs(x = NULL, y = NULL) +
      theme_minimal(base_size = 13)
  })

  output$estimation_preview <- renderTable({
    rbind(
      data.frame(
      setting = c("Distribution", "Grouping structure", "REML", "Optimizer", "Max function evaluations"),
      value = c(spec_reactive()$distribution, spec_reactive()$structure, as.character(input$reml), input$optimizer, input$maxfun)
      ),
      data.frame(
        setting = names(model_missingness(data_reactive(), spec_reactive())),
        value = as.character(unlist(model_missingness(data_reactive(), spec_reactive()), use.names = FALSE))
      )
    )
  }, striped = TRUE, bordered = TRUE)

  output$model_readiness_table <- renderTable({
    model_readiness_table(data_reactive(), spec_reactive())
  }, striped = TRUE, bordered = TRUE)

  output$model_missingness_table <- renderTable({
    model_missingness_table(data_reactive(), spec_reactive())
  }, striped = TRUE, bordered = TRUE)

  output$complete_case_impact <- renderTable({
    model_missingness(data_reactive(), spec_reactive())
  }, striped = TRUE, bordered = TRUE)

  output$missing_pattern_table <- renderTable({
    missing_pattern_table(data_reactive(), spec_reactive())
  }, striped = TRUE, bordered = TRUE)

  output$imputation_code_panel <- renderText({
    current_formula <- tryCatch(build_formula(spec_reactive()), error = function(e) NULL)
    paste(imputation_repro_code(spec_reactive(), current_formula), collapse = "\n")
  })

  run_fit <- function(optimizer) {
    readiness <- model_readiness_table(data_reactive(), spec_reactive())
    if (model_readiness_has_stops(readiness)) {
      fit_state(list(result = NULL, error = "Model readiness checks found issues that must be fixed before fitting.", optimizer = optimizer))
      showNotification("Fix Model Readiness stop items before fitting.", type = "error")
      updateNavbarPage(session, "main_nav", selected = "Estimate")
      return(invisible(NULL))
    }
    withProgress(message = "Fitting multilevel model", value = 0.4, {
      tryCatch(
        {
          result <- mlm_fit(spec_reactive(), REML = isTRUE(input$reml), optimizer = optimizer, maxfun = input$maxfun)
          fit_state(list(result = result, error = NULL, optimizer = optimizer))
          history <- model_history()
          label <- sprintf("Model %s", length(history) + 1)
          history[[label]] <- list(result = result, optimizer = optimizer, REML = isTRUE(input$reml), maxfun = input$maxfun)
          model_history(history)
          showNotification("Model fit completed. Results are ready.", type = "message")
          updateNavbarPage(session, "main_nav", selected = "Results")
        },
        error = function(e) {
          fit_state(list(result = NULL, error = conditionMessage(e), optimizer = optimizer))
          showNotification("Model fit failed. See Fit Status for details.", type = "error")
        }
      )
    })
  }

  observeEvent(input$fit_model, {
    run_fit(input$optimizer)
  })

  run_example_fit <- function() {
    withProgress(message = "Fitting example model", value = 0.4, {
      tryCatch(
        {
          dat <- example_hsb()
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
            grouping = list(schoolid = "schoolid", districtid = "districtid"),
            nesting = "students %in% schools %in% districts",
            structure = "nested",
            random = list(
              schoolid = list(intercept = TRUE, slopes = c("ses", "priorachieve", "homework"), correlation = TRUE),
              districtid = list(intercept = TRUE, slopes = character(), correlation = TRUE)
            ),
            interactions = list(c("ses", "sector"), c("homework", "climate")),
            data = dat
          )
          result <- mlm_fit(spec, REML = isTRUE(input$reml), optimizer = input$optimizer, maxfun = input$maxfun)
          fit_state(list(result = result, error = NULL, optimizer = input$optimizer))
          history <- model_history()
          label <- sprintf("Example Model %s", length(history) + 1)
          history[[label]] <- list(result = result, optimizer = input$optimizer, REML = isTRUE(input$reml), maxfun = input$maxfun)
          model_history(history)
          showNotification("Example model fit completed. Results are ready.", type = "message")
          updateNavbarPage(session, "main_nav", selected = "Results")
          updateTabsetPanel(session, "results_section", selected = "Dashboard")
        },
        error = function(e) {
          fit_state(list(result = NULL, error = conditionMessage(e), optimizer = input$optimizer))
          showNotification("Example model fit failed. See Fit Status for details.", type = "error")
        }
      )
    })
  }

  observeEvent(input$fit_example, {
    run_example_fit()
  })

  observeEvent(input$fit_demo_from_data, {
    run_example_fit()
  })

  observeEvent(input$refit_model, {
    run_fit("Nelder_Mead")
  })

  active_result <- reactive(fit_state()$result)
  active_error <- reactive(fit_state()$error)

  output$fit_status <- renderText({
    res <- active_result()
    if (is.null(res)) {
      if (!is.null(active_error())) return("Model fit failed.")
      return("Model has not been fit yet. Click Fit Model, then the app will switch to Results when the fit is complete.")
    }
    diag <- mlm_diagnostics(res$fit)
    paste(c(
      sprintf("Model fit completed with optimizer: %s", fit_state()$optimizer %||% input$optimizer),
      sprintf("Formula: %s", paste(deparse(res$formula), collapse = " ")),
      paste(diag$check, diag$value, sep = ": ")
    ), collapse = "\n")
  })

  output$fit_error <- renderUI({
    err <- active_error()
    if (is.null(err)) return(NULL)
    div(class = "error-box", strong("Fit error"), p(err))
  })

  output$results_status <- renderText({
    res <- active_result()
    if (is.null(res)) {
      if (!is.null(active_error())) return("The last fit failed. The error is shown below.")
      return("No model has been fit yet. Go to Estimate and click Fit Model.")
    }
    sprintf("Showing results for: %s", paste(deparse(res$formula), collapse = " "))
  })

  output$results_error <- renderUI({
    err <- active_error()
    if (is.null(err)) return(NULL)
    div(class = "error-box", strong("Fit error"), p(err))
  })

  output$dashboard_status <- renderUI({
    res <- active_result()
    err <- active_error()
    if (!is.null(res)) {
      return(div(
        class = "dashboard-status success",
        strong("Model fit complete"),
        span(sprintf(" %s", paste(deparse(res$formula), collapse = " ")))
      ))
    }
    if (!is.null(err)) {
      return(div(class = "dashboard-status danger", strong("Last fit failed"), span(" Review the Fit Status card below.")))
    }
    div(class = "dashboard-status waiting", strong("No fitted model yet"), span(" Build the model, then click Fit Model on the Estimate tab."))
  })

  output$dashboard_actions <- renderUI({
    tagList(
      actionButton("go_estimation", "Fit or Refit Model", class = "btn-primary")
    )
  })

  observeEvent(input$go_estimation, updateNavbarPage(session, "main_nav", selected = "Estimate"))

  output$dashboard_summary <- renderTable({
    res <- active_result()
    if (is.null(res)) {
      return(data.frame(Metric = c("Outcome", "Formula preview", "Grouping structure"), Value = c(input$outcome %||% "", paste(deparse(build_formula(spec_reactive())), collapse = " "), input$structure_type %||% "nested"), check.names = FALSE))
    }
    model_summary_cards(res, fit_state()$optimizer %||% input$optimizer)
  }, striped = TRUE, bordered = TRUE)

  output$dashboard_guidance <- renderUI({
    res <- active_result()
    if (is.null(res)) {
      return(tags$ul(
        tags$li("Choose a model template if you want a fast starting point."),
        tags$li("Check centering choices before fitting random slopes."),
        tags$li("Use ML rather than REML when comparing nested fixed-effect models.")
      ))
    }
    tags$ul(lapply(model_guidance(res, isTRUE(input$reml)), tags$li))
  })

  output$equations <- renderText({
    res <- active_result()
    if (is.null(res)) return("Fit a model to generate equations.")
    paste(mlm_equations(res), collapse = "\n")
  })

  output$interpretation_panel <- renderUI({
    res <- active_result()
    if (is.null(res)) return(p("Fit a model to generate interpretation guidance."))
    tags$ul(lapply(interpretation_points(res), tags$li))
  })

  output$assumptions_panel <- renderUI({
    res <- active_result()
    if (is.null(res)) return(p("Fit a model to generate assumption checks."))
    tags$ul(lapply(assumption_points(res), tags$li))
  })

  output$latex_equations_results <- renderUI({
    res <- active_result()
    if (is.null(res)) return(p("Fit a model to generate equations."))
    eq <- mlm_latex_equations(res)
    withMathJax(tags$div(class = "equation-stack", lapply(seq_along(eq$level), function(i) {
      tags$div(class = "equation-card", tags$strong(eq$level[[i]]), tags$div(HTML(latex_display_equation(eq$equations[[i]], terms_per_line = 1))))
    })))
  })

  output$combined_equation_results <- renderUI({
    res <- active_result()
    if (is.null(res)) return(p("Fit a model to generate a combined equation."))
    eq <- mlm_latex_equations(res)
    withMathJax(tags$div(class = "equation-card", tags$div(HTML(latex_wrapped_equation(eq$combined, terms_per_line = 2)))))
  })

  output$tau_equations_results <- renderUI({
    res <- active_result()
    if (is.null(res)) return(NULL)
    eq <- mlm_latex_equations(res)
    if (!length(eq$tau)) return(NULL)
    withMathJax(tags$div(
      class = "equation-stack",
      tags$strong("Tau Variance-Covariance Matrix"),
      lapply(eq$tau, function(tau) tags$div(class = "equation-card", tags$p(paste0("\\[", tau, "\\]"))))
    ))
  })

  output$tau_label_table_results <- renderTable({
    res <- active_result()
    req(res)
    tau_label_table(res)
  }, striped = TRUE, bordered = TRUE)

  output$raw_latex_results <- renderText({
    res <- active_result()
    if (is.null(res)) return("Fit a model to generate raw LaTeX.")
    eq <- mlm_latex_equations(res)
    paste(
      "Level equations:",
      paste(eq$equations, collapse = "\n\n"),
      "",
      "Combined equation:",
      eq$combined,
      "",
      "Tau matrices:",
      paste(eq$tau, collapse = "\n\n"),
      sep = "\n"
    )
  })

  output$fixed_table <- renderTable({
    res <- active_result()
    req(res)
    fixed_effects_table(res$fit)
  }, striped = TRUE, bordered = TRUE, digits = 4)

  output$apa_table <- renderTable({
    res <- active_result()
    req(res)
    apa_fixed_table(res$fit)
  }, striped = TRUE, bordered = TRUE)

  output$apa_table_html <- renderUI({
    res <- active_result()
    if (is.null(res)) return(p("Fit a model to generate an APA-style table."))
    HTML(apa_fixed_html(res$fit))
  })

  output$dummy_table_html <- renderUI({
    res <- active_result()
    if (is.null(res)) return(p("Fit a model to generate a dummy-coding table."))
    HTML(apa_dummy_html(res$data, names(res$spec$fixed)))
  })

  output$variance_table_html <- renderUI({
    res <- active_result()
    if (is.null(res)) return(p("Fit a model to generate variance components."))
    HTML(apa_variance_html(res$fit))
  })

  output$icc_table_html <- renderUI({
    res <- active_result()
    if (is.null(res)) return(p("Fit a model to generate ICCs."))
    HTML(apa_icc_html(res$fit))
  })

  output$apa_latex_table <- renderText({
    res <- active_result()
    if (is.null(res)) return("Fit a model to generate a LaTeX table.")
    paste(apa_fixed_latex(res$fit), collapse = "\n")
  })

  output$raw_latex_bundle <- renderText({
    res <- active_result()
    if (is.null(res)) return("Fit a model to generate copy/paste LaTeX.")
    raw_latex_bundle(res)
  })

  output$dummy_table <- renderTable({
    res <- active_result()
    req(res)
    dummy_coding_table(res$data, names(res$spec$fixed))
  }, striped = TRUE, bordered = TRUE)

  output$variance_table <- renderTable({
    res <- active_result()
    req(res)
    variance_table(res$fit)
  }, striped = TRUE, bordered = TRUE, digits = 4)

  output$icc_table <- renderTable({
    res <- active_result()
    req(res)
    icc_table(res$fit)
  }, striped = TRUE, bordered = TRUE, digits = 4)

  output$diagnostics_table <- renderTable({
    res <- active_result()
    req(res)
    mlm_diagnostics(res$fit)
  }, striped = TRUE, bordered = TRUE)

  output$diagnostic_x_selector <- renderUI({
    res <- active_result()
    if (is.null(res)) return(p("Fit a model to choose graph variables."))
    choices <- intersect(names(res$data), numeric_vars(res$data))
    selected <- first_or(intersect(c("ses_CWC", "priorachieve_CWC", "homework_CWC"), choices), first_or(choices))
    selectInput("diagnostic_x", "X variable for model lines", choices = choices, selected = selected)
  })

  output$diagnostic_group_selector <- renderUI({
    res <- active_result()
    if (is.null(res)) return(NULL)
    groups <- unlist(res$spec$grouping, use.names = FALSE)
    selectInput("diagnostic_group", "Grouping factor for model lines", choices = groups, selected = first_or(groups))
  })

  diagnostic_df <- reactive({
    res <- active_result()
    req(res)
    data.frame(
      observed = res$data[[res$spec$outcome]],
      fitted = as.numeric(stats::fitted(res$fit)),
      residual = as.numeric(stats::resid(res$fit)),
      pearson = as.numeric(stats::resid(res$fit, type = "pearson")),
      res$data,
      check.names = FALSE
    )
  })

  output$residual_fitted_plot <- renderPlot({
    df <- diagnostic_df()
    ggplot(df, aes(fitted, residual)) +
      geom_hline(yintercept = 0, color = "#8a969c") +
      geom_point(alpha = 0.62, color = "#245a73") +
      geom_smooth(method = "loess", se = FALSE, color = "#8b3f2f", linewidth = 0.8) +
      labs(x = "Fitted values", y = "Residuals") +
      theme_minimal(base_size = 12)
  })

  output$qq_plot <- renderPlot({
    df <- diagnostic_df()
    ggplot(df, aes(sample = residual)) +
      stat_qq(color = "#245a73", alpha = 0.7) +
      stat_qq_line(color = "#8b3f2f", linewidth = 0.8) +
      labs(x = "Theoretical quantiles", y = "Sample residual quantiles") +
      theme_minimal(base_size = 12)
  })

  output$observed_fitted_plot <- renderPlot({
    df <- diagnostic_df()
    rng <- range(c(df$observed, df$fitted), na.rm = TRUE)
    ggplot(df, aes(fitted, observed)) +
      geom_abline(intercept = 0, slope = 1, color = "#8a969c") +
      geom_point(alpha = 0.62, color = "#245a73") +
      coord_equal(xlim = rng, ylim = rng) +
      labs(x = "Fitted values", y = "Observed values") +
      theme_minimal(base_size = 12)
  })

  output$random_effects_interval_plot <- renderPlot({
    res <- active_result()
    req(res)
    re <- as.data.frame(lme4::ranef(res$fit, condVar = TRUE))
    req(nrow(re) > 0)
    term <- "(Intercept)"
    if (!term %in% re$term) term <- first_or(unique(re$term))
    plot_df <- re[re$term == term, , drop = FALSE]
    plot_df <- plot_df[order(plot_df$condval), , drop = FALSE]
    plot_df$grp <- factor(plot_df$grp, levels = plot_df$grp)
    plot_df$low <- plot_df$condval - 1.96 * plot_df$condsd
    plot_df$high <- plot_df$condval + 1.96 * plot_df$condsd
    ggplot(plot_df, aes(condval, grp)) +
      geom_vline(xintercept = 0, color = "#8a969c") +
      geom_errorbarh(aes(xmin = low, xmax = high), height = 0, color = "#6b7a83") +
      geom_point(color = "#245a73", size = 2) +
      labs(x = paste("Empirical Bayes estimate:", term), y = NULL) +
      theme_minimal(base_size = 12)
  })

  output$model_lines_plot <- renderPlot({
    res <- active_result()
    req(res, input$diagnostic_x, input$diagnostic_group)
    df <- diagnostic_df()
    req(input$diagnostic_x %in% names(df), input$diagnostic_group %in% names(df))
    group_counts <- sort(table(df[[input$diagnostic_group]]), decreasing = TRUE)
    keep <- names(group_counts)[seq_len(min(8, length(group_counts)))]
    plot_df <- df[df[[input$diagnostic_group]] %in% keep, , drop = FALSE]
    plot_df <- plot_df[order(plot_df[[input$diagnostic_group]], plot_df[[input$diagnostic_x]]), , drop = FALSE]
    ggplot(plot_df, aes(x = .data[[input$diagnostic_x]], y = observed, color = .data[[input$diagnostic_group]])) +
      geom_point(alpha = 0.45) +
      geom_line(aes(y = fitted, group = .data[[input$diagnostic_group]]), linewidth = 0.9) +
      labs(x = input$diagnostic_x, y = res$spec$outcome, color = input$diagnostic_group) +
      theme_minimal(base_size = 12)
  })

  output$model_compare_table <- renderTable({
    history <- model_history()
    if (!length(history)) {
      return(data.frame(Message = "Fit one or more models to populate model comparison."))
    }
    rows <- lapply(names(history), function(label) {
      fit <- history[[label]]$result$fit
      data.frame(
        Model = label,
        REML = history[[label]]$REML,
        Optimizer = history[[label]]$optimizer,
        AIC = round(stats::AIC(fit), 2),
        BIC = round(stats::BIC(fit), 2),
        logLik = round(as.numeric(stats::logLik(fit)), 2),
        Deviance = round(stats::deviance(fit), 2),
        Parameters = attr(stats::logLik(fit), "df"),
        Singular = lme4::isSingular(fit, tol = 1e-4),
        check.names = FALSE
      )
    })
    do.call(rbind, rows)
  }, striped = TRUE, bordered = TRUE)

  output$lrt_note <- renderUI({
    history <- model_history()
    if (length(history) < 2) return(p("Fit at least two models to request a likelihood-ratio comparison."))
    div(
      class = "mode-note",
      "Likelihood-ratio comparisons are most meaningful for nested models fit to the same data. REML-fitted Gaussian models are refit with ML for comparison when possible."
    )
  })

  output$lrt_table <- renderTable({
    history <- model_history()
    req(length(history) >= 2)
    fits <- lapply(history, function(x) x$result$fit)
    out <- tryCatch(
      do.call(stats::anova, c(fits, list(refit = TRUE))),
      error = function(e) data.frame(Message = conditionMessage(e))
    )
    as.data.frame(out)
  }, striped = TRUE, bordered = TRUE, digits = 4)

  output$model_formula_history <- renderText({
    history <- model_history()
    if (!length(history)) return("No models stored yet.")
    paste(vapply(names(history), function(label) {
      formula_text <- paste(deparse(history[[label]]$result$formula, width.cutoff = 500), collapse = " ")
      sprintf("%s: %s", label, formula_text)
    }, character(1)), collapse = "\n\n")
  })

  output$caterpillar <- renderPlot({
    res <- active_result()
    req(res)
    re <- lme4::ranef(res$fit)[[1]]
    req(nrow(re) > 0)
    term <- colnames(re)[1]
    plot_df <- data.frame(group = rownames(re), effect = re[[term]])
    plot_df <- plot_df[order(plot_df$effect), ]
    plot_df$group <- factor(plot_df$group, levels = plot_df$group)
    ggplot(plot_df, aes(effect, group)) +
      geom_vline(xintercept = 0, color = "#8a969c") +
      geom_point(color = "#245a73", size = 2.5) +
      labs(x = paste("BLUP:", term), y = NULL) +
      theme_minimal(base_size = 12)
  })

  output$code_panel <- renderText({
    res <- active_result()
    if (is.null(res)) return("Fit a model to generate reproducible R code.")
    paste(generate_repro_code(res, isTRUE(input$reml), input$optimizer, input$maxfun), collapse = "\n")
  })

  output$download_r_code <- downloadHandler(
    filename = function() "mlmr_reproducible_analysis.R",
    content = function(file) {
      res <- active_result()
      text <- if (is.null(res)) {
        "Fit a model to generate reproducible R code."
      } else {
        paste(generate_repro_code(res, isTRUE(input$reml), input$optimizer, input$maxfun), collapse = "\n")
      }
      writeLines(text, file, useBytes = TRUE)
    }
  )

  output$download_latex <- downloadHandler(
    filename = function() "mlmr_tables_and_equations.tex",
    content = function(file) {
      res <- active_result()
      text <- if (is.null(res)) {
        "Fit a model to generate copy/paste LaTeX."
      } else {
        raw_latex_bundle(res)
      }
      writeLines(text, file, useBytes = TRUE)
    }
  )

  output$download_report <- downloadHandler(
    filename = function() "mlmr_model_report.qmd",
    content = function(file) {
      res <- active_result()
      text <- if (is.null(res)) {
        c("# mlmr Model Report", "", "Fit a model to generate a report.")
      } else {
        manuscript_report_markdown(res, isTRUE(input$reml), input$optimizer, input$maxfun)
      }
      writeLines(text, file, useBytes = TRUE)
    }
  )

  output$download_tables_html <- downloadHandler(
    filename = function() "mlmr_apa_tables.html",
    content = function(file) {
      res <- active_result()
      text <- if (is.null(res)) "<p>Fit a model to generate APA tables.</p>" else apa_tables_html_document(res)
      writeLines(text, file, useBytes = TRUE)
    }
  )

  output$download_tables_doc <- downloadHandler(
    filename = function() "mlmr_apa_tables.doc",
    content = function(file) {
      res <- active_result()
      text <- if (is.null(res)) "<p>Fit a model to generate APA tables.</p>" else apa_tables_html_document(res)
      writeLines(text, file, useBytes = TRUE)
    }
  )

  output$download_tables_tex <- downloadHandler(
    filename = function() "mlmr_apa_tables.tex",
    content = function(file) {
      res <- active_result()
      text <- if (is.null(res)) "% Fit a model to generate APA tables." else apa_tables_latex_document(res)
      writeLines(text, file, useBytes = TRUE)
    }
  )
}

shinyApp(ui, server)
