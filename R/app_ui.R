mlmr_www_dir <- function() {
  local_candidates <- c(
    file.path(getwd(), "inst", "app", "www"),
    file.path(getwd(), "www"),
    file.path(dirname(getwd()), "app", "www")
  )
  existing_local <- local_candidates[dir.exists(local_candidates)]
  if (length(existing_local)) {
    return(normalizePath(existing_local[[1]], winslash = "/", mustWork = TRUE))
  }
  pkg_www <- system.file("app", "www", package = "mlmr")
  if (nzchar(pkg_www) && dir.exists(pkg_www)) return(pkg_www)
  ""
}

copy_button_tag <- function(target_id, label = "Copy") {
  shiny::tags$button(
    type = "button",
    class = "btn btn-outline-secondary copy-button",
    onclick = sprintf("copyOutputText('%s', this)", target_id),
    label
  )
}

app_ui <- function() {
  www_dir <- mlmr_www_dir()
  if (nzchar(www_dir)) {
    shiny::addResourcePath("mlmr-assets", www_dir)
  }

  theme <- bslib::bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#3f6f78",
    secondary = "#6e7f8d",
    base_font = bslib::font_collection("Source Sans Pro", "Segoe UI", "Arial", "sans-serif"),
    heading_font = bslib::font_collection("Source Serif Pro", "Georgia", "Times New Roman", "serif")
  )

  default_df_method <- if (requireNamespace("lmerTest", quietly = TRUE)) "satterthwaite" else "none"

  bslib::page_navbar(
    title = "mlmr",
    id = "main_nav",
    theme = theme,
    header = shiny::tags$head(
      shiny::tags$link(rel = "stylesheet", type = "text/css", href = "mlmr-assets/style.css"),
      shiny::tags$script(shiny::HTML("
        function copyOutputText(id, btn) {
          const el = document.getElementById(id);
          if (!el) return;
          const text = el.innerText || el.textContent || '';
          if (navigator.clipboard && navigator.clipboard.writeText) {
            navigator.clipboard.writeText(text).then(function() {
              if (btn) {
                const prev = btn.innerText;
                btn.innerText = 'Copied!';
                setTimeout(function() { btn.innerText = prev; }, 1400);
              }
            });
          }
        }
      "))
    ),
    bslib::nav_spacer(),
    bslib::nav_item(shiny::div(class = "theme-toggle", bslib::input_dark_mode(id = "theme_mode", mode = "light"))),
    bslib::nav_panel(
      "Data",
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          shiny::fileInput(
            "data_file",
            "Upload data file",
            accept = c(".csv", ".txt", ".tsv", ".xlsx", ".xls", ".sav", ".por", ".sas7bdat", ".xpt", ".dta")
          ),
          shiny::checkboxInput("use_example_data", "Use built-in example data", TRUE),
          shiny::conditionalPanel(
            "input.use_example_data == true",
            shiny::selectInput(
              "example_dataset",
              "Built-in dataset",
              choices = c(
                "HSB Educational (students in schools)" = "hsb",
                "Longitudinal Growth (waves in students)" = "longitudinal"
              ),
              selected = "hsb"
            )
          ),
          shiny::checkboxInput("strings_as_factors", "Convert text / labelled columns to factors", TRUE),
          shiny::uiOutput("data_source_note"),
          shiny::uiOutput("group_selector"),
          shiny::selectInput(
            "structure_type",
            "Grouping structure",
            choices = c(
              "Strict nested" = "nested",
              "Crossed / non-nested" = "crossed",
              "Multiple membership scaffold" = "multiple membership"
            ),
            selected = "nested"
          ),
          shiny::textInput("nesting_text", "Nesting or membership note", "students %in% schools"),
          shiny::checkboxInput("advanced_mode", "Advanced mode", FALSE),
          shiny::uiOutput("mode_badge"),
          shiny::conditionalPanel(
            "input.advanced_mode == true",
            shiny::div(
              class = "mode-note",
              shiny::strong("Advanced mode"),
              shiny::p("Adds GLMM distribution/link controls and exposes settings intended for non-Gaussian outcomes. The standard HLM workflow can stay in Gaussian mode.")
            )
          )
        ),
        shiny::tabsetPanel(
          id = "data_section",
          shiny::tabPanel(
            "Overview",
            shiny::br(),
            bslib::card(
              bslib::card_header("Try the Built-In Demo"),
              shiny::p("Fit the preset example model to inspect APA tables, equations, Tau matrices, diagnostics, and reproducible code in under a minute."),
              shiny::actionButton("fit_demo_from_data", "Fit Example Model", class = "btn-primary")
            ),
            bslib::card(bslib::card_header("Preview"), shiny::div(class = "scroll-table", shiny::tableOutput("data_preview")))
          ),
          shiny::tabPanel(
            "Structure",
            shiny::br(),
            bslib::layout_columns(
              col_widths = c(5, 7),
              bslib::card(
                bslib::card_header("Grouping Summary"),
                shiny::uiOutput("group_summary"),
                shiny::tableOutput("group_structure_table"),
                shiny::uiOutput("group_structure_note")
              ),
              bslib::card(bslib::card_header("Rows Per Group"), shiny::plotOutput("group_plot", height = "360px"))
            )
          ),
          shiny::tabPanel(
            "Missingness",
            shiny::br(),
            bslib::layout_columns(
              col_widths = c(6, 6),
              bslib::card(bslib::card_header("All Variables"), shiny::div(class = "scroll-table", shiny::tableOutput("missingness_table"))),
              bslib::card(bslib::card_header("Model Variables"), shiny::div(class = "scroll-table", shiny::tableOutput("model_missingness_table"))),
              bslib::card(bslib::card_header("Complete-Case Impact"), shiny::tableOutput("complete_case_impact")),
              bslib::card(bslib::card_header("Missing-Data Patterns"), shiny::div(class = "scroll-table", shiny::tableOutput("missing_pattern_table")))
            ),
            bslib::card(
              bslib::card_header("Multiple Imputation Code"),
              shiny::p("This scaffold follows the current model specification and is ready to adapt once you choose an imputation strategy for a manuscript."),
              shiny::verbatimTextOutput("imputation_code_panel")
            )
          ),
          shiny::tabPanel(
            "Roles",
            shiny::br(),
            bslib::card(bslib::card_header("Variable Roles"), shiny::div(class = "scroll-table", shiny::tableOutput("variable_role_table")))
          )
        )
      )
    ),
    bslib::nav_panel(
      "Model",
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          shiny::uiOutput("outcome_selector"),
          shiny::conditionalPanel(
            "input.advanced_mode == true",
            shiny::div(
              class = "preset-row",
              shiny::actionButton("binary_preset", "Binary Example"),
              shiny::actionButton("poisson_preset", "Count Example")
            ),
            shiny::selectInput("distribution", "Outcome distribution", c("gaussian", "binomial", "poisson", "negative binomial", "Gamma"), selected = "gaussian"),
            shiny::selectInput("link", "Link", c("identity", "logit", "probit", "log", "inverse"), selected = "identity"),
            shiny::div(class = "mode-note", "For HLM-style Gaussian models, leave Advanced mode off.")
          ),
          shiny::selectInput(
            "model_preset",
            "Model template",
            choices = c(
              "Current custom model" = "custom",
              "Null / unconditional model" = "null",
              "Random intercept model" = "random_intercept",
              "Random slope model" = "random_slope",
              "Cross-level interaction model" = "cross_level",
              "Three-level nested model" = "three_level",
              "Longitudinal growth model" = "longitudinal",
              "Crossed school + teacher model" = "crossed",
              "Logistic HGLM" = "logistic",
              "Poisson HGLM" = "poisson"
            ),
            selected = "custom"
          ),
          shiny::actionButton("apply_model_preset", "Apply Template", class = "btn-outline-primary"),
          shiny::uiOutput("model_template_explanation"),
          shiny::uiOutput("fixed_selector"),
          shiny::uiOutput("centering_controls"),
          shiny::textInput("interactions", "Interactions, comma separated", value = "ses:sector, homework:climate", placeholder = "ses:sector, ses:meanses"),
          shiny::hr(),
          shiny::uiOutput("random_intercept_selector"),
          shiny::uiOutput("random_group_selector"),
          shiny::uiOutput("random_slope_selector"),
          shiny::checkboxInput("random_correlation", "Correlated intercept/slope effects (focused group)", TRUE)
        ),
        shiny::div(
          class = "model-summary-page",
          shiny::br(),
          bslib::layout_columns(
            col_widths = c(5, 7),
            bslib::card(bslib::card_header("Model Builder Summary"), shiny::tableOutput("model_builder_summary")),
            bslib::card(bslib::card_header("Working Formula"), shiny::verbatimTextOutput("formula_preview"))
          ),
          bslib::card(
            bslib::card_header("Declared Predictors by Level"),
            shiny::div(class = "mode-note", "Place predictors into the level where they belong conceptually. Variation across declared grouping factors is checked automatically."),
            shiny::div(class = "scroll-table", shiny::tableOutput("predictor_level_table"))
          ),
          bslib::card(
            bslib::card_header("Random-Effects Covariance (Tau) Preview"),
            shiny::plotOutput("vcov_template", height = "260px"),
            shiny::uiOutput("vcov_template_note")
          ),
          bslib::card(bslib::card_header("Available Variables"), shiny::uiOutput("variable_palette"))
        )
      )
    ),
    bslib::nav_panel(
      "Estimate",
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          shiny::checkboxInput("reml", "Use REML for Gaussian models", TRUE),
          shiny::selectInput(
            "df_method",
            "Gaussian degrees of freedom",
            choices = c(
              "Large-sample normal (lme4)" = "none",
              "Satterthwaite (lmerTest)" = "satterthwaite",
              "Kenward-Roger (lmerTest + pbkrtest)" = "kenward-roger"
            ),
            selected = default_df_method
          ),
          shiny::selectInput("optimizer", "Optimizer", c("bobyqa", "Nelder_Mead", "nloptwrap", "optimx"), selected = "bobyqa"),
          shiny::numericInput("maxfun", "Maximum function evaluations", value = 30000, min = 1000, step = 1000),
          shiny::actionButton("fit_example", "Fit Example Model", class = "btn-outline-primary"),
          shiny::actionButton("fit_model", "Fit Model", class = "btn-primary"),
          shiny::actionButton("refit_model", "Refit With Nelder_Mead", class = "btn-outline-secondary")
        ),
        bslib::card(
          bslib::card_header("Estimation Choices"),
          shiny::tableOutput("estimation_preview")
        ),
        bslib::card(
          bslib::card_header("Model Readiness"),
          shiny::p("Review these checks before fitting uploaded data or a custom random-effects structure."),
          shiny::div(class = "scroll-table", shiny::tableOutput("model_readiness_table"))
        ),
        bslib::card(
          bslib::card_header("Fit Status"),
          shiny::verbatimTextOutput("fit_status"),
          shiny::uiOutput("fit_error")
        )
      )
    ),
    bslib::nav_panel(
      "Results",
      shiny::div(
        class = "results-page",
        shiny::tabsetPanel(
          id = "results_section",
          shiny::tabPanel(
            "Dashboard",
            shiny::br(),
            bslib::card(
              class = "dashboard-hero",
              bslib::card_header("Model Dashboard"),
              shiny::uiOutput("dashboard_status"),
              shiny::uiOutput("dashboard_actions")
            ),
            bslib::layout_columns(
              col_widths = c(6, 6),
              bslib::card(bslib::card_header("Model Summary"), shiny::tableOutput("dashboard_summary")),
              bslib::card(bslib::card_header("Guidance"), shiny::uiOutput("dashboard_guidance"))
            ),
            bslib::card(bslib::card_header("Fit Status"), shiny::verbatimTextOutput("results_status"), shiny::uiOutput("results_error"))
          ),
          shiny::tabPanel(
            "Tables",
            shiny::br(),
            shiny::uiOutput("inference_notice"),
            bslib::layout_columns(
              col_widths = c(6, 6),
              bslib::card(bslib::card_header("APA Fixed Effects Table"), shiny::uiOutput("apa_table_html")),
              bslib::card(bslib::card_header("Dummy Coding"), shiny::uiOutput("dummy_table_html")),
              bslib::card(bslib::card_header("Variance Components"), shiny::uiOutput("variance_table_html")),
              bslib::card(bslib::card_header("ICC"), shiny::uiOutput("icc_table_html")),
              bslib::card(bslib::card_header("Variance Explained (R2) and Reliability"), shiny::uiOutput("r2_table_html")),
              bslib::card(bslib::card_header("Software and R Packages"), shiny::uiOutput("software_table_html"))
            )
          ),
          shiny::tabPanel(
            "Equations",
            shiny::br(),
            bslib::card(
              bslib::card_header("Level-by-Level Equations"),
              shiny::uiOutput("latex_equations_results"),
              shiny::uiOutput("equation_note_results")
            ),
            bslib::card(bslib::card_header("Combined Full Equation"), shiny::uiOutput("combined_equation_results")),
            bslib::card(
              bslib::card_header("Tau Variance-Covariance Structure"),
              shiny::uiOutput("tau_equations_results"),
              shiny::div(class = "scroll-table", shiny::tableOutput("tau_label_table_results"))
            ),
            bslib::card(
              bslib::card_header("Coefficient Key"),
              shiny::p(class = "mode-note", "Each symbol in the equations matched to its row in the fixed-effects table and whether it carries a random component."),
              shiny::div(class = "scroll-table", shiny::tableOutput("equation_key_results"))
            ),
            bslib::card(
              bslib::card_header("Raw LaTeX"),
              shiny::div(class = "code-actions", copy_button_tag("raw_latex_results", "Copy Equations LaTeX")),
              shiny::verbatimTextOutput("raw_latex_results")
            )
          ),
          shiny::tabPanel(
            "Interpretation",
            shiny::br(),
            bslib::layout_columns(
              col_widths = c(6, 6),
              bslib::card(bslib::card_header("Model Interpretation"), shiny::uiOutput("interpretation_panel")),
              bslib::card(bslib::card_header("Assumptions and Checks"), shiny::uiOutput("assumptions_panel"))
            )
          ),
          shiny::tabPanel(
            "Diagnostics",
            shiny::br(),
            bslib::layout_sidebar(
              sidebar = bslib::sidebar(
                shiny::uiOutput("diagnostic_x_selector"),
                shiny::uiOutput("diagnostic_group_selector"),
                shiny::hr(),
                shiny::uiOutput("ranef_group_selector"),
                shiny::uiOutput("ranef_term_selector")
              ),
              shiny::div(
                class = "results-page",
                bslib::card(bslib::card_header("Diagnostics"), shiny::tableOutput("diagnostics_table")),
                bslib::layout_columns(
                  col_widths = c(6, 6),
                  bslib::card(bslib::card_header("Residuals vs Fitted"), shiny::plotOutput("residual_fitted_plot", height = "340px")),
                  bslib::card(bslib::card_header("Normal QQ Plot"), shiny::plotOutput("qq_plot", height = "340px")),
                  bslib::card(bslib::card_header("Observed vs Fitted"), shiny::plotOutput("observed_fitted_plot", height = "340px")),
                  bslib::card(bslib::card_header("Random Effects With Intervals"), shiny::plotOutput("random_effects_interval_plot", height = "340px"))
                ),
                bslib::card(bslib::card_header("Model-Implied Lines by Group"), shiny::plotOutput("model_lines_plot", height = "460px"))
              )
            )
          ),
          shiny::tabPanel(
            "Compare",
            shiny::br(),
            bslib::card(
              bslib::card_header("Model Fit Comparison"),
              shiny::div(
                class = "code-actions",
                shiny::actionButton("clear_model_history", "Clear Comparison History", class = "btn-outline-secondary")
              ),
              shiny::div(class = "scroll-table", shiny::tableOutput("model_compare_table"))
            ),
            bslib::card(
              bslib::card_header("Likelihood-Ratio Comparison"),
              shiny::uiOutput("lrt_note"),
              shiny::div(class = "scroll-table", shiny::tableOutput("lrt_table"))
            ),
            bslib::card(bslib::card_header("Stored Model Formulas"), shiny::verbatimTextOutput("model_formula_history"))
          )
        )
      )
    ),
    bslib::nav_panel(
      "Report & Code",
      shiny::div(
        class = "results-page",
        bslib::card(
          bslib::card_header("Quarto Report"),
          shiny::p("Download a Quarto-ready report with the model summary, APA tables, R2/reliability, equations, diagnostics, R code, and LaTeX."),
          shiny::downloadButton("download_report", "Download .qmd Report", class = "btn-primary")
        ),
        bslib::layout_columns(
          col_widths = c(6, 6),
          bslib::card(
            bslib::card_header("Publication Readiness Checklist & Archive"),
            shiny::p("Export a complete reproducibility .zip archive (Quarto report, standalone R script, LaTeX, HTML tables, checklist CSV, and software CSV)."),
            shiny::div(class = "scroll-table", shiny::tableOutput("publication_checklist")),
            shiny::downloadButton("download_publication_bundle", "Download Bundle (.zip)", class = "btn-primary")
          ),
          bslib::card(
            bslib::card_header("Methods Text"),
            shiny::p("Review and adapt this language for the target journal, course assignment, or supplemental materials."),
            shiny::div(class = "code-actions", copy_button_tag("methods_text", "Copy Methods Text")),
            shiny::verbatimTextOutput("methods_text")
          )
        ),
        bslib::card(
          full_screen = TRUE,
          bslib::card_header("Reproducible R Code"),
          shiny::div(
            class = "code-actions",
            copy_button_tag("code_panel", "Copy R Code"),
            shiny::downloadButton("download_r_code", "Download .R")
          ),
          shiny::verbatimTextOutput("code_panel")
        ),
        bslib::card(
          bslib::card_header("Copy/Paste Raw LaTeX"),
          shiny::div(
            class = "code-actions",
            copy_button_tag("raw_latex_bundle", "Copy LaTeX"),
            shiny::downloadButton("download_latex", "Download .tex")
          ),
          shiny::verbatimTextOutput("raw_latex_bundle")
        ),
        bslib::card(
          bslib::card_header("APA Table Exports"),
          shiny::p("Export manuscript tables separately for Word, HTML, or LaTeX workflows."),
          shiny::div(
            class = "code-actions",
            shiny::downloadButton("download_tables_doc", "Download Word Tables"),
            shiny::downloadButton("download_tables_html", "Download HTML Tables"),
            shiny::downloadButton("download_tables_tex", "Download LaTeX Tables")
          )
        ),
        bslib::card(
          bslib::card_header("APA Software Statement"),
          shiny::p("Copy this into a manuscript methods, reproducibility, or supplemental materials section."),
          shiny::div(class = "code-actions", copy_button_tag("software_statement", "Copy Software Statement")),
          shiny::verbatimTextOutput("software_statement")
        ),
        bslib::card(
          bslib::card_header("Optional papaja Citation Code"),
          shiny::p("Use this in an APA manuscript workflow to generate R/package citations and BibTeX references with papaja."),
          shiny::div(class = "code-actions", copy_button_tag("papaja_code", "Copy papaja Code")),
          shiny::verbatimTextOutput("papaja_code")
        )
      )
    ),
    bslib::nav_panel(
      "About",
      shiny::div(
        class = "results-page",
        bslib::card(
          bslib::card_header("Purpose"),
          shiny::p("mlmr is a free, open-source Shiny interface and R package for fitting, teaching, and reporting multilevel models in R. The goal is to provide an HLM-style guided workflow while producing transparent lme4 code that users can inspect, copy, rerun, and publish."),
          shiny::p("The app supports uploaded data (including SPSS, SAS, Stata, Excel, CSV, and TSV), built-in HSB and longitudinal datasets, centering decisions, per-level random effects, interactions, denominator degrees of freedom, model diagnostics, APA-style tables, equations, and reproducible exports.")
        ),
        bslib::card(
          bslib::card_header("Project Links"),
          shiny::tags$ul(
            shiny::tags$li(shiny::tags$a("GitHub repository", href = "https://github.com/MarcusHarrisUConn/mlmr", target = "_blank", rel = "noopener noreferrer")),
            shiny::tags$li(shiny::tags$a("Package website and documentation", href = "https://marcusharrisphd.com/mlmr/", target = "_blank", rel = "noopener noreferrer")),
            shiny::tags$li(shiny::tags$a("Creator: Marcus Harris, PhD", href = "https://marcusharrisphd.com", target = "_blank", rel = "noopener noreferrer"))
          )
        ),
        bslib::card(
          bslib::card_header("HLM-Style Feature Parity"),
          shiny::p("This table tracks the goal of replacing expensive HLM-style workflows with a free, transparent R interface."),
          shiny::div(class = "scroll-table", shiny::tableOutput("hlm8_parity_table"))
        ),
        bslib::card(
          bslib::card_header("Validation Benchmarks"),
          shiny::p("These deterministic model templates validate formulas, readiness checks, fitted estimates, SEs, variances, ICCs, and round-trip exported R code."),
          shiny::div(
            class = "code-actions",
            shiny::actionButton("run_benchmark_validation", "Run Fit Validation", class = "btn-outline-primary"),
            shiny::downloadButton("download_validation_report", "Download Validation Report")
          ),
          shiny::div(class = "scroll-table", shiny::tableOutput("validation_benchmarks_table"))
        ),
        bslib::layout_columns(
          col_widths = c(6, 6),
          bslib::card(
            bslib::card_header("Current Strengths"),
            shiny::tags$ul(
              shiny::tags$li("Two- and three-level nested Gaussian multilevel models and longitudinal growth curves."),
              shiny::tags$li("Per-grouping-factor random intercepts, slopes, and correlated or independent Tau structures."),
              shiny::tags$li("Grand-mean and cluster-mean centering controls, plus Satterthwaite and Kenward-Roger degrees of freedom."),
              shiny::tags$li("APA-style fixed effects, variance components, ICC, Nakagawa R2, HLM reliability, and dummy-coding tables."),
              shiny::tags$li("Level-by-level equations, combined equations, and row/column-labeled Tau matrices."),
              shiny::tags$li("Quarto, R script, LaTeX, and .zip publication bundle exports.")
            )
          ),
          bslib::card(
            bslib::card_header("Advanced Workflows"),
            shiny::tags$ul(
              shiny::tags$li("GLMM workflows for binary, count, and positive skewed outcomes with OR / IRR reporting."),
              shiny::tags$li("Crossed and non-nested random-effect structures with mixed-model equation rendering."),
              shiny::tags$li("Missing-data pattern review and mice multiple-imputation scaffold code."),
              shiny::tags$li("Multi-model comparison with sample-compatibility checks and ML-refit likelihood-ratio tests.")
            )
          )
        ),
        bslib::card(
          bslib::card_header("Recommended Workflow"),
          shiny::tags$ol(
            shiny::tags$li("Start on Data to confirm the dataset, grouping factors, structure, missingness, and variable roles."),
            shiny::tags$li("Use Model to choose the outcome, predictors, centering, interactions, and random-effects structure."),
            shiny::tags$li("Use Estimate to verify Model Readiness and fit the model (or fit the built-in example model)."),
            shiny::tags$li("Use Results for dashboard summaries, APA tables, equations, diagnostics, and model comparison."),
            shiny::tags$li("Use Report & Code to export a Quarto report, raw LaTeX, a reproducible R script, or a .zip publication bundle.")
          )
        )
      )
    )
  )
}
