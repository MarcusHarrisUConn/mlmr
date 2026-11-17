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

step_header_ui <- function(num, title) {
  shiny::div(
    class = "builder-step-title",
    shiny::span(class = "step-num", as.character(num)),
    shiny::span(title)
  )
}

distribution_choices_vec <- function() {
  c(
    "Gaussian (Continuous)" = "gaussian",
    "Binomial (Binary 0 / 1)" = "binomial",
    "Poisson (Count)" = "poisson",
    "Negative Binomial (Overdispersed Count)" = "negative binomial",
    "Gamma (Positive Skewed)" = "Gamma"
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
          width = 330,
          shiny::div(
            class = "builder-step",
            step_header_ui(1, "Data Source"),
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
            shiny::uiOutput("data_source_note")
          ),
          shiny::div(
            class = "builder-step",
            step_header_ui(2, "Dependent Variable & Hierarchy"),
            shiny::uiOutput("data_outcome_selector"),
            shiny::selectInput(
              "distribution_data",
              "Outcome distribution",
              choices = distribution_choices_vec(),
              selected = "gaussian"
            ),
            shiny::conditionalPanel(
              "input.distribution_data != 'gaussian'",
              shiny::selectInput("link_data", "Link function", c("logit", "probit", "log", "inverse"), selected = "logit"),
              shiny::div(
                class = "preset-row",
                shiny::actionButton("binary_preset_data", "Binary Logit Demo", class = "btn-outline-primary btn-sm"),
                shiny::actionButton("poisson_preset_data", "Poisson Count Demo", class = "btn-outline-primary btn-sm")
              )
            ),
            shiny::selectInput(
              "design_mode",
              "Multilevel clustering design",
              choices = c(
                "Two-level nested (e.g., students in schools)" = "two_level",
                "Three-level nested (e.g., schools in districts)" = "three_level",
                "Longitudinal growth (waves in persons)" = "longitudinal",
                "Cross-classified (non-nested A x B)" = "crossed"
              ),
              selected = "two_level"
            ),
            shiny::uiOutput("cluster_selectors"),
            shiny::uiOutput("hierarchy_status_badge")
          )
        ),
        shiny::tabsetPanel(
          id = "data_section",
          shiny::tabPanel(
            "Overview",
            shiny::br(),
            bslib::card(
              bslib::card_header("Active Multilevel Model Blueprint (Dependent & Independent Variables)"),
              shiny::uiOutput("home_blueprint_tiles"),
              shiny::div(
                class = "code-actions",
                shiny::actionButton("fit_current_from_home", "Fit Current Model", class = "btn-primary"),
                shiny::actionButton("go_customize_model", "Customize Variables & Equations", class = "btn-outline-primary"),
                shiny::actionButton("fit_demo_from_data", "Reset & Fit HSB Demo", class = "btn-outline-secondary")
              ),
              shiny::tags$strong("Independent Variables (Predictors) in Current Model"),
              shiny::div(class = "scroll-table", shiny::tableOutput("home_active_ivs_table")),
              shiny::tags$strong("Live Multilevel Equations (Pre-Fit)"),
              shiny::uiOutput("home_live_equations")
            ),
            bslib::card(
              bslib::card_header("Dataset Variable Guide (Auto-Classified by Level)"),
              shiny::p(class = "equation-subnote", "Each column is checked against your selected cluster IDs so you can see which columns vary within clusters (Level-1 candidates) versus stay constant within clusters (Level-2/3 candidates)."),
              shiny::div(class = "scroll-table", shiny::tableOutput("auto_variable_guide_table"))
            ),
            bslib::card(
              bslib::card_header("Raw Data Preview (First 10 Rows)"),
              shiny::div(class = "scroll-table", shiny::tableOutput("data_preview"))
            )
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
            bslib::card(bslib::card_header("Variable Roles & Labels"), shiny::div(class = "scroll-table", shiny::tableOutput("variable_role_table")))
          )
        )
      )
    ),
    bslib::nav_panel(
      "Model",
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          width = 360,
          shiny::div(
            class = "builder-step",
            step_header_ui(1, "Outcome (DV) & Model Template"),
            shiny::uiOutput("outcome_selector"),
            shiny::selectInput(
              "distribution",
              "Outcome distribution",
              choices = distribution_choices_vec(),
              selected = "gaussian"
            ),
            shiny::conditionalPanel(
              "input.distribution != 'gaussian'",
              shiny::selectInput("link", "Link function", c("logit", "probit", "log", "inverse"), selected = "logit"),
              shiny::div(
                class = "preset-row",
                shiny::actionButton("binary_preset", "Binary Logit Demo", class = "btn-outline-primary btn-sm"),
                shiny::actionButton("poisson_preset", "Poisson Count Demo", class = "btn-outline-primary btn-sm")
              )
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
                "Logistic mixed model (binary)" = "logistic",
                "Poisson mixed model (count)" = "poisson"
              ),
              selected = "custom"
            ),
            shiny::actionButton("apply_model_preset", "Apply Template", class = "btn-outline-primary"),
            shiny::uiOutput("model_template_explanation")
          ),
          shiny::div(
            class = "builder-step",
            step_header_ui(2, "Level-1 Predictors (Within-Cluster IVs)"),
            shiny::uiOutput("level1_selector_ui"),
            shiny::uiOutput("l1_inline_controls")
          ),
          shiny::div(
            class = "builder-step",
            step_header_ui(3, "Level-2 Predictors & Slope Moderators"),
            shiny::uiOutput("level2_selector_ui"),
            shiny::uiOutput("l2_inline_controls"),
            shiny::uiOutput("cross_level_moderator_controls")
          ),
          shiny::uiOutput("level3_builder_step_ui"),
          shiny::div(
            class = "builder-step",
            step_header_ui(4, "Random-Effects Covariance & Interactions"),
            shiny::uiOutput("random_intercept_selector"),
            shiny::checkboxInput("random_correlation", "Estimate covariances between random intercept & slopes (Tau)", TRUE),
            shiny::uiOutput("within_interaction_picker_ui"),
            shiny::hr(style = "margin: 10px 0;"),
            shiny::checkboxInput("advanced_mode", "Show advanced slope & custom interaction overrides", FALSE),
            shiny::conditionalPanel(
              "input.advanced_mode == true",
              shiny::div(
                class = "advanced-panel-highlight",
                shiny::uiOutput("random_group_selector"),
                shiny::uiOutput("random_slope_selector"),
                shiny::textInput("interactions", "Custom interaction terms (comma separated)", value = "ses:sector, homework:climate", placeholder = "ses:sector, ses:meanses")
              )
            )
          )
        ),
        shiny::div(
          class = "model-summary-page",
          shiny::br(),
          bslib::card(
            bslib::card_header(
              shiny::div(
                style = "display:flex; justify-content:space-between; align-items:center; flex-wrap:wrap; gap:8px;",
                shiny::span("Live Equation Workspace (Updates as You Customize)"),
                shiny::actionButton("fit_from_model_tab", "Estimate This Model", class = "btn-primary btn-sm")
              )
            ),
            shiny::uiOutput("live_model_equations_ui")
          ),
          bslib::layout_columns(
            col_widths = c(6, 6),
            bslib::card(
              bslib::card_header("Generated lme4 Formula & Summary"),
              shiny::verbatimTextOutput("formula_preview"),
              shiny::tableOutput("model_builder_summary")
            ),
            bslib::card(
              bslib::card_header("Random-Effects Covariance (Tau) Preview"),
              shiny::plotOutput("vcov_template", height = "250px"),
              shiny::uiOutput("vcov_template_note")
            )
          ),
          bslib::card(
            bslib::card_header("Declared Predictors, Centering & Empirical Variation Check"),
            shiny::div(class = "scroll-table", shiny::tableOutput("predictor_level_table"))
          ),
          bslib::card(bslib::card_header("Available Dataset Variables"), shiny::uiOutput("variable_palette"))
        )
      )
    ),
    bslib::nav_panel(
      "Estimate",
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          shiny::checkboxInput("reml", "Use REML for Gaussian models (Full ML when unchecked)", TRUE),
          shiny::selectInput(
            "df_method",
            "Gaussian degrees of freedom",
            choices = c(
              "Satterthwaite (lmerTest)" = "satterthwaite",
              "Kenward-Roger (lmerTest + pbkrtest)" = "kenward-roger",
              "Large-sample normal (lme4 default)" = "none"
            ),
            selected = default_df_method
          ),
          shiny::selectInput("optimizer", "Optimizer", c("bobyqa", "Nelder_Mead", "nloptwrap", "optimx"), selected = "bobyqa"),
          shiny::numericInput("maxfun", "Maximum function evaluations", value = 30000, min = 1000, step = 1000),
          shiny::actionButton("fit_model", "Fit Current Model", class = "btn-primary"),
          shiny::actionButton("fit_example", "Reset & Fit HSB Demo", class = "btn-outline-primary"),
          shiny::actionButton("refit_model", "Refit With Nelder_Mead", class = "btn-outline-secondary")
        ),
        bslib::card(
          bslib::card_header("Estimation Choices"),
          shiny::tableOutput("estimation_preview")
        ),
        bslib::card(
          bslib::card_header("Model Readiness Checklist"),
          shiny::p("Review these pre-fit checks before estimating uploaded data or complex random-slope specifications."),
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
              bslib::card(bslib::card_header("Fixed Effects (Model-Based SEs)"), shiny::uiOutput("apa_table_html")),
              bslib::card(bslib::card_header("Fixed Effects (Cluster-Robust Sandwich SEs)"), shiny::uiOutput("robust_table_html")),
              bslib::card(bslib::card_header("Variance Components & Random-Effect LRTs"), shiny::uiOutput("variance_table_html"), shiny::div(class = "scroll-table", shiny::tableOutput("variance_lrt_table"))),
              bslib::card(bslib::card_header("Empirical Bayes Random-Coefficient Reliabilities"), shiny::uiOutput("reliability_table_html")),
              bslib::card(bslib::card_header("Intraclass Correlations (ICC)"), shiny::uiOutput("icc_table_html")),
              bslib::card(bslib::card_header("Variance Explained (Nakagawa R2)"), shiny::uiOutput("r2_table_html")),
              bslib::card(bslib::card_header("Dummy Coding"), shiny::uiOutput("dummy_table_html")),
              bslib::card(bslib::card_header("Software and R Packages"), shiny::uiOutput("software_table_html"))
            ),
            bslib::card(
              bslib::card_header("Multiparameter Fixed-Effect Hypothesis Test (Joint Wald Chi-Square)"),
              shiny::p(class = "equation-subnote", "Test a composite null hypothesis across multiple fixed-effect coefficients simultaneously (e.g., main effect + cross-level interaction = 0):"),
              bslib::layout_columns(
                col_widths = c(7, 5),
                shiny::uiOutput("wald_terms_selector"),
                shiny::selectInput(
                  "wald_vcov_type",
                  "Covariance matrix",
                  choices = c(
                    "Cluster-robust sandwich (CR1)" = "CR1",
                    "Model-based (GLS / ML)" = "model",
                    "Cluster-robust unadjusted (CR0)" = "CR0"
                  ),
                  selected = "CR1"
                )
              ),
              shiny::div(class = "scroll-table", shiny::tableOutput("wald_test_table"))
            ),
            shiny::uiOutput("pop_average_card_ui")
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
                shiny::uiOutput("ranef_term_selector"),
                shiny::hr(),
                shiny::tags$strong(style = "font-size:0.86rem;", "Export Residual Files"),
                shiny::p(class = "equation-subnote", "Download complete-case Level-1 residuals or cluster empirical Bayes estimates as CSV:"),
                shiny::downloadButton("download_level1_residuals", "Level-1 Residuals (.csv)", class = "btn-outline-primary btn-sm"),
                shiny::downloadButton("download_cluster_residuals", "Cluster EB Residuals (.csv)", class = "btn-outline-primary btn-sm")
              ),
              shiny::div(
                class = "results-page",
                bslib::layout_columns(
                  col_widths = c(6, 6),
                  bslib::card(bslib::card_header("Convergence & Fit Diagnostics"), shiny::tableOutput("diagnostics_table")),
                  bslib::card(
                    bslib::card_header("Homogeneity of Level-1 Variance Test"),
                    shiny::div(class = "scroll-table", shiny::tableOutput("homogeneity_table"))
                  )
                ),
                bslib::layout_columns(
                  col_widths = c(6, 6),
                  bslib::card(bslib::card_header("Residuals vs Fitted"), shiny::plotOutput("residual_fitted_plot", height = "340px")),
                  bslib::card(bslib::card_header("Normal QQ Plot"), shiny::plotOutput("qq_plot", height = "340px")),
                  bslib::card(bslib::card_header("Observed vs Fitted"), shiny::plotOutput("observed_fitted_plot", height = "340px")),
                  bslib::card(bslib::card_header("Empirical Bayes Random Effects (95% Intervals)"), shiny::plotOutput("random_effects_interval_plot", height = "340px"))
                ),
                bslib::card(bslib::card_header("Model-Implied Lines by Group"), shiny::plotOutput("model_lines_plot", height = "460px"))
              )
            )
          ),
          shiny::tabPanel(
            "Compare",
            shiny::br(),
            bslib::card(
              bslib::card_header("Model Fit & Deviance Comparison"),
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
          shiny::p("Download a Quarto-ready report with the model summary, model-based and cluster-robust APA tables, R2/reliability, equations, diagnostics, R code, and LaTeX."),
          shiny::downloadButton("download_report", "Download .qmd Report", class = "btn-primary")
        ),
        bslib::layout_columns(
          col_widths = c(6, 6),
          bslib::card(
            bslib::card_header("Publication Readiness Checklist & Archive"),
            shiny::p("Export a complete reproducibility .zip archive (Quarto report, standalone R script, LaTeX, HTML tables, Level-1 & cluster residual CSVs, checklist CSV, and software CSV)."),
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
          bslib::card_header("Purpose & Architecture"),
          shiny::p("mlmr is an open-source Shiny interface and R package for specifying, fitting, teaching, and reporting multilevel and mixed-effects models in R. It pairs a level-by-level equation builder with transparent lme4 and lmerTest code that researchers and students can inspect, rerun, and publish."),
          shiny::p("Supported workflows include empirical level classification, live pre-fit Level-1/Level-2/Level-3 symbolic equations, group-mean and grand-mean centering, inline random-slope toggles, cross-level slope moderators, Huber-White cluster-robust sandwich SEs, multiparameter fixed-effect Wald tests, empirical Bayes intercept and slope reliabilities, variance-component LRT chi-square tests, Level-1 variance homogeneity tests, unit-specific and population-average logistic odds ratios, residual CSV exports, and .zip reproducibility bundles.")
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
          bslib::card_header("Multilevel Capabilities Matrix"),
          shiny::p("Overview of statistical capabilities, model structures, inferential outputs, and current scope."),
          shiny::div(class = "scroll-table", shiny::tableOutput("feature_matrix_table"))
        ),
        bslib::card(
          bslib::card_header("Deterministic Validation Benchmarks"),
          shiny::p("Eight reference models validate formulas, readiness checks, point estimates, standard errors, variance components, ICCs, and round-trip exported R code execution."),
          shiny::div(
            class = "code-actions",
            shiny::actionButton("run_benchmark_validation", "Run Fit Validation", class = "btn-outline-primary"),
            shiny::downloadButton("download_validation_report", "Download Validation Report")
          ),
          shiny::div(class = "scroll-table", shiny::tableOutput("validation_benchmarks_table"))
        )
      )
    )
  )
}
