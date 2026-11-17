mlmr_shiny_app <- function() {
  shiny::shinyApp(ui = app_ui(), server = app_server)
}

run_mlmr <- function(launch.browser = TRUE, port = getOption("shiny.port", NULL), host = getOption("shiny.host", "127.0.0.1")) {
  shiny::runApp(
    mlmr_shiny_app(),
    launch.browser = launch.browser,
    port = port,
    host = host
  )
}
