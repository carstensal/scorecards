# Application - UI --------------------------------------------------------

ui <- shinyUI(
   
  navbarPage(
    "Scorecard Builder",
    theme = shinytheme("flatly"),
    id = "nav_top",
    tabPanel(
      span("Data", title = "Load and manage data"),
      data_ui("data_module"),
      value = "data"
    ),
    tabPanel(
      span("Explore",
           title = "Variable Exporation"),
      explore_ui("explore_module"),
      value = "explore"
    ),
    tabPanel(
      span("Sample", title = "Training and test samples"),
      sample_ui("sample_module"),
      value = "sample"
    ),
    tabPanel(
      span("Binning", title = "Weight of evidence and information value"),
      useShinyjs(debug = TRUE),
      binning_ui("binning_module"),
      value = "binning"
    ),
    tabPanel(
      span("Model", title = "Scorecard development and alignment"),
      model_ui("model_module"),
      value = "model"
    ),
    tabPanel(
      span("Validation", title = "Scorecard validation"),
      validation_ui("validation_module"),
      value = "validation"
    ),
    tabPanel(
      "",
      value = "download",
      download_ui("download_module"),
      icon = icon("download")
    ),
    tabPanel(
      "",
      value = "help",
      helpText("Help"),
      icon = icon("question")
    ),
    tabPanel(
      "",
      value = "stop",
      icon = icon("stop")
    )
  )
)