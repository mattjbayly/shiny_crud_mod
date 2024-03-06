ui <- fluidPage(
  shinyFeedback::useShinyFeedback(),
  shinyjs::useShinyjs(),
  # Application Title
  titlePanel(
    h1("Shiny CRUD - Auditable", align = 'center'),
    windowTitle = "Shiny CRUD - Auditable"
  ),
  module_table_ui("cars_table")
  
)

