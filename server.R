server <- function(input, output, session) {

  # user session$userData to store user data that will be needed throughout
  # the Shiny application
  session$userData$email <- 'tycho.brahe@tychobra.com'
  session$userData$db_trigger <- reactiveVal(0)
  
  # Call the table server function
  module_table_server("cars_table")
}
