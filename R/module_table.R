#' Table Module UI
#'
#' The UI portion of the module for displaying the main datatable
#'
#' @importFrom shiny NS tagList fluidRow column actionButton tags
#' @importFrom DT DTOutput
#' @importFrom shinycssloaders withSpinner
#'
#' @param id The id for this module
#'
#' @return a \code{shiny::\link[shiny]{tagList}} containing UI elements

module_table_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    fluidRow(column(
      width = 2,
      actionButton(
        ns("add_row"),
        "Add",
        class = "btn-success",
        style = "color: #fff;",
        icon = icon('plus'),
        width = '100%'
      ),
      tags$br(),
      tags$br()
    )),
    fluidRow(
      column(
        width = 12,
        title = "Main Table Title Here",
        DTOutput(ns('main_table')) %>%
          withSpinner(),
        tags$br(),
        tags$br()
      )
    ),
    tags$script(src = "main_table_module.js"),
    tags$script(paste0("main_table_module_js('", ns(''), "')"))
  )
}

#' Main Table Module Server
#'
#' The Server portion of the module for displaying the main datatable
#'
#' @importFrom shiny reactive reactiveVal observeEvent req callModule eventReactive
#' @importFrom DT renderDT datatable replaceData dataTableProxy
#' @importFrom dplyr tbl collect mutate arrange select filter pull
#' @importFrom purrr map_chr
#' @importFrom tibble tibble
#'
#' @param None
#'
#' @return None

module_table_server <- function(id) {
  moduleServer(id,
               function(input, output, session) {
                 
                 ns <- session$ns
                 
                 # Read in "mtcars" table from the database
                 r_table <- reactive({
                   session$userData$db_trigger()
                   
                   out <- NULL
                   
                   tryCatch({
                     out <- conn %>%
                       tbl('mtcars') %>%
                       select(-uid) %>%
                       collect() %>%
                       mutate(
                         created_at = as.POSIXct(created_at, tz = "UTC"),
                         modified_at = as.POSIXct(modified_at, tz = "UTC")
                       ) %>%
                       # find the most recently modified row for each car
                       group_by(id_) %>%
                       filter(modified_at == max(modified_at)) %>%
                       ungroup() %>%
                       # filter out deleted cars
                       filter(is_deleted == 0) %>%
                       arrange(desc(modified_at))
                     
                   }, error = function(err) {
                     msg <- "Database Connection Error"
                     # print `msg` so that we can find it in the logs
                     print(msg)
                     # print the actual error to log it
                     print(error)
                     # show error `msg` to user.  User can then tell us about error and we can
                     # quickly identify where it cam from based on the value in `msg`
                     showToast("error", msg)
                   })
                   
                   out
                 })
                 
                 
                 main_table_prep <- reactiveVal(NULL)
                 
                 observeEvent(r_table(), {
                   out <- r_table()
                   
                   ids <- out$id_
                   
                   # Add on the action buttons html as new columns for each row
                   actions <- purrr::map_chr(ids, function(id_) {
                     paste0(
                       '<div class="btn-group" style="width: 75px;" role="group" aria-label="Basic example">
          <button class="btn btn-primary btn-sm edit_btn" data-toggle="tooltip" data-placement="top" title="Edit" id = ',
                       id_,
                       ' style="margin: 0"><i class="fa fa-pencil-square-o"></i></button>
          <button class="btn btn-danger btn-sm delete_btn" data-toggle="tooltip" data-placement="top" title="Delete" id = ',
                       id_,
                       ' style="margin: 0"><i class="fa fa-trash-o"></i></button>
        </div>'
                     )
                   })
                   
                   # Remove the `id_` and `is_deleted` columns. We don't want to show this column to the user
                   out <- out %>%
                     select(-id_,-is_deleted)
                   
                   # Set the Action Buttons row to the first column of the `mtcars` table
                   out <- cbind(tibble(" " = actions),
                                out)
                   
                   if (is.null(main_table_prep())) {
                     # loading data into the table for the first time, so we render the entire table
                     # rather than using a DT proxy
                     main_table_prep(out)
                     
                   } else {
                     # table has already rendered, so use DT proxy to update the data in the
                     # table without rerendering the entire table
                     replaceData(main_table_proxy,
                                 out,
                                 resetPaging = FALSE,
                                 rownames = FALSE)
                     
                   }
                 })
                 
                 
                 # Render the full data table for the first time
                 # if not already created.
                 # Alternative is replaceData(main_table_proxy)
                 
                 output$main_table <- renderDT({
                   req(main_table_prep())
                   
                   out <- main_table_prep()
                   
                   datatable(
                     out,
                     rownames = FALSE,
                     colnames = c(
                       'Model',
                       'Miles/Gallon',
                       'Cylinders',
                       'Displacement (cu.in.)',
                       'Horsepower',
                       'Rear Axle Ratio',
                       'Weight (lbs)',
                       '1/4 Mile Time',
                       'Engine',
                       'Transmission',
                       'Forward Gears',
                       'Carburetors',
                       'Created At',
                       'Created By',
                       'Modified At',
                       'Modified By'
                     ),
                     selection = "none",
                     class = "compact stripe row-border nowrap",
                     # Escape the HTML in all except 1st column (which has the buttons)
                     escape = -1,
                     extensions = c("Buttons"),
                     options = list(
                       scrollX = TRUE,
                       dom = 'Bftip',
                       buttons = list(
                         list(
                           extend = "excel",
                           text = "Download",
                           title = paste0("mtcars-", Sys.Date()),
                           exportOptions = list(columns = 1:(length(out) - 1))
                         )
                       ),
                       columnDefs = list(list(
                         targets = 0, orderable = FALSE
                       )),
                       drawCallback = JS(
                         "function(settings) {
          // removes any lingering tooltips
          $('.tooltip').remove()
        }"
                       )
                     )
                   ) %>%
                     formatDate(columns = c("created_at", "modified_at"),
                                method = 'toLocaleString')
                   
                 })
                 
                 
                 
                 # Table has already been created.
                 # Just need to reload table content with proxy.
                 
                 main_table_proxy <-
                   DT::dataTableProxy('main_table')
                 
                 
                 # Source additional modules for add and edit.
                 # Add and edit use the same source module.

                 # Add new entry

                 module_table_edit_server(
                   "add_row",
                   modal_title = "Add Record",
                   row_to_edit = function()
                     NULL,
                   modal_trigger = reactive({
                     input$add_row
                   })
                 )

                 # Trigger the row to edit
                 row_to_edit <-
                   eventReactive(input$main_id_to_edit, {
                     r_table() %>%
                       filter(id_ == input$main_id_to_edit)
                   })

                 # Edit existing entry

                 module_table_edit_server(
                   "edit_row",
                   modal_title = "Edit Record",
                   row_to_edit = row_to_edit,
                   modal_trigger = reactive({
                     input$main_id_to_edit
                   })
                 )

                 # Delete existing entry

                 row_to_delete <-
                   eventReactive(input$main_id_to_delete, {
                     r_table() %>%
                       filter(id_ == input$main_id_to_delete) %>%
                       as.list()
                   })


                 module_table_delete_server(
                   "delete_row",
                   modal_title = "Delete Record",
                   row_to_delete = row_to_delete,
                   modal_trigger = reactive({
                     input$main_id_to_delete
                   })
                 )

                 
               })
}
