#' Row Add & Edit Module
#'
#' Module to add & edit rows in the main database file
#'
#' @importFrom shiny observeEvent showModal modalDialog removeModal fluidRow column textInput numericInput selectInput modalButton actionButton reactive eventReactive
#' @importFrom shinyFeedback showFeedbackDanger hideFeedback
#' @importFrom shinyjs enable disable
#' @importFrom lubridate with_tz
#' @importFrom uuid UUIDgenerate
#' @importFrom DBI dbExecute
#' @importFrom shinyFeedback showToast
#'
#' @param modal_title string - the title for the modal
#' @param row_to_edit reactive returning a 1 row data frame of the row to edit
#' from the "mt_cars" table
#' @param modal_trigger reactive trigger to open the modal (Add or Edit buttons)
#'
#' @return None
#'
module_table_edit_server <-
  function(id,
           modal_title,
           row_to_edit,
           modal_trigger) {
    moduleServer(id,
                 function(input, output, session) {
                   
                   ns <- session$ns
                   
                   # modal_trigger() originates from input$add_row
                   # input$car_id_to_edit in table module
                   
                   observeEvent(modal_trigger(), {
                     
                     hold <- row_to_edit()
                     
                     
                     showModal(modalDialog(
                       fluidRow(
                         column(
                           width = 6,
                           textInput(ns("model"),
                                     'Model',
                                     value = ifelse(is.null(hold), "", hold$model)),
                           numericInput(
                             ns('mpg'),
                             'Miles/Gallon',
                             value = ifelse(is.null(hold), "", hold$mpg),
                             min = 0,
                             step = 0.1
                           ),
                           selectInput(
                             ns('am'),
                             'Transmission',
                             choices = c('Automatic', 'Manual'),
                             selected = ifelse(is.null(hold), "", hold$am)
                           ),
                           numericInput(
                             ns('disp'),
                             'Displacement (cu.in.)',
                             value = ifelse(is.null(hold), "", hold$disp),
                             min = 0,
                             step = 0.1
                           ),
                           numericInput(
                             ns('hp'),
                             'Horsepower',
                             value = ifelse(is.null(hold), "", hold$hp),
                             min = 0,
                             step = 1
                           ),
                           numericInput(
                             ns('drat'),
                             'Rear Axle Ratio',
                             value = ifelse(is.null(hold), "", hold$drat),
                             min = 0,
                             step = 0.01
                           )
                         ),
                         column(
                           width = 6,
                           numericInput(
                             ns('wt'),
                             'Weight (lbs)',
                             value = ifelse(is.null(hold), "", hold$wt),
                             min = 0,
                             step = 1
                           ),
                           numericInput(
                             ns('qsec'),
                             '1/4 Mile Time',
                             value = ifelse(is.null(hold), "", hold$qsec),
                             min = 0,
                             step = 0.01
                           ),
                           selectInput(
                             ns('vs'),
                             'Engine',
                             choices = c('Straight', 'V-shaped'),
                             selected = ifelse(is.null(hold), "", hold$vs)
                           ),
                           numericInput(
                             ns('cyl'),
                             'Cylinders',
                             value = ifelse(is.null(hold), "", hold$cyl),
                             min = 0,
                             max = 20,
                             step = 1
                           ),
                           numericInput(
                             ns('gear'),
                             'Forward Gears',
                             value = ifelse(is.null(hold), "", hold$gear),
                             min = 0,
                             step = 1
                           ),
                           numericInput(
                             ns('carb'),
                             'Carburetors',
                             value = ifelse(is.null(hold), "", hold$carb),
                             min = 0,
                             step = 1
                           )
                         )
                       ),
                       title = modal_title,
                       size = 'm',
                       footer = list(
                         modalButton('Cancel'),
                         actionButton(
                           ns('submit'),
                           'Submit',
                           class = "btn btn-primary",
                           style = "color: white"
                         )
                       )
                     )) # end of module UI
                     
                     # Observe event for "Model" text input in Add/Edit Car Modal
                     # `shinyFeedback`
                     observeEvent(input$model, {
                       if (input$model == "") {
                         
                         shinyFeedback::showFeedbackDanger("model",
                                                           text = "Must enter model of car!")
                         
                         shinyjs::disable('submit')
                         
                       } else {
                         
                         shinyFeedback::hideFeedback("model")
                         
                         shinyjs::enable('submit')
                         
                       }
                     })
                   }) # end of modal_trigger()
                   
                   
                   
                   edit_row_dat <- reactive({
                     
                     hold <- row_to_edit()
                     
                     out <- list(
                       "id_" = if (is.null(hold))
                         uuid::UUIDgenerate()
                       else
                         hold$id_,
                       "model" = input$model,
                       "mpg" = input$mpg,
                       "cyl" = input$cyl,
                       "disp" = input$disp,
                       "hp" = input$hp,
                       "drat" = input$drat,
                       "wt" = input$wt,
                       "qsec" = input$qsec,
                       "vs" = input$vs,
                       "am" = input$am,
                       "gear" = input$gear,
                       "carb" = input$carb
                     )
                     
                     time_now <-
                       as.character(lubridate::with_tz(Sys.time(), tzone = "UTC"))
                     
                     if (is.null(hold)) {
                       # adding a new car
                       
                       out$created_at <- time_now
                       out$created_by <- session$userData$email
                     } else {
                       # Editing existing car
                       
                       out$created_at <- as.character(hold$created_at)
                       out$created_by <- hold$created_by
                     }
                     
                     out$modified_at <- time_now
                     out$modified_by <- session$userData$email
                     
                     out$is_deleted <- FALSE
                     
                     out
                   })
                   
                   validate_edit <- eventReactive(input$submit, {
                     dat <- edit_row_dat()
                     
                     # Logic to validate inputs...
                     
                     dat
                   })
                   
                   observeEvent(validate_edit(), {
                     
                     removeModal()
                     
                     dat <- validate_edit()
                     
                     tryCatch({
                       
                       # creating a new car
                       uid <- uuid::UUIDgenerate()
                       
                       dbExecute(
                         conn,
                         "INSERT INTO mtcars (uid, id_, model, mpg, cyl, disp, hp, drat, wt, qsec, vs, am,
        gear, carb, created_at, created_by, modified_at, modified_by, is_deleted) VALUES
        ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14, $15, $16, $17, $18, $19)",
                         params = c(list(uid),
                                    unname(dat))
                       )
                       
                       session$userData$db_trigger(session$userData$db_trigger() + 1)
                       
                       showToast("success", paste0(modal_title, " Success"))
                       
                     }, error = function(error) {
                       
                       msg <- "Error Adding Car"
                       
                       # print `msg` so that we can find it in the logs
                       print(msg)
                       
                       # print the actual error to log it
                       print(error)
                       
                       # show error `msg` to user.  User can then tell us about error and we can
                       # quickly identify where it cam from based on the value in `msg`
                       showToast("error", msg)
                       
                     })
                   })
                   
                 })
  }
