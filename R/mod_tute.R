#' #' mod_tute UI Function
#' #'
#' #' @description A shiny Module to present tutorials
#' #'
#' #' @param id,input,output,session Internal parameters for {shiny}.
#' #'
#' #' @noRd 
#' #'
#' #' @importFrom shiny NS tagList 
#' mod_tuteUI <- function(id){
#'   ns <- NS(id)
#'   
#'   # Central column
#'   column(width = 6, 
#'          mod_drawDag_ui("drawDag_ui_3"),
#'          tags$div(style="text-align:center;",
#'                   strong("Your solution:"),
#'                   textOutput("printSelected2", inline = TRUE)
#'          ),
#'          tags$div(style = 'text-align: center;',
#'                   actionButton('previous', NULL, icon = icon("arrow-left"), width = 68),
#'                   actionButton('advance', NULL, icon = icon("arrow-right"), width = 68),
#'                   actionButton("submit2", "Submit answer", icon = icon('share-square'), width = 140),
#'                   actionButton("reveal2", "Reveal solution", icon = icon('project-diagram'), width = 140)
#'          )
#'   ),
#'   
#'   # Output controls
#'   column(width = 5, 
#'          htmlOutput("tuteText")
#'   )
#'   
#'   )
#'   
#' }
#' 
#' #' mod_tute Server Function
#' #'
#' #' @noRd 
#' mod_tuteServer <- function(id){
#'   moduleServer(id, function(input, output, session){
#'   
#'     
#'     })
#'   
#' }
#' 
#' #' mod_tutes test Function
#' #'
#' #' @noRd 
#' mod_tuteTest <- function() {
#'   ui <- fluidPage(
#'     mod_tuteUI('mod1')
#'   )
#'   server <- function(input, output, session) {
#'     mod_tuteServer('mod1')
#'     
#'   }
#'   
#'   shinyApp(ui, server)  
#' }
#' 
#' 
#' 
#' 
#' ## To be copied in the UI
#' # mod_tuteUI('mod1')
#' 
#' ## To be copied in the server
#' # mod_tuteServer('mod1')
#' 
#' # Test
#' mod_tuteTest()
#' 
