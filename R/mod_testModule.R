#' testModule UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_testModule_ui <- function(id){
  ns <- NS(id)
  tagList(
  plotOutput(ns("plot"))
  )
}
    
#' testModule Server Functions
#'
#' @noRd 
mod_testModule_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$plot <- renderPlot({
      shinipsum::random_ggplot()
    })
    
  })
}
    
## To be copied in the UI
# mod_testModule_ui("testModule_ui_1")
    
## To be copied in the server
# mod_testModule_server("testModule_ui_1")
