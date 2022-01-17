#' testMod UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_testMod_ui <- function(id){
  ns <- NS(id)
  tagList(
    plotOutput(ns("test"))
  )
}
    
#' testMod Server Functions
#'
#' @noRd 
mod_testMod_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$test <- renderPlot({
      dag  <- ggdag::dagify(y ~ x + z2 + w2 + w1,
                            x ~ z1 + w1,
                            z1 ~ w1 + v,
                            z2 ~ w2 + v,
                            w1 ~~ w2)
      
      ggdag::ggdag(dag) + theme_dag() 
    })
  })
}
    
## To be copied in the UI
# mod_testMod_ui("testMod_ui_1")
    
## To be copied in the server
# mod_testMod_server("testMod_ui_1")
