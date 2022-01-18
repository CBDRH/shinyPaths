#' drawDag UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList
#' @importFrom ggplot2 ggplot aes ggproto guides guide_legend scale_color_manual scale_shape_manual scale_fill_manual
#' @importFrom ggdag ggdag adjust_for geom_dag_point geom_dag_edges geom_dag_collider_edges geom_dag_text theme_dag geom_dag_label_repel scale_adjusted dag_label node_status
mod_drawDag_ui <- function(id){
  ns <- NS(id)
  tagList(
    plotOutput(ns("plot"), click = "plotClick") #
  )
}
    
#' drawDag Server Functions
#'
#' @noRd 
# mod_drawDag_server <- function(id, dag, controls, label = NULL, colliderlines, show){
mod_drawDag_server <- function(id, dag, label = 0, colliderlines = 0){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

        output$plot <- renderPlot({
 
        p <- dag() %>% 
          node_status() %>% # Need to refresh node status here in case adjustment has added new records to the tidy_dagitty data object
          ggplot(aes(x = x, y = y, xend = xend, yend = yend, fill = status, shape = adjusted)) +
          geom_dag_point(aes(color = adjusted)) +
          geom_dag_edges(arrow_directed = grid::arrow(length = grid::unit(10, "pt"), type = "closed")) +
          geom_dag_text() +
          theme_dag() +
          scale_adjusted() +
          guides(fill = guide_legend(override.aes = list(color = c(exposureCol, outcomeCol)))) +
          scale_fill_manual("Status",
                            values = c('exposure' = exposureCol, 'outcome' = outcomeCol),
                            labels = c('exposure' = 'Exposure', 'outcome' = 'Outcome'),
                            na.value = naCol) +
          scale_color_manual(NULL, guide = 'none',
                             values = c('adjusted' = 'gray20', 'unadjusted' = 'white')) +
          scale_shape_manual(NULL, guide = 'none',
                             values = c(unadjusted = 21, adjusted = 22))  

        # Add label if one is defined
        if (label == 1) {
          p <- p + geom_dag_label_repel(aes(label = label, fill = status), show.legend = FALSE, box.padding = 4, segment.color = 'grey80')
        }

        # Add collider lines if requested
        if (colliderlines == 1) {
          p <- p + geom_dag_collider_edges(color = 'pink')
        }
        
        return(p)

      })
  })
}
    
## To be copied in the UI
# mod_drawDag_ui("drawDag_ui_1")
    
## To be copied in the server
# mod_drawDag_server("drawDag_ui_1")
