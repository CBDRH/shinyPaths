#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  
  dag <- reactiveValues()
  control <- reactiveValues()
  
  observeEvent(input$run,{
    
    control$vars <- NULL

  })
  
  clickedVar <- reactive({
  
    req(input$plotClick$x)
    
    tol <- 0.1
    
    dag()$data %>%
      dplyr::filter(dplyr::between(x, input$plotClick$x - tol, input$plotClick$x + tol)) %>%
      dplyr::filter(dplyr::between(y, input$plotClick$y - tol, input$plotClick$y + tol)) %>%
      dplyr::select(name) %>%
      dplyr::filter(name != 'X') %>%
      dplyr::filter(name != 'Y') %>%
      unlist() %>%
      unique()
  })
  
  observeEvent(input$plotClick, {
    
    req(clickedVar())
    
    if (clickedVar() %in% control$vars) {
      control$vars <- control$vars[! control$vars %in% clickedVar()]
    }
    else {
      control$vars <- append(control$vars, clickedVar()) %>% unique()
    }
    
  })
  
  output$printSelected <- renderText({
    if (all.equal(0, length(control$vars)) == TRUE) {
      return("No adjustment needed")
    } else return(control$vars)
  })
  
  dag <- eventReactive(input$run, {
    randDAG(input$n, input$p)
  }, ignoreNULL = FALSE)
  
  dagAdj <- reactive({
    dag() %>% adjust_for(control$vars)
  })
  
  dagSolved <- reactive({
    solution <- dagitty::adjustmentSets(dag()$dag, type = 'minimal', effect = input$effect)
    dag() %>% adjust_for(solution[[1]])
  })
  
  mod_drawDag_server("drawDag_ui_1", dagAdj)
  # mod_drawDag_server("drawDag_ui_2", dagSolved, colliderlines = 1, show = reactive(button$reveal))
  mod_drawDag_server("drawDag_ui_2", dagSolved, colliderlines = 1)
  
      # Calculate marks
      observeEvent(input$submit, {

          submission <- control$vars
          solution <- dagitty::adjustmentSets(dag()$dag, type = 'minimal', effect = input$effect)
          nSol <- length(solution)
          mark <- grader(submission, solution, nSol)
          message <- ifelse(mark, paste('Correct!', emo::ji('happy')), paste('Incorrect!', emo::ji('sad')))

          shinyalert::shinyalert(title = message,
                                 text = NULL,
                                 animation = FALSE,
                                 showConfirmButton = FALSE,
                                 className = "alert",
                                 timer = 600)

      })
  
      
  output$test1 <- renderText({
  })
  
  # OLDOLDOLDOLDOLDOLDOLDOLDOLDOLDOLDOLDOLDOLDOLDOLDOLDOLDOLDOLDOLDOLDOLDOLD
  # 
  # # Your application server logic 
  # 
  # # Set up reactive values
  # button <- reactiveValues(run = 0, submit = 0, reveal = 0)
  # dag <- reactiveValues(show = 1)
  # control <- reactiveValues()
  # pointer <- reactiveValues()
  # 
  # 
  # # Reveal button logic
  # observeEvent(input$reveal, {
  #   button$reveal <- abs(button$reveal - 1)
  # })
  # 
  # # Update random DAG input parameters on click
  # user_choice <- eventReactive(input$run,{
  # 
  #   control$vars <- NULL
  #   pointer$x <- NULL
  #   pointer$y <- NULL
  #   button$reveal <- 0
  #   dag$mark <- NULL
  # 
  #   n = input$n
  #   p = as.numeric(input$p)
  # 
  #   return(c(n, p))
  # 
  #   ## use ignoreNULL to fire the event at startup
  # }, ignoreNULL = FALSE)
  # 
  # 
  # # Generate random DAG
  # observe({
  #   uc <- user_choice()
  #   dag$rand <- randDAG(uc[1], uc[2])
  # 
  #   # Because the scale for the DAG plot changes randomly, need to redefine the tol value for user clicks
  #   # dag$tol <- round((max(dag$rand$data$xend, na.rm = T) - min(dag$rand$data$x, na.rm = T))/20, 2)
  # })
  # 
  # # Get the valid adjustment sets for the random dag
  # observe({
  #   dag$adjSets <- dagitty::adjustmentSets(dag$rand$dag, type = 'minimal', effect = input$effect)
  #   dag$nSets <- length(dag$adjSets)
  #   dag$solutionID <- 1
  # })
  # 
  # observeEvent(input$solutionID, {
  # 
  #   dag$solutionID <- input$solutionID
  # 
  # })
  # 
  # observe({
  #   dag$solution <- dag$adjSets[[dag$solutionID]]
  # })
  # 
  # observe({
  #   req(input$plotClick$x, input$plotClick$y)
  #   pointer$x <- input$plotClick$x
  #   pointer$y <- input$plotClick$y
  # })
  # 
  # clickedVar <- reactive({
  # 
  #   req(pointer$x, pointer$y)
  # 
  #   myDag()$data %>%
  #     dplyr::filter(dplyr::between(x, pointer$x - dag$tol, pointer$x + dag$tol)) %>%
  #     dplyr::filter(dplyr::between(y, pointer$y - dag$tol, pointer$y + dag$tol)) %>%
  #     dplyr::select(name) %>%
  #     dplyr::filter(name != 'X') %>%
  #     dplyr::filter(name != 'Y') %>%
  #     unlist() %>%
  #     unique()
  # })
  # 
  # observeEvent(input$plotClick, {
  # 
  #   dag$mark <- NULL
  #   req(clickedVar())
  # 
  #   if (clickedVar() %in% control$vars) {
  #     control$vars <- control$vars[! control$vars %in% clickedVar()]
  #   }
  #   else {
  #     control$vars <- append(control$vars, clickedVar()) %>% unique()
  #   }
  # 
  # })
  # 
  # 
  # # mod_drawDag_server("drawDag_ui_1", dag = reactive(dag$rand))
  # #                    # dag = reactive(dag$rand),
  # #                    # controls = reactive(control$vars),
  # #                    # label = NULL,
  # #                    # colliderlines = FALSE,
  # #                    # show = reactive(dag$show))
  # # 
  # output$test <- renderDataTable({
  #   x <- myDag() %>% adjust_for(control$vars)
  #   x$data
  #   })
  # 
  # # mod_drawDag_server("drawDag_ui_2", dag = reactive(dag$rand))
  # #                        # dag = reactive(dag$rand),
  # #                        # controls = reactive(dag$solution),
  # #                        # label = NULL,
  # #                        # colliderlines = TRUE,
  # #                        # show = reactive(button$reveal))
  # 
  #     # Show solution options if more than 1 solution
  #     output$solutionOpts <- renderUI({
  # 
  #       req(dag$nSets)
  # 
  #       if (dag$nSets >=2 & button$reveal==1 ) {
  #         # Update the possible solution to view
  #         choiceSolutions <- c(paste('Solution', seq(1, dag$nSets)))
  #         radioButtons("solutionID",
  #                      "There was more than one valid solution, choose which to view:",
  #                      choiceNames = choiceSolutions,
  #                      choiceValues = seq(1:dag$nSets),
  #                      selected = 1
  #                      )
  #       }
  # 
  #       else {
  #         return(NULL)
  #       }
  # 
  #     })
  # 
  #     # Calculate marks
  #     observeEvent(input$submit, {
  # 
  #         submission <- control$vars
  #         dag$mark <- grader(submission, dag$adjSets, dag$nSets)
  # 
  #         message <- ifelse(dag$mark, paste('Correct!', emo::ji('happy')), paste('Incorrect!', emo::ji('sad')))
  # 
  #         shinyalert::shinyalert(title = message,
  #                                text = NULL,
  #                                animation = FALSE,
  #                                showConfirmButton = FALSE,
  #                                className = "alert",
  #                                timer = 600)
  # 
  #     })
  # 
  # output$printSelected <- renderText({
  #   if (all.equal(0, length(control$vars)) == TRUE) {
  #     return("No adjustment needed")
  #   } else return(control$vars)
  # 
  # })
  # 
  # # output$randPlot <- renderPlot({
  # #     shinipsum::random_ggplot()
  # #     })
  # # 
  # # output$plotTest <- renderPlot({
  # # 
  # #   x  <- dag$rand
  # #   p <- x$dag %>% 
  # #     ggdag::adjust_for(c('Z1')) %>% 
  # #     ggdag::node_status() %>% 
  # #     ggdag::ggdag()
  # #   
  # #   return(p)
  # # })
  # 
  # myDag <- eventReactive(input$run, {
  #   randDAG(input$n, input$p)
  #   # dagitty::randomDAG(input$n, input$p) %>% 
  #   #     dagitty::setVariableStatus('exposure', 'x1') %>% 
  #   #     dagitty::setVariableStatus('outcome', 'x4') %>% 
  #   #     tidy_dagitty()
  # }, ignoreNULL = FALSE)
  # 
  # observe({
  #   dag$tol <- round((max(myDag()$data$xend, na.rm = T) - min(myDag()$data$x, na.rm = T))/20, 2)
  # })
  # 
  # output$test2 <- renderPlot({
  #     myDag() %>% 
  #     adjust_for('Z1') %>%  
  #     node_status() %>% # Need to refresh node status here in case adjustment has added new records to the tidy_dagitty data object
  #     ggplot(aes(x = x, y = y, xend = xend, yend = yend, fill = status, shape = adjusted)) +
  #     geom_dag_point(aes(color = adjusted)) +
  #     geom_dag_edges(arrow_directed = grid::arrow(length = grid::unit(10, "pt"), type = "closed")) +
  #     geom_dag_text() +
  #     theme_dag() +
  #     scale_adjusted() +
  #     guides(fill = guide_legend(override.aes = list(color = c(exposureCol, outcomeCol)))) +
  #     scale_fill_manual("Status",
  #                       values = c('exposure' = exposureCol, 'outcome' = outcomeCol),
  #                       labels = c('exposure' = 'Exposure', 'outcome' = 'Outcome'),
  #                       na.value = naCol) +
  #     scale_color_manual(NULL, guide = 'none',
  #                        values = c('adjusted' = 'gray20', 'unadjusted' = 'white')) +
  #     scale_shape_manual(NULL, guide = 'none',
  #                        values = c(unadjusted = 21, adjusted = 22))
  #   # ggplot(data = df, aes(x = x, y = y, xend = xend, yend = yend, fill = status, shape = adjusted)) +
  #   #   geom_dag_point(aes(color = adjusted)) +
  #   #   geom_dag_edges(arrow_directed = grid::arrow(length = grid::unit(10, "pt"), type = "closed")) +
  #   #   geom_dag_text() +
  #   #   theme_dag(legend.position = 'bottom') +
  #   #   scale_adjusted() +
  #   #   guides(fill = guide_legend(override.aes = list(color = c(exposureCol, outcomeCol)))) +
  #   #   scale_fill_manual(NULL,
  #   #                     values = c('exposure' = exposureCol, 'outcome' = outcomeCol),
  #   #                     labels = c('exposure' = 'Exposure', 'outcome' = 'Outcome'),
  #   #                     na.value = naCol) +
  #   #   scale_color_manual(NULL, guide = 'none',
  #   #                      values = c('adjusted' = 'gray20', 'unadjusted' = 'white')) +
  #   #   scale_shape_manual(NULL, guide = 'none',
  #   #                      values = c(unadjusted = 21, adjusted = 22))
  #   
  # })
  
}
