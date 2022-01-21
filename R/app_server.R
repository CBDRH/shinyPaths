#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  
  rv <- reactiveValues(reveal = 0, reveal2 = 0, solutionChoice = 1)
  
  # Reset reactive values on panel change
  observeEvent(input$panel,{
    rv$controls <- NULL
    rv$reveal <- 0
    rv$reveal2 <- 0
    rv$solutionChoice <- 1
  })
  
  # Reveal button logic
  observeEvent(input$reveal, {
    rv$reveal <- abs(rv$reveal - 1)
  })
  observeEvent(input$reveal2, {
    rv$reveal2 <- abs(rv$reveal2 - 1)
  })
  
  # Control conditional panel for solutions
  
  ## Reveal Random DAG solution
  output$reveal <- reactive({
    
    ifelse(rv$reveal, 'show', 'hide')
  })
  outputOptions(output, "reveal", suspendWhenHidden = FALSE)
  
  ## Reveal Tutorial DAG solution
  output$reveal2 <- reactive({
    
    ifelse(rv$reveal2, 'show', 'hide')
  })
  outputOptions(output, "reveal2", suspendWhenHidden = FALSE)
  
  
  # Control clicked variable
  clickedVar <- reactive({
  
    req(input$plotClick$x)
    
    tol <- 0.1
    currentDag <- dag1()
    if (input$panel == "Tutorial") {
      currentDag <- dag2()
    }
    
    currentDag$data %>%
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
    
    if (clickedVar() %in% rv$controls) {
      rv$controls <- rv$controls[! rv$controls %in% clickedVar()]
    }
    else {
      rv$controls <- append(rv$controls, clickedVar()) %>% unique()
    }
    
  })
  
  output$printSelected <- output$printSelected2 <- renderText({
    
    if (all.equal(0, length(rv$controls)) == TRUE) {
      "No adjustment needed"
    } else c('Adjust for', rv$controls)
  })
  
  
  # Control the active DAG
  
  # Reset reactive values on Run
  observeEvent(input$run,{
    rv$controls <- NULL
    rv$reveal <- 0
    rv$reveal2 <- 0
    rv$solutionChoice <- 1
    updateNumericInput(session, "pid", value = round(runif(1)*10000))
  })
  
  ## Random DAG
  dag1 <- eventReactive(input$run, {
    
    req(input$panel, input$n, input$p)
    
    randDAG(input$n, input$p, input$pid)
  }, ignoreNULL = FALSE)
  
  ## Random DAG
  dag1 <- eventReactive(input$pid, {
    
    req(input$panel, input$n, input$p)
    
    randDAG(input$n, input$p, input$pid)
  }, ignoreNULL = FALSE)
  
  ## Tutorial DAG
  dag2 <- reactive({
    
    req(input$panel, input$tuteID)
    
    dag <- eval(as.name(paste0('g', input$tuteID)))
    labs <- eval(as.name(paste0('label', input$tuteID)))
    
    dag %>%
      dagitty::dagitty() %>%
      dag_label(labels = labs) %>%
      adjust_for(NULL) %>% 
      node_status()
  })  
  
  # Control the adjusted DAG
  dagAdj1 <- reactive({
    dag1() %>% adjust_for(rv$controls)
  })
  
  # Control the adjusted DAG
  dagAdj2 <- reactive({
    dag2() %>% adjust_for(rv$controls)
  })

  dagSolution1 <- reactive({
    dagitty::adjustmentSets(dag1()$dag, type = 'minimal', effect = input$effect)
  })
  
  dagSolution2 <- reactive({
    dagitty::adjustmentSets(dag2()$dag, type = 'minimal', effect = eval(as.name(paste0("effect", input$tuteID))))
    
  })
  
observeEvent(input$solutionID, {
  rv$solutionChoice <- input$solutionID
})
  
  dagSolved1 <- reactive({
    dag1() %>% adjust_for(dagSolution1()[[rv$solutionChoice]])
  })
  
  dagSolved2 <- reactive({
    dag2() %>% adjust_for(dagSolution2()[[rv$solutionChoice]])
  })
  
  mod_drawDag_server("drawDag_ui_1", dagAdj1) # Random DAG
  mod_drawDag_server("drawDag_ui_2", dagSolved1, colliderlines = 1) # Random DAG solution
  mod_drawDag_server("drawDag_ui_3", dagAdj2, label = 1) # Tutorial DAG
  mod_drawDag_server("drawDag_ui_4", dagSolved2, label = 1, colliderlines = 1) # Tutorial DAG solution
  
      # Calculate marks
      observeEvent(input$submit | input$submit2, {
          
          submission <- rv$controls
          solution <- dagSolution1()
          if (input$panel=="Tutorial") {
            solution <- dagSolution2()
            }
          nSol <- length(solution)
          grade <- grader(submission, solution)
          mark <- grade[[1]]
          message <- ifelse(mark, paste('Correct!', emo::ji('happy')), paste('Incorrect!', emo::ji('sad')))
          text <- grade[[2]]
          
          shinyalert::shinyalert(title = message,
                                 text = text,
                                 animation = FALSE,
                                 showConfirmButton = FALSE,
                                 className = "alert",
                                 timer = 600)

      }, ignoreInit = TRUE)
  
      
          # Show solution options if more than 1 solution
          output$solutionOpts <- renderUI({

            req(dagSolution1())
            
            nSets <- length(dagSolution1())
            
            if (nSets >=2 & rv$reveal==1) {
              # Update the possible solution to view
              choiceSolutions <- c(paste('Solution', seq(1, nSets)))
              radioButtons("solutionID",
                           "There was more than one valid solution, choose which to view:",
                           choiceNames = choiceSolutions,
                           choiceValues = seq(1:nSets),
                           selected = 1
                           )
            }

            else {
              return(NULL)
            }

          })
          

  output$solutionText <- renderUI({
    req(rv$reveal)
    if(rv$reveal==0) {
      return(NULL)
    } else {
      
      text <- paste(unlist(dagSolution1()), sep = '\n')
      
      tagList(
        hr(),
        h4("Solution")
        # p(text)
      )
    }
  })

  output$solutionText2 <- renderUI({
    req(rv$reveal2)
    if(rv$reveal2==0) {
      return(NULL)
    } else {
      
      text <- paste(dagSolution2(), sep = '\n')
      
      tagList(
        hr(),
        h4("Solution"),
        p(text)
      )
    }
  })
          
  ###########################        
  ## Tutorial server logic ##        
  ###########################
          
    
  observeEvent(input$previous, {
    req(input$tuteID)
    prevTute = max(1, as.numeric(input$tuteID) - 1)
    updateRadioButtons(session, "tuteID", selected = prevTute)
    rv$reveal2 <- 0
    rv$controls <- NULL
  })
  
  observeEvent(input$advance, {
    req(input$tuteID)
    nextTute = min(nExamples, as.numeric(input$tuteID) + 1)
    updateRadioButtons(session, "tuteID", selected = nextTute)
    rv$reveal2 <- 0
    rv$controls <- NULL
  })

  
  output$tuteText <- renderUI({
    tute <- paste0("R/tute", input$tuteID, ".md")
    includeMarkdown(tute)
    
  })
  
  
  
  link <- reactive({
    # paste("http://google.com")
    paste0('https://twitter.com/intent/tweet?text=Can%20you%20solve%20this%20DAG!?&url=https://cbdrh.shinyapps.io/daggle/?_inputs_%26n=', input$n, '%26p=%27', input$p, '%27', '%26pid=', input$pid)
  })   
  
  output$tweet <- renderUI({
    tags$a(href=link(), "Tweet", class="btn btn-default twitter-share-button", icon("twitter"), target = "_blank")
  })
  
  output$test1 <- renderText({
    paste(link())
  })
  
} # Close app_server function
