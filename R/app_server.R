#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # Your application server logic 

  mod_testMod_server("testMod_ui_1")
  # Set up reactive values
  button <- reactiveValues(run = 0, submit = 0, reveal = 0)
  dag <- reactiveValues(show = 1)
  control <- reactiveValues()
  pointer <- reactiveValues()


  # Reveal button logic
  observeEvent(input$reveal, {
    button$reveal <- abs(button$reveal - 1)
  })

  # Update random DAG input parameters on click
  user_choice <- eventReactive(input$run,{

    control$vars <- NULL
    pointer$x <- NULL
    pointer$y <- NULL
    button$reveal <- 0
    dag$mark <- NULL

    n = input$n
    p = as.numeric(input$p)

    return(c(n, p))

    ## use ignoreNULL to fire the event at startup
  }, ignoreNULL = FALSE)


  # Generate random DAG
  observe({
    uc <- user_choice()
    dag$rand <- randDAG(uc[1], uc[2])

    # Because the scale for the DAG plot changes randomly, need to redefine the tol value for user clicks
    dag$tol <- round((max(dag$rand$data$xend, na.rm = T) - min(dag$rand$data$x, na.rm = T))/20, 2)
  })

  # Get the valid adjustment sets for the random dag
  observe({
    dag$adjSets <- dagitty::adjustmentSets(dag$rand$dag, type = 'minimal', effect = input$effect)
    dag$nSets <- length(dag$adjSets)
    dag$solutionID <- 1
  })

  observeEvent(input$solutionID, {

    dag$solutionID <- input$solutionID

  })

  observe({
    dag$solution <- dag$adjSets[[dag$solutionID]]
  })

  observe({
    req(input$plotClick$x, input$plotClick$y)
    pointer$x <- input$plotClick$x
    pointer$y <- input$plotClick$y
  })

  clickedVar <- reactive({

    req(pointer$x, pointer$y)

    dag$rand$data %>%
      dplyr::filter(dplyr::between(x, pointer$x - dag$tol, pointer$x + dag$tol)) %>%
      dplyr::filter(dplyr::between(y, pointer$y - dag$tol, pointer$y + dag$tol)) %>%
      dplyr::select(name) %>%
      dplyr::filter(name != 'X') %>%
      dplyr::filter(name != 'Y') %>%
      unlist() %>%
      unique()
  })

  observeEvent(input$plotClick, {

    dag$mark <- NULL
    req(clickedVar())

    if (clickedVar() %in% control$vars) {
      control$vars <- control$vars[! control$vars %in% clickedVar()]
    }
    else {
      control$vars <- append(control$vars, clickedVar()) %>% unique()
    }

  })


  mod_drawDag_server("drawDag_ui_1", dag = reactive(dag$rand))
  #                    # dag = reactive(dag$rand),
  #                    # controls = reactive(control$vars),
  #                    # label = NULL,
  #                    # colliderlines = FALSE,
  #                    # show = reactive(dag$show))
  # 
  output$test <- renderText({
    paste("Grade: ", dag$mark, "Solution:", dag$adjSets)
    })

  mod_drawDag_server("drawDag_ui_2", dag = reactive(dag$rand))
  #                        # dag = reactive(dag$rand),
  #                        # controls = reactive(dag$solution),
  #                        # label = NULL,
  #                        # colliderlines = TRUE,
  #                        # show = reactive(button$reveal))

      # Show solution options if more than 1 solution
      output$solutionOpts <- renderUI({

        req(dag$nSets)

        if (dag$nSets >=2 & button$reveal==1 ) {
          # Update the possible solution to view
          choiceSolutions <- c(paste('Solution', seq(1, dag$nSets)))
          radioButtons("solutionID",
                       "There was more than one valid solution, choose which to view:",
                       choiceNames = choiceSolutions,
                       choiceValues = seq(1:dag$nSets),
                       selected = 1
                       )
        }

        else {
          return(NULL)
        }

      })

      # Calculate marks
      observeEvent(input$submit, {

          submission <- control$vars
          dag$mark <- grader(submission, dag$adjSets, dag$nSets)

          message <- ifelse(dag$mark, paste('Correct!', emo::ji('happy')), paste('Incorrect!', emo::ji('sad')))

          shinyalert::shinyalert(title = message,
                                 text = NULL,
                                 animation = FALSE,
                                 showConfirmButton = FALSE,
                                 className = "alert",
                                 timer = 1000)

      })

  output$printSelected <- renderText({

    if (all.equal(0, length(control$vars)) == TRUE) {
      return("No adjustment needed")
    } else return(control$vars)

  })

  output$randPlot <- renderPlot({
      shinipsum::random_ggplot()
      })

  output$plotTest <- renderPlot({

    dag  <- ggdag::dagify(y ~ x + z2 + w2 + w1,
                          x ~ z1 + w1,
                          z1 ~ w1 + v,
                          z2 ~ w2 + v,
                          w1 ~~ w2)

    ggdag::ggdag(dag)
  })
  
}
