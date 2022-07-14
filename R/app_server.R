#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {

  # set up reactive values pid = NULL, 
  rv <- reactiveValues(n = 5, p = '0.6', effect = 'total', id = NULL, solutionChoice = 1)
  
  observe({
    rv$pid <- ifelse(is.null(rv$pid), s1, rv$pid) 
  })

  # Update settings if a unique id is supplied
  observeEvent(rv$id, {
    req(rv$id)

    idA <- as.numeric(substr(rv$id, 1, 1))
    idB <- as.numeric(substr(rv$id, 2, 2))
    idC <- as.numeric(substr(rv$id, 3, 3))
    idD <- as.numeric(substr(rv$id, 4, 6))

    rv$effect <- ifelse(idA == 1, 'total', 'direct')
    rv$n <- floor((idB + 8)/2)
    rv$p <- if (idC < 3) {
      "0.4"
    } else if (idC < 7) {
      "0.6"
    } else "0.8"

    rv$pid <- as.integer(idD)

  })

  # Update unique ID based on current settings
  observe({

    req(rv$n, rv$p, rv$pid, rv$effect)

    aID <- ifelse(rv$effect=="total", 1, 2)
    bID <- (rv$n*2)-8
    cID <- if (rv$p == "0.4") {
      "0"
    } else if (rv$p == "0.6") {
      "5"
    } else "9"

    dID <- as.character(rv$pid)

    rv$id <- as.numeric(paste0(aID, bID, cID, dID))
  })

  
  # Show settings on click
  
  # Make the url available on click
  observeEvent(input$settings, {
    
    updateNumericInput(session, 'n', value = rv$n)
    updateSelectInput(session, 'p', selected = rv$p)
    updateNumericInput(session, 'pid', value = rv$pid)
    updateRadioButtons(session, 'effect', selected = rv$effect)
    
    showModal(modalDialog(
      title = HTML(paste(icon('cog'), "Settings")),
      numericInput("n", "Number of nodes", value = 5, min = 3, max = 8, step = 1),
      selectInput("p", "Complexity", choices = c("Easy" = .4, "Moderate" = .6, "Difficult" = .8), selected = .6),
      numericInput("pid", "Puzzle ID", NULL, step = 1, min = 100, max = 999),
      radioButtons("effect", "Effect of interest", choices = c('Total effect of X on Y' = 'total', 'Direct effect of X on Y' = 'direct'), selected = 'total', inline = FALSE),
      footer = tagList(div(style = "text-align:right;",
        actionButton("cancelSettings", "Cancel", icon = icon('window-close')),
        actionButton("saveSettings", "Save and run", icon = icon('save'))
      )),
      easyClose = FALSE,
      fade = TRUE
    ))
  })  
  
  
  # Close settings modal
  observeEvent(input$cancelSettings, {
    removeModal()
  })
  
  observeEvent(input$saveSettings, {
    removeModal()
    rv$n <- input$n
    rv$p <- input$p
    rv$pid <- input$pid
    rv$effect <- input$effect
  })
  
  
  observe({
    updateNumericInput(session, 'n', value = rv$n)
    updateSelectInput(session, 'p', selected = rv$p)
    updateNumericInput(session, 'pid', value = rv$pid)
    updateRadioButtons(session, 'effect', selected = rv$effect)
  })
  
  observeEvent(rv$n == rv$n, {
    updateNumericInput(session, 'n', value = rv$n)
  }, ignoreNULL = FALSE)
  
    
  # Instructions modal
  observeEvent(input$instructions, {
    
    showModal(modalDialog(
      title = HTML(paste(icon('question-circle'), "How to play")),
      HTML(paste("The aim is to identify a minimal adjustment set to identify the effect of an exposure", 
                   tags$span(class='xNode', "X"),
                   "on an outcome", 
                   tags$span(class='yNode', "Y"), "."
                   )), 

      p("Click or tap on a node to add that variable to the adjustment list. Click or tap on an adjusted variable to remove it from the list."),
      
      p("To estimate the total effect, a minimal adjusment set must close any open backdoor paths between X and Y. To estimate the direct effect you must also control for mediating variables between X and Y." ),
      
      p('To check if a path is closed, split the path up into consecutive triplets and examine each triplet. If any triplet is closed the whole path is closed'),
      
      tags$img(class="center", src="www/dag-examples.png"),
      
      p(HTML(paste("Click on the cog icon", span(style = "color: #ccc; font-weight: bold;", icon('cog')), "to change the number of nodes, the DAG complexity or the effect of interest
                   (either the ", span(style = "color: black; background-color:white; font-weight: bold;", "total"), "effect or the",
                   span(style = "color: black; background-color:white; font-weight: bold;", "direct"), "effect)."
                   ))),
      
      footer = tagList(div(style = "text-align:right;",
                           actionButton("closeInstructions", "Got it", icon = icon('thumbs-up'))
      )),
      easyClose = TRUE,
      fade = TRUE
    ))
  })  
  
  # Close instructions modal
  observeEvent(input$closeInstructions, {
    removeModal()
  })
  
  
  # Directions text
  
  output$directions <- renderUI({
    
    req(rv$effect)
    
    effect <- rv$effect
    text <- HTML(paste("Select a minimal adjustment set to identify the", tags$strong(effect), "effect of", 
                       tags$span(class = 'xNode', "X"), "on", 
                       tags$span(class = 'yNode', "Y")
                       ))
    
    helpText(text)
    
  })
  
  
  # Reset reactive values on panel change
  observeEvent(input$panel,{
    rv$controls <- NULL
    rv$solutionChoice <- 1
  })
  

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
    } else c('Adjust for', knitr::combine_words(sort(rv$controls), oxford_comma = FALSE))
  })
  
  
  # Control the active DAG
  
  # Reset reactive values on Run
  observeEvent(input$run,{
    
    rv$controls <- NULL
    rv$reveal <- 0
    rv$reveal2 <- 0
    rv$solutionChoice <- 1
    rv$pid <- as.integer(100 + runif(1)*900)
  })
  
  # Reset reactive values on Run2
  observeEvent(input$run2,{
    
    removeModal()
    rv$controls <- NULL
    rv$reveal <- 0
    rv$reveal2 <- 0
    rv$solutionChoice <- 1
    rv$pid <- as.integer(100 + runif(1)*900)
  })
  
  ## Random DAG
  dag1 <- eventReactive(input$run | input$saveSettings | input$run2, {
    
    req(rv$n, rv$p, rv$pid)

    randDAG(rv$n, rv$p, rv$pid)
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

  dagSolution1 <- eventReactive(input$reveal | input$submit, {
    dagitty::adjustmentSets(dag1()$dag, type = 'minimal', effect = rv$effect)
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
    dag2() %>% adjust_for(dagSolution2()[[1]])
  })
  
  mod_drawDag_server("drawDag_ui_1", dagAdj1, did = reactive(rv$id), n = reactive(rv$n), pid = reactive(rv$pid)) # Random DAG
  mod_drawDag_server("drawDag_ui_2", dagSolved1, did = reactive(rv$id), n = reactive(rv$n), pid = reactive(rv$pid), colliderlines = 1) # Random DAG solution
  mod_drawDag_server("drawDag_ui_3", dagAdj2, did = reactive(rv$id), n = reactive(5), pid = reactive(rv$pid), label = 1) # Tutorial DAG
  mod_drawDag_server("drawDag_ui_4", dagSolved2, did = reactive(rv$id), n = reactive(5), pid = reactive(rv$pid), label = 1) # Tutorial DAG solution

  # Make the code available on click
  observeEvent(input$code, {
    
    codeSnip <- untidy_dagitty(dagAdj1())
    
    showModal(modalDialog(
      title = HTML(paste(icon('code'), "Code to draw this DAG")),
      footer = modalButton("Done"),
      splitLayout(
        column(width = 6,
               h3("dagitty.net"),
               hr(),
               helpText(HTML(paste("Reproduce on", tags$a(href="http://www.dagitty.net/dags.html", "dagitty.net", target = "_blank")))),
               rclipboard::rclipButton("copy1", "Copy and close", codeSnip$dagitty, modal = TRUE, icon = icon("copy")),
               br(), br(),
               tags$div(class = 'codeBlock', HTML(gsub("\n","<br/>",codeSnip$dagitty[[1]])))
               
        ),
        column(width = 6,
               h3("R"),
               hr(),
               helpText(HTML(paste("Reproduce  in R using", tags$code("dagitty"), "or", tags$code("ggdag")))),
               rclipboard::rclipButton("copy2", "Copy and close", codeSnip$r, modal = TRUE, icon = icon("copy")),
               br(), br(),
               tags$div(class = 'codeBlock',HTML(gsub("\n","<br/>",codeSnip$r[[1]])))
               
        )
      ),
      easyClose = TRUE,
      fade = TRUE
    ))
  })  
  
  # Close modal when code is copied
  observeEvent(input$copy1, {
    removeModal()
  })
  
  # Close modal when code is copied
  observeEvent(input$copy2, {
    removeModal()
  })
  
  url <- reactive({
    paste0('https://cbdrh.shinyapps.io/daggle/?_values_&id=', rv$id)
  })
  
  twitterLink <- reactive({
    paste0('https://twitter.com/intent/tweet?text=Can%20you%20solve%20this%20%23daggle?%0A&url=https://cbdrh.shinyapps.io/daggle/?_values_%26id=', rv$id)
  })   
  
  # Make the url available on click
  observeEvent(input$link, {

    showModal(modalDialog(
      title = HTML(paste(icon('link'), "URL link to this daggle")),
      footer = modalButton("Done"),
      div(style="color:#4FBAE4; background-color:white;", url()),
      br(),
      rclipboard::rclipButton("copy3", "Copy and close", url(), modal = TRUE, icon = icon("copy")),
      easyClose = TRUE,
      fade = TRUE
    ))
  })  
  
  observeEvent(input$link2, {
    
    showModal(modalDialog(
      title = "daggle url",
      footer = modalButton("Done"),
      div(style="color:#4FBAE4; background-color:white;", url()),
      br(),
      rclipboard::rclipButton("copy4", "Copy and close", url(), modal = TRUE, icon = icon("copy")),
      easyClose = TRUE,
      fade = TRUE
    ))
  })  
  
  # Close modal when code is copied
  observeEvent(input$copy3, {
    removeModal()
  })
  
  # Close modal when code is copied
  observeEvent(input$copy4, {
    removeModal()
  })
  
  # Tweet on click
  eventReactive(input$tweet, {
    twitterLink()
  }) 
  
  # Twitter button
  output$tweet <- renderUI({
    tags$a(href=twitterLink(), NULL, class="btn btn-default download2", icon("twitter"), target = "_blank")
  })  
  
# Check solution in random mode
observeEvent(input$submit, {

    submission <- rv$controls

    solution <- dagSolution1()
    if (input$panel=="Tutorial") {
      solution <- dagSolution2()
      }
    nSol <- length(solution)
    grade <- grader(submission, solution)
    mark <- grade[[1]]

    if(mark == TRUE) {

      exhaltation <- sample(encouragementList, 1)

      showModal(modalDialog(title = div(style = "text-align: center; font-size: 22pt;", paste('Correct!', emo::ji('happy'), '\n')),
                            div(style = "text-align: center;", tagList(
                                 text = learnr::random_praise(),
                                 br(),
                                 actionButton("run2", "Generate DAG", icon = icon('sync'), width = 140, class="btn btn-default"),
                                 actionButton("link2", "Get url", icon = icon('link'), width = 140, class="btn btn-default"),
                                 tags$a(href=twitterLink(), "Share", class="btn btn-default twitter-share-button", icon("twitter"), target = "_blank")
                               )),
                            easyClose = TRUE,
                            fade = TRUE
      ))

    }
    else if (mark == FALSE){

      shinyalert::shinyalert(title =  paste('Incorrect', emo::ji('sad'), '\n'),
                             text = learnr::random_encouragement(),
                             animation = FALSE,
                             showConfirmButton = FALSE,
                             className = "alert",
                             timer = 600,
                             closeOnClickOutside = TRUE,
                             closeOnEsc = TRUE)

    }


}, ignoreInit = TRUE)
  

output$done <- renderUI({

  if(input$tuteID < nExamples) {
    return(actionButton("advance2", "Next!", icon = icon('arrow-right'), width = 140, class="btn btn-default"))
  }

  else if(input$tuteID == nExamples) {
    return(actionButton("done", "Done!", icon = icon('check'), width = 140, class="btn btn-default"))
  }

})


# Check solution in tutorial mode
observeEvent(input$submit2, {

  submission <- rv$controls

  solution <- dagSolution2()
  nSol <- length(solution)
  grade <- grader(submission, solution)
  mark <- grade[[1]]

  if(mark == TRUE) {

    showModal(modalDialog(title = div(style = "text-align: center; font-size: 22pt;", paste('Correct!', emo::ji('happy'), '\n')),
                          div(style = "text-align: center;", tagList(
                            text = learnr::random_praise(),
                            br(), br(),
                            uiOutput("done")
                          )),
                          easyClose = TRUE,
                          fade = TRUE
    ))

  }
  else if (mark == FALSE){

    shinyalert::shinyalert(title =  paste('Incorrect', emo::ji('sad'), '\n'),
                           text = learnr::random_encouragement(),
                           animation = FALSE,
                           showConfirmButton = FALSE,
                           className = "alert",
                           timer = 600,
                           closeOnClickOutside = TRUE,
                           closeOnEsc = TRUE)

  }


}, ignoreInit = TRUE)


      # Show solution options if more than 1 solution
      output$solutionOpts <- renderUI({

        req(dagSolution1())

        nSets <- length(dagSolution1())

        # Update the possible solution to view
        choiceSolutions <- c(paste('Solution', seq(1, nSets)))
      tagList(
        h4(HTML((paste("Minimal adjustment sets to estimate the", tags$strong(rv$effect), "effect of X on Y")))),
        br(),
        radioButtons("solutionID",
                     NULL,
                     choiceNames = choiceSolutions,
                     choiceValues = seq(1:nSets),
                     selected = 1
                     )
      )

      })
          

  output$solutionText <- renderUI({

    text <- if (length(dagSolution1()[[1]]) == 0) {
      "No adjustment necessary!"
    } else {
      knitr::combine_words(sort(dagSolution1()[[rv$solutionChoice]]))
    }
    
    tagList(
      
      p(text)
    )
    
  })

  # Reveal solution on click
  observeEvent(input$reveal, {
    
    req(dagSolution1(), rv$solutionChoice)
    
    showModal(modalDialog(
      title = HTML(paste(icon('project-diagram'), 'Solution')),
      tagList(
        htmlOutput("solutionOpts"),
        htmlOutput("solutionText"),
        mod_drawDag_ui("drawDag_ui_2")
      ),
      easyClose = TRUE,
      fade = TRUE,
      footer = tagList(div(style = "text-align:right;", actionButton("closeSolution", "Got it", icon = icon('thumbs-up'))))
    ))
    
  })
  
  # Close solution modal on click
  observeEvent(input$closeSolution, {
    removeModal()
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
  
  observeEvent(input$advance2, {
    req(input$tuteID)
    removeModal()
    nextTute = min(nExamples, as.numeric(input$tuteID) + 1)
    updateRadioButtons(session, "tuteID", selected = nextTute)
    rv$reveal2 <- 0
    rv$controls <- NULL
  })
  
  observeEvent(input$done, {
    removeModal()
    rv$reveal2 <- 0
  })

  output$tuteHeader <- renderUI({
    req(input$tuteID)
    tags$h4(tuteHeaders[as.numeric(input$tuteID)])
  })

  output$tuteText <- renderUI({
    # tute <- paste0("R/tute", input$tuteID, ".html")
    # includeHTML(tute)
    if(input$tuteID=='1'){ includeHTML("R/tute1.html")}
    else if (input$tuteID=='2'){ includeHTML("R/tute2.html")}
    else if (input$tuteID=='3'){ includeHTML("R/tute3.html")}
    else if (input$tuteID=='4'){ includeHTML("R/tute4.html")}
    else if (input$tuteID=='5'){ includeHTML("R/tute5.html")}
    else if (input$tuteID=='6'){ includeHTML("R/tute6.html")}
    else if (input$tuteID=='7'){ includeHTML("R/tute7.html")}
    else if (input$tuteID=='8'){ includeHTML("R/tute8.html")}
  })

  # Reveal solution on click
  observeEvent(input$reveal2, {

    req(dagSolution2())

    text <- if (length(dagSolution2()[[1]]) == 0) {
      "No adjustment necessary!"
    } else {
      knitr::combine_words(sort(dagSolution2()[[1]]))
    }
    # 
    showModal(modalDialog(
      title = HTML(paste(icon('project-diagram'), 'Solution')),
      tagList(
        h4(HTML((paste("Minimal adjustment sets to estimate the",
                       tags$strong(eval(as.name(paste0("effect", input$tuteID)))),
                       "effect of X on Y")))),
        p(text),
        mod_drawDag_ui("drawDag_ui_4")
      ),
      easyClose = TRUE,
      fade = TRUE, 
      footer = tagList(div(style = "text-align:right;", actionButton("closeSolution2", "Got it", icon = icon('thumbs-up'))))
    ))

  })

  # Close tutorial solution modal on click
  observeEvent(input$closeSolution2, {
    removeModal()
  })
  
  # Bookmarking
  # Exclude certain parameters from bookmark
  setBookmarkExclude(names = c("run", "reveal2", "instructions", "settings", "link", "previous", "panel", "submit", "tuteID", "submit2", "code", "reveal", "advance", "plotClick", "cancelSettings", "saveSettings", "n", "p", "pid", "pid2", "effect"))

  # Save extra values when we bookmark
  onBookmark(function(state) {
    state$values$id <- rv$id
  })

  # Read values from state$values when we restore
  onRestore(function(state) {
    rv$id <- state$values$id
  })

} # Close app_server function
