#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    shinyjs::useShinyjs(),
    navbarPage("daggle", id = "panel",

  # Random mode               
  tabPanel("Random", icon = icon('project-diagram'),
          
        # Input controls
        column(width = 3,
          numericInput("n", "Number of nodes", value = 5, min = 3, max = 8, step = 1),
          selectInput("p", "Complexity", choices = c("Easy" = .4, "Moderate" = .6, "Difficult" = .8), selected = .6),
          numericInput("pid", "Puzzle ID", s1),
          radioButtons("effect", "What effect are you intersted in?", choices = c('Total' = 'total', 'Direct' = 'direct'), selected = 'total', inline = TRUE),
          # textOutput("test1")
        ),

        # Central column
        column(width = 6,
               mod_drawDag_ui("drawDag_ui_1"),
               tags$div(style="text-align:center;",
                  strong("Your solution:"),
                  textOutput("printSelected", inline = TRUE)
               ),
               tags$div(style="text-align:center;",
                 actionButton("run", "Generate DAG", icon = icon('sync'), width = 140),
                 actionButton("submit", "Submit answer", icon = icon('share-square'), width = 140),
                 actionButton("reveal", "Reveal solution", icon = icon('project-diagram'), width = 140)
               ),
               htmlOutput("solutionOpts"),
               htmlOutput("solutionText"),
               conditionalPanel("output.reveal=='show'", mod_drawDag_ui("drawDag_ui_2"))
               ),

        # Output controls
        column(width = 3,
               uiOutput("tweet")
               )
    ),

  # Tutorial mode
  tabPanel("Tutorial", icon = icon('chalkboard-teacher'),

         column(width = 3, 
                radioButtons("tuteID", "Tutorial", choiceValues = 1:nExamples, choiceNames = tuteNames)
                ),

         # Central column
         column(width = 6, 
                mod_drawDag_ui("drawDag_ui_3"),
                tags$div(style="text-align:center;",
                    strong("Your solution:"),
                    textOutput("printSelected2", inline = TRUE)
                ),
                tags$div(style = 'text-align: center;',
                         actionButton('previous', NULL, icon = icon("arrow-left"), width = 68),
                         actionButton('advance', NULL, icon = icon("arrow-right"), width = 68),
                         actionButton("submit2", "Submit answer", icon = icon('share-square'), width = 140),
                         actionButton("reveal2", "Reveal solution", icon = icon('project-diagram'), width = 140)
                ),
                htmlOutput("solutionOpts2"),
                htmlOutput("solutionText2"),
                conditionalPanel("output.reveal2=='show'", mod_drawDag_ui("drawDag_ui_4"))
                ),

         # Output controls
         column(width = 3, 
                uiOutput("tuteText")
                )

         ),

  # About section
  tabPanel("About", icon = icon('info-circle'), p("Placeholder"))

    )
)
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'daggle'
    ),
    shinyjs::useShinyjs(),
    rclipboard::rclipboardSetup(),
    tags$style(
      HTML(
        ".alert {
          background-color: white; height: 120px; padding: 0px 10px 10px 10px;
          }"
      ),
      HTML(
        ".code {
          background-color: white;
          }"
      ),
      HTML(
        ".download {
          border-color: white; color: #ccc;
          }"
      ),
      HTML(
        ".twitter-share-button {
          background-color: #1DA1F2; /* Twitter Blue */
          color: white;
          padding: 6px 12px;
          text-align: center;
        }"
      )
    )
    # Add here other external resources
  )
}

