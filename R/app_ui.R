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
    navbarPage("shinyPaths",

  # Random mode               
  tabPanel("Random", icon = icon('project-diagram'),
          
        # Input controls
        column(width = 3,
          numericInput("n", "Number of nodes", value = 5, min = 3, max = 8, step = 1),
          selectInput("p", "Complexity", choices = c("Easy" = .4, "Moderate" = .6, "Difficult" = .8), selected = .6),
          radioButtons("effect", "What effect are you intersted in?", choices = c('Total' = 'total', 'Direct' = 'direct'), selected = 'total', inline = TRUE),
          strong("Selected minimal adjustment set:"),
          textOutput("printSelected"),
          textOutput("test1")
        ),

        # Central column
        column(width = 6,
               mod_drawDag_ui("drawDag_ui_1"),
               div(style="text-align:center;",
                 actionButton("run", "Generate DAG", icon = icon('sync'), width = 140),
                 actionButton("submit", "Submit answer", icon = icon('share-square'), width = 140),
                 actionButton("reveal", "Reveal solution", icon = icon('project-diagram'), width = 140)
               ),
               htmlOutput("solutionOpts"),
               conditionalPanel("output.reveal=='show'", mod_drawDag_ui("drawDag_ui_2"))
               ),

        # Output controls
        column(width = 3,
               actionButton("download", "Download", icon = icon('download')),
               actionButton("code", "Get code", icon = icon('code')),
               actionButton("share", "Share", icon = icon('twitter'))
               )
    ),

  # Tutorial mode
  tabPanel("Tutorial", icon = icon('chalkboard-teacher'),

         column(width = 3, shinipsum::random_text(nwords = 200)),

         # Central column
         column(width = 6, plotOutput("randPlot")),

         # Output controls
         column(width = 3, shinipsum::random_text(nwords = 200))

         ),

  # About section
  tabPanel("About", icon = icon('info-circle'), p("Placeholder"), plotOutput("plotTest"))

    )
)
}


# Define colors
exposureCol <- "#CDDC55"
outcomeCol <- "#4FBAE4"
adjustedCol <- "seagreen"
unadjustedCol <- "pink"
naCol <- 'grey80'



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
      app_title = 'shinyPaths'
    ),
    shinyalert::useShinyalert(),
    shinyjs::useShinyjs(),
    tags$style(
      HTML(
        ".alert {
          background-color: white; height: 120px; padding: 0px 10px 10px 10px;
          }"
      )
    )
    # Add here other external resources
  )
}

