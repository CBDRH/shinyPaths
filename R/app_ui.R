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
    navbarPage("daggle", id = "panel", selected = 'Random', 

  # Random mode               
  tabPanel("Random", icon = icon('project-diagram'),

        # Left hand column
        column(width = 3,
               tippy::tippy_class("tool-tip") # Need this for tippy libraries to be loaded
        ),

        # Central column
        column(width = 6,
               tags$div(style="text-align:center;", 
                        actionButton("instructions", NULL, icon = icon('question-circle'), class = "download"),
                        actionButton("settings", NULL, icon = icon('cog'), class = "download"),
                        hr(class = 'myHr'),
                        uiOutput("directions"),
                        hr(class = 'myHr')
               ),
               
               mod_drawDag_ui("drawDag_ui_1"),
               
               tags$div(style="text-align:right;",
                        tags$div(style="display:inline-block",title="Get the code", actionButton("code", NULL, icon = icon('code'), class = "download")),
                        tags$div(style="display:inline-block",title="Get a link", actionButton("link", NULL, icon = icon('link'), class = "download")),
                        tags$div(style="display:inline-block",title="Share on twitter", uiOutput("tweet"))
               ),
               br(),
               hr(class = 'myHr'),
               tags$div(style="text-align:center;",
                  strong("Your answer:"),
                  textOutput("printSelected", inline = TRUE)
               ),
               hr(class = 'myHr'),
               br(),
               tags$div(style="text-align:center;",
                 actionButton("run", "Generate DAG", icon = icon('sync'), width = 140),
                 actionButton("submit", "Submit answer", icon = icon('share-square'), width = 140),
                 actionButton("reveal", "Reveal solution", icon = icon('project-diagram'), width = 140)
               )
        ),

        # Output controls
        column(width = 3,
               
               )
    ),

  # Tutorial mode
  tabPanel("Tutorial", icon = icon('chalkboard-teacher'),

         tags$div(style="text-align:center;",   
            uiOutput("tuteHeader")
         ),
         hr(), 
         br(),
           
         column(width = 2, 
                radioButtons("tuteID", "Tutorial", 
                             choiceValues = 1:nExamples, 
                             choiceNames = tuteNames)
                ),

         # Central column
         column(width = 5, 
                mod_drawDag_ui("drawDag_ui_3"),
                tags$div(style="text-align:center;",
                    strong("Your solution:"),
                    textOutput("printSelected2", inline = TRUE)
                ),
                tags$div(style = 'text-align: center;',
                         actionButton('previous', NULL, icon = icon("arrow-left"), width = 68),
                         actionButton('advance', NULL, icon = icon("arrow-right"), width = 68),
                         actionButton("submit2", "Submit answer", icon = icon('share-square'), width = 140),
                         actionButton("reveal2", "Reveal solution", icon = icon('project-diagram'), width = 140),
                         br(), br()
                )
                ),

         # Output controls
         column(width = 5, 
                htmlOutput("tuteText")
                )

         ),


  # About section
  tabPanel(NULL, icon = icon('info-circle'), p("page under construction"))
  
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
    rclipboard::rclipboardSetup()
    # Add here other external resources
  )
}

