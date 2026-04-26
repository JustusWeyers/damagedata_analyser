#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd

app_ui <- function(request) {
  tagList(

    golem_add_external_resources(),  # Leave this function for adding external resources

    
    bs4Dash::dashboardPage(
      bs4Dash::dashboardHeader(
        title = bs4Dash::dashboardBrand(
          title = "Damagedata Analyser",
          # color = "gray-dark",
          href = NULL
        ),
        titleWidth = 250
      ),
      bs4Dash::dashboardSidebar(
        
        bs4Dash::sidebarMenu(
          id = "sidebar",   # nur EIN Menü!
          
          tags$li(class = "nav-header", tags$h5("HOME")),
          bs4Dash::menuItem("Introduction", tabName = "introduction"),
          bs4Dash::menuItem("Methods", tabName = "methods"),
          bs4Dash::menuItem("Literature", tabName = "literature"),
          
          tags$li(class = "nav-header", tags$h5("DATA")),
          bs4Dash::menuItem("Damage Data", tabName = "fulldata"),
          bs4Dash::menuItem("DWD", tabName = "othersources1"),
          bs4Dash::menuItem("BKG", tabName = "othersources2"),
          
          bs4Dash::menuItem(
            "Datasets",
            startExpanded = FALSE,  # optional
            
            bs4Dash::menuSubItem("Dataset 1", tabName = "datasets-sub1"),
            bs4Dash::menuSubItem("Dataset 2", tabName = "datasets-sub2"),
            bs4Dash::menuSubItem("Dataset 3", tabName = "datasets-sub3")
          ),
          
          tags$li(class = "nav-header", tags$h5("MODELS")),
          bs4Dash::menuItem("Random Forest 3", tabName = "rf3"),
          bs4Dash::menuItem("Random Forest 4", tabName = "rf4"),
          
          tags$li(class = "nav-header", tags$h5("OTHER METHODS")),
          bs4Dash::menuItem("RFE", tabName = "mathods1"),
          # bs4Dash::menuItem("Method 2", tabName = "mathods2"),
          # bs4Dash::menuItem("Method 3", tabName = "mathods3"),
          
          tags$li(class = "nav-header", tags$h5("SETTINGS")),
          bs4Dash::menuItem("Settings", tabName = "settings")
        )
      ),
      
      bs4Dash::dashboardBody(
        # Boxes need to be put in a row (or column)
        shiny::fluidRow(
          shiny::titlePanel("Database Connection"),
        ),
        shiny::fluidRow(
          shiny::uiOutput("db_ui"),
          
          shiny::hr(),
          
          shiny::tableOutput("preview")
        )
      ),
      dark = TRUE
    ),
    
    tags$head(
      tags$style(HTML("
        .brand-link {
          pointer-events: none; 
          text-decoration: none !important;
          color: inherit !important;
        }
      ")),
      tags$style(HTML("
        .nav-sidebar .nav-link.active {
          background-color: transparent !important;
          color: inherit !important;
        }
      ")),
      tags$style(HTML("
        .nav-sidebar .nav-header {
          color: #ced4da  !important;
        }
      "))
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
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "ForestAnalytics"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
