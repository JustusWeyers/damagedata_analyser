#' Insight UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_Insight_ui <- function(id) {
  ns <- NS(id)
  tagList(
 
  )
}
    
#' Insight Server Functions
#'
#' @noRd 
mod_Insight_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_Insight_ui("Insight_1")
    
## To be copied in the server
# mod_Insight_server("Insight_1")
