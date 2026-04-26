#' Datasets UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_Datasets_ui <- function(id) {
  ns <- NS(id)
  tagList(
 
  )
}
    
#' Datasets Server Functions
#'
#' @noRd 
mod_Datasets_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_Datasets_ui("Datasets_1")
    
## To be copied in the server
# mod_Datasets_server("Datasets_1")
