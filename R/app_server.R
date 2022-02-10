#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # Your application server logic 
  updateSelectizeInput(
    session,
    inputId = 'Your_NFT',
    label   = '',
    choices = yield_data$Name
  )
  
  SteinsQT_selection <- reactive({input$Your_NFT})
  
  mod_home_server("home_ui_1", 
                  SteinsQT_selection)
}
