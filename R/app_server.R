#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  mod_Data_Collection_server("Data_Collection_1")
  mod_Data_Exploration_server("Data_Exploration_1")
  mod_ResourcesReferences_server("ResourcesReferences_1")
}
