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
  mod_BroadBayAdvice_server("BroadBayAdvice_1")
    mod_ClydeAdvice_server("ClydeAdvice_1")
    mod_CollAndTireeAdvice_server("CollAndTireeAdvice_1")
    mod_ColonsayAdvice_server("ColonsayAdvice_1")
    mod_FirthOfForthAdvice_server("FirthOfForthAdvice_1")
    mod_GighaAdvice_server("GighaAdvice_1")
    mod_SoundOfSleatAdvice_server("SoundOfSleatAdvice_1")
    mod_WigtownBayAdvice_server("WigtownBayAdvice_1")
  mod_ResourcesReferences_server("ResourcesReferences_1")
}
