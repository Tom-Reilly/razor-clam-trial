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
    navbarPage("RCT App",
               footer = includeHTML(app_sys("footer.html")),
               tabPanel("Home", mod_Home_ui("Home_1")),
               tabPanel("Data Collection", mod_Data_Collection_ui("Data_Collection_1")),
               tabPanel("Data Exploration", mod_Data_Exploration_ui("Data_Exploration_1")),
               tabPanel("Resources and References", mod_ResourcesReferences_ui("ResourcesReferences_1"))
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

    add_resource_path(
      "img",
      system.file("app/img",package='RazorClamTrial')
    )

  add_resource_path(
    "shapefiles",
    system.file("app/shapefiles",package='RazorClamTrial')
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "RazorClamTrial"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
