#' ResourcesReferences UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList icon
mod_ResourcesReferences_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
    fluidRow(
      column(8, div(class="box",includeHTML(app_sys("about.html"))),
             div(class="box",
                 includeHTML(app_sys("references.html")),
             )),
      column(4,
        div(class="box",
          h3("Social Media"),
          p(icon("x-twitter"), a("X", href="https://twitter.com/scotgovmarine")),
          p(icon("linkedin"), a("LinkedIn", href="https://uk.linkedin.com/company/marine-directorate")),
          p(icon("flickr"), a("flickr", href="https://www.flickr.com/photos/marinescotland")),
          p(icon("blog"), a("Blog", href="https://blogs.gov.scot/marine-scotland/"))),
        div(class="box",
            includeHTML(app_sys("pubReports.html"))
            )
      )
    ),
    fluidRow(

    )
  )
  )
}

#' ResourcesReferences Server Functions
#'
#' @noRd
mod_ResourcesReferences_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_ResourcesReferences_ui("ResourcesReferences_1")

## To be copied in the server
# mod_ResourcesReferences_server("ResourcesReferences_1")
