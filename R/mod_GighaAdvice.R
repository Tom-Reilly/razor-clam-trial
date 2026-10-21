#' GighaAdvice UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_GighaAdvice_ui <- function(id){
  ns <- NS(id)
  tagList(
    div(class="shiny-fluid-row",
        column(1,),
        column(2,
               infoBoxCustom(dominant_text = "Increasing",
                             submissive_text = "Landings per unit Effort",
                             icon_name = "bar-chart")),
        column(2,
               infoBoxCustom(dominant_text = "Insufficient Data",
                             submissive_text = "Previous Assessment Fbar position to FMSY",
                             icon_name = "line-chart")),
        column(2,
               infoBoxCustom(dominant_text = "Insufficient Data",
                             submissive_text = "Updated Assessment Fbar position to FMSY",
                             icon_name = "line-chart")),
        column(2,
               infoBoxCustom(dominant_text = "Good",
                             submissive_text =  "Length Based Indicators",
                             icon_name = "ruler")),
        column(2,
               infoBoxCustom(dominant_text = "No Survey",
                             submissive_text = "Last Survey",
                             icon_name = "calendar")),
        column(1,)
    ),
    fluidRow(
      column(12,
             div(class="advice-box",
                 includeHTML(app_sys("gighaAdvice.html"))))
    ),
    fluidRow(
      column(width = 6,
             div(class="advice-box",
                 id = "landings",
                 width = 12,
                 echarts4rOutput(ns("gighaLanPlot"))
             ),
             div(class="advice-box",
                 id = "lpue",
                 width = 12,
                 #div(class = "advice-box",
                 #    width = 12,
                 #     echarts4rOutput(ns("clydeLPUEPlot")))
                 #img(src="img/annual_lpue_Firth of Clyde.jpeg", class="img-col")
             )
      ),
      column(width = 6,
             div(class="advice-box",
                 id = "effort",
                 width = 12,
                 img(src="img/gigha_Trial Year 7.png", class="img-col")
             )
      )
    )
  )
}

#' GighaAdvice Server Functions
#'
#' @noRd
mod_GighaAdvice_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$gighaLanPlot <- renderEcharts4r({

      gighaLan = read_fst("extdata/gighaLandings.fst")

      gighaLan |>
        e_charts(TrialYear) |>
        e_bar(LanWt) |>
        e_title(subtext = stringr::str_wrap("Figure 1. Total razor clam landings (in Kg) by trial year in the Gigha trial area",60),
                left = "center",
                bottom = 0) |>
        e_grid(left = "15%") |> # add padding to the left of plot
        e_tooltip(formatter = htmlwidgets::JS("
      function(params){
        return('<strong>' + params.name +
        '</strong><br />Landed Weight: ' + params.value[1] + ' Kg')
      }
    ")) |>
        e_legend(show=FALSE) |>
        e_theme("wonderland") |>
        e_dims(height='auto',width='auto')

    })
  })
}

## To be copied in the UI
# mod_GighaAdvice_ui("GighaAdvice_1")

## To be copied in the server
# mod_GighaAdvice_server("GighaAdvice_1")
