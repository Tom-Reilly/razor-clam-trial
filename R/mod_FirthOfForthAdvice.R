#' FirthOfForthAdvice UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_FirthOfForthAdvice_ui <- function(id){
  ns <- NS(id)
  tagList(
    div(class="shiny-fluid-row",
        column(1,),
        column(2,
               infoBoxCustom(dominant_text = "Stable",
                             submissive_text = "Landings per unit Effort",
                             icon_name = "bar-chart")),
        column(2,
               infoBoxCustom(dominant_text = "Below",
                             submissive_text = "Previous Assessment Fbar position to FMSY",
                             icon_name = "line-chart")),
        column(2,
               infoBoxCustom(dominant_text = "Below",
                             submissive_text = "Updated Assessment Fbar position to FMSY",
                             icon_name = "line-chart")),
        column(2,
               infoBoxCustom(dominant_text = "Good",
                             submissive_text =  "Length Based Indicators",
                             icon_name = "ruler")),
        column(2,
               infoBoxCustom(dominant_text = "2020",
                             submissive_text = "Last Survey",
                             icon_name = "calendar")),
        column(1,)
    ),
    fluidRow(
      column(12,
             div(class="advice-box",
                 includeHTML(app_sys("firthofforthAdvice.html"))))
    ),
    fluidRow(
      column(width = 12,
        div(class="advice-box",
            id = "fof-landings",
            width = 12,
            h3("Total landings by year", style="color:#1A5276;"),
            echarts4rOutput(ns("firthofforthLanPlot")),
            HTML('<a href="#top">Back to top</a>')
        )
      )
    ),
    fluidRow(
      column(width = 12,
        div(class="advice-box",
            id = "fof-lpue",
            width = 12,
            h3("Landings per unit effort", style="color:#1A5276;"),
            echarts4rOutput(ns("firthofforthLPUEPlot")),
            HTML('<a href="#top">Back to top</a>')
        )
      )
    ),
    fluidRow(
      column(width = 12,
        div(id = "fof-effort",
            leafletOutput(ns("firthofforthEffortMap"),
                         height = 600),
            HTML('<a href="#top">Back to top</a>')
        )
      )
    ),
    fluidRow(
      column(12,
        div(class="advice-box",
            id = "fof-lca",
            width = 12,
            dropdown(includeHTML(app_sys("glossaryTable.html")),
                     style = "fill", icon = icon("info"),
                     status = "success", width = "90%",
                     animate = animateOptions(
                               enter = "fadeInLeftBig",
                               exit = "fadeOutRightBig"
                     )),
            includeHTML(app_sys("clydeAdviceLCA.html")),
            HTML('<a href="#top">Back to top</a>')
      )
    )
  ),
    fluidRow(
      column(12,
        div(class = "advice-box",
            id = "fof-lbi",
            width=12,
            includeHTML(app_sys("clydeAdviceLBI.html")),
            HTML('<a href="#top">Back to top</a>')
        )
      )
    )
  )
}

#' FirthOfForthAdvice Server Functions
#'
#' @noRd
mod_FirthOfForthAdvice_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$firthofforthLanPlot <- renderEcharts4r({

      firthofforthLan = read_fst("extdata/firthofforthLandings.fst")

      firthofforthLan |>
        e_charts(TrialYear) |>
        e_bar(LanWt) |>
        e_title(subtext = stringr::str_wrap("Figure 1. Total razor clam landings (in Kg) by trial year in the Firth of Forth trial area",60),
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
# mod_FirthOfForthAdvice_ui("FirthOfForthAdvice_1")

## To be copied in the server
# mod_FirthOfForthAdvice_server("FirthOfForthAdvice_1")
