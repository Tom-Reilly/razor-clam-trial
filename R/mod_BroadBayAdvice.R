#' BroadBayAdvice UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_BroadBayAdvice_ui <- function(id){
  ns <- NS(id)
  tagList(
    div(class="shiny-fluid-row",
        column(1,),
        column(2,
               infoBoxCustom(dominant_text = "Stable",
                             submissive_text = "Landings per unit Effort",
                             icon_name = "bar-chart")),
        column(2,
               infoBoxCustom(dominant_text = "Insufficient Data",
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
               infoBoxCustom(dominant_text = "No Survey",
                             submissive_text = "Last Survey",
                             icon_name = "calendar")),
        column(1,)
    ),
    fluidRow(
      column(12,
             div(class="advice-box",
                 includeHTML(app_sys("broadbayAdvice.html"))))
    ),
    column(width = 6,
           div(class="advice-box",
               id = "bbay-landings",
               width = 12,
               echarts4rOutput(ns("broadbayLanPlot"))
           ),
           div(class="advice-box",
               id = "bbay-lpue",
               width = 12,
               echarts4rOutput(ns("bbayLPUEPlot"))
               #img(src="img/annual_lpue_Firth of Clyde.jpeg", class="img-col")
           )
    ),
    column(width = 6,
           div(class="advice-box",
               id = "bbay-effort",
               width = 12,
               img(src="img/b_bay_Trial Year 7.png", class="img-col")
           )
    )
  )
}

#' BroadBayAdvice Server Functions
#'
#' @noRd
mod_BroadBayAdvice_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$broadbayLanPlot <- renderEcharts4r({

      broadbayLan = read_fst("extdata/broadbayLandings.fst")

      broadbayLan |>
        e_charts(TrialYear) |>
        e_bar(LanWt) |>
        e_title(subtext = stringr::str_wrap("Figure 1. Total razor clam landings (in Kg) by trial year in the Broad Bay trial area",60),
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

    output$bbayLPUEPlot <- renderEcharts4r({

      bbayLPUE = lpueData %>% filter(trial_area == "Broad Bay")

      bbayLPUE |>
        e_charts(activity_date) |>
        e_scatter(LPUE_kg_hr) |>
        e_title(subtext = stringr::str_wrap("Figure 2. Landings per Unit Effort (LPUE) in the Broad Bay trial area",60),
                left = "center",
                bottom = 0) |>
        e_grid(containLabel = TRUE) |> # include labels when rescaling plot
        e_tooltip(trigger="axis", formatter = htmlwidgets::JS(
          "function(params) {
        return('<strong>Date:</strong> ' + params[0].name);
      }"
        )) |>
        e_legend(show=FALSE) |>
        e_theme("wonderland") |>
        e_dims(height='auto',width='auto') |>
        e_y_axis(
          name = "Landings per Unit Effort (Kg/hr)",
          nameLocation = "center",
          nameGap = 40
        ) |>
        e_x_axis(
          name = "Date",
          nameLocation = "center",
          nameGap = 30
        )

    })

  })
}

## To be copied in the UI
# mod_BroadBayAdvice_ui("BroadBayAdvice_1")

## To be copied in the server
# mod_BroadBayAdvice_server("BroadBayAdvice_1")
