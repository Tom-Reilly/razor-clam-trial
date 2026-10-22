#' WigtownBayAdvice UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_WigtownBayAdvice_ui <- function(id){
  ns <- NS(id)
  tagList(
    div(class="shiny-fluid-row",
        column(1,),
        column(2,
               infoBoxCustom(dominant_text = "Decreasing",
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
               infoBoxCustom(dominant_text = "Good but Limited Data",
                             submissive_text =  "Length Based Indicators",
                             icon_name = "ruler")),
        column(2,
               infoBoxCustom(dominant_text = "2024",
                             submissive_text = "Last Survey",
                             icon_name = "calendar")),
        column(1,)
    ),
    fluidRow(
      column(12,
             div(class="advice-box",
                 includeHTML(app_sys("wigtownbayAdvice.html"))))
    ),
    fluidRow(
      column(width = 12,
        div(class="advice-box",
            id = "wig-landings",
            width = 12,
            h3("Total landings by year", style="color:#1A5276;"),
            echarts4rOutput(ns("wigtownLanPlot")),
            HTML('<a href="#top">Back to top</a>')
           ),
    fluidRow(
      column(width = 12,
        div(class="advice-box",
            id = "wig-lpue",
            width = 12,
            h3("Landings per unit effort", style="color:#1A5276;"),
            echarts4rOutput(ns("wigtownLPUEPlot")),
            HTML('<a href="#top">Back to top</a>')
        )
      )
    ),
    fluidRow(
      div(id = "wig-effort",
             leafletOutput(ns("wigtownEffortMap"),
                          height = 600),
             HTML('<a href="#top">Back to top</a>')
      )
    )
  )
}

#' WigtownBayAdvice Server Functions
#'
#' @noRd
mod_WigtownBayAdvice_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$wigtownLanPlot <- renderEcharts4r({

      wigtownbayLan = read_fst("extdata/wigtownbayLandings.fst")

      wigtownbayLan |>
        e_charts(TrialYear) |>
        e_bar(LanWt) |>
        e_title(subtext = stringr::str_wrap("Figure 1. Total razor clam landings (in Kg) by trial year in the Wigtown Bay trial area",60),
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

output$wigtownLPUEPlot <- renderEcharts4r({

      wigtownLPUE = lpueData %>% filter(trial_area == "Wigtown Bay")

    wigtownLPUE |>
      e_charts(activity_date) |>
      e_scatter(LPUE_kg_hr) |>
      e_title(subtext = stringr::str_wrap("Figure 2. Landings Per Unit Effort (LPUE) in the Wigtown Bay trial area",60),
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
      e_dims(height = 'auto', width = 'auto') |>
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

    output$wigtownEffortMap = renderLeaflet({
      cols <- paletteer::paletteer_dynamic("cartography::harmo.pal", 12)
      cols_bl <- colorRampPalette(cols)
      prodAreas = st_read(app_sys("app/shapefiles/fss_classified_razor_areas_2026 - no_overlap.shp"), quiet = TRUE)
      prodAreas = prodAreas %>% filter(trial_yr == 8, Status == "Classified")

      clipped_polygons = st_read(app_sys("app/shapefiles/map_clipd_poly.shp"), quiet = TRUE)

      leaflet() %>%
        addProviderTiles("Stadia.AlidadeSmooth") %>%
        addPolygons(data = prodAreas, weight = 1, fillOpacity = 0, color = "grey") %>%
        addPolygons(data = clipped_polygons, weight = 0.5, fillOpacity = 0.8, color = ~map_col, label = ~lapply(labs, HTML)) %>% #cols_bl(pt_hors)
        setView(lng=-4.7, lat=55.4, zoom=9)
      })

  })
}

## To be copied in the UI
# mod_WigtownBayAdvice_ui("WigtownBayAdvice_1")

## To be copied in the server
# mod_WigtownBayAdvice_server("WigtownBayAdvice_1")
