#' ClydeAdvice UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import echarts4r
#' @importFrom fst read_fst
#' @importFrom stringr str_wrap
#' @importFrom shinyWidgets dropdown animateOptions
#' @importFrom leaflet addTiles addLegend
#' @importFrom paletteer paletteer_dynamic
mod_ClydeAdvice_ui <- function(id){
  ns <- NS(id)
  tagList(
    div(class="shiny-fluid-row",
        column(1,),
        column(2,
               infoBoxCustom(dominant_text = "Decreasing",
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
               infoBoxCustom(dominant_text = "2023",
                             submissive_text = "Last Survey",
                             icon_name = "calendar")),
        column(1,)
    ),
    fluidRow(
      column(12,
             div(class="advice-box",
                 id = "top",
                 includeHTML(app_sys("clydeAdvice.html"))))
    ),
    fluidRow(
    column(width = 12,
      div(class="advice-box",
          id = "clyde-landings",
          width = 12,
          h3("Total landings by year", style="color:#1A5276;"),
          echarts4rOutput(ns("clydeLanPlot")),
          HTML('<a href="#top">Back to top</a>')
      )
    )),
    fluidRow(
      column(12,
      div(class="advice-box",
          id = "clyde-lpue",
          width = 12,
          h3("Landings per unit effort", style="color:#1A5276;"),
          echarts4rOutput(ns("clydeLPUEPlot")),
          HTML('<a href="#top">Back to top</a>')
      ))
    ),
    fluidRow(
      div(id = "clyde-effort",
          leafletOutput(ns("clydeEffortMap"),
          height = 600),
          HTML('<a href="#top">Back to top</a>')
          )
    ),
    fluidRow(
      column(12,
      div(class="advice-box",
          id = "clyde-lca",
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
      ))
    ),
    fluidRow(
      column(12,
             div(class = "advice-box",
                 id = "clyde-lbi",
                 width=12,
                 includeHTML(app_sys("clydeAdviceLBI.html")),
             HTML('<a href="#top">Back to top</a>'))
            )
    )
  )
}

#' ClydeAdvice Server Functions
#'
#' @noRd
mod_ClydeAdvice_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$clydeLanPlot <- renderEcharts4r({

    clydeLan = read_fst("extdata/clydeLandings.fst")

    clydeLan |>
      e_charts(TrialYear) |>
      e_bar(LanWt) |>
      e_title(subtext = stringr::str_wrap("Figure 1. Total razor clam landings (in Kg) by trial year in the Firth of Clyde trial area",60),
              left = "center",
              bottom = 0) |>
      e_grid(containLabel = TRUE) |> # add padding to the left of plot
      e_tooltip(formatter = htmlwidgets::JS("
      function(params){
        return('<strong>' + params.name +
        '</strong><br />Landed Weight: ' + params.value[1] + ' Kg')
      }
    ")) |>
      e_legend(show=FALSE) |>
      e_theme("wonderland") |>
      e_dims(height = 'auto', width = 'auto') |>
      e_y_axis(
        name = "Landings (Kg)",
        nameLocation = "center",
        nameRotate = 90,
        nameGap = 60
      ) |>
      e_x_axis(
        name = "Trial Year",
        nameLocation = "center",
        nameGap = "30"
      )

    })

    output$clydeLPUEPlot <- renderEcharts4r({

      clydeLPUE = lpueData %>% filter(trial_area == "Firth of Clyde")

    clydeLPUE |>
      e_charts(activity_date) |>
      e_scatter(LPUE_kg_hr) |>
      e_title(subtext = stringr::str_wrap("Figure 2. Landings Per Unit Effort (LPUE) in the Firth of Clyde trial area",60),
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

    output$clydeEffortMap = renderLeaflet({
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
# mod_ClydeAdvice_ui("ClydeAdvice_1")

## To be copied in the server
# mod_ClydeAdvice_server("ClydeAdvice_1")
