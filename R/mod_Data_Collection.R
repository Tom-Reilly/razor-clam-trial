#' Data_Collection UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom dplyr %>% filter mutate case_when
#' @importFrom leaflet leaflet addProviderTiles renderLeaflet leafletOutput addPolygons highlightOptions colorFactor setView
#' @importFrom sf st_read st_transform
mod_Data_Collection_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(h1("Data Collection", align = "center")),
    fluidRow(column(6,
                    includeHTML(app_sys("dataCollect_TrialLocations.html")),
                    includeHTML(app_sys("dataCollect_SurveyData.html")),
                    includeHTML(app_sys("dataCollect_REMData.html"))),
             column(6,
                    leafletOutput(ns("trialZoneMap"), height = "100vh"),
                    includeHTML(app_sys("dataCollect_LandingsData.html")),
                    includeHTML(app_sys("dataCollect_BiologicalData.html")),
                    div(img(src="img/IMG-20230226-WA0006.jpg", class="img-col", style="border-radius: 3px;"),
                        p("Photo used with permission from Scott McKinlay and Clive Fox"),class="seven"))
             )
  )
}

#' Data_Collection Server Functions
#'
#' @noRd
mod_Data_Collection_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$trialZoneMap = renderLeaflet({
      # Load shapefile with ICES rectangles
      # Shapefile downloaded from https://gis.ices.dk/sf/index.html?widget=StatRec (Quick downloads)
      ICESrect = st_read(app_sys("app/shapefiles/ICESrect.shp"), quiet = TRUE)
      trialAreas = st_read(app_sys("app/shapefiles/trialZonesLoRes.shp"), quiet = TRUE) %>%
                   st_transform('+proj=longlat +datum=WGS84')
      landPoly = st_read(app_sys("app/shapefiles/landPoly.shp"), quiet = TRUE)

      labs = sprintf(paste0("Zone: ", trialAreas$zoneN,
                            "<br/>Trial Area: ", trialAreas$areaN
                            )) %>% #"<br/>Area Covered Km2: ", trialAreas$sq_km
        lapply(htmltools::HTML)

      zoneColours = colorFactor(c("#12436D","#28A197","#801650","#F46A25"), trialAreas$zone)

      leaflet() %>%
        addPolygons(data = ICESrect, weight = 1, fillOpacity = 0, color = "grey", label = ~ICESNAME) %>%
        addPolygons(data = trialAreas, weight = 1, fillOpacity = 0.5, color = ~zoneColours(zone), label = ~labs,
                    highlightOptions = highlightOptions(fillOpacity = 0.8)) %>%
        addPolygons(data = landPoly, color = NA,fillColor="white",fillOpacity = 1) %>%
        setView(lng=-4.5, lat=56.5, zoom=6)
    })
  })
}

## To be copied in the UI
# mod_Data_Collection_ui("Data_Collection_1")

## To be copied in the server
# mod_Data_Collection_server("Data_Collection_1")
