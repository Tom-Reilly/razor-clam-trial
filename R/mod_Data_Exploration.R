#' Data_Exploration UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList renderUI uiOutput observeEvent downloadButton downloadHandler
#' @importFrom DT renderDataTable DTOutput datatable
#' @importFrom shinyWidgets pickerInput pickerOptions updatePickerInput
#' @importFrom dplyr %>% filter
#' @importFrom fst read.fst
#' @importFrom tidyft parse_fst
#' @importFrom shinycssloaders withSpinner
#' @importFrom utils write.csv
mod_Data_Exploration_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      sidebarPanel(
        h3("Select Dataset to view", style = "margin-top: 5px"),
        selectInput(ns("dataset"),
                    label = "Select the Dataset Type",
                    choices = c("Landings Data", "Self-Sampling Length-Frequency Data", "Biological Data", "Survey Data", "REM Data")),
        conditionalPanel(condition = "input.dataset != 'Survey Data' && input.dataset != 'REM Data'",
                         ns = ns,
                         pickerInput(ns("years"),
                                     label = "Select Data Years",
                                     choices = c(2018:2025),
                                     multiple = TRUE,
                                     options = pickerOptions(actionsBox = TRUE, dropupAuto = FALSE))
        ),
        conditionalPanel(condition = "input.dataset == 'REM Data'",
                         ns = ns,
                         pickerInput(ns("trialYears"),
                                     label = "Select Trial Data Years",
                                     choices = c(1:7),
                                     multiple = TRUE,
                                     options = pickerOptions(actionsBox = TRUE, dropupAuto = FALSE))
        ),
        conditionalPanel(condition = "input.dataset == 'Self-Sampling Length-Frequency Data' || input.dataset == 'Biological Data' || input.dataset == 'REM Data'",
                         ns = ns,
                         pickerInput(ns("trialZones"),
                                     label = "Select Trial Zone",
                                     choices = c("Outer Hebrides", "West Coast NW", "West Coast SW", "Firth of Forth"),
                                     multiple = TRUE,
                                     options = pickerOptions(actionsBox = TRUE, dropupAuto = FALSE))
        ),
        actionButton(ns("go"),
                     label = "Go!",
                     class = "btn-primary")
      ),
      mainPanel(
        uiOutput(ns("page")) %>% withSpinner()
      )
    )
  )
}

#' Data_Exploration Server Functions
#'
#' @noRd
mod_Data_Exploration_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    #Create reactive object of data frame to present
    react_df <- eventReactive(input$go, {

      if(isolate(input$dataset) == "Landings Data") {
        if(! is.null(isolate(input$years))) {
          dataToShowLan = RazorClamTrial::lanData %>% filter(RazorClamTrial::lanData$Year %in% isolate(input$years))
          return(dataToShowLan)
        }
      }

      if(isolate(input$dataset) == "Self-Sampling Length-Frequency Data") {
        if(! any(is.null(isolate(input$years)), is.null(isolate(input$trialZones)))) {
          parse_fst(app_sys("extdata/selfSampData.fst")) -> selfSampft
          dataToShowSelfSamp = selfSampft[selfSampft$Year %in% isolate(input$years) & selfSampft$Trial_zone_name %in% isolate(input$trialZones), ]
          return(dataToShowSelfSamp)
        }
      }

      if(isolate(input$dataset) == "Biological Data") {
        if(! any(is.null(isolate(input$years)), is.null(isolate(input$trialZones)))) {
          parse_fst(app_sys("extdata/bioData.fst")) -> bioft
          dataToShowBio = bioft[bioft$Year %in% isolate(input$years) & bioft$Trial_zone_name %in% isolate(input$trialZones), ]
          return(dataToShowBio)
        }
      }

      if(isolate(input$dataset) == "REM Data") {
        if(! any(is.null(isolate(input$trialYears)), is.null(isolate(input$trialZones)))) {
          parse_fst(app_sys("extdata/remData.fst")) -> ft
          dataToShowREM = ft[ft$Trial_year %in% isolate(input$trialYears) & ft$Trial_zone_name %in% isolate(input$trialZones),]
          return(dataToShowREM)
        }
      }

      if(isolate(input$dataset) == "Survey Data") {
        return(RazorClamTrial::surveyData)
      }

    })

    # On click of Go, run the following to render UI
    observeEvent(input$go, {

      output$page = renderUI({
        if(isolate(input$dataset) == "Survey Data") {
          output$survTable = DT::renderDataTable({datatable(react_df(),
                                                            rownames = FALSE,
                                                            escape = FALSE,
                                                            options = list(dom = 't', scrollX = TRUE))})
          tagList(DTOutput(ns("survTable")),
                  div(class="box",HTML("Survey work completed by Scottish Association for Marine Science (<a href='https://www.sams.ac.uk/about/'>SAMS</a>) in collaboration with Marine Directorate, under contracts issued by <a href='https://www.gov.scot/policies/marine-and-fisheries-grants/'>Marine Fund Scotland</a>."))
                  )
        } else if(isolate(input$dataset) == "Self-Sampling Length-Frequency Data") {
          validate(need(! any(is.null(isolate(input$years)), is.null(isolate(input$trialZones))),
                        message = "Please make a selection for each field."))
          output$selfSampTable = DT::renderDataTable({datatable(react_df(),
                                                                rownames = FALSE,
                                                                options = list(scrollX = TRUE))})
          tagList(downloadButton(ns("downloadData"),
                                 label = "Download the selected data!",
                                 class = "btn-primary",
                                 style = "margin-bottom: 5px"),
                  DTOutput(ns("selfSampTable")))
        } else if(isolate(input$dataset) == "Landings Data") {
          validate(need(! is.null(isolate(input$years)),
                        message = "Please make a selection for the years field."))
          output$lanTable = DT::renderDataTable({datatable(react_df(),
                                                           rownames = FALSE,
                                                           options = list(scrollX = TRUE))})
          tagList(h3("Monthly aggregated reported razor clam landings by ICES statistical rectangle"),
                  downloadButton(ns("downloadData"),
                                 label = "Download the selected data!",
                                 class = "btn-primary",
                                 style = "margin-bottom: 5px"),
                  div(class = "box", p("Landings data presented here have not been processed and are subject to change, data were last updated on 05/12/2025")),
                  DTOutput(ns("lanTable"))
                  )
        } else if(isolate(input$dataset) == "Biological Data") {
          validate(need(! any(is.null(isolate(input$years)), is.null(isolate(input$trialZones))),
                        message = "Please make a selection for each field."))
          output$bioTable = DT::renderDataTable({datatable(react_df(),
                                                           rownames = FALSE,
                                                           options = list(scrollX = TRUE))})
          tagList(downloadButton(ns("downloadData"),
                                 label = "Download the selected data!",
                                 class = "btn-primary",
                                 style = "margin-bottom: 5px"),
                  DTOutput(ns("bioTable")))
        } else if(isolate(input$dataset) == "REM Data") {
          validate(need(! any(is.null(isolate(input$trialYears)), is.null(isolate(input$trialZones))),
                        message = "Please make a selection for each field."))
          output$remTable = DT::renderDataTable({datatable(react_df(),
                                                           rownames = FALSE,
                                                           options = list(scrollX = TRUE))})
          tagList(h3("Annual aggregated estimates of fishing intensity; hours fished per c-square"),
                  #div(class="box", p("These data are undergoing further quality checks and will be available for download shortly.", style = "color: red;")),
                  downloadButton(ns("downloadData"),
                                 label = "Download the selected data!",
                                 class = "btn-primary",
                                 style = "margin-bottom: 5px"),
                  DTOutput(ns("remTable")),
                  div(class="box", p("pt_count represents the count of one minute fishing periods within the given c-square and trial year. The total estimated fishing hours, or pt_hours, is simply the pt_count divided by 60."))
          )
        }
      })

    })


      output$downloadData = downloadHandler(
        filename = function() {
          paste0("dataDownload_",Sys.Date(), ".csv")
          },
        content = function(file){
          write.csv(react_df(), file = file, row.names = FALSE)
        }
      )

  })
}

## To be copied in the UI
# mod_Data_Exploration_ui("Data_Exploration_1")


# To be copied in the server# mod_Data_Exploration_server("Data_Exploration_1")
