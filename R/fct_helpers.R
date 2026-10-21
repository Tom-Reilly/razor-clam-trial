#' helpers
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
infoBoxCustom <- function(dominant_text, submissive_text, icon_name) {
  tags$div(
    class = "custom-info-box",
    # Icon Container (FontAwesome)
    tags$div(
      class = "info-box-icon",
      tags$i(class = paste0("fa-solid fa-", icon_name))
    ),
    # Text Container
    tags$div(
      class = "info-box-content",
      tags$div(class = "info-box-dominant", dominant_text),
      tags$div(class = "info-box-submissive", submissive_text)
    )
  )
}
