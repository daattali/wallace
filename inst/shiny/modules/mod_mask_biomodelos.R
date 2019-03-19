biomodelos_UI <- function(id) {
  ns <- NS(id)
  tagList(
    radioButtons(ns("selBgMsk"), label = "Select input", choices = c("draw", "user"), inline = TRUE),
    conditionalPanel(sprintf("input['%s'] == 'draw'", ns("selBgMsk")),
                     "Draw a polygon"),
    conditionalPanel(sprintf("input['%s'] == 'user'", ns("selBgMsk")),
                     fileInput(ns("userBgMsk"), 
                               label = 'Upload polygon in shapefile (.shp, .shx, .dbf) or CSV file with field order (longitude, latitude)',
                               accept=c(".csv", ".dbf", ".shx", ".shp"), multiple = TRUE)),
    radioButtons(ns("selActMsk"), label = "Select action (**)", choices = c("add", "delete"), inline = TRUE)
  )
}
biomodelos_INFO <- infoGenerator(modName = "Biomodelos (**)",
                                 modAuts = "Peter Galante, Gonzalo E. Pinilla-Buitrago",
                                 pkgName = "wallace")