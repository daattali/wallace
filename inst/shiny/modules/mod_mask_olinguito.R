olinguito_UI <- function(id) {
  ns <- NS(id)
  tagList(
    shinyWidgets::pickerInput(ns("selOliRaster"), label = "Select rasters",
                choices = c("Other", "coverForest1", "coverForest2", "coverForest4",
                            "coverForest3"),
                multiple = TRUE),
    textInput(ns("tempInput"),
              label = "Type years in the same order of the rasters selected in the previous step")
  )
}
olinguito_INFO <- infoGenerator(modName = "Masking by land cover",
                                modAuts = "Peter Galante, Gonzalo E. Pinilla-Buitrago",
                                pkgName = NULL)
