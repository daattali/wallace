userAddRem_UI <- function(id) {
  ns <- NS(id)
  tagList(
    fileInput(
      ns("userBgMsk"),
      label = 'Upload polygon in shapefile (.shp, .shx, .dbf) or CSV file with field order (longitude, latitude)',
      accept = c(".csv", ".dbf", ".shx", ".shp"),
      multiple = TRUE)
  )
}
