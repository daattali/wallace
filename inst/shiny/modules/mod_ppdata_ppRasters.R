ppRasters_UI <- function(id) {
  ns <- NS(id)
  tagList(
    fileInput(ns("ppRasters"), label = "Upload pp rasters (**)", multiple = TRUE)
  )
}

ppRasters_INFO <- infoGenerator(modName = "User-specified pp rasters (**)",
                                modAuts = "Wallace dev-team (**)",
                                pkgName = NULL)