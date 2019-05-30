ppRasters_UI <- function(id) {
  ns <- NS(id)
  tagList(
    fileInput(ns("ppRasters"), label = "Upload post-processed rasters", multiple = TRUE)
  )
}

ppRasters_INFO <- infoGenerator(modName = "User-specified post-processed rasters",
                                modAuts = "Wallace dev-team",
                                pkgName = NULL)