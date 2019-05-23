userSDM_UI <- function(id) {
  ns <- NS(id)
  tagList(
    fileInput(ns("userSDM"), label = "Upload SDM prediction (**)")
  )
}

userSDM_INFO <- infoGenerator(modName = "User-specified SDM prediction (**)",
                              modAuts = "Gonzalo E. Pinilla-Buitrago",
                              pkgName = NULL)
