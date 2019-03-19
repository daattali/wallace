userSDM_UI <- function(id) {
  ns <- NS(id)
  tagList(
    fileInput(ns("userSDM"), label = "Upload Occurrence CSV")
  )
}

userSDM_INFO <- infoGenerator(modName = "User-specified SDMs (**)",
                               modAuts = "Dev-team Wallace (**)",
                               pkgName = NULL)