ppRasters_UI <- function(id) {
  ns <- NS(id)
  tagList(
    fileInput(ns("ppRasters"),
              label = "Upload environmental rasters for masking distribution map",
              multiple = TRUE)
  )
}

ppRasters_MOD <- function(input, output, session) {
  reactive({
    # ERRORS ####
    if (is.null(curSp())) {
      shinyLogs %>% writeLog(type = 'error', "Upload some occs or userSDM (**).")
      return()
    }

    ppRasters <- ppdata_rasters(rasPath = input$ppRasters$datapath,
                                rasName = input$ppRasters$name,
                                shinyLogs)

    # LOAD INTO SPP ####
    spp[[curSp()]]$postProc$rasters <- ppRasters

    # METADATA ####

  })
}

ppRasters_INFO <- infoGenerator(modName = "User-specified post-processed rasters",
                                modAuts = "Gonzalo E. Pinilla-Buitrago",
                                pkgName = "raster")
