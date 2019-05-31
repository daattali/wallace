ppRasters_UI <- function(id) {
  ns <- NS(id)
  tagList(
    fileInput(ns("ppRasters"), label = "Upload post-processed rasters", multiple = TRUE)
  )
}

ppRasters_MOD <- function(input, output, session) {
  reactive({
    # ERRORS ####
    if (is.null(curSp())) {
      shinyLogs %>% writeLog(type = 'error', "Upload some occs or userSDM (**).")
      return()
    }
    if (is.null(input$ppRasters)) {
      shinyLogs %>% writeLog(type = 'error', "Raster files not uploaded.")
      return()
    }

    ppRasters <- ppdata_rasters(rasPath = input$ppRasters$datapath,
                                rasName = input$ppRasters$name,
                                shinyLogs)

    # LOAD INTO SPP ####
    spp[[curSp()]]$postProc$rasters <- ppRasters

    # METADATA ####
    # spp[[sp]]$rmm$data$environment$variableNames <- names(userEnvs)
    # spp[[sp]]$rmm$data$environment$resolution <- raster::res(userEnvs)
    # spp[[sp]]$rmm$data$environment$sources <- 'user'
    # spp[[sp]]$rmm$wallaceSettings$userRasName <- input$userEnvs$name
  })
}

ppRasters_INFO <- infoGenerator(modName = "User-specified post-processed rasters",
                                modAuts = "Gonzalo E. Pinilla-Buitrago",
                                pkgName = "raster")
