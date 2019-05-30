ppRasters_UI <- function(id) {
  ns <- NS(id)
  tagList(
    fileInput(ns("ppRasters"), label = "Upload post-processed rasters", multiple = TRUE)
  )
}

ppRasters_MOD <- function(input, output, session) {
  reactive({
    # ERRORS ####
    if (is.null(input$ppRasters)) {
      shinyLogs %>% writeLog(type = 'error', "Raster files not uploaded.")
      return()
    }

    ppRasters <- ppdata_rasters(rasPath = input$ppRasters$datapath,
                                rasName = input$ppRasters$name)

    # loop over all species if batch is on
    if(input$batch == TRUE) spLoop <- allSp() else spLoop <- curSp()

    for(sp in spLoop) {
      # get environmental variable values per occurrence record
      withProgress(message = paste0("Extracting environmental values for occurrences of ", sp, "..."), {
        occsEnvsVals <- as.data.frame(raster::extract(userEnvs, spp[[sp]]$occs[c('longitude', 'latitude')]))
      })
      # remove occurrence records with NA environmental values
      spp[[sp]]$occs <- remEnvsValsNA(spp[[sp]]$occs, occsEnvsVals, shinyLogs)
      # also remove variable value rows with NA environmental values
      occsEnvsVals <- na.omit(occsEnvsVals)

      # LOAD INTO SPP ####
      spp[[sp]]$envs <- userEnvs
      # add columns for env variable values for each occurrence record
      spp[[sp]]$occs <- cbind(spp[[sp]]$occs, occsEnvsVals)

      # METADATA ####
      spp[[sp]]$rmm$data$environment$variableNames <- names(userEnvs)
      spp[[sp]]$rmm$data$environment$resolution <- raster::res(userEnvs)
      spp[[sp]]$rmm$data$environment$sources <- 'user'

      spp[[sp]]$rmm$wallaceSettings$userRasName <- input$userEnvs$name
    }

  })
}

ppRasters_INFO <- infoGenerator(modName = "User-specified post-processed rasters",
                                modAuts = "Gonzalo E. Pinilla-Buitrago",
                                pkgName = "raster")
