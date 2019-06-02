dataAnnotate_UI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput("curPpRastersUI"),
    textInput(ns("yearInput"),
              label = "Type years in the same order of the rasters selected in the previous step")
  )
}

dataAnnotate_MOD <- function(input, output, session) {
  reactive({
    # ERRORS ####
    if (is.null(occs())) {
      shinyLogs %>% writeLog(type = 'error', "Upload occs (**).")
      return()
    }
    if (is.null(spp[[curSp()]]$postProc$prediction)) {
      shinyLogs %>% writeLog(type = 'error', "Upload SDM prediction (**).")
      return()
    }
    if (is.null(spp[[curSp()]]$postProc$rasters)) {
      shinyLogs %>% writeLog(type = 'error', "Raster files not uploaded.")
      return()
    }
    # Prepare rasters
    env <- raster::stack(spp[[curSp()]]$postProc$rasters[selDrivenRaster()])
    # crop climate data to study region
    env <- raster::crop(env, spp[[curSp()]]$postProc$prediction)
    # Prepare year vector
    dates <- trimws(strsplit(input$yearInput, ",")[[1]])
    # FUNCTION CALL
    dataAnnotate <- mask_dataAnnotate(occs = occs(),
                                      env = env,
                                      envDates = dates,
                                      shinyLogs)
    occsEnvs <- cbind.data.frame(occs()[, c(5, 1:4)],
                                 drivenValue = dataAnnotate,
                                 occs()[, 6:13])
    # LOAD INTO SPP ####
    spp[[curSp()]]$postProc$occs <- occsEnvs
    spp[[curSp()]]$occs <- occsEnvs

    # METADATA ####
  })
}

dataAnnotate_INFO <- infoGenerator(modName = "Masking by land cover",
                                   modAuts = "Gonzalo E. Pinilla-Buitrago, Pete Galante",
                                   pkgName = 'maskRangeR')
