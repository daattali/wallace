tempExtract_UI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput("curPpRastersUI"),
    textInput(ns("yearInput"),
              label = paste0("Type the years to be used for extracting ",
                             "environmental data, separated by commas"))
    )
}

tempExtract_MOD <- function(input, output, session) {
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
    env <- raster::stack(spp[[curSp()]]$postProc$rasters[selTempRaster()])
    # crop climate data to study region
    env <- raster::crop(env, spp[[curSp()]]$postProc$prediction)
    # Prepare year vector
    dates <- trimws(strsplit(input$yearInput, ",")[[1]])
    # FUNCTION CALL
    tempExtract <- mask_tempExtract(occs = occs(),
                                    env = env,
                                    envDates = dates,
                                    shinyLogs)

    shinyLogs %>%
      writeLog("Values were extracted (**)")

    # subset by key columns and make id and popup columns
    cols <- c("occID", "scientific_name", "longitude", "latitude", "year",
              "extractedValue", "country", "state_province", "locality", "record_type",
              "catalog_number", "institution_code", "elevation", "uncertainty",
              "pop")
    occsEnvs <- occs()
    if (!('extractedValue' %in% names(occsEnvs))) {
      occsEnvs <- cbind.data.frame(occsEnvs, extractedValue = tempExtract)
      occsEnvs <- occsEnvs[, cols]
    } else {
      occsEnvs[, 'extractedValue'] <- tempExtract
    }

    # LOAD INTO SPP ####
    spp[[curSp()]]$postProc$occs <- occsEnvs
    spp[[curSp()]]$occs <- occsEnvs

    # METADATA ####
  })
}

tempExtract_INFO <- infoGenerator(modName = "Mask by environmental rasters",
                                  modAuts = "Gonzalo E. Pinilla-Buitrago, Pete Galante",
                                  pkgName = 'maskRangeR')
