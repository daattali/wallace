doDataDriven_UI <- function(id) {
  ns <- NS(id)
  tagList(
    splitLayout(
      numericInput(ns('lowerInput'), label = "Lower bound value (**)",
                   value = NULL),
      numericInput(ns('upperInput'), label = "Upper bound value (**)",
                   value = NULL)
      ),
    uiOutput("curMaskRasterUI")
  )
}

doDataDriven_MOD <- function(input, output, session) {
  reactive({
    # ERRORS ####
    if (is.null(spp[[curSp()]]$postProc$occs)) {
      shinyLogs %>% writeLog(type = 'error', "Get first masking values in previous step (**).")
      return()
    }
    # FUNCTION CALL
    doDataDriven <-
      mask_doDataDriven(
        lowerInp = input$lowerInput, upperInp = input$upperInput,
        maskRaster = spp[[curSp()]]$postProc$rasters[[selMaskRaster()]],
        pred = spp[[curSp()]]$postProc$prediction, shinyLogs)

    # LOAD INTO SPP ####
    spp[[curSp()]]$postProc$prediction <- doDataDriven

    # METADATA ####
  })
}
