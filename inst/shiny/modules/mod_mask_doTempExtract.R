doTempExtract_UI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput("curMaskRasterUI"),
    p("Provide lower and/or upper bound values for masking"),
    splitLayout(
      numericInput(ns('lowerInput'), label = "Lower bound value",
                   value = NULL),
      numericInput(ns('upperInput'), label = "Upper bound value",
                   value = NULL)
      )
  )
}

doTempExtract_MOD <- function(input, output, session) {
  reactive({
    # ERRORS ####
    if (is.null(spp[[curSp()]]$postProc$occs)) {
      shinyLogs %>% writeLog(type = 'error', "Get first masking values in previous step (**).")
      return()
    }
    # FUNCTION CALL
    doTempExtract <-
      mask_doTempExtract(
        lowerInp = input$lowerInput, upperInp = input$upperInput,
        maskRaster = spp[[curSp()]]$postProc$rasters[[selMaskRaster()]],
        pred = spp[[curSp()]]$postProc$prediction, shinyLogs)

    shinyLogs %>% writeLog("The prediction was masked (**)")

    # LOAD INTO SPP ####
    spp[[curSp()]]$postProc$prediction <- doTempExtract

    # METADATA ####
  })
}
