doAddRem_UI <- function(id) {
  ns <- NS(id)
  tagList(
    radioButtons(ns("doAddRem"), "Choose action",
                 choices = list("Add polygon" = 'addMask',
                                "Remove polygon" = 'remMAsk'), inline = TRUE)
  )
}

doAddRem_MOD <- function(input, output, session) {
  reactive({
    # ERRORS ####
    # GEPB: Add possible errors
    # FUNCTION CALL ####
    removePoly <- ifelse(input$doAddRem == 'addMask', FALSE, TRUE)
    polyAddRem <- spp[[curSp()]]$mask$polyAddRem[[length(spp[[curSp()]]$mask$polyAddRem)]]
    doAddRem <- mask_doAddRem(polyAddRem,
                              spp[[curSp()]]$postProc$prediction,
                              removePoly,
                              shinyLogs)
    if (removePoly == FALSE) {
      shinyLogs %>% writeLog("The polygon was added (**)")
    } else {
      shinyLogs %>% writeLog("The polygon was removed (**)")
    }
    # LOAD INTO SPP ####
    spp[[curSp()]]$postProc$prediction <- doAddRem
    spp[[curSp()]]$mask$removePoly <- c(spp[[curSp()]]$mask$removePoly, removePoly)

    # GEPB: Add metadata ####

  })
}
