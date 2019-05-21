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
    remBol <- ifelse(input$doAddRem == 'addMask', FALSE, TRUE)
    polyAddRem <- spp[[curSp()]]$mask$polyAddRem[[length(spp[[curSp()]]$mask$polyAddRem)]]
    doAddRem <- mask_doAddRem(polyAddRem,
                              spp[[curSp()]]$postProc$prediction,
                              remBol,
                              shinyLogs)
    # LOAD INTO SPP ####
    spp[[curSp()]]$postProc$prediction <- doAddRem
    print("Testing function")

    # GEPB: Add metadata ####

  })
}

doAddRem_MAP <- function(map, session) {

}
