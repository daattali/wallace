drawAddRem_UI <- function(id) {
  ns <- NS(id)
  tagList(

  )
}

drawAddRem_MOD <- function(input, output, session) {
  reactive({
    # ERRORS ####
    # GEPB: Polygon outside boundaries
<<<<<<< HEAD
    # GEPB: Add thresholded map error spp[[curSp()]]$postProc$prediction
    if (is.null(spp[[curSp()]]$postProc$prediction)) {
      shinyLogs %>% writeLog(
        type = 'error',
        "SDM predicion need to be thresholded. Please use a threshold in Visualize component. (**)"
      )
      return()
    }
=======
    # GEPB: Add thresholded map error
>>>>>>> 033dc3cbf7e578cc841e00731ff6776e3c20d75d
    if (is.null(spp[[curSp()]]$polyMaskXY)) {
      shinyLogs %>% writeLog(
        type = 'error',
        "The polygon has not been drawn and finished. Please use the draw toolbar on the left-hand of the map to complete the polygon."
      )
      return()
    }
    # FUNCTION CALL ####
    drawAddRem <- mask_drawAddRem(spp[[curSp()]]$polyMaskXY,
                                  spp[[curSp()]]$polyMaskID,
                                  shinyLogs)

    # LOAD INTO SPP ####
    spp[[curSp()]]$mask$polyAddRem <- drawAddRem

    # GEPB: ADD METADATA ####
  })
}

drawAddRem_MAP <- function(map, session) {
  updateTabsetPanel(session, 'main', selected = 'Map')
  # map %>% clearAll()
  map %>% leaflet.extras::addDrawToolbar(
    targetGroup = 'draw',
    polylineOptions = FALSE,
    rectangleOptions = FALSE,
    circleOptions = FALSE,
    markerOptions = FALSE,
    circleMarkerOptions = FALSE,
    editOptions = leaflet.extras::editToolbarOptions()
  )

  req(spp[[curSp()]]$mask$polyAddRem)
  polyMaskXY <- spp[[curSp()]]$polyMaskXY
  map %>% clearAll() %>%
    addPolygons(lng = polyMaskXY[,1], lat = polyMaskXY[,2],
                weight = 4, color = "gray", group = 'maskShp') %>%
    # add background polygon
    mapBgPolys(bgShpXY())
}
