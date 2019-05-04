drawAddRem_UI <- function(id) {
  ns <- NS(id)
  tagList(

  )
}

drawAddRem_MOD <- function(input, output, session) {
  reactive({
    # ERRORS ####
    # GEPB: Polygon outside boundaries
    # GEPB: Add thresholded map error spp[[curSp()]]$postProc$prediction
    if (is.null(spp[[curSp()]]$postProc$prediction)) {
      shinyLogs %>% writeLog(
        type = 'error',
        "SDM predicion need to be thresholded. Please use a threshold in Visualize component. (**)"
      )
      return()
    }
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
    if(is.null(spp[[curSp()]]$mask$polyAddRem)) {
      spp[[curSp()]]$mask$polyAddRem <- list()
    }
    spp[[curSp()]]$mask$polyAddRem <- c(spp[[curSp()]]$mask$polyAddRem, drawAddRem)

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
  # polyMaskXY <- spp[[curSp()]]$polyMaskXY
  polyAddRem <- spp[[curSp()]]$mask$polyAddRem

  map %>% clearMarkers() %>%
    clearShapes() %>%
    # add background polygon
    mapBgPolys(bgShpXY())
  for(i in 1:length(polyAddRem)) {
    xy <- ggplot2::fortify(polyAddRem[[i]])
    map %>%
      addPolygons(lng = xy[,1], lat = xy[,2],
                  weight = 4, color = "gray", group = 'maskShp')
  }
}
