drawAddRem_UI <- function(id) {
  ns <- NS(id)
  tagList(

  )
}

drawAddRem_MOD <- function(input, output, session) {
  reactive({
    # ERRORS ####
    # GEPB: Add thresholded map error
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
    spp[[sp]]$mask$polyAddRem <- drawAddRem

    # GEPB: ADD METADATA ####
  })
}

drawAddRem_MAP <- function(map, session) {
  updateTabsetPanel(session, 'main', selected = 'Map')
  map %>% leaflet.extras::addDrawToolbar(
    targetGroup = 'draw',
    polylineOptions = FALSE,
    rectangleOptions = FALSE,
    circleOptions = FALSE,
    markerOptions = FALSE,
    circleMarkerOptions = FALSE,
    editOptions = leaflet.extras::editToolbarOptions()
  )

  req(spp[[curSp()]]$polyMaskXY)
  polyMaskXY <- spp[[curSp()]]$polyExtXY

  for(shp in bgShpXY()) {
    map %>% clearAll() %>%
      addPolygons(lng=shp[,1], lat=shp[,2], weight=4, color="gray", group='bgShp')
  }

}
