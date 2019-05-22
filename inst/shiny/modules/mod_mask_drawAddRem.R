drawAddRem_UI <- function(id) {
  ns <- NS(id)
  tagList(
    radioButtons(
      "addRemSel", label = "Select input",
      choices = list("draw" = 'maskDrawAddRem',
                     "user" = 'maskUserAddRem'),
      inline = TRUE
    )
  )
}

drawAddRem_MOD <- function(input, output, session) {
  reactive({
    # ERRORS ####
    # GEPB: Polygon outside boundaries
    if (is.null(spp[[curSp()]]$postProc$prediction)) {
      shinyLogs %>% writeLog(
        type = 'error',
        "SDM predicion need to be thresholded. Please provide a threshold in the Visualize component."
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

    if (!rgeos::gContains(spp[[curSp()]]$procEnvs$bgExt, drawAddRem)) {
      shinyLogs %>% writeLog(
        type = 'error',
        "The polygon is outside the background extent. Please draw a new polygon. (**)"
      )
      return()
    } else {
      # LOAD INTO SPP ####
      if(is.null(spp[[curSp()]]$mask$polyAddRem)) {
        spp[[curSp()]]$mask$polyAddRem <- list()
      }
      spp[[curSp()]]$mask$polyAddRem <- c(spp[[curSp()]]$mask$polyAddRem, drawAddRem)

      # GEPB: ADD METADATA ####
    }
  })
}

# This include draw and do visualization on map
AddRem_MAP <- function(map, session) {
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

  req(spp[[curSp()]]$mask$polyAddRem, spp[[curSp()]]$postProc$prediction)
  polyAddRem <- spp[[curSp()]]$mask$polyAddRem

  map %>% clearMarkers() %>%
    clearShapes() %>%
    # add background polygon
    mapBgPolys(bgShpXY())

  for(i in 1:length(polyAddRem)) {
    xy <- ggplot2::fortify(polyAddRem[[i]])
    if (i == 1) {
      map %>%
        addPolygons(lng = xy[,1], lat = xy[,2],
                    weight = 4, color = "gray", group = 'maskShp') %>%
        removeImage(layerId = 'mapPred') %>%
        addRasterImage(spp[[curSp()]]$postProc$prediction, colors = c('gray', 'purple'),
                       opacity = 0.7, group = 'mask', layerId = 'postPred',
                       method = "ngb")
    } else {
      map %>% clearGroup('maskShp') %>%
        addPolygons(lng = xy[,1], lat = xy[,2],
                    weight = 4, color = "gray", group = 'maskShp') %>%
        removeImage(layerId = 'postPred') %>%
        addRasterImage(spp[[curSp()]]$postProc$prediction, colors = c('gray', 'purple'),
                       opacity = 0.7, group = 'mask', layerId = 'postPred',
                       method = "ngb")

    }
  }
}

addRem_INFO <- infoGenerator(modName = "Editing using expert maps (**)",
                             modAuts = "Gonzalo E. Pinilla-Buitrago, Peter Galante",
                             # GEPB: Change name when a new version is released.
                             pkgName = "maskedRangeR")
