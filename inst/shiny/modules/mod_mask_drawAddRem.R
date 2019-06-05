drawAddRem_UI <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    tags$div(
      radioButtons(ns("addRemSel"), label = "Select polygon input",
                   choices = c("Draw polygon" = 'maskDrawAddRem',
                               "User-provided file(s)" = 'maskUserAddRem'),
                   inline = TRUE),
      conditionalPanel(sprintf("input['%s'] == 'maskUserAddRem'",
                               ns("addRemSel")),
                       fileInput(
                         ns("userShpAddRem"),
                         label = paste0('Upload polygon in shapefile (.shp, .shx, ',
                                        '.dbf [must include all three]) or CSV file',
                                        ' with field order (longitude, latitude)'),
                         accept = c(".csv", ".dbf", ".shx", ".shp"),
                         multiple = TRUE)
      )
    )
  )
}

drawAddRem_MOD <- function(input, output, session) {
  reactive({
    # ERRORS ####
    # GEPB: Polygon outside boundaries
    binBool <- length(unique(raster::values(spp[[curSp()]]$postProc$prediction)))
    if (!(binBool == 3 | binBool == 2)) {
      shinyLogs %>% writeLog(
        type = 'error',
        "SDM prediction needs to be thresholded. Please provide a threshold in the Visualize component or provide a thresholded prediction in the Post-Data component (**)."
      )
      return()
    }
    if (is.null(spp[[curSp()]]$polyMaskXY) & input$addRemSel == 'maskDrawAddRem') {
      shinyLogs %>% writeLog(
        type = 'error',
        "The polygon has not been drawn and finished. Please use the draw toolbar on the left-hand of the map to complete the polygon."
      )
      return()
    }
    # FUNCTION CALL ####
    if (input$addRemSel == 'maskDrawAddRem') {
      addRemPoly <- mask_drawAddRem(spp[[curSp()]]$polyMaskXY,
                                    spp[[curSp()]]$polyMaskID,
                                    shinyLogs)
    } else if (input$addRemSel == 'maskUserAddRem') {
      addRemPoly <- mask_userAddRem(input$userShpAddRem$datapath,
                                    input$userShpAddRem$name,
                                    shinyLogs)
    }

    if (rgeos::gDisjoint(spp[[curSp()]]$procEnvs$bgExt, addRemPoly)) {
      shinyLogs %>% writeLog(
        type = 'error',
        "The polygon is outside the background extent. Please specify a new polygon. (**)"
      )
      return()
    } else {
      if (input$addRemSel == 'maskDrawAddRem') {
        shinyLogs %>% writeLog("The draw polygon is ready for action (add or remove) (**)")
      } else if (input$addRemSel == 'maskUserAddRem') {
        shinyLogs %>% writeLog("The user polygon is ready for action (add or remove) (**)")
      }

      # LOAD INTO SPP ####
      if(is.null(spp[[curSp()]]$mask$polyAddRem)) {
        spp[[curSp()]]$mask$polyAddRem <- list()
        spp[[curSp()]]$mask$removePoly<- list()
      }
      spp[[curSp()]]$mask$polyAddRem <- c(spp[[curSp()]]$mask$polyAddRem, addRemPoly)
      # GEPB: ADD METADATA ####
    }
  })
}

# This include draw and do visualization on map
addRem_MAP <- function(map, session) {
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

  req(spp[[curSp()]]$postProc$prediction)

  zoomExt <- raster::extent(spp[[curSp()]]$postProc$prediction)
  map %>% fitBounds(lng1 = zoomExt[1], lng2 = zoomExt[2],
                    lat1 = zoomExt[3], lat2 = zoomExt[4]) %>%
    removeImage(layerId = 'mapPred') %>%
    removeControl(layerId = 'train')

  map %>% clearMarkers() %>%
    clearShapes() %>%
    # add background polygon
    mapBgPolys(bgShpXY())

  rasterValues <- raster::values(spp[[curSp()]]$postProc$prediction)

  # Define raster colors and shiny legend
  rasCols <- c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", "#d7191c")
  # if it is threshold specified
  if (length(unique(rasterValues)) == 3 |
      length(unique(rasterValues)) == 2) {
    map %>%
      addLegend("bottomright", colors = c('gray', 'purple'),
                title = "Distribution<br>map",
                labels = c("Unsuitable", "Suitable"),
                opacity = 1, layerId = 'expert') %>%
      addRasterImage(spp[[curSp()]]$postProc$prediction, colors = c('gray', 'purple'),
                     opacity = 0.7, group = 'mask', layerId = 'postPred',
                     method = "ngb")
  } else {
    # if threshold specified
    legendPal <- colorNumeric(rev(rasCols), rasterValues, na.color='transparent')
    rasPal <- colorNumeric(rasCols, rasterValues, na.color='transparent')
    map %>%
      addLegend("bottomright", pal = legendPal, title = "Suitability<br>(User) (**)",
                values = rasterValues, layerId = "expert",
                labFormat = reverseLabels(2, reverse_order=TRUE)) %>%
      addRasterImage(spp[[curSp()]]$postProc$prediction, colors = rasPal,
                     opacity = 0.7, group = 'mask', layerId = 'postPred',
                     method = "ngb")
  }
  # If there is a new polygon
  req(spp[[curSp()]]$mask$polyAddRem)
  polyAddRem <- spp[[curSp()]]$mask$polyAddRem
  xy <- ggplot2::fortify(polyAddRem[[length(polyAddRem)]])
  if (length(polyAddRem) == 1) {
    map %>%
      addPolygons(lng = xy[,1], lat = xy[,2],
                  weight = 4, color = "gray", group = 'maskShp')
  } else {
    map %>% clearGroup('maskShp') %>%
      addPolygons(lng = xy[,1], lat = xy[,2],
                  weight = 4, color = "gray", group = 'maskShp') %>%
      removeImage(layerId = 'postPred') %>%
      addRasterImage(spp[[curSp()]]$postProc$prediction,
                     colors = c('gray', 'purple'), opacity = 0.7, group = 'mask',
                     layerId = 'postPred', method = "ngb")
  }
}

addRem_INFO <- infoGenerator(modName = "Add/Remove polygon",
                             modAuts = "Gonzalo E. Pinilla-Buitrago, Peter Galante",
                             pkgName = "maskRangeR")
