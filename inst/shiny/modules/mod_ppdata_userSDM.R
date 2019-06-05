userSDM_UI <- function(id) {
  ns <- NS(id)
  tagList(
    fileInput(ns("userSDMs"), label = "Upload distribution map",
      multiple = TRUE, accept = c(".tif", ".asc"))
  )
}

userSDM_MOD <- function(input, output, session) {
  reactive({
    # ERRORS ####
    if (is.null(input$userSDMs)) {
      shinyLogs %>% writeLog(type = 'error', "Raster files not uploaded.")
      return()
    }

    # FUNCTION CALL ####
    for (i in 1:length(input$userSDMs$name)) {
      userSDMs <- ppdata_userSDM(rasPath = input$userSDMs$datapath[i],
                                 rasName = input$userSDMs$name[i],
                                 shinyLogs)
      shinyLogs %>% writeLog("User SDM prediction loaded (**)")
      # Create name for curSp()
      newSppName <- fileNameNoExt(formatSpName(input$userSDMs$name[i]))
      if (!(newSppName %in% names(spp))) spp[[newSppName]] <- list()
      # Create new bgExt based on cells with data
      # rasterExt <- userSDMs > -Inf
      # extPoly <- raster::rasterToPolygons(rasterExt, dissolve = TRUE)
      # Create new bgExt
      e <- raster::extent(userSDMs)
      # coerce to a SpatialPolygons object
      extPoly <- as(e, 'SpatialPolygons')
      # LOAD INTO SPP ####
      spp[[newSppName]]$postProc$prediction <- userSDMs
      spp[[newSppName]]$postProc$OrigPred <- userSDMs
      spp[[newSppName]]$procEnvs$bgExt <- extPoly

      # METADATA ####

    }
  })
}

userSDM_MAP <- function(map, session) {
  req(spp[[curSp()]]$postProc$prediction, spp[[curSp()]]$procEnvs$bgExt)
  # Zoom
  userRaster <- spp[[curSp()]]$postProc$prediction
  userValues <- raster::values(userRaster)
  zoomExt <- raster::extent(userRaster)
  map %>%  fitBounds(lng1 = zoomExt[1], lng2 = zoomExt[2],
                     lat1 = zoomExt[3], lat2 = zoomExt[4])

  map %>% clearMarkers() %>%
    clearShapes() %>%
    # add background polygon
    mapBgPolys(bgShpXY())

  # Define raster colors and shiny legend
  rasCols <- c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", "#d7191c")
  # if it is threshold specified
  if (length(unique(userValues)) == 3 |
      length(unique(userValues)) == 2) {
    map %>% removeImage(layerId = 'mapPred') %>%
      removeImage(layerId = 'postPred') %>%
      removeControl(layerId = 'expert') %>%
      addLegend("bottomright", colors = c('gray', 'purple'),
                      title = "Distribution<br>map",
                      labels = c("Unsuitable", "Suitable"),
                      opacity = 1, layerId = 'expert') %>%
      addRasterImage(userRaster, colors = c('gray', 'purple'),
                     opacity = 0.7, group = 'mask', layerId = 'postPred',
                     method = "ngb")
  } else {
    # if threshold specified
    legendPal <- colorNumeric(rev(rasCols), userValues, na.color='transparent')
    rasPal <- colorNumeric(rasCols, userValues, na.color='transparent')
    map %>% removeImage(layerId = 'mapPred') %>%
      removeImage(layerId = 'postPred') %>%
      removeControl(layerId = 'expert') %>%
      addLegend("bottomright", pal = legendPal, title = "Suitability<br>(User) (**)",
                values = userValues, layerId = "expert",
                labFormat = reverseLabels(2, reverse_order=TRUE)) %>%
      addRasterImage(userRaster, colors = rasPal,
                     opacity = 0.7, group = 'mask', layerId = 'postPred',
                     method = "ngb")
  }


}

userSDM_INFO <- infoGenerator(modName = "User-provided distribution map",
                              modAuts = "Gonzalo E. Pinilla-Buitrago",
                              pkgName = "raster")
