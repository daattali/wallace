userSDM_UI <- function(id) {
  ns <- NS(id)
  tagList(
    fileInput(ns("userSDMs"), label = "Upload SDM predictions (**)",
      multiple = TRUE)
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
                                 rasName = input$userSDMs$name[i])
      # Create name for curSp()
      newSppName <- fileNameNoExt(formatSpName(input$userSDMs$name[i]))
      spp[[newSppName]] <- list()
      # Create new bgExt based on cells with data
      # rasterExt <- userSDMs > -Inf
      # extPoly <- raster::rasterToPolygons(rasterExt, dissolve = TRUE)
      # Create new bgExt
      e <- raster::extent(userSDMs)
      # coerce to a SpatialPolygons object
      extPoly <- as(e, 'SpatialPolygons')
      # LOAD INTO SPP ####
      spp[[newSppName]]$postProc$prediction <- userSDMs
      spp[[newSppName]]$procEnvs$bgExt <- extPoly

      # METADATA ####

    }
  })
}

userSDM_MAP <- function(map, session) {
  updateTabsetPanel(session, 'main', selected = 'Map')
  req(spp[[curSp()]]$postProc$prediction, spp[[curSp()]]$procEnvs$bgExt)
  map %>% clearMarkers() %>%
    clearShapes() %>%
    # add background polygon
    mapBgPolys(bgShpXY())
    # removeImage(layerId = 'mapPred') %>%
    # removeImage(layerId = 'postPred') %>%
    # removeControl(layerId = 'train') %>%
  map %>% addLegend("bottomright", colors = c('gray', 'purple'),
                    title = "Expert Suitability (**)",
                    labels = c("Absence (**)", "Presence (**)"),
                    opacity = 1, layerId = 'expert') %>%
    addRasterImage(spp[[curSp()]]$postProc$prediction, colors = c('gray', 'purple'),
                   opacity = 0.7, group = 'mask', layerId = 'postPred',
                   method = "ngb")
#
#
#   xy <- ggplot2::fortify(polyAddRem[[length(polyAddRem)]])
#
#   if (length(polyAddRem) == 1) {
#     map %>%
#       addPolygons(lng = xy[,1], lat = xy[,2],
#                   weight = 4, color = "gray", group = 'maskShp') %>%
#       removeImage(layerId = 'mapPred') %>%
#       removeControl(layerId = 'train') %>%
#       addLegend("bottomright", colors = c('gray', 'purple'),
#                 title = "Expert Suitability (**)",
#                 labels = c("Absence (**)", "Presence (**)"),
#                 opacity = 1, layerId = 'expert') %>%
#       addRasterImage(spp[[curSp()]]$postProc$prediction, colors = c('gray', 'purple'),
#                      opacity = 0.7, group = 'mask', layerId = 'postPred',
#                      method = "ngb")
#   } else {
#     map %>% clearGroup('maskShp') %>%
#       addPolygons(lng = xy[,1], lat = xy[,2],
#                   weight = 4, color = "gray", group = 'maskShp') %>%
#       removeImage(layerId = 'postPred') %>%
#       addRasterImage(spp[[curSp()]]$postProc$prediction, colors = c('gray', 'purple'),
#                      opacity = 0.7, group = 'mask', layerId = 'postPred',
#                      method = "ngb")
#   }
}

userSDM_INFO <- infoGenerator(modName = "User-specified SDM prediction (**)",
                              modAuts = "Gonzalo E. Pinilla-Buitrago",
                              pkgName = NULL)
