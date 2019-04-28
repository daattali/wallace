mask_drawAddRem <- function(polyMaskXY, polyMaskID, shinyLogs = NULL) {
  newPoly <- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(polyMaskXY)),
                                                   ID = polyMaskID)))
  ## Add shiny logs
  ## Example
  # shinyLogs %>% writeLog(em(spName(occs)), ' : Draw polygon without buffer(**).')
  drawAddRemExt <- sp::SpatialPolygonsDataFrame(newPoly,
                                                data = data.frame(x=1),
                                                match.ID = FALSE)
  return(drawAddRemExt)
}
