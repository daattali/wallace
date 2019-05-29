
#' @title mask_userAddRem
#' @description ..
#'
#' @details
#' See Examples.
#'
#' @param path x
#' @param name x
#' @param shinyLogs x
# @keywords
#'
# @examples
#'
#'
# @return
#' @author Gonzalo E. Pinilla-Buitrago <gpinillabuitrago@@gradcenter.cuny.edu>
#' @author Jamie Kass <jkass@@gradcenter.cuny.edu>
# @note

# @seealso
# @references
# @aliases - a list of additional topic names that will be mapped to
# this documentation when the user looks them up from the command
# line.
# @family - a family name. All functions that have the same family tag will be linked in the documentation.
#' @export

mask_userAddRem <- function(path, name, shinyLogs=NULL) {
  pathdir <- dirname(path)
  pathfile <- basename(path)
  # get extensions of all input files
  exts <- sapply(strsplit(name, '\\.'), FUN = function(x) x[2])

  if (length(exts) == 1 & exts == 'csv') {
    f <- read.csv(path, header = TRUE)
    polyAddRem <- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(f)), 1)))
  } else if ('shp' %in% exts) {
    if (length(exts) < 3) {
      shinyLogs %>% writeLog(type = 'error', 'If entering a shapefile, please
                             select all the following files: .shp, .shx, .dbf.')
      return()
    }
    file.rename(path, file.path(pathdir, name))
    # get index of .shp
    i <- which(exts == 'shp')
    shpName <- strsplit(name[i], '\\.')[[1]][1]
    # read in shapefile and extract coords
    polyAddRem <- rgdal::readOGR(pathdir[i], shpName)
  } else {
    shinyLogs %>% writeLog(type = 'error', 'Please enter either a CSV file of
                           vertex coordinates or shapefile (.shp, .shx, .dbf).')
    return()
  }
  return(polyAddRem)
}
