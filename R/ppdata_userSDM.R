#'
#' @title  ppdata_userSDM
#' @description Upload user-specified SDM prediction
#'
#' @details
#' See Examples.
#'
#' @param rasPath character of directory to predictions
#' @param rasName character vector of predictions raster names
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
#'
#'

ppdata_userSDM <- function(rasPath, rasName, shinyLogs = NULL) {
  r <- raster::raster(rasPath)
  names(r) <- fileNameNoExt(rasName)
  shinyLogs %>% writeLog("Raster: User SDM input.(**)")
  if(is.na(raster::crs(r))) {
    shinyLogs %>% writeLog(type = "warning",'Input rasters have undefined coordinate reference system (CRS). Mapping functionality in components Visualize Model Results and Project Model will not work. If you wish to map rasters in these components, please define their projections and upload again. See guidance text in this module for more details.')
  }
  print("Anakyn: It is working!")
  return(r)
}
