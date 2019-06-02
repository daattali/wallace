#'
#' @title  ppdata_rasters
#' @description Upload raster for post-processing
#'
#' @details
#' See Examples.
#'
#' @param rasPath character of directory to rasters
#' @param rasName character vector of raster names
# @keywords
#'
# @examples
#'
#'
# @return
#' @author Gonzalo E. Pinila-Buitrago <gpinillabuitrago@@gradcenter.cuny.edu>
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

ppdata_rasters <- function(rasPath, rasName, shinyLogs = NULL) {
  smartProgress(shinyLogs, message = "Reading in rasters...", {
    listPpRas <- lapply(X = rasPath, FUN = raster::raster)
  })
  sameAtt <- raster::compareRaster(listPpRas, stopiffalse = FALSE)
  if (sameAtt == FALSE) {
    shinyLogs %>%
      writeLog(type = 'error',
               "Rasters don't have the same resolution, extent, crs or origin. (**)")
    return()
  }
  # assign names
  names(listPpRas) <- fileNameNoExt(rasName)

  shinyLogs %>% writeLog("Post-processing rasters uploaded (**)")

  return(listPpRas)
}
