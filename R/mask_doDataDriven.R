
#' @title mask_doDataDriven
#' @description ..
#'
#' @details
#' See Examples.
#'
#' @param lower x
#' @param upper x
#' @param maskRaster x
#' @param pred x
#' @param shinyLogs x
# @keywords
#'
# @examples
#'
#'
# @return
#' @author Gonzalo Pinilla gpinillabuitrago@@gradcenter.cuny.edu
# @note
# @seealso
# @references
# @aliases - a list of additional topic names that will be mapped to
# this documentation when the user looks them up from the command
# line.
# @family - a family name. All functions that have the same family tag will be linked in the documentation.

#' @export
#'

mask_doDataDriven <- function(lowerInp, upperInp,
                              maskRaster, pred, shinyLogs = NULL) {
  if (is.na(lowerInp) & is.na(upperInp)) {
    shinyLogs %>% writeLog(type = 'error', "Please, provide bound(s) (**)")
    return()
  }
  # compare prediction and mask Raster
  sameExt <- raster::compareRaster(maskRaster, pred, extent = FALSE, rowcol = FALSE,
                                   crs = TRUE, res = TRUE, stopiffalse = FALSE)
  if (sameExt == FALSE) {
    shinyLogs %>%
      writeLog(type = 'error',
               "Rasters don't have the same resolution, crs or origin. (**)")
    return()
  }
  maskRaster <- raster::crop(maskRaster, pred)
  if (!is.na(lowerInp) & is.na(upperInp)) {
    postPred <- pred * (maskRaster >= lowerInp)
  }
  if (is.na(lowerInp) & !is.na(upperInp)) {
    postPred <- pred * (maskRaster <= upperInp)
  }
  if (!is.na(lowerInp) & !is.na(upperInp)) {
    postPred <- pred * (maskRaster >= lowerInp) * (maskRaster <= upperInp)
  }
  return(postPred)
}
