
#' @title mask_doAddRem
#' @description ..
#'
#' @details
#' See Examples.
#'
#' @param polyAddRem x
#' @param prediction x
#' @param rem x
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

mask_doAddRem <- function(polyAddRem, prediction, rem = FALSE,
                          shinyLogs = NULL) {
  ## Add shiny logs
  if (rem == FALSE) {
    addRaster <- raster::rasterize(polyAddRem, prediction, 1)
    addRaster[is.na(addRaster)] <- 0
    newPred <- prediction + addRaster
    newPred[newPred > 1] <- 1
    return(newPred)
  } else {
    remRaster <- raster::rasterize(polyAddRem, prediction, 1)
    remRaster[is.na(remRaster)] <- 0
    newPred <- prediction - remRaster
    newPred[newPred < 0] <- 0
    return(newPred)
  }
}
