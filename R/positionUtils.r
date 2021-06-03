#' create array of positions
#' @param boxDim - the dimension of the box (row, column)
#' @param by - the direction of filling the box
#' @return a data.frame with row and column positions
#'
#' @examples
#' g <- getPlatePos(by = "col")
#' @export
getPlatePos <- function(boxDim = c(8, 12), by = "row"){
  rowMax = boxDim[1]
  colMax = boxDim[2]
  if (by == "row") {
    col <- rep(seq(1:colMax), rowMax)
    row <- rep(c(1:rowMax), each = colMax)
  } else if (by == "col") {
    row <- rep(seq(1:rowMax), colMax)
    col <- rep(c(1:colMax), each = rowMax)
  }
  pos <- RCToPos(row, col)
  return(pos)
}

#' convert A1 position to row and column
#' @param pos - an array of positions
#' @param collapse - if TRUE, will collapse the output into a single string
#' @return a data.frame with row and column positions
#' @export
posToRC <- function(pos, collapse = FALSE) {
  # rowIdx <- LETTERS[1:boxDim[1]]
  row <- col <- c()
  for (i in pos) {
    row <- c(row, as.numeric(which(LETTERS == substr(i, 1, 1))))
    col <- c(col, as.numeric(substr(i, 2, nchar(i))))
  }
  if (collapse) {
    return(paste0("R", row, "C", col))
  } else {
    return(data.frame(row=row, col=col))
  }
}

#' convert position to numeric position
#' @param pos - the position or an array of positions
#' @param boxDim - the dimension of the box (row, column)
#' @return an array of numeric positions
#' @examples
#' posToNum(c("B2", "C3"), boxDim = c(10,10))
#' @export
posToNum <- function(pos, boxDim = c(8, 12)) {
  rc <- posToRC(pos)
  num <- (rc$row - 1) * boxDim[2] + rc$col
  return(num)
}

#' convert row and column to a position
#' @param row - the row or an array of rows
#' @param col - the column or an array of columns
#' @return the position or an array of positions
#' @export
RCToPos <- function(row, col){
  pos <- paste0(LETTERS[row],col)
  return(pos)
}

#' convert row and column to a numeric position
#' @param row - the row or an array of rows
#' @param col - the column or an array of columns
#' @param boxDim - the dimension of the box (row, column)
#' @return the numeric position or an array of them
#' @export
RCToNum <- function(row, col, boxDim = c(8, 12)){
  pos <- RCToPos(row, col)
  num <- posToNum(pos, boxDim)
  return(num)
}

#' convert a numeric position to a letter position
#' @param num - the position or an array of positions
#' @param boxDim - the dimension of the box (row, column)
#' @return the letter position or an array of them
#' @export
numToPos <- function(num, boxDim = c(8, 12)){
  row <- ((num - 1) %/% boxDim[2]) + 1
  col <- ((num - 1) %% boxDim[2]) + 1
  pos <- RCToPos(row, col)
  return(pos)
}

#' find empty positions
#' @param positions - the position or an array of positions
#' @param boxDim - the dimension of the box (row, column)
#' @param by - the margin
#' @return empty positions
#' @export
findEmptyPositions <- function(positions, boxDim = c(8, 10), by = "col") {
  g <- getPlatePos(boxDim, by = by)
  diff <- setdiff(g, positions)
  return(diff)
}

findEmptyPositionsOn <- function(selectedSamples, plate) {
  plate <- plate
  F <- selectedSamples$plateID == plate
  positions <- paste0(LETTERS[selectedSamples$newRow[F]], selectedSamples$newCol[F])
  return(findEmptyPositions(positions))
}

findAllEmptyPositions <- function(selectedSamples, boxDim = c(8, 10), by = "col") {
  emptyPositions <- data.frame()
  for (plate in levels(factor(selectedSamples$plateID))) {
    F <- selectedSamples$plateID == plate
    positions <- selectedSamples$wellPos[F]
    empty <- findEmptyPositions(positions, boxDim = boxDim, by = by)
    empty <- data.frame(position = empty, plateID = rep(plate, length(empty)))
    emptyPositions <- rbind(emptyPositions, empty)
  }
  return(emptyPositions)
}

