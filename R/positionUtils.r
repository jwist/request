#' convert A1 position to row and column
#' @param pos - an array of positions
#' @param boxDim - the dimension of the box (row, column)
#' @return a data.frame with row and column positions
#' @export
posToRC <- function(pos, boxDim = c(8, 10)) {
  rowIdx <- LETTERS[1:boxDim[1]]
  row <- col <- c()
  for (i in pos) {
    row <- c(row, as.numeric(which(rowIdx == substr(i, 1, 1))))
    col <- c(col, as.numeric(substr(i, 2, nchar(i))))
  }

  return(data.frame(row=row, col=col))
}

#' convert position to numeric position
#' @param pos - the position or an array of positions
#' @param boxDim - the dimension of the box (row, column)
#' @return an array of numeric positions
#'
#' @examples
#' posToNum(c("B2", "C3"), boxDim = c(10,10))
#' @export
posToNum <- function(pos, boxDim = c(8, 10)) {
  rc <- posToRC(pos, boxDim)
  num <- (rc$row - 1) * boxDim[2] + rc$col
  return(num)
}

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

  return(data.frame(row=row, col=col))
}

#' convert row and column to a position
#' @param row - the row or an array of rows
#' @param col - the column or an array of columns
#' @return the position or an array of positions
#' @examples
#' g <- getPlatePos(by = "col")
#' RCToPos(g[,1], g[,2])
#' @export
RCToPos <- function(row, col){
  pos <- paste0(LETTERS[row],col)
  return(pos)
}

