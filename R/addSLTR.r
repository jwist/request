
#' add SLTR to selectedSamples
#' @param ssl - selectedSamples
#' @return selectedSamples with SLTR
#' @export
#' @importFrom dplyr %>%
addSLTR <- function(ssl) {
  # adding SLTR
  excludeSLTR <- c("B3", "B8", "G3", "G8")
  numberOfPlates <- length(unique(ssl$plateID))
  exclude <- rep(excludeSLTR, numberOfPlates)
  row <- ssl[1:length(exclude),]
  for (i in 1:length(exclude)) {
    row[i, ] <- NA
  }
  row$sampleID <- "SLTR"
  row$projectName <- ssl$projectName[1]
  row$cohortName <- ssl$cohortName[1]
  row$sampleMatrixType <- ssl$sampleMatrixType[1]
  row$sourceID <- ssl$sampleMatrixType[1]
  row$tubeLabel <- "SLTR"
  row$wellPos <- exclude
  row$wellRC <- posToRC(exclude, collapse = TRUE)
  row$plateID <- rep(unique(ssl$plateID), each = length(excludeSLTR))
  ssl <- rbind(ssl, row) %>% arrange(plateID, wellPos)

  if (sum(duplicated(paste0(ssl$plateID, ssl$wellPos))) != 0) {
    stop("request >> addSLTR >> duplicated positions")
  } else {
    cat(crayon::green("SLTR added:", nrow(row), "\n"))
    cat(crayon::green("new row count:", nrow(ssl), "\n"))
  }
  return(ssl)
}
