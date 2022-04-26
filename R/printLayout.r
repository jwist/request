#' print html layout of plates
#' @param selectedSamples - the list of sample to process
#' @return an html table
#' @export
#' @importFrom htmlTable addHtmlTableStyle htmlTable
#' @importFrom magrittr %>%
printLayout <- function(selectedSamples, boxDim = c(8, 10), by = "row", chk = FALSE ) {
  cat("<p style=\"page-break-before: always\">")
  plateList <- levels(factor(selectedSamples$plateID))
  platePositions <- getPlatePos(boxDim = boxDim, by = by)
  for (plate in 1:length(plateList)) {
    ss <- selectedSamples[selectedSamples$plateID == plateList[plate],]
    n <- nrow(ss)
    sn <- sample(n, 3)
    mtrx <- ss$sampleMatrixType[1]
    if (chk) { # sample for checking function
      ss <- ss[sample(n, 3), ]
    }
    ep <- data.frame(wellPos = findEmptyPositions(ss$wellPos, boxDim = boxDim, by = by))
    ss <- merge(ep, ss, by = "wellPos", all = TRUE) # adding missing slots
    fi <- is.na(ss$sampleID)
    ss$sampleID[fi] <- ""
    ss$tubeLabel[fi] <- ""
    ss$tubePosition[fi] <- ""

    position <- posToRC(ss$wellPos)
    F <- sort(as.numeric(position[,2]), index.return = TRUE)$ix
    ss <- ss[F,]

    color <- list()
    for (i in 1:nrow(ss)) {
      if (grepl("A|C|E|G", ss[i,]$wellPos)) {
        color[[i]] <- c(0, 0, 0, 0.1)
      } else {
        color[[i]] <- c(0, 0, 0, 0)
      }
    }
    for (i in 1:nrow(ss)) {
      if (grepl("2|4|6|8|10", ss[i,]$wellPos)) {
        color[[i]] <- color[[i]] + c(0, 0, 0, 0.1)
      }
      if (ss[i,]$sampleID == "SLTR")  {
        color[[i]] <- c(255, 0, 0, 0.7)
      }
      if (ss[i,]$sampleID == "") {
        color[[i]] <- c(0, 0, 0, 0.7)
      }
    }
    color <- sapply(color, function(x) rgb(t(x[1:3]), alpha = x[4]*255, maxColorValue = 255))
    color <- paste0("background-color:", color)

    # modifying text color for check
    for (i in 1:nrow(ss)) {
      if (i %in% sn) {
        color[i] <- paste0(color[i], "; color:#000000")
      } else {
        color[i] <- paste0(color[i], "; color:#333333")
      }
    }
    css.cell <- matrix(color, boxDim[1], boxDim[2])

    vec <- c(paste0( "", ss$tubeLabel, "",
                     "<br>", "<b>", ss$plateName, "</b>",
                     ss$tubePos, "->" , ss$wellPos, "<br>"),
             rep(NA, boxDim[1]*boxDim[2] - nrow(ss)))

    # adding a dot for check
    for (i in 1:nrow(ss)) {
      if (i %in% sn) {
        vec[i] <- paste0(vec[i], ".")
      }
    }

    mat <- matrix(vec, boxDim[1], boxDim[2])

    rownames(mat) <- LETTERS[1:boxDim[1]]
    colnames(mat) <- c(1:boxDim[2])
    caption <- paste("Number of wells (includes SLTR):", n)
    print(mat %>%
            # addHtmlTableStyle(col.rgroup = c("none", "#CBD3F2"),
            #                   col.columns = c("none", "#D17DF2")) %>%
            htmlTable(cgroup = c(paste0("<h3>", plateList[plate], " matrix:" ,mtrx, "</h3>")),
                      n.cgroup = c(boxDim[2]),
                      css.cell = css.cell,
                      caption = caption))

    if (plate %% 2 == 0) {
      cat("<p style=\"page-break-before: always\">")
    }
  }
}
