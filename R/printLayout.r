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
    if (chk) {
      ss <- ss[sample(nrow(ss), 3), ]
    }
    ep <- data.frame(wellPos = findEmptyPositions(ss$wellPos, boxDim = boxDim, by = by))
    ss <- merge(ep, ss, by = "wellPos", all = TRUE) # adding missing slots
    ss[is.na(ss)] <- " "

    position <- posToRC(ss$wellPos)
    F <- sort(as.numeric(position[,2]), index.return = TRUE)$ix
    ss <- ss[F,]

    # prepare coloring
    # color <- sapply(ss$sampleID,
    #                   function(i) ifelse(i == " ",
    #                                      "background-color:#333333",
    #                                      "background-color:#ffffff"))
    # coloring LTRs
    color <- list()
    for (i in 1:nrow(ss)) {
      if(ss[i,]$sampleID == " ") {
        color[[i]] <- "background-color:#333333"
      } else {
        color[[i]] <- "background-color:#ffffff"
      }
      if (ss[i,]$wellPos %in% c("B3", "G3", "B8", "G8")) {
        color[[i]] <- "background-color:#99ffff;opacity:1"
      }
    }
    color <- unlist(color)

    vec <- c(paste0( "", ss$tubeLabel, "",
                     "<br>", "<b>", ss$plateName, "</b>",
                     ss$tubePos, "(" , ss$wellPos, ")<br>"),
             rep(NA, boxDim[1]*boxDim[2] - nrow(ss)))

    mat <- matrix(vec, boxDim[1], boxDim[2])
    css.cell <- matrix(color, boxDim[1], boxDim[2])

    rownames(mat) <- LETTERS[1:boxDim[1]]
    colnames(mat) <- c(1:boxDim[2])
    print(mat %>%
            addHtmlTableStyle(col.rgroup = c("none", "#CBD3F2"),
                              col.columns = c("none", "#D17DF2")) %>%
            htmlTable(cgroup = c(paste0("", plateList[plate], "")),
                      n.cgroup = c(boxDim[2]), css.cell = css.cell))

    if (plate %% 2 == 0) {
      cat("<p style=\"page-break-before: always\">")
    }
  }
}
