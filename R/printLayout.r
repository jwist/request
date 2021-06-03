#' print html layout of plates
#' @param selectedSamples - the list of sample to process
#' @return an html table
#' @export
#' @importFrom htmlTable addHtmlTableStyle htmlTable
#' @importFrom magrittr %>%
printLayout <- function(selectedSamples, boxDim = c(8, 10)) {
  cat("<p style=\"page-break-before: always\">")
  plateList <- levels(factor(selectedSamples$plateID))
  for (plate in 1:length(plateList)) {
    ss <- selectedSamples[selectedSamples$plateID == plateList[plate],]
    position <- posToRC(ss$wellPos)
    F <- sort(as.numeric(position[,2]), index.return = TRUE)$ix
    ss <- ss[F,]
    # F <- sort(position[,1], index.return = TRUE)$ix
    # ss <- ss[F,]
    vec <- c(paste0( "", ss$tubeLabel, "",
                     "<br>", "<b>", ss$plateName, "</b> ",
                     ss$tubePos, "(" , ss$wellPos, ")<br>"),
             rep(NA, 80 - nrow(ss)))

    mat <- matrix(vec, boxDim[1], boxDim[2])
    rownames(mat) <- LETTERS[1:boxDim[1]]
    colnames(mat) <- c(1:boxDim[2])
    print(mat %>%
            addHtmlTableStyle(col.rgroup = c("none", "#CBD3F2"), col.columns = c("none", "#D17DF2")) %>%
            htmlTable(cgroup = c(paste0("", plateList[plate], "")),
                      n.cgroup = c(10)))

    if (plate %% 2 == 0) {
      cat("<p style=\"page-break-before: always\">")
    }
  }
}
