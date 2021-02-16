
printLayout <- function(selectedSamples) {
  cat("<p style=\"page-break-before: always\">")
  plateList <- levels(factor(selectedSamples$plateID))
  for (plate in 1:length(plateList)) {
    ss <- selectedSamples[selectedSamples$plateID == plateList[plate],]
    position <- posToRC(ss$wellPos)
    F <- sort(as.numeric(position[,2]), index.return = TRUE)$ix
    ss <- ss[F,]
    # F <- sort(position[,1], index.return = TRUE)$ix
    # ss <- ss[F,]
    vec <- c(paste0( "", ss$sourceID, "",
                     "<br>", "<b>", ss$plateName, "</b> ",
                     ss$tubePos, "(" , ss$wellPos, ")<br>"),
             rep(NA, 80 - nrow(ss)))

    mat <- matrix(vec, 8, 10)
    rownames(mat) <- LETTERS[1:8]
    colnames(mat) <- c(1:10)
    print(mat %>%
            addHtmlTableStyle(col.rgroup = c("none", "#CBD3F2"), col.columns = c("none", "#D17DF2")) %>%
            htmlTable(cgroup = c(paste0("", plateList[plate], "")),
                      n.cgroup = c(10)))

    if (plate %% 2 == 0) {
      cat("<p style=\"page-break-before: always\">")
    }
  }
}
