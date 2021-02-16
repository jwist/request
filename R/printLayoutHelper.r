
printLayoutHelper <- function(selectedSamples) {
  cat("<p style=\"page-break-before: always\">")

  boxList <- levels(factor(selectedSamples$boxName))
  for (box in boxList) {
    fi <- selectedSamples$boxName == box
    mat <- data.frame(oldRC = selectedSamples$tubeRC[fi],
                      oldPos = selectedSamples$tubePosition[fi],
                      sourceID = selectedSamples$sourceID[fi],
                      plateName = selectedSamples$plateID[fi],
                      position = selectedSamples$wellPos[fi],
                      RC = selectedSamples$wellRC[fi])

    if (nrow(mat) %% 2 != 0) {
      mat <- rbind(mat, rep(NA, 5))
    }
    l <- nrow(mat)
    newMat <- cbind(mat[1:(l %/% 2),],
                    " " = rep("   ", l %/% 2),
                    mat[(l %/% 2 + 1):l,])
    print(newMat %>%
            addHtmlTableStyle(col.rgroup = c("none", "#CBD3F2")) %>%
            htmlTable(cgroup = c(paste0("<h2>", box, "</h2>")),
                      n.cgroup = c(13)))
    cat("<p style=\"page-break-before: always\">")
  }
}
