#' print html layout of plates to help sample splitting
#' @param selectedSamples - the list of sample to process
#' @return an html table
#' @export
#' @importFrom htmlTable addHtmlTableStyle htmlTable
#' @importFrom magrittr %>%
printLayoutHelper <- function(selectedSamples) {
  cat("<p style=\"page-break-before: always\">")

  minHeaders <- c("boxName",
                  "tubePosition",
                  "tubeLabel",
                  "plateID",
                  "wellPos")

  fi <- minHeaders %in% colnames(selectedSamples)
  if (sum(fi) < length(minHeaders)) {
    stop(paste("request >> printLayoutHelper >> missing headers", minHeaders[!fi], "\n"))
  }

  boxList <- levels(factor(selectedSamples$boxName))
  for (box in boxList) {
    fi <- selectedSamples$boxName == box
    mat <- data.frame(oldRC = posToRC(selectedSamples$tubePosition[fi], collapse = TRUE),
                      oldPos = selectedSamples$tubePosition[fi],
                      tubeLabel = selectedSamples$tubeLabel[fi],
                      plateID = selectedSamples$plateID[fi],
                      wellPos = selectedSamples$wellPos[fi])

    if (nrow(mat) %% 2 != 0) {
      mat <- rbind(mat, rep(NA, 5))
    }
    l <- nrow(mat)
    newMat <- cbind(mat[1:(l %/% 2),],
                    " " = rep("   ", l %/% 2),
                    mat[(l %/% 2 + 1):l,])
    caption <- paste("Number of tubes:", sum(fi))
    print(newMat %>%
            addHtmlTableStyle(col.rgroup = c("none", "#FFBFBC")) %>%
            htmlTable(cgroup = rbind(c(paste0("<h3>", box, "</h3>"), rep(NA, 6)),
                                    c("box", "", "plate", "", "box", "", "plate")),
                      n.cgroup = rbind(c(7, rep(NA, 6)), c(2,1,2,1,2,1,2)),
                      rnames = FALSE,
                      caption = caption))

    cat("<p style=\"page-break-before: always\">")
  }
}
