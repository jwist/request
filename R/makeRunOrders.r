makeRunOrder <- function(batchID,
                         runID,
                         type,
                         deviceName,
                         methodName,
                         templateFile) {
  lop <- rolBatchInfo(batchID = batchID)
  print(lop)
  ssl <- rolSelectedSamples(lop$sampleLayoutID)
  if (sum(lop$countSamples) == nrow(ssl)) {
    # add RC column
    ssl$wellRC <- posToRC(ssl$wellpos, collapse = TRUE)
    ssl <- addSLTR(ssl)
    con <- openDb()
    matrixID <- dbGetQuery(con, paste0("select matrix_id
                         from matrix
                         where matrix_name ='",
                         lop$sampleMatrixType[1],
                         "'"))$matrix_id

    ptt <- file.path(
      system.file(
        package = "request"),
      "extdata")
    # loading template without headers
    t <- read.table(file.path(ptt, templateFile),
                    header = FALSE,
                    sep = "\t",
                    check.names = FALSE)

    # removing helper column from template
    t <- t[,-which(t[1,] == "helper")]

    # formating information
    runName <- paste(ssl$projectName[1],
                     ssl$cohortName[1],
                     ssl$sampleMatrixType[1],
                     methodName,
                     runID,
                     sep = "_")

    outputPath <- runName
    sampleMatrixType <- ssl$sampleMatrixType[1]
    projectName <- ssl$projectName[1]

    # substituting code snippets and create run orders
    n <- length(lop$sampleLayoutID)

    for (i in 1:n) {
      # formating information for plate and padding with empty rows
      fi <- ssl$plateID == lop$sampleLayoutID[i]
      r <- ssl[fi,]
      r$numberedPosition <- posToNum(r$wellPos)
      r$positionNumber <- substring(r$wellPos, 2)
      r$positionLetter <- substr(r$wellPos, 1, 1)
      r[(sum(fi) + 1):80,] <- "EMPTY"

      # evaluating code snippets
      counter <- 0
      res <- data.frame()
      for (j in 1:nrow(t)) {
        if (!grepl("EMPTY",  tmplUpdate(tmpl(as.character(t[j,1]))))) {
          counter <- counter + 1
          for (k in 1:ncol(t)) {
            res[counter,k] <- tmplUpdate(tmpl(as.character(t[j,k])))
          }
        }
      }
      res <- data.table(do.call("cbind", res))
      # duplicated(res$VialPos)
      print(getwd())
      # writing run orders
      fileName <- paste0(runName, "_",
                         lop$sampleLayoutID[i],
                         ".tsv")
      write.table(res, file.path("configurationFiles",
                                 fileName),
                  sep = "\t",
                  quote = FALSE,
                  row.names = FALSE,
                  col.names = FALSE)
      cat(crayon::green("run order created:", fileName, "\n"))
    }

    # nmr <- getRun(ssl, type,
    #               matrixName = lop$sampleMatrixType[1],
    #               deviceName = deviceName,
    #               methodName = methodName,
    #               lop$cohortName[1],
    #               lop$projectName[1],
    #               runID)
    insertRuns(runID = runID,
               batchID = batchID,
               type = type,
               method = methodName,
               device = deviceName)

    dbDisconnect(con)
  } else {
    stop("makeRunOrder >> dim mismatch")
  }
}
