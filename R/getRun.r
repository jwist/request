#' create configuration files
#' @param selectedSamples - the selected samples for run
#' @param type - the type of run
#' @param matrixID - the id of the samples matrix
#' @param deviceID - the id of the device to run the experiments
#' @param methodID - the id of the method to be run
#' @param cohortName - the name of the cohort to run
#' @param projectName - the name of the project
#' @param options - options
#' @return the name of the device
#' @export
getRun <- function(selectedSamples,
                   type,
                   matrixID,
                   deviceID,
                   methodID,
                   cohortName,
                   projectName,
                   options = list()) {
  types <- c("NMR",
             "MS-TRY",
             "MS-AA",
             "MS-EICOS",
             "MS-URPP",
             "MS-LIPIDS",
             "MS-MRMSP",
             "MS-MRMSN",
             "TIMS-LIPIDS-P",
             "TIMS-LIPIDS-N")
  choice <- which(type == types)

  cat(crayon::blue("request >> getRUN >> running: ", types[choice], "\n"))

  deviceName <- getDevice(c(deviceID = deviceID))[[1]]
  methodName <- getDevice(c(methodID = methodID))[[1]]
  matrixName <- getDevice(c(matrixID = matrixID))[[1]]

  runName <- paste(projectName,
                   cohortName,
                   matrixName,
                   methodName,
                   deviceName,
                   sep = "_")

  LTR_NAME <- paste0("LTR_", matrixName)

  if ("date" %in% names(options)) {
    date <- options$date
  } else {
    date <- format(Sys.time(), "%d%m%y")
  }

  switch(choice,
         runNMR(selectedSamples, runName, projectName, matrixID, deviceID, methodID, LTR_NAME, date),
         runMS_TRY(selectedSamples, runName, projectName, matrixID, deviceID, methodID, LTR_NAME, date),
         runMS_AA(selectedSamples, runName, projectName, matrixID, deviceID, methodID, LTR_NAME, date),
         runMS_EICOS(selectedSamples, runName, projectName, matrixID, deviceID, methodID, LTR_NAME, date),
         runMS_URPP(selectedSamples, runName, projectName, matrixID, deviceID, methodID, LTR_NAME, date),
         runMS_LIPIDS(selectedSamples, runName, projectName, matrixID, deviceID, methodID, LTR_NAME, date),
         runMS_MRMSP(selectedSamples, runName, projectName, matrixID, deviceID, methodID, LTR_NAME, date),
         runMS_MRMSN(selectedSamples, runName, projectName, matrixID, deviceID, methodID, LTR_NAME, date),
         runTIMS_LIPIDS_P(selectedSamples, runName, projectName, matrixID, deviceID, methodID, LTR_NAME, date),
         runTIMS_LIPIDS_N(selectedSamples, runName, projectName, matrixID, deviceID, methodID, LTR_NAME, date))
}

#' write run file for MS-TRY
#' @param selectedSamples - the selected samples for run
#' @param runName - the name of the run
#' @param projectName - the name of the project
#' @param matrixID - the id of the sample matrix
#' @param deviceID - the id of the device
#' @param methodID - the id of the method
#' @param date - the date of the run
#' @return void
runMS_TRY <- function(selectedSamples, runName, projectName, matrixID, deviceID, methodID, LTR_NAME, date) {

  plateList <- levels(factor(selectedSamples$plateID))

  plateCounter <- 1
  req <- list()
  for (plate in plateList) {

    currentRunName <- paste(runName,
                     plate,
                     date,
                     sep = "_")

    plateNames <- selectedSamples$plateID

    sampleID <- selectedSamples$sampleID[plateNames == plate]
    tubeLabel <- selectedSamples$tubeLabel[plateNames == plate]
    positions <- selectedSamples$wellPos[plateNames == plate]
    RC <- posToRC(positions)
    columns <- RC$col
    rows <- RC$row

    r <- new("request")
    runParam <- list("runName" = currentRunName,
                     "projectName" = projectName,
                     "methodID" = methodID,
                     "deviceID" = deviceID,
                     "matrixID" = matrixID,
                     "platePosition" = plateCounter)
    rl <- new("requestList")

    doubleBlk <- fillRequest(r, request = c(runParam, "sampleID" = "Double Blank", "row" = 0, "column" = 0, "sampleType" = "Blank"))
    singleBlk <- fillRequest(r, request = c(runParam, "sampleID" = "Single Blank", "row" = 8, "column" = 12, "sampleType" = "Blank"))
    mixTest <- fillRequest(r, request = c(runParam, "sampleID" = "mix test", "row" = -1, "column" = 12, "sampleType" = "Analyte"))
    cal1 <- fillRequest(r, request = c(runParam, "sampleID" = "Cal 1", "row" = 8, "column" = 11, "sampleType" = "Standard", "options" = list(list("conc" = c(20,	40,	80,	100,	200,	400,	2000,	10000,	20000)))))
    cal2 <- fillRequest(r, request = c(runParam, "sampleID" = "Cal 2", "row" = 7, "column" = 11, "sampleType" = "Standard", "options" = list(list("conc" = c(10,	20,	40,	50,	100,	200,	1000,	5000,	10000)))))
    cal3 <- fillRequest(r, request = c(runParam, "sampleID" = "Cal 3", "row" = 6, "column" = 11, "sampleType" = "Standard", "options" = list(list("conc" = c(5,	10,	20,	25,	50,	100,	500,	2500, 5000)))))
    cal4 <- fillRequest(r, request = c(runParam, "sampleID" = "Cal 4", "row" = 5, "column" = 11, "sampleType" = "Standard", "options" = list(list("conc" = c(3,	6,	12,	15,	30,	60,	300,	1500,	3000)))))
    cal5 <- fillRequest(r, request = c(runParam, "sampleID" = "Cal 5", "row" = 4, "column" = 11, "sampleType" = "Standard", "options" = list(list("conc" = c(2,	4,	8,	10,	20,	40,	200,	1000,	2000)))))
    cal6 <- fillRequest(r, request = c(runParam, "sampleID" = "Cal 6", "row" = 3, "column" = 11, "sampleType" = "Standard", "options" = list(list("conc" = c(0.8,	1.6,	3.2,	4,	8,	16,	80,	400,	800)))))
    cal7 <- fillRequest(r, request = c(runParam, "sampleID" = "Cal 7", "row" = 2, "column" = 11, "sampleType" = "Standard", "options" = list(list("conc" = c(0.4,	0.8,	1.6,	2,	4,	8,	40,	200,	400)))))
    cal8 <- fillRequest(r, request = c(runParam, "sampleID" = "Cal 8", "row" = 1, "column" = 11, "sampleType" = "Standard", "options" = list(list("conc" = c(0.2,	0.4,	0.8,	1,	2,	4,	20,	100,	200)))))
    #QC6 <- fillRequest(r, request = c(runParam, "sampleID" = "QC 6", "row" = 2, "column" = 12, "sampleType" = "QC", "options" = list(list("conc" = c(0.4, 0.8, 1.6, 2, 4 ,8, 40, 200, 400)))))
    #QC5 <- fillRequest(r, request = c(runParam, "sampleID" = "QC 5", "row" = 3, "column" = 12, "sampleType" = "QC", "options" = list(list("conc" = c(0.8, 1.6, 3.2, 4, 8, 16, 80, 400, 800)))))
    QC4 <- fillRequest(r, request = c(runParam, "sampleID" = "QC 4", "row" = 1, "column" = 12, "sampleType" = "QC", "options" = list(list("conc" = c(2, 4, 8, 10, 20, 40, 200, 1000, 2000)))))
    QC3 <- fillRequest(r, request = c(runParam, "sampleID" = "QC 3", "row" = 2, "column" = 12, "sampleType" = "QC", "options" = list(list("conc" = c(8, 16, 32, 40, 80, 160, 800, 4000, 8000)))))
    QC2 <- fillRequest(r, request = c(runParam, "sampleID" = "QC 2", "row" = 3, "column" = 12, "sampleType" = "QC", "options" = list(list("conc" = c(16, 32, 64, 80, 160, 320, 1600, 8000, 16000)))))
    QC1 <- fillRequest(r, request = c(runParam, "sampleID" = "QC 1", "row" = 4, "column" = 12, "sampleType" = "QC", "options" = list(list("conc" = c(20, 40, 80, 100, 200, 400, 2000, 10000, 20000)))))
    LTR <- fillRequest(r, request = c(runParam, "sampleID" = LTR_NAME, "row" = 5, "column" = 12))
    LTR2 <- fillRequest(r, request = c(runParam, "sampleID" = LTR_NAME, "row" = 6, "column" = 12))
    LTR3 <- fillRequest(r, request = c(runParam, "sampleID" = LTR_NAME, "row" = 7, "column" = 12))
    LTR4 <- fillRequest(r, request = c(runParam, "sampleID" = LTR_NAME, "row" = 8, "column" = 12))

    ### header
    rl <- addRequest(rl, list(doubleBlk, mixTest, doubleBlk))
    rl <- addRequest(rl, list(cal8, cal7, cal6, cal5, cal4, cal3, cal2, cal1))
    rl <- addRequest(rl, list(LTR, doubleBlk, LTR))

    qcList <- list(QC4, LTR2, QC1, LTR2, QC3, LTR3, LTR3, QC2)
    chunkSize <- floor(length(rows)/8)
    for (i in 1:length(rows)) {
      rlist <- fillRequest(r, request = c(runParam,
                                          "sampleType" = "Sample",
                                          "sampleID" = paste0(sampleID[i], "_", tubeLabel[i]),
                                          "row" = as.numeric(rows[i]),
                                          "column" = as.numeric(columns[i])))
      if (i %% chunkSize == 0) {
        rl <- addRequest(rl,
                         list(rlist,
                              qcList[[(((i - 1)/(chunkSize)) %% length(qcList)) + 1]]))
      } else {
        rl <- addRequest(rl, list(rlist))
      }
    }

    # qcList <- list(QC4, QC2, QC5, QC3)
    # for (i in 1:length(rows)) {
    #   rlist <- fillRequest(r, request = c(runParam, "sampleType" = "Analyte" ,"sampleID" = paste0(sampleID[i], "_", tubeLabel[i]), "row" = as.numeric(rows[i]), "column" = as.numeric(columns[i])))
    #   if (i %% floor(length(rows)/4) == 0) {
    #     rl <- addRequest(rl, list(rlist))
    #   } else {
    #     rl <- addRequest(rl, list(rlist))
    #   }
    #   if (i %% floor(length(rows)/4) == 0) {
    #     rl <- addRequest(rl, list(rlist, qcList[[(((i - 1)/(floor(length(rows)/4))) %% length(qcList)) + 1]], doubleBlk, LTR))
    #     j <- i %/% floor(length(rows)/4)
    #     if (j %% 2 == 0) {
    #       LTR <- setPosition(LTR, "A12")
    #     } else {
    #       LTR <- setPosition(LTR, "H12")
    #     }
    #   }
    # }
    # if (length(sampleID) < 60){
    #   rl <- addRequest(rl, list(qcList[[6]], doubleBlk))
    # }
    # if (length(sampleID) < 70){
    #   rl <- addRequest(rl, list(qcList[[(70/10) %% 9]], doubleBlk))
    # }
    # if (length(sampleID) < 80){
    #   rl <- addRequest(rl, list(LTR, doubleBlk))
    # }
    ### footer
    rl <- addRequest(rl, list(doubleBlk, LTR4))
    rl <- addRequest(rl, list(doubleBlk, LTR4))
    rl <- addRequest(rl, list(cal8, cal7, cal6, cal5, cal4, cal3, cal2, cal1))
    rl <- addRequest(rl, list(doubleBlk, doubleBlk))
    # rl <- addRequest(rl, list(doubleBlk))

    run <- printRequest(rl, list("assay" = "MSWaters"))
    saveRun(run, currentRunName)
    plateCounter <- plateCounter + 1
    req <- c(req, list(requestList = rl, run = run))
  }
  return(req)
}

#' write run file for MS AA
#' @param selectedSamples - the selected samples for run
#' @param runName - the name of the run
#' @param projectName - the name of the project
#' @param matrixID - the id of the sample matrix
#' @param deviceID - the id of the device
#' @param methodID - the id of the method
#' @param date - the date of the run
#' @return void
runMS_AA <- function(selectedSamples, runName, projectName, matrixID, deviceID, methodID, LTR_NAME, date) {

  plateList <- levels(factor(selectedSamples$plateID))

  plateCounter <- 1
  req <- list()
  for (plate in plateList) {

    currentRunName <- paste(runName,
                     plate,
                     date,
                     sep = "_")

    plateNames <- selectedSamples$plateID

    sampleID <- selectedSamples$sampleID[plateNames == plate]
    tubeLabel <- selectedSamples$tubeLabel[plateNames == plate]
    positions <- selectedSamples$wellPos[plateNames == plate]
    RC <- posToRC(positions)
    columns <- RC$col
    rows <- RC$row

    r <- new("request")
    runParam <- list("runName" = currentRunName,
                     "projectName" = projectName,
                     "methodID" = methodID,
                     "deviceID" = deviceID,
                     "matrixID" = matrixID,
                     "platePosition" = 1 + (plateCounter %% 2))

    rl <- new("requestList")

    doubleBlk <- fillRequest(r, request = c(runParam, "sampleID" = "Double Blank", "row" = 0, "column" = 12, "sampleType" = "Blank"))
    singleBlk <- fillRequest(r, request = c(runParam, "sampleID" = "Single Blank", "row" = 0, "column" = 12, "sampleType" = "Blank"))
    cal9 <- fillRequest(r, request = c(runParam, "sampleID" = "Cal 9", "row" = 1, "column" = 12, "sampleType" = "Calibrant"))
    cal8 <- fillRequest(r, request = c(runParam, "sampleID" = "Cal 8", "row" = 1, "column" = 11, "sampleType" = "Calibrant"))
    cal7 <- fillRequest(r, request = c(runParam, "sampleID" = "Cal 7", "row" = 2, "column" = 11, "sampleType" = "Calibrant"))
    cal6 <- fillRequest(r, request = c(runParam, "sampleID" = "Cal 6", "row" = 3, "column" = 11, "sampleType" = "Calibrant"))
    cal5 <- fillRequest(r, request = c(runParam, "sampleID" = "Cal 5", "row" = 4, "column" = 11, "sampleType" = "Calibrant"))
    cal4 <- fillRequest(r, request = c(runParam, "sampleID" = "Cal 4", "row" = 5, "column" = 11, "sampleType" = "Calibrant"))
    cal3 <- fillRequest(r, request = c(runParam, "sampleID" = "Cal 3", "row" = 6, "column" = 11, "sampleType" = "Calibrant"))
    cal2 <- fillRequest(r, request = c(runParam, "sampleID" = "Cal 2", "row" = 7, "column" = 11, "sampleType" = "Calibrant"))
    cal1 <- fillRequest(r, request = c(runParam, "sampleID" = "Cal 1", "row" = 8, "column" = 11, "sampleType" = "Calibrant"))
    QC4 <- fillRequest(r, request = c(runParam, "sampleID" = "QC 4", "row" = 1, "column" = 12, "sampleType" = "QC"))
    QC3 <- fillRequest(r, request = c(runParam, "sampleID" = "QC 3", "row" = 2, "column" = 12, "sampleType" = "QC"))
    QC2 <- fillRequest(r, request = c(runParam, "sampleID" = "QC 2", "row" = 3, "column" = 12, "sampleType" = "QC"))
    QC1 <- fillRequest(r, request = c(runParam, "sampleID" = "QC 1", "row" = 4, "column" = 12, "sampleType" = "QC"))
    LTR <- fillRequest(r, request = c(runParam, "sampleID" = LTR_NAME, "row" = 5, "column" = 12, "sampleType" = "Sample"))
    LTR2 <- fillRequest(r, request = c(runParam, "sampleID" = LTR_NAME, "row" = 6, "column" = 12, "sampleType" = "Sample"))
    LTR3 <- fillRequest(r, request = c(runParam, "sampleID" = LTR_NAME, "row" = 7, "column" = 12, "sampleType" = "Sample"))
    LTR4 <- fillRequest(r, request = c(runParam, "sampleID" = LTR_NAME, "row" = 8, "column" = 12, "sampleType" = "Sample"))

    ### header
    rl <- addRequest(rl, list(doubleBlk, QC1, doubleBlk))
    rl <- addRequest(rl, list(cal8, cal7, cal6, cal5, cal4, cal3, cal2, cal1))
    rl <- addRequest(rl, list(doubleBlk))
    rl <- addRequest(rl, list(QC4, QC3, QC2, QC1))
    rl <- addRequest(rl, list(LTR, doubleBlk, LTR))

    qcList <- list(QC4, LTR2, QC1, LTR2, QC3, LTR3, LTR3, QC2)
    chunkSize <- floor(length(rows)/8)
    for (i in 1:length(rows)) {
      rlist <- fillRequest(r, request = c(runParam,
                                          "sampleType" = "Sample",
                                          "sampleID" = paste0(sampleID[i], "_", tubeLabel[i]),
                                          "row" = as.numeric(rows[i]),
                                          "column" = as.numeric(columns[i])))
      if (i %% chunkSize == 0) {
        rl <- addRequest(rl,
                         list(rlist,
                              qcList[[(((i - 1)/(chunkSize)) %% length(qcList)) + 1]]))
      } else {
        rl <- addRequest(rl, list(rlist))
      }
    }
    ### footer
    rl <- addRequest(rl, list(doubleBlk, LTR4))
    rl <- addRequest(rl, list(doubleBlk, LTR4))
    rl <- addRequest(rl, list(cal8, cal7, cal6, cal5, cal4, cal3, cal2, cal1))
    rl <- addRequest(rl, list(doubleBlk, doubleBlk))

    run <- printRequest(rl, list("assay" = "MSBruker"))
    saveRun(run, currentRunName)
    plateCounter <- plateCounter + 1
    req <- c(req, list(requestList = rl, run = run))
  }
  return(req)
}

#' write run file for MS MRMSP
#' @param selectedSamples - the selected samples for run
#' @param runName - the name of the run
#' @param projectName - the name of the project
#' @param matrixID - the id of the sample matrix
#' @param deviceID - the id of the device
#' @param methodID - the id of the method
#' @param date - the date of the run
#' @return void
runMS_MRMSP <- function(selectedSamples, runName, projectName, matrixID, deviceID, methodID, LTR_NAME, date) {
  plateList <- levels(factor(selectedSamples$plateID))
  req <- list()
  for (plate in plateList) {

    currentRunName <- paste(runName,
                            plate,
                            date,
                            sep = "_")

    plateNames <- selectedSamples$plateID

    sampleID <- selectedSamples$sampleID[plateNames == plate]
    tubeLabel <- selectedSamples$tubeLabel[plateNames == plate]
    positions <- selectedSamples$wellPos[plateNames == plate]
    RC <- posToRC(positions)
    columns <- RC$col
    rows <- RC$row

    r <- new("request")
    runParam <- list("runName" = currentRunName,
                     "projectName" = projectName,
                     "methodID" = methodID,
                     "deviceID" = deviceID,
                     "matrixID" = matrixID,
                     "sampleType" = "Sample")

    rl <- new("requestList")

    pqc1 <- fillRequest(r, request = c(runParam, "sampleID" = "PQC_Burnin1", "row" = 1, "column" = 1, "platePosition" =2, "sampleType" = "standard"))
    pqc2 <- fillRequest(r, request = c(runParam, "sampleID" = "PQC_Burnin2", "row" = 2, "column" = 1, "platePosition" =2, "sampleType" = "standard"))
    pqc3 <- fillRequest(r, request = c(runParam, "sampleID" = "PQC_Burnin3", "row" = 3, "column" = 1, "platePosition" =2, "sampleType" = "standard"))
    pqc4 <- fillRequest(r, request = c(runParam, "sampleID" = "PQC_Burnin4", "row" = 4, "column" = 1, "platePosition" =2, "sampleType" = "standard"))
    pqc5 <- fillRequest(r, request = c(runParam, "sampleID" = "PQC_Burnin5", "row" = 5, "column" = 1, "platePosition" =2, "sampleType" = "standard"))
    pqc6 <- fillRequest(r, request = c(runParam, "sampleID" = "PQC_Burnin6", "row" = 6, "column" = 1, "platePosition" =2, "sampleType" = "standard"))
    pqc7 <- fillRequest(r, request = c(runParam, "sampleID" = "PQC_Burnin7", "row" = 7, "column" = 1, "platePosition" =2, "sampleType" = "standard"))
    pqc8 <- fillRequest(r, request = c(runParam, "sampleID" = "PQC_Burnin8", "row" = 8, "column" = 1, "platePosition" =2, "sampleType" = "standard"))
    LTR <- fillRequest(r, request = c(runParam, "sampleID" = LTR_NAME, "row" = 8, "column" = 12))

    casib1 <- fillRequest(r, request = c(runParam, "sampleID" = "CASI_Burnin1", "row" = 5, "column" = 4, "platePosition" =2, "sampleType" = "standard"))
    casib2 <- fillRequest(r, request = c(runParam, "sampleID" = "CASI_Burnin2", "row" = 6, "column" = 4, "platePosition" =2, "sampleType" = "standard"))
    casib3 <- fillRequest(r, request = c(runParam, "sampleID" = "CASI_Burnin3", "row" = 7, "column" = 4, "platePosition" =2, "sampleType" = "standard"))
    casib4 <- fillRequest(r, request = c(runParam, "sampleID" = "CASI_Burnin4", "row" = 8, "column" = 4, "platePosition" =2, "sampleType" = "standard"))
    ### header
    rl <- addRequest(rl, list(pqc1,
                              pqc2,
                              pqc3,
                              pqc4,
                              pqc5,
                              LTR <- setPosition(LTR, "A11"),
                              LTR <- setPosition(LTR, "B11"),
                              pqc6,
                              pqc7,
                              pqc8))


    for (i in 1:length(rows)) {
      rlist <- fillRequest(r, request = c(runParam, "sampleID" = paste0(sampleID[i], "_", tubeLabel[i]), "row" = rows[i], "column" = columns[i]))

      if ((i - 1) %% 10 == 0) {
        pqc <- fillRequest(r, request = c(runParam, "sampleID" = "PQC", "row" = ((i - 1) %/% 10) + 1, "column" = 12, "platePosition" =1, "sampleType" = "pqc"))
        rl <- addRequest(rl, list(pqc, rlist))
      } else {
        rl <- addRequest(rl, list(rlist))
      }
    }
    ### footer
    rl <- addRequest(rl, list(pqc,
                              LTR <- setPosition(LTR, "C11"),
                              LTR <- setPosition(LTR, "D11")))

    run <- printRequest(rl, list("assay" = "MS_MRMSP"))
    saveRun(run, currentRunName)
    req <- c(req, list(requestList = rl, run = run))
  }
  return(req)
}

#' write run file for MS MRMSN
#' @param selectedSamples - the selected samples for run
#' @param runName - the name of the run
#' @param projectName - the name of the project
#' @param matrixID - the id of the sample matrix
#' @param deviceID - the id of the device
#' @param methodID - the id of the method
#' @param date - the date of the run
#' @return void
runMS_MRMSN <- function(selectedSamples, runName, projectName, matrixID, deviceID, methodID, LTR_NAME, date) {
  plateList <- levels(factor(selectedSamples$plateID))
  req <- list()
  for (plate in plateList) {

    currentRunName <- paste(runName,
                            plate,
                            date,
                            sep = "_")

    plateNames <- selectedSamples$plateID

    sampleID <- selectedSamples$sampleID[plateNames == plate]
    tubeLabel <- selectedSamples$tubeLabel[plateNames == plate]
    positions <- selectedSamples$wellPos[plateNames == plate]
    RC <- posToRC(positions)
    columns <- RC$col
    rows <- RC$row

    r <- new("request")
    runParam <- list("runName" = currentRunName,
                     "projectName" = projectName,
                     "methodID" = methodID,
                     "deviceID" = deviceID,
                     "matrixID" = matrixID,
                     "sampleType" = "Sample")

    rl <- new("requestList")

    pqc1 <- fillRequest(r, request = c(runParam, "sampleID" = "PQC_Burnin1", "row" = 1, "column" = 1, "platePosition" =2, "sampleType" = "standard"))
    pqc2 <- fillRequest(r, request = c(runParam, "sampleID" = "PQC_Burnin2", "row" = 2, "column" = 1, "platePosition" =2, "sampleType" = "standard"))
    pqc3 <- fillRequest(r, request = c(runParam, "sampleID" = "PQC_Burnin3", "row" = 3, "column" = 1, "platePosition" =2, "sampleType" = "standard"))
    pqc4 <- fillRequest(r, request = c(runParam, "sampleID" = "PQC_Burnin4", "row" = 4, "column" = 1, "platePosition" =2, "sampleType" = "standard"))
    pqc5 <- fillRequest(r, request = c(runParam, "sampleID" = "PQC_Burnin5", "row" = 5, "column" = 1, "platePosition" =2, "sampleType" = "standard"))
    pqc6 <- fillRequest(r, request = c(runParam, "sampleID" = "PQC_Burnin6", "row" = 6, "column" = 1, "platePosition" =2, "sampleType" = "standard"))
    pqc7 <- fillRequest(r, request = c(runParam, "sampleID" = "PQC_Burnin7", "row" = 7, "column" = 1, "platePosition" =2, "sampleType" = "standard"))
    pqc8 <- fillRequest(r, request = c(runParam, "sampleID" = "PQC_Burnin8", "row" = 8, "column" = 1, "platePosition" =2, "sampleType" = "standard"))
    LTR <- fillRequest(r, request = c(runParam, "sampleID" = LTR_NAME, "row" = 8, "column" = 12))

    casib1 <- fillRequest(r, request = c(runParam, "sampleID" = "CASI_Burnin1", "row" = 5, "column" = 4, "platePosition" =2, "sampleType" = "standard"))
    casib2 <- fillRequest(r, request = c(runParam, "sampleID" = "CASI_Burnin2", "row" = 6, "column" = 4, "platePosition" =2, "sampleType" = "standard"))
    casib3 <- fillRequest(r, request = c(runParam, "sampleID" = "CASI_Burnin3", "row" = 7, "column" = 4, "platePosition" =2, "sampleType" = "standard"))
    casib4 <- fillRequest(r, request = c(runParam, "sampleID" = "CASI_Burnin4", "row" = 8, "column" = 4, "platePosition" =2, "sampleType" = "standard"))
    ### header
    rl <- addRequest(rl, list(pqc1,
                              pqc2,
                              pqc3,
                              pqc4,
                              pqc5,
                              LTR <- setPosition(LTR, "A11"),
                              LTR <- setPosition(LTR, "B11"),
                              pqc6,
                              pqc7,
                              pqc8))


    for (i in 1:length(rows)) {
      rlist <- fillRequest(r, request = c(runParam, "sampleID" = paste0(sampleID[i], "_", tubeLabel[i]), "row" = rows[i], "column" = columns[i]))

      if ((i - 1) %% 10 == 0) {
        pqc <- fillRequest(r, request = c(runParam, "sampleID" = "PQC", "row" = ((i - 1) %/% 10) + 1, "column" = 12, "platePosition" =1, "sampleType" = "pqc"))
        rl <- addRequest(rl, list(pqc, rlist))
      } else {
        rl <- addRequest(rl, list(rlist))
      }
    }
    ### footer
    rl <- addRequest(rl, list(pqc,
                              LTR <- setPosition(LTR, "C11"),
                              LTR <- setPosition(LTR, "D11")))

    run <- printRequest(rl, list("assay" = "MS_MRMSN"))
    saveRun(run, currentRunName)
    req <- c(req, list(requestList = rl, run = run))
  }
  return(req)
}

#' write run file for TIMS EICOSANOIDS
#' @param selectedSamples - the selected samples for run
#' @param runName - the name of the run
#' @param projectName - the name of the project
#' @param matrixID - the id of the sample matrix
#' @param deviceID - the id of the device
#' @param methodID - the id of the method
#' @param date - the date of the run
#' @return void
runMS_EICOS <- function(selectedSamples, runName, projectName, matrixID, deviceID, methodID, LTR_NAME, date) {
  plateList <- levels(factor(selectedSamples$plateID))

  plateCounter <- 1
  req <- list()
  for (plate in plateList) {

    currentRunName <- paste(runName,
                            plate,
                            date,
                            sep = "_")

    plateNames <- selectedSamples$plateID

    sampleID <- selectedSamples$sampleID[plateNames == plate]
    tubeLabel <- selectedSamples$tubeLabel[plateNames == plate]
    positions <- selectedSamples$wellPos[plateNames == plate]
    RC <- posToRC(positions)
    columns <- RC$col
    rows <- RC$row

    r <- new("request")
    runParam <- list("runName" = runName,
                     "methodID" = methodID,
                     "deviceID" = deviceID,
                     "matrixID" = matrixID)

    rl <- new("requestList")

    conc <- list("a" = 1)
    conditioning1 <- fillRequest(r, request = c(runParam, "sampleID" = "CALIBRATION01", "row" = 1, "column" = 11, "sampleType" = "Standard"))
    conditioning2 <- fillRequest(r, request = c(runParam, "sampleID" = "CALIBRATION02", "row" = 2, "column" = 11, "sampleType" = "Standard"))
    conditioning3 <- fillRequest(r, request = c(runParam, "sampleID" = "CALIBRATION03", "row" = 3, "column" = 11, "sampleType" = "Standard"))
    conditioning4 <- fillRequest(r, request = c(runParam, "sampleID" = "CALIBRATION04", "row" = 4, "column" = 11, "sampleType" = "Standard"))
    conditioning5 <- fillRequest(r, request = c(runParam, "sampleID" = "CALIBRATION05", "row" = 5, "column" = 11, "sampleType" = "Standard"))
    conditioning6 <- fillRequest(r, request = c(runParam, "sampleID" = "CALIBRATION06", "row" = 6, "column" = 11, "sampleType" = "Standard"))
    conditioning7 <- fillRequest(r, request = c(runParam, "sampleID" = "CALIBRATION07", "row" = 7, "column" = 11, "sampleType" = "Standard"))
    conditioning8 <- fillRequest(r, request = c(runParam, "sampleID" = "CALIBRATION08", "row" = 8, "column" = 11, "sampleType" = "Standard"))
    QC1 <- fillRequest(r, request = c(runParam, "sampleID" = "QC01", "row" = 1, "column" = 12, "sampleType" = "Standard"))
    QC2 <- fillRequest(r, request = c(runParam, "sampleID" = "QC02", "row" = 2, "column" = 12, "sampleType" = "Standard"))
    QC3 <- fillRequest(r, request = c(runParam, "sampleID" = "QC03", "row" = 3, "column" = 12, "sampleType" = "Standard"))
    QC4 <- fillRequest(r, request = c(runParam, "sampleID" = "QC04", "row" = 4, "column" = 12, "sampleType" = "Standard"))
    doubleBlk <- fillRequest(r, request = c(runParam, "sampleID" = "DoubleBlank", "row" = 0, "column" = 11, "sampleType" = "Standard"))
    LTR1 <- fillRequest(r, request = c(runParam, "sampleID" = LTR_NAME, "row" = 5, "column" = 12))
    LTR2 <- fillRequest(r, request = c(runParam, "sampleID" = LTR_NAME, "row" = 6, "column" = 12))
    LTR3 <- fillRequest(r, request = c(runParam, "sampleID" = LTR_NAME, "row" = 7, "column" = 12))
    LTR4 <- fillRequest(r, request = c(runParam, "sampleID" = LTR_NAME, "row" = 8, "column" = 12))
    ### header
    rl <- addRequest(rl, list(doubleBlk,
                              conditioning1,
                              conditioning2,
                              conditioning3,
                              conditioning4,
                              conditioning5,
                              conditioning6,
                              conditioning7,
                              conditioning8,
                              doubleBlk,
                              QC1,
                              QC2,
                              QC3,
                              QC4,
                              doubleBlk))
    qcList <- list(QC1, QC2, QC3, QC4)
    ltrList <- list(LTR1, LTR2, LTR3, LTR4)
    counter <- 1
    for (i in 1:length(rows)) {
      rlist <- fillRequest(r, request = c(runParam, "sampleType" = "Unknown",
                                          "sampleID" = paste0(sampleID[i], "_", tubeLabel[i]),
                                          "row" = as.numeric(rows[i]),
                                          "column" = as.numeric(columns[i])))
      if (i %% floor(length(rows)/4) == 0) {
        j <- i %/% floor(length(rows)/4)
        rl <- addRequest(rl, list(rlist, qcList[[j]], ltrList[[j]]))
        # if (j %% 2 == 0) {
        #   LTR <- setPosition(LTR, "A11")
        # } else {
        #   LTR <- setPosition(LTR, "H11")
        # }
      } else {
        rl <- addRequest(rl, list(rlist))
      }
    }
    rl <- addRequest(rl, list(doubleBlk))
    run <- printRequest(rl, list("assay" = "MS_EICOS"))
    saveRun(run, currentRunName)
    plateCounter <- plateCounter + 1
    req <- c(req, list(requestList = rl, run = run))
  }
  return(req)
}

#' write run file for TIMS LIPIDS Positive
#' @param selectedSamples - the selected samples for run
#' @param runName - the name of the run
#' @param projectName - the name of the project
#' @param matrixID - the id of the sample matrix
#' @param deviceID - the id of the device
#' @param methodID - the id of the method
#' @param date - the date of the run
#' @return void
runTIMS_LIPIDS_P <- function(selectedSamples, runName, projectName, matrixID, deviceID, methodID, LTR_NAME, date) {
  plateList <- levels(factor(selectedSamples$plateID))
  req <- list()
  for (plate in plateList) {

    currentRunName <- paste(runName,
                            plate,
                            date,
                            sep = "_")

    plateNames <- selectedSamples$plateID

    sampleID <- selectedSamples$sampleID[plateNames == plate]
    tubeLabel <- selectedSamples$tubeLabel[plateNames == plate]
    positions <- selectedSamples$wellPos[plateNames == plate]
    RC <- posToRC(positions)
    columns <- RC$col
    rows <- RC$row

    r <- new("request")
    runParam <- list("runName" = currentRunName,
                     "projectName" = projectName,
                     "methodID" = methodID,
                     "deviceID" = deviceID,
                     "matrixID" = matrixID,
                     "sampleType" = "Sample")

    rl <- new("requestList")

    pqc1 <- fillRequest(r, request = c(runParam, "sampleID" = "PQC", "row" = 1, "column" = 12, "platePosition" = 1, "sampleType" = "standard"))
    LTR <- fillRequest(r, request = c(runParam, "sampleID" = LTR_NAME, "row" = 3, "column" = 1, "platePosition" = 0))
    sblk <- fillRequest(r, request = c(runParam, "sampleID" = "Blank", "row" = 2, "column" = 1, "platePosition" = 0, "sampleType" = "standard"))
    blk <- fillRequest(r, request = c(runParam, "sampleID" = "Double Blank", "row" = 1, "column" = 1, "platePosition" = 0, "sampleType" = "standard"))

    ### header
    rl <- addRequest(rl, list(LTR,
                              LTR,
                              LTR,
                              LTR,
                              pqc1<- setPosition(pqc1, "A11"),
                              pqc1 <- setPosition(pqc1, "B11"),
                              pqc1 <- setPosition(pqc1, "C11")))

    pqcList <- c(1:8)
    for (i in 1:length(rows)) {
      rlist <- fillRequest(r, request = c(runParam, "sampleID" = paste0(sampleID[i], "_", tubeLabel[i]), "row" = rows[i], "column" = columns[i]))

      if ((i - 1) %% 10 == 0) {
        pqc <- fillRequest(r, request = c(runParam, "sampleID" = "PQC", "row" = pqcList[((i - 1) %/% 10) + 1], "column" = 12, "platePosition" = 1, "sampleType" = "pqc"))
        rl <- addRequest(rl, list(pqc, rlist))
      } else {
        rl <- addRequest(rl, list(rlist))
      }
    }
    if (length(rows) < 80){
      rl <- addRequest(rl, list(pqc))
    }
    ### footer
    rl <- addRequest(rl, list(pqc1 <- setPosition(pqc1, "D11"),
                              pqc1 <- setPosition(pqc1, "E11"),
                              LTR,
                              LTR,
                              sblk, sblk,
                              blk, blk, blk))

    run <- printRequest(rl, list("assay" = "TIMS_LIPIDS_P"))
    saveRun(run, currentRunName)
    req <- c(req, list(requestList = rl, run = run))
  }
  return(req)
}

#' write run file for TIMS LIPIDS Negative
#' @param selectedSamples - the selected samples for run
#' @param runName - the name of the run
#' @param projectName - the name of the project
#' @param matrixID - the id of the sample matrix
#' @param deviceID - the id of the device
#' @param methodID - the id of the method
#' @param date - the date of the run
#' @return void
runTIMS_LIPIDS_N <- function(selectedSamples, runName, projectName, matrixID, deviceID, methodID, LTR_NAME, date) {
  plateList <- levels(factor(selectedSamples$plateID))
  req <- list()
  for (plate in plateList) {

    currentRunName <- paste(runName,
                            plate,
                            date,
                            sep = "_")

    plateNames <- selectedSamples$plateID

    sampleID <- selectedSamples$sampleID[plateNames == plate]
    tubeLabel <- selectedSamples$tubeLabel[plateNames == plate]
    positions <- selectedSamples$wellPos[plateNames == plate]
    RC <- posToRC(positions)
    columns <- RC$col
    rows <- RC$row

    r <- new("request")
    runParam <- list("runName" = currentRunName,
                     "projectName" = projectName,
                     "methodID" = methodID,
                     "deviceID" = deviceID,
                     "matrixID" = matrixID,
                     "sampleType" = "Sample")

    rl <- new("requestList")

    pqc1 <- fillRequest(r, request = c(runParam, "sampleID" = "PQC", "row" = 1, "column" = 12, "platePosition" = 1, "sampleType" = "standard"))
    LTR <- fillRequest(r, request = c(runParam, "sampleID" = LTR_NAME, "row" = 3, "column" = 1, "platePosition" = 0))
    sblk <- fillRequest(r, request = c(runParam, "sampleID" = "Blank", "row" = 2, "column" = 1, "platePosition" = 0, "sampleType" = "standard"))
    blk <- fillRequest(r, request = c(runParam, "sampleID" = "Double Blank", "row" = 1, "column" = 1, "platePosition" = 0, "sampleType" = "standard"))

    ### header
    rl <- addRequest(rl, list(LTR,
                              LTR,
                              LTR,
                              LTR,
                              pqc1<- setPosition(pqc1, "A11"),
                              pqc1 <- setPosition(pqc1, "B11"),
                              pqc1 <- setPosition(pqc1, "C11")))

    pqcList <- c(1:8)
    for (i in 1:length(rows)) {
      rlist <- fillRequest(r, request = c(runParam, "sampleID" = paste0(sampleID[i], "_", tubeLabel[i]), "row" = rows[i], "column" = columns[i]))

      if ((i - 1) %% 10 == 0) {
        pqc <- fillRequest(r, request = c(runParam, "sampleID" = "PQC", "row" = pqcList[((i - 1) %/% 10) + 1], "column" = 12, "platePosition" = 1, "sampleType" = "pqc"))
        rl <- addRequest(rl, list(pqc, rlist))
      } else {
        rl <- addRequest(rl, list(rlist))
      }
    }
    if (length(rows) < 80){
      rl <- addRequest(rl, list(pqc))
    }
    ### footer
    rl <- addRequest(rl, list(pqc1 <- setPosition(pqc1, "D11"),
                              pqc1 <- setPosition(pqc1, "E11"),
                              LTR,
                              LTR,
                              sblk, sblk,
                              blk, blk, blk))

    run <- printRequest(rl, list("assay" = "TIMS_LIPIDS_N"))
    saveRun(run, currentRunName)
    req <- c(req, list(requestList = rl, run = run))
  }
  return(req)
}

#' write run file for MS-URPP
#' @param selectedSamples - the selected samples for run
#' @param runName - the name of the run
#' @param projectName - the name of the project
#' @param matrixID - the id of the sample matrix
#' @param deviceID - the id of the device
#' @param methodID - the id of the method
#' @param date - the date of the run
#' @return void
runMS_URPP <- function(selectedSamples, runName, projectName, matrixID, deviceID, methodID, LTR_NAME, date) {

  plateList <- levels(factor(selectedSamples$plateID))

  plateCounter <- 1
  req <- list()
  for (plate in plateList) {

    currentRunName <- paste(runName,
                            plate,
                            date,
                            sep = "_")

    plateNames <- selectedSamples$plateID

    sampleID <- selectedSamples$sampleID[plateNames == plate]
    tubeLabel <- selectedSamples$tubeLabel[plateNames == plate]
    positions <- selectedSamples$wellPos[plateNames == plate]
    RC <- posToRC(positions)
    columns <- RC$col
    rows <- RC$row

    r <- new("request")
    runParam <- list("runName" = currentRunName,
                     "projectName" = projectName,
                     "methodID" = methodID,
                     "deviceID" = deviceID,
                     "matrixID" = matrixID,
                     "platePosition" = plateCounter)

    rl <- new("requestList")

    doubleBlk <- fillRequest(r, request = c(runParam, "sampleID" = "Double Blank", "row" = 7, "column" = 12, "sampleType" = "Blank"))
    singleBlk <- fillRequest(r, request = c(runParam, "sampleID" = "Single Blank", "row" = 6, "column" = 12, "sampleType" = "Blank"))
    QC <- fillRequest(r, request = c(runParam, "sampleID" = "QC 1", "row" = 5, "column" = 12, "sampleType" = "QC"))
    LTR <- fillRequest(r, request = c(runParam, "sampleID" = LTR_NAME, "row" = 0, "column" = 0))
    LTR_cond <- fillRequest(r, request = c(runParam, "sampleID" = "LTR cond", "row" = 8, "column" = 12, "sampleType" = "QC"))

    ### header
    rl <- addRequest(rl, list(doubleBlk,
                              LTR_cond,
                              LTR_cond,
                              LTR_cond,
                              LTR_cond,
                              LTR_cond,
                              LTR_cond,
                              LTR_cond,
                              LTR_cond,
                              LTR_cond,
                              LTR_cond,
                              LTR))
    LTR@column <- 3
    rl <- addRequest(rl, list(LTR))
    for (i in 1:length(rows)) {
      rlist <- fillRequest(r, request = c(runParam, "sampleID" = paste0(sampleID[i], "_", tubeLabel[i]), "row" = rows[i], "column" = columns[i]))

      if (i %% 8 == 0) {
        LTR@column <- i %/% 8
        rl <- addRequest(rl, list(rlist, LTR))
      } else {
        rl <- addRequest(rl, list(rlist))
      }
    }
    if (length(rows) < 80){
      rl <- addRequest(rl, list(LTR))
    }

    run <- printRequest(rl, list("assay" = "MS_URPP"))
    saveRun(run, currentRunName)
    plateCounter <- plateCounter + 1
    req <- c(req, list(requestList = rl, run = run))
  }
  return(req)
}

#' write run file for MS-LIPIDS
#' @param selectedSamples - the selected samples for run
#' @param runName - the name of the run
#' @param projectName - the name of the project
#' @param matrixID - the id of the sample matrix
#' @param deviceID - the id of the device
#' @param methodID - the id of the method
#' @param date - the date of the run
#' @return void
runMS_LIPIDS <- function(selectedSamples, runName, projectName, matrixID, deviceID, methodID, LTR_NAME, date) {

  plateList <- levels(factor(selectedSamples$plateID))

  plateCounter <- 1
  req <- list()
  for (plate in plateList) {

    currentRunName <- paste(runName,
                            plate,
                            date,
                            sep = "_")

    plateNames <- selectedSamples$plateID

    sampleID <- selectedSamples$sampleID[plateNames == plate]
    tubeLabel <- selectedSamples$tubeLabel[plateNames == plate]
    positions <- selectedSamples$wellPos[plateNames == plate]
    RC <- posToRC(positions)
    columns <- RC$col
    rows <- RC$row

    r <- new("request")
    runParam <- list("runName" = runName,
                     "methodID" = methodID,
                     "deviceID" = deviceID,
                     "matrixID" = matrixID)

    rl <- new("requestList")

    conc <- list("a" = 1)
    conditioning1 <- fillRequest(r, request = c(runParam, "sampleID" = "conditioning1", "row" = 1, "column" = 12, "sampleType" = "Standard"))
    conditioning2 <- fillRequest(r, request = c(runParam, "sampleID" = "conditioning2", "row" = 2, "column" = 12, "sampleType" = "Standard"))
    conditioning3 <- fillRequest(r, request = c(runParam, "sampleID" = "conditioning3", "row" = 3, "column" = 12, "sampleType" = "Standard"))
    conditioning4 <- fillRequest(r, request = c(runParam, "sampleID" = "conditioning4", "row" = 4, "column" = 12, "sampleType" = "Standard"))
    conditioning5 <- fillRequest(r, request = c(runParam, "sampleID" = "conditioning5", "row" = 5, "column" = 12, "sampleType" = "Standard"))
    conditioning6 <- fillRequest(r, request = c(runParam, "sampleID" = "conditioning6", "row" = 6, "column" = 12, "sampleType" = "Standard"))
    conditioning7 <- fillRequest(r, request = c(runParam, "sampleID" = "conditioning7", "row" = 7, "column" = 12, "sampleType" = "Standard"))
    conditioning8 <- fillRequest(r, request = c(runParam, "sampleID" = "conditioning8", "row" = 8, "column" = 12, "sampleType" = "Standard"))

    ### header
    rl <- addRequest(rl, list(conditioning1,
                              conditioning2,
                              conditioning3,
                              conditioning4,
                              conditioning5,
                              conditioning6,
                              conditioning7,
                              conditioning8))

    counter <- 1
    for (i in 1:length(rows)) {
      rlist <- fillRequest(r, request = c(runParam, "sampleType" = "Unknown" ,"sampleID" = paste0(sampleID[i], "_", tubeLabel[i]), "row" = as.numeric(rows[i]), "column" = as.numeric(columns[i])))

      if (i %% 10 == 0) {
        LTR <- fillRequest(r, request = c(runParam, "sampleID" = LTR_NAME, "row" = counter, "column" = 11, "sampleType" = "Unknown"))
        rl <- addRequest(rl, list(rlist, LTR))
        counter <- counter + 1
      } else {
        rl <- addRequest(rl, list(rlist))
      }
    }

    run <- printRequest(rl, list("assay" = "MSSciex"))
    saveRun(run, currentRunName)
    plateCounter <- plateCounter + 1
    req <- c(req, list(requestList = rl, run = run))
  }
  return(req)
}

#' write run file for NMR
#' @param selectedSamples - the selected samples for run
#' @param runName - the name of the run
#' @param projectName - the name of the project
#' @param matrixID - the id of the sample matrix
#' @param deviceID - the id of the device
#' @param methodID - the id of the method
#' @param date - the date of the run
#' @return a list
runNMR <- function(selectedSamples, runName, projectName, matrixID, deviceID, methodID, LTR_NAME, date) {
  LTR <- 4

  plateList <- levels(factor(selectedSamples$plateID))
  req <- list()
  for (plate in plateList) {
    currentRunName <- paste(runName,
                     plate,
                     date,
                     sep = "_")

    plateNames <- selectedSamples$plateID

    sampleID <- selectedSamples$sampleID[plateNames == plate]
    tubeLabel <- selectedSamples$tubeLabel[plateNames == plate]
    positions <- selectedSamples$wellPos[plateNames == plate]
    RC <- posToRC(positions)
    columns <- RC$col
    rows <- RC$row

    plateLength <- length(positions)

    plate1S <- data.frame("_sampleID" = paste0(sampleID, "_", tubeLabel),
                          "_matrixID" = rep(matrixID, plateLength),
                          "_runName" = rep(currentRunName, plateLength),
                          "_sampleType" = rep("S", plateLength),
                          "_methodID" = rep(methodID, plateLength),
                          "_deviceID" = rep(deviceID, plateLength),
                          "_projectName" = rep(projectName, plateLength),
                          "_platePosition" = rep(1, plateLength),
                          "row" = rows,
                          "column" = columns, check.names = FALSE)

    plate1LTR <- data.frame("_sampleID" = rep(LTR_NAME, LTR),
                            "_matrixID" = rep(matrixID, LTR),
                            "_runName" = rep(currentRunName, LTR),
                            "_sampleType" = rep("LTR", LTR),
                            "_methodID" = rep(methodID, LTR),
                            "_deviceID" = rep(deviceID, LTR),
                            "_projectName" = rep(projectName, LTR),
                            "_platePosition" = rep(1, LTR),
                            "row" = seq(2,8, by = 2)[1:LTR], #rep(c(1:8), each = 1),
                            "column" = rep(c(12), LTR), check.names = FALSE)

    plate1 <- rbind(plate1S, plate1LTR)
    # ordering plate by row
    F <- sort(plate1$col, index.return = TRUE)$ix
    plate1 <- plate1[F,]
    F <- sort(plate1$row, index.return = TRUE)$ix
    plate1 <- plate1[F,]
    saveRun(plate1, currentRunName)
    req <- c(req, list(requestList = NA, run = plate1))
  }
  return(req)
}

#' write run file
#' @param run - the selected samples for run
#' @param runName - the run name
#' @return write the file
#' @importFrom utils write.table
saveRun <- function(run, runName){
  file <- paste0("configurationFiles/",
                 runName,
                 ".tsv")
  write.table(run,
              file = file,
              sep = "\t",
              dec = ".",
              row.names = FALSE,
              quote = FALSE)
  cat(crayon::green("request >> getRUN >> ", file, " written\n"))
}
