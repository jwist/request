#' #' create configuration files
#' #' @param selectedSamples - the selected samples for run
#' #' @param type - the type of run
#' #' @param matrixName - the id of the samples matrix
#' #' @param deviceName - the id of the device to run the experiments
#' #' @param methodName - the id of the method to be run
#' #' @param cohortName - the name of the cohort to run
#' #' @param projectName - the name of the project
#' #' @param runID - the name of the run
#' #' @param options - options
#' #' @return the name of the device
#' #' @export
#' getRun <- function(selectedSamples,
#'                    type,
#'                    matrixName,
#'                    deviceName,
#'                    methodName,
#'                    cohortName,
#'                    projectName,
#'                    runID,
#'                    options = list()) {
#'   dups <- sum(duplicated(paste0(selectedSamples$plateID, selectedSamples$wellPos)))
#'   ifelse(dups != 0, stop("duplicated positions"), 1)
#'
#'   types <- c("NMR",
#'              "MS-TRY",
#'              "MS-TRY-EVOQ",
#'              "MS-BILE-EVOQ",
#'              "MS-Q300",
#'              "MS-BILE",
#'              "MS-AA",
#'              "MS-EICOS",
#'              "MS-URPP",
#'              "MS-LIPIDS",
#'              "MS-MRMSP",
#'              "MS-MRMSN",
#'              "TIMS-LIPIDS-P",
#'              "TIMS-LIPIDS-N",
#'              "CYT")
#'   choice <- which(type == types)
#'
#'   cat(crayon::blue("request >> getRUN >> running: ", types[choice], "\n"))
#'
#'   # qry <- getDevice(c(deviceName = deviceName,
#'   #                           methodName = methodName,
#'   #                           matrixName = matrixName))
#'   # deviceName <- qry[[1]]
#'   # methodName <- qry[[2]]
#'   # matrixName <- qry[[3]]
#'
#'
#'   runName <- paste(projectName,
#'                    cohortName,
#'                    matrixName,
#'                    methodName,
#'                    runID,
#'                    sep = "_")
#'
#'   if (type == "NMR" && matrixName == "SER") {
#'     LTR_NAME <- "LTR-PLA"
#'   } else {
#'     LTR_NAME <- paste0("LTR-", matrixName)
#'   }
#'
#'
#'   if ("date" %in% names(options)) {
#'     date <- options$date
#'   } else {
#'     date <- format(Sys.time(), "%d%m%y")
#'   }
#'
#'   switch(choice,
#'          runNMR(selectedSamples, runName, projectName, matrixName, deviceName, methodName, LTR_NAME, date),
#'          runMS_TRY(selectedSamples, runName, projectName, matrixName, deviceName, methodName, LTR_NAME, date),
#'          runMS_TRYE(selectedSamples, runName, projectName, matrixName, deviceName, methodName, LTR_NAME, date),
#'          runMS_BILEE(selectedSamples, runName, projectName, matrixName, deviceName, methodName, LTR_NAME, date),
#'          runMS_Q300(selectedSamples, runName, projectName, matrixName, deviceName, methodName, LTR_NAME, date),
#'          runMS_BILE(selectedSamples, runName, projectName, matrixName, deviceName, methodName, LTR_NAME, date),
#'          runMS_AA(selectedSamples, runName, projectName, matrixName, deviceName, methodName, LTR_NAME, date),
#'          runMS_EICOS(selectedSamples, runName, projectName, matrixName, deviceName, methodName, LTR_NAME, date),
#'          runMS_URPP(selectedSamples, runName, projectName, matrixName, deviceName, methodName, LTR_NAME, date),
#'          runMS_LIPIDS(selectedSamples, runName, projectName, matrixName, deviceName, methodName, LTR_NAME, date),
#'          runMS_MRMSP(selectedSamples, runName, projectName, matrixName, deviceName, methodName, LTR_NAME, date),
#'          runMS_MRMSN(selectedSamples, runName, projectName, matrixName, deviceName, methodName, LTR_NAME, date),
#'          runTIMS_LIPIDS_P(selectedSamples, runName, projectName, matrixName, deviceName, methodName, LTR_NAME, date),
#'          runTIMS_LIPIDS_N(selectedSamples, runName, projectName, matrixName, deviceName, methodName, LTR_NAME, date),
#'          runCYT(selectedSamples, runName, projectName, matrixName, deviceName, methodName, LTR_NAME, date))
#'   return(runName)
#' }
#'
#' #' write run file for MS-TRY
#' #' @param selectedSamples - the selected samples for run
#' #' @param runName - the name of the run
#' #' @param projectName - the name of the project
#' #' @param matrixName - the id of the sample matrix
#' #' @param deviceName - the id of the device
#' #' @param methodName - the id of the method
#' #' @param LTR_NAME - the name of LTR sample
#' #' @param date - the date of the run
#' #' @return void
#' runMS_TRY <- function(selectedSamples, runName, projectName, matrixName, deviceName, methodName, LTR_NAME, date) {
#'
#'   plateList <- levels(factor(selectedSamples$plateID))
#'
#'   plateCounter <- 0
#'   req <- list()
#'   for (plate in plateList) {
#'
#'     currentRunName <- paste(runName,
#'                      plate,
#'                      sep = "_")
#'
#'     plateNames <- selectedSamples$plateID
#'
#'     sampleID <- selectedSamples$sampleID[plateNames == plate]
#'     tubeLabel <- selectedSamples$tubeLabel[plateNames == plate]
#'     positions <- selectedSamples$wellPos[plateNames == plate]
#'     RC <- posToRC(positions)
#'     columns <- RC$col
#'     rows <- RC$row
#'
#'     r <- new("request")
#'     runParam <- list("runName" = currentRunName,
#'                      "projectName" = projectName,
#'                      "methodName" = methodName,
#'                      "deviceName" = deviceName,
#'                      "matrixName" = matrixName,
#'                      "platePosition" =  1 + plateCounter %% 2)
#'     rl <- new("requestList")
#'
#'     doubleBlk <- fillRequest(r, request = c(runParam, "sampleID" = "Double Blank", "row" = 0, "column" = 0, "sampleType" = "Blank"))
#'     singleBlk <- fillRequest(r, request = c(runParam, "sampleID" = "Single Blank", "row" = 8, "column" = 12, "sampleType" = "Blank"))
#'     mixTest <- fillRequest(r, request = c(runParam, "sampleID" = "mix test", "row" = -1, "column" = 12, "sampleType" = "Analyte"))
#'     cal1 <- fillRequest(r, request = c(runParam, "sampleID" = "CAL01", "row" = 8, "column" = 11, "sampleType" = "Standard", "options" = list(list("conc" = c(20,	40,	80,	100,	200,	400,	2000,	10000,	20000)))))
#'     cal2 <- fillRequest(r, request = c(runParam, "sampleID" = "CAL02", "row" = 7, "column" = 11, "sampleType" = "Standard", "options" = list(list("conc" = c(10,	20,	40,	50,	100,	200,	1000,	5000,	10000)))))
#'     cal3 <- fillRequest(r, request = c(runParam, "sampleID" = "CAL03", "row" = 6, "column" = 11, "sampleType" = "Standard", "options" = list(list("conc" = c(5,	10,	20,	25,	50,	100,	500,	2500, 5000)))))
#'     cal4 <- fillRequest(r, request = c(runParam, "sampleID" = "CAL04", "row" = 5, "column" = 11, "sampleType" = "Standard", "options" = list(list("conc" = c(3,	6,	12,	15,	30,	60,	300,	1500,	3000)))))
#'     cal5 <- fillRequest(r, request = c(runParam, "sampleID" = "CAL05", "row" = 4, "column" = 11, "sampleType" = "Standard", "options" = list(list("conc" = c(2,	4,	8,	10,	20,	40,	200,	1000,	2000)))))
#'     cal6 <- fillRequest(r, request = c(runParam, "sampleID" = "CAL06", "row" = 3, "column" = 11, "sampleType" = "Standard", "options" = list(list("conc" = c(0.8,	1.6,	3.2,	4,	8,	16,	80,	400,	800)))))
#'     cal7 <- fillRequest(r, request = c(runParam, "sampleID" = "CAL07", "row" = 2, "column" = 11, "sampleType" = "Standard", "options" = list(list("conc" = c(0.4,	0.8,	1.6,	2,	4,	8,	40,	200,	400)))))
#'     cal8 <- fillRequest(r, request = c(runParam, "sampleID" = "CAL08", "row" = 1, "column" = 11, "sampleType" = "Standard", "options" = list(list("conc" = c(0.2,	0.4,	0.8,	1,	2,	4,	20,	100,	200)))))
#'     QC4 <- fillRequest(r, request = c(runParam, "sampleID" = "QC04", "row" = 1, "column" = 12, "sampleType" = "QC", "options" = list(list("conc" = c(2, 4, 8, 10, 20, 40, 200, 1000, 2000)))))
#'     QC3 <- fillRequest(r, request = c(runParam, "sampleID" = "QC03", "row" = 2, "column" = 12, "sampleType" = "QC", "options" = list(list("conc" = c(8, 16, 32, 40, 80, 160, 800, 4000, 8000)))))
#'     QC2 <- fillRequest(r, request = c(runParam, "sampleID" = "QC02", "row" = 3, "column" = 12, "sampleType" = "QC", "options" = list(list("conc" = c(16, 32, 64, 80, 160, 320, 1600, 8000, 16000)))))
#'     QC1 <- fillRequest(r, request = c(runParam, "sampleID" = "QC01", "row" = 4, "column" = 12, "sampleType" = "QC", "options" = list(list("conc" = c(20, 40, 80, 100, 200, 400, 2000, 10000, 20000)))))
#'     LTR <- fillRequest(r, request = c(runParam, "sampleID" = LTR_NAME, "row" = 5, "column" = 12, "sampleType" = "Analyte"))
#'     LTR2 <- fillRequest(r, request = c(runParam, "sampleID" = LTR_NAME, "row" = 6, "column" = 12, "sampleType" = "Analyte"))
#'     LTR3 <- fillRequest(r, request = c(runParam, "sampleID" = LTR_NAME, "row" = 7, "column" = 12, "sampleType" = "Analyte"))
#'     LTR4 <- fillRequest(r, request = c(runParam, "sampleID" = LTR_NAME, "row" = 8, "column" = 12, "sampleType" = "Analyte"))
#'
#'     ### header
#'     rl <- addRequest(rl, list(doubleBlk, mixTest, doubleBlk))
#'     rl <- addRequest(rl, list(cal8, cal7, cal6, cal5, cal4, cal3, cal2, cal1))
#'     rl <- addRequest(rl, list(LTR, doubleBlk, LTR))
#'
#'     qcList <- list(QC4, LTR2, QC1, LTR2, QC3, LTR3, LTR3, QC2)
#'     chunkSize <- floor(length(rows)/8)
#'     for (i in 1:length(rows)) {
#'       rlist <- fillRequest(r, request = c(runParam,
#'                                           "sampleType" = "Analyte",
#'                                           "sampleID" = paste0(sampleID[i], "_", tubeLabel[i]),
#'                                           "row" = as.numeric(rows[i]),
#'                                           "column" = as.numeric(columns[i])))
#'       if (i %% chunkSize == 0) {
#'         rl <- addRequest(rl,
#'                          list(rlist,
#'                               qcList[[(((i - 1)/(chunkSize)) %% length(qcList)) + 1]]))
#'       } else {
#'         rl <- addRequest(rl, list(rlist))
#'       }
#'     }
#'
#'     ### footer
#'     rl <- addRequest(rl, list(doubleBlk, LTR4))
#'     rl <- addRequest(rl, list(doubleBlk, LTR4))
#'     rl <- addRequest(rl, list(cal8, cal7, cal6, cal5, cal4, cal3, cal2, cal1))
#'     rl <- addRequest(rl, list(doubleBlk, doubleBlk))
#'
#'     run <- printRequest(rl, list("assay" = "MSWaters"))
#'     saveRun(run, currentRunName, methodName)
#'     plateCounter <- plateCounter + 1
#'     req <- c(req, list(requestList = rl, run = run))
#'   }
#'   return(req)
#' }
#'
#' #' write run file for MS-TRY
#' #' @param selectedSamples - the selected samples for run
#' #' @param runName - the name of the run
#' #' @param projectName - the name of the project
#' #' @param matrixName - the id of the sample matrix
#' #' @param deviceName - the id of the device
#' #' @param methodName - the id of the method
#' #' @param LTR_NAME - the name of LTR sample
#' #' @param date - the date of the run
#' #' @return void
#' runMS_TRYE <- function(selectedSamples, runName, projectName, matrixName, deviceName, methodName, LTR_NAME, date) {
#'
#'   plateList <- levels(factor(selectedSamples$plateID))
#'
#'   plateCounter <- 0
#'   req <- list()
#'   for (plate in plateList) {
#'
#'     currentRunName <- paste(runName,
#'                             plate,
#'                             sep = "_")
#'
#'     plateNames <- selectedSamples$plateID
#'
#'     sampleID <- selectedSamples$sampleID[plateNames == plate]
#'     tubeLabel <- selectedSamples$tubeLabel[plateNames == plate]
#'     positions <- selectedSamples$wellPos[plateNames == plate]
#'     RC <- posToRC(positions)
#'     columns <- RC$col
#'     rows <- RC$row
#'
#'     r <- new("request")
#'     runParam <- list("runName" = currentRunName,
#'                      "projectName" = projectName,
#'                      "methodName" = methodName,
#'                      "deviceName" = deviceName,
#'                      "matrixName" = matrixName,
#'                      "platePosition" =  1 + plateCounter %% 2)
#'     rl <- new("requestList")
#'
#'     doubleBlk <- fillRequest(r, request = c(runParam, "sampleID" = "Double Blank", "row" = 1, "column" = 10, "sampleType" = "Blank"))
#'     mixTest <- fillRequest(r, request = c(runParam, "sampleID" = "mix test", "row" = 2, "column" = 10, "sampleType" = "Sample"))
#'     cal1 <- fillRequest(r, request = c(runParam, "sampleID" = "CAL01", "row" = 8, "column" = 11, "sampleType" = "Calibration sample", "options" = list(list("conc" = c(20,	40,	80,	100,	200,	400,	2000,	10000,	20000)))))
#'     cal2 <- fillRequest(r, request = c(runParam, "sampleID" = "CAL02", "row" = 7, "column" = 11, "sampleType" = "Calibration sample", "options" = list(list("conc" = c(10,	20,	40,	50,	100,	200,	1000,	5000,	10000)))))
#'     cal3 <- fillRequest(r, request = c(runParam, "sampleID" = "CAL03", "row" = 6, "column" = 11, "sampleType" = "Calibration sample", "options" = list(list("conc" = c(5,	10,	20,	25,	50,	100,	500,	2500, 5000)))))
#'     cal4 <- fillRequest(r, request = c(runParam, "sampleID" = "CAL04", "row" = 5, "column" = 11, "sampleType" = "Calibration sample", "options" = list(list("conc" = c(3,	6,	12,	15,	30,	60,	300,	1500,	3000)))))
#'     cal5 <- fillRequest(r, request = c(runParam, "sampleID" = "CAL05", "row" = 4, "column" = 11, "sampleType" = "Calibration sample", "options" = list(list("conc" = c(2,	4,	8,	10,	20,	40,	200,	1000,	2000)))))
#'     cal6 <- fillRequest(r, request = c(runParam, "sampleID" = "CAL06", "row" = 3, "column" = 11, "sampleType" = "Calibration sample", "options" = list(list("conc" = c(0.8,	1.6,	3.2,	4,	8,	16,	80,	400,	800)))))
#'     cal7 <- fillRequest(r, request = c(runParam, "sampleID" = "CAL07", "row" = 2, "column" = 11, "sampleType" = "Calibration sample", "options" = list(list("conc" = c(0.4,	0.8,	1.6,	2,	4,	8,	40,	200,	400)))))
#'     cal8 <- fillRequest(r, request = c(runParam, "sampleID" = "CAL08", "row" = 1, "column" = 11, "sampleType" = "Calibration sample", "options" = list(list("conc" = c(0.2,	0.4,	0.8,	1,	2,	4,	20,	100,	200)))))
#'     QC4 <- fillRequest(r, request = c(runParam, "sampleID" = "QC04", "row" = 1, "column" = 12, "sampleType" = "Qualitycontrol sample", "options" = list(list("conc" = c(2, 4, 8, 10, 20, 40, 200, 1000, 2000)))))
#'     QC3 <- fillRequest(r, request = c(runParam, "sampleID" = "QC03", "row" = 2, "column" = 12, "sampleType" = "Qualitycontrol sample", "options" = list(list("conc" = c(8, 16, 32, 40, 80, 160, 800, 4000, 8000)))))
#'     QC2 <- fillRequest(r, request = c(runParam, "sampleID" = "QC02", "row" = 3, "column" = 12, "sampleType" = "Qualitycontrol sample", "options" = list(list("conc" = c(16, 32, 64, 80, 160, 320, 1600, 8000, 16000)))))
#'     QC1 <- fillRequest(r, request = c(runParam, "sampleID" = "QC01", "row" = 4, "column" = 12, "sampleType" = "Qualitycontrol sample", "options" = list(list("conc" = c(20, 40, 80, 100, 200, 400, 2000, 10000, 20000)))))
#'     LTR <- fillRequest(r, request = c(runParam, "sampleID" = LTR_NAME, "row" = 5, "column" = 12, "sampleType" = "Sample"))
#'     LTR2 <- fillRequest(r, request = c(runParam, "sampleID" = LTR_NAME, "row" = 6, "column" = 12, "sampleType" = "Sample"))
#'     LTR3 <- fillRequest(r, request = c(runParam, "sampleID" = LTR_NAME, "row" = 7, "column" = 12, "sampleType" = "Sample"))
#'     LTR4 <- fillRequest(r, request = c(runParam, "sampleID" = LTR_NAME, "row" = 8, "column" = 12, "sampleType" = "Sample"))
#'
#'     ### header
#'     rl <- addRequest(rl, list(doubleBlk, mixTest, doubleBlk))
#'     rl <- addRequest(rl, list(cal8, cal7, cal6, cal5, cal4, cal3, cal2, cal1))
#'     rl <- addRequest(rl, list(LTR, doubleBlk, LTR))
#'
#'     qcList <- list(QC4, LTR2, QC1, LTR2, QC3, LTR3, LTR3, QC2)
#'     chunkSize <- floor(length(rows)/8)
#'     for (i in 1:length(rows)) {
#'       rlist <- fillRequest(r, request = c(runParam,
#'                                           "sampleType" = "Sample",
#'                                           "sampleID" = paste0(sampleID[i], "_", tubeLabel[i]),
#'                                           "row" = as.numeric(rows[i]),
#'                                           "column" = as.numeric(columns[i])))
#'       if (i %% chunkSize == 0) {
#'         rl <- addRequest(rl,
#'                          list(rlist,
#'                               qcList[[(((i - 1)/(chunkSize)) %% length(qcList)) + 1]]))
#'       } else {
#'         rl <- addRequest(rl, list(rlist))
#'       }
#'     }
#'
#'     ### footer
#'     rl <- addRequest(rl, list(doubleBlk, LTR4))
#'     rl <- addRequest(rl, list(doubleBlk, LTR4))
#'     rl <- addRequest(rl, list(cal8, cal7, cal6, cal5, cal4, cal3, cal2, cal1))
#'     rl <- addRequest(rl, list(doubleBlk, doubleBlk))
#'
#'     run <- printRequest(rl, list("assay" = "MS-TRYE"))
#'     saveRun(run, currentRunName, methodName)
#'     plateCounter <- plateCounter + 1
#'     req <- c(req, list(requestList = rl, run = run))
#'   }
#'   return(req)
#' }
#'
#' #' write run file for MS-BILE Evoq
#' #' @param selectedSamples - the selected samples for run
#' #' @param runName - the name of the run
#' #' @param projectName - the name of the project
#' #' @param matrixName - the id of the sample matrix
#' #' @param deviceName - the id of the device
#' #' @param methodName - the id of the method
#' #' @param LTR_NAME - the name of LTR sample
#' #' @param date - the date of the run
#' #' @return void
#' runMS_BILEE <- function(selectedSamples, runName, projectName, matrixName, deviceName, methodName, LTR_NAME, date) {
#'
#'   plateList <- levels(factor(selectedSamples$plateID))
#'   blkPos <- getPlatePos(by = "col")
#'   plateCounter <- 0
#'   req <- list()
#'   for (plate in plateList) {
#'
#'     currentRunName <- paste(runName,
#'                             plate,
#'                             sep = "_")
#'
#'     plateNames <- selectedSamples$plateID
#'
#'     sampleID <- selectedSamples$sampleID[plateNames == plate]
#'     tubeLabel <- selectedSamples$tubeLabel[plateNames == plate]
#'     positions <- selectedSamples$wellPos[plateNames == plate]
#'     RC <- posToRC(positions)
#'     columns <- RC$col
#'     rows <- RC$row
#'
#'     r <- new("request")
#'     runParam <- list("runName" = currentRunName,
#'                      "projectName" = projectName,
#'                      "methodName" = methodName,
#'                      "deviceName" = deviceName,
#'                      "matrixName" = matrixName)
#'                      #"platePosition" =  1 + plateCounter %% 2)
#'     rl <- new("requestList")
#'
#'     doubleBlk <- fillRequest(r, request = c(runParam, "sampleID" = "Double Blank", "row" = 0, "column" = 0, "sampleType" = "Blank", "platePosition" = 2))
#'     cal1 <- fillRequest(r, request = c(runParam, "sampleID" = "CAL01", "row" = 8, "column" = 11, "sampleType" = "Standard", "options" = list(list("conc" = c(20,	40,	80,	100,	200,	400,	2000,	10000,	20000)))))
#'     cal2 <- fillRequest(r, request = c(runParam, "sampleID" = "CAL02", "row" = 7, "column" = 11, "sampleType" = "Standard", "options" = list(list("conc" = c(10,	20,	40,	50,	100,	200,	1000,	5000,	10000)))))
#'     cal3 <- fillRequest(r, request = c(runParam, "sampleID" = "CAL03", "row" = 6, "column" = 11, "sampleType" = "Standard", "options" = list(list("conc" = c(5,	10,	20,	25,	50,	100,	500,	2500, 5000)))))
#'     cal4 <- fillRequest(r, request = c(runParam, "sampleID" = "CAL04", "row" = 5, "column" = 11, "sampleType" = "Standard", "options" = list(list("conc" = c(3,	6,	12,	15,	30,	60,	300,	1500,	3000)))))
#'     cal5 <- fillRequest(r, request = c(runParam, "sampleID" = "CAL05", "row" = 4, "column" = 11, "sampleType" = "Standard", "options" = list(list("conc" = c(2,	4,	8,	10,	20,	40,	200,	1000,	2000)))))
#'     cal6 <- fillRequest(r, request = c(runParam, "sampleID" = "CAL06", "row" = 3, "column" = 11, "sampleType" = "Standard", "options" = list(list("conc" = c(0.8,	1.6,	3.2,	4,	8,	16,	80,	400,	800)))))
#'     cal7 <- fillRequest(r, request = c(runParam, "sampleID" = "CAL07", "row" = 2, "column" = 11, "sampleType" = "Standard", "options" = list(list("conc" = c(0.4,	0.8,	1.6,	2,	4,	8,	40,	200,	400)))))
#'     cal8 <- fillRequest(r, request = c(runParam, "sampleID" = "CAL08", "row" = 1, "column" = 11, "sampleType" = "Standard", "options" = list(list("conc" = c(0.2,	0.4,	0.8,	1,	2,	4,	20,	100,	200)))))
#'     QC4 <- fillRequest(r, request = c(runParam, "sampleID" = "QC04", "row" = 1, "column" = 12, "sampleType" = "QC", "options" = list(list("conc" = c(2, 4, 8, 10, 20, 40, 200, 1000, 2000)))))
#'     QC3 <- fillRequest(r, request = c(runParam, "sampleID" = "QC03", "row" = 2, "column" = 12, "sampleType" = "QC", "options" = list(list("conc" = c(8, 16, 32, 40, 80, 160, 800, 4000, 8000)))))
#'     QC2 <- fillRequest(r, request = c(runParam, "sampleID" = "QC02", "row" = 3, "column" = 12, "sampleType" = "QC", "options" = list(list("conc" = c(16, 32, 64, 80, 160, 320, 1600, 8000, 16000)))))
#'     QC1 <- fillRequest(r, request = c(runParam, "sampleID" = "QC01", "row" = 4, "column" = 12, "sampleType" = "QC", "options" = list(list("conc" = c(20, 40, 80, 100, 200, 400, 2000, 10000, 20000)))))
#'     LTR <- fillRequest(r, request = c(runParam, "sampleID" = LTR_NAME, "row" = 5, "column" = 12, "sampleType" = "Analyte"))
#'     LTR2 <- fillRequest(r, request = c(runParam, "sampleID" = LTR_NAME, "row" = 6, "column" = 12, "sampleType" = "Analyte"))
#'     LTR3 <- fillRequest(r, request = c(runParam, "sampleID" = LTR_NAME, "row" = 7, "column" = 12, "sampleType" = "Analyte"))
#'     LTR4 <- fillRequest(r, request = c(runParam, "sampleID" = LTR_NAME, "row" = 8, "column" = 12, "sampleType" = "Analyte"))
#'
#'     ### header
#'
#'     rl <- addRequest(rl, list(doubleBlk <- setPosition(doubleBlk, blkPos[1 + (plateCounter * 7)])))
#'     rl <- addRequest(rl, list(cal8, cal7, cal6, cal5, cal4, cal3, cal2, cal1))
#'     rl <- addRequest(rl, list(doubleBlk <- setPosition(doubleBlk, blkPos[2 + (plateCounter * 7)]),
#'                               QC4, QC3, QC2, QC1,
#'                               doubleBlk <- setPosition(doubleBlk, blkPos[3 + (plateCounter * 7)])))
#'
#'     qcList <- list(QC4, LTR2, QC1, LTR2, QC3, LTR3, LTR3, QC2)
#'     chunkSize <- floor(length(rows)/8)
#'     for (i in 1:length(rows)) {
#'       rlist <- fillRequest(r, request = c(runParam,
#'                                           "sampleType" = "Sample",
#'                                           "sampleID" = paste0(sampleID[i], "_", tubeLabel[i]),
#'                                           "row" = as.numeric(rows[i]),
#'                                           "column" = as.numeric(columns[i])))
#'       if (i %% chunkSize == 0) {
#'         rl <- addRequest(rl,
#'                          list(rlist,
#'                               qcList[[(((i - 1)/(chunkSize)) %% length(qcList)) + 1]]))
#'       } else {
#'         rl <- addRequest(rl, list(rlist))
#'       }
#'     }
#'
#'     ### footer
#'     rl <- addRequest(rl, list(doubleBlk <- setPosition(doubleBlk, blkPos[4 + (plateCounter * 7)]), LTR4))
#'     rl <- addRequest(rl, list(doubleBlk <- setPosition(doubleBlk, blkPos[5 + (plateCounter * 7)]), LTR4))
#'     rl <- addRequest(rl, list(cal8, cal7, cal6, cal5, cal4, cal3, cal2, cal1))
#'     rl <- addRequest(rl, list(doubleBlk <- setPosition(doubleBlk, blkPos[6 + (plateCounter * 7)]),
#'                               doubleBlk <- setPosition(doubleBlk, blkPos[7 + (plateCounter * 7)])))
#'
#'     run <- printRequest(rl, list("assay" = "MS-BILEE"))
#'     saveRun(run, currentRunName, methodName)
#'     plateCounter <- plateCounter + 1
#'     req <- c(req, list(requestList = rl, run = run))
#'   }
#'   return(req)
#' }
#'
#' #' write run file for MS-TRY
#' #' @param selectedSamples - the selected samples for run
#' #' @param runName - the name of the run
#' #' @param projectName - the name of the project
#' #' @param matrixName - the id of the sample matrix
#' #' @param deviceName - the id of the device
#' #' @param methodName - the id of the method
#' #' @param LTR_NAME - the name of LTR sample
#' #' @param date - the date of the run
#' #' @return void
#' runMS_Q300 <- function(selectedSamples, runName, projectName, matrixName, deviceName, methodName, LTR_NAME, date) {
#'
#'   plateList <- levels(factor(selectedSamples$plateID))
#'
#'   plateCounter <- 0
#'   req <- list()
#'   for (plate in plateList) {
#'
#'     currentRunName <- paste(runName,
#'                             plate,
#'                             sep = "_")
#'
#'     plateNames <- selectedSamples$plateID
#'
#'     sampleID <- selectedSamples$sampleID[plateNames == plate]
#'     tubeLabel <- selectedSamples$tubeLabel[plateNames == plate]
#'     positions <- selectedSamples$wellPos[plateNames == plate]
#'     RC <- posToRC(positions)
#'     columns <- RC$col
#'     rows <- RC$row
#'
#'     r <- new("request")
#'     runParam <- list("runName" = currentRunName,
#'                      "projectName" = projectName,
#'                      "methodName" = methodName,
#'                      "deviceName" = deviceName,
#'                      "matrixName" = matrixName,
#'                      "platePosition" =  1 + plateCounter %% 2)
#'     rl <- new("requestList")
#'
#'     doubleBlk <- fillRequest(r, request = c(runParam, "sampleID" = "Double Blank", "row" = 0, "column" = 0, "sampleType" = "Blank"))
#'     singleBlk <- fillRequest(r, request = c(runParam, "sampleID" = "Single Blank", "row" = 8, "column" = 12, "sampleType" = "Blank"))
#'     mixTest <- fillRequest(r, request = c(runParam, "sampleID" = "mix test", "row" = -1, "column" = 12, "sampleType" = "Analyte"))
#'     cal1 <- fillRequest(r, request = c(runParam, "sampleID" = "CAL01", "row" = 8, "column" = 11, "sampleType" = "Standard", "options" = list(list("conc" = c(20,	40,	80,	100,	200,	400,	2000,	10000,	20000)))))
#'     cal2 <- fillRequest(r, request = c(runParam, "sampleID" = "CAL02", "row" = 7, "column" = 11, "sampleType" = "Standard", "options" = list(list("conc" = c(10,	20,	40,	50,	100,	200,	1000,	5000,	10000)))))
#'     cal3 <- fillRequest(r, request = c(runParam, "sampleID" = "CAL03", "row" = 6, "column" = 11, "sampleType" = "Standard", "options" = list(list("conc" = c(5,	10,	20,	25,	50,	100,	500,	2500, 5000)))))
#'     cal4 <- fillRequest(r, request = c(runParam, "sampleID" = "CAL04", "row" = 5, "column" = 11, "sampleType" = "Standard", "options" = list(list("conc" = c(3,	6,	12,	15,	30,	60,	300,	1500,	3000)))))
#'     cal5 <- fillRequest(r, request = c(runParam, "sampleID" = "CAL05", "row" = 4, "column" = 11, "sampleType" = "Standard", "options" = list(list("conc" = c(2,	4,	8,	10,	20,	40,	200,	1000,	2000)))))
#'     cal6 <- fillRequest(r, request = c(runParam, "sampleID" = "CAL06", "row" = 3, "column" = 11, "sampleType" = "Standard", "options" = list(list("conc" = c(0.8,	1.6,	3.2,	4,	8,	16,	80,	400,	800)))))
#'     cal7 <- fillRequest(r, request = c(runParam, "sampleID" = "CAL07", "row" = 2, "column" = 11, "sampleType" = "Standard", "options" = list(list("conc" = c(0.4,	0.8,	1.6,	2,	4,	8,	40,	200,	400)))))
#'     cal8 <- fillRequest(r, request = c(runParam, "sampleID" = "CAL08", "row" = 1, "column" = 11, "sampleType" = "Standard", "options" = list(list("conc" = c(0.2,	0.4,	0.8,	1,	2,	4,	20,	100,	200)))))
#'     LTR <- fillRequest(r, request = c(runParam, "sampleID" = LTR_NAME, "row" = 1, "column" = 12, "sampleType" = "Analyte"))
#'     LTR2 <- fillRequest(r, request = c(runParam, "sampleID" = LTR_NAME, "row" = 2, "column" = 12, "sampleType" = "Analyte"))
#'     LTR3 <- fillRequest(r, request = c(runParam, "sampleID" = LTR_NAME, "row" = 3, "column" = 12, "sampleType" = "Analyte"))
#'     LTR4 <- fillRequest(r, request = c(runParam, "sampleID" = LTR_NAME, "row" = 4, "column" = 12, "sampleType" = "Analyte"))
#'     LTR5 <- fillRequest(r, request = c(runParam, "sampleID" = LTR_NAME, "row" = 5, "column" = 12, "sampleType" = "Analyte"))
#'     LTR6 <- fillRequest(r, request = c(runParam, "sampleID" = LTR_NAME, "row" = 6, "column" = 12, "sampleType" = "Analyte"))
#'     LTR7 <- fillRequest(r, request = c(runParam, "sampleID" = LTR_NAME, "row" = 7, "column" = 12, "sampleType" = "Analyte"))
#'
#'     ### header
#'
#'     rl <- addRequest(rl, list(doubleBlk, cal8, cal7, cal6, cal5, cal4, cal3, cal2, cal1))
#'     rl <- addRequest(rl, list(LTR, doubleBlk, LTR))
#'
#'     qcList <- list(LTR2, LTR3, LTR4, LTR5, LTR6)
#'     chunkSize <- floor(length(rows)/8)
#'     for (i in 1:length(rows)) {
#'       rlist <- fillRequest(r, request = c(runParam,
#'                                           "sampleType" = "Analyte",
#'                                           "sampleID" = paste0(sampleID[i], "_", tubeLabel[i]),
#'                                           "row" = as.numeric(rows[i]),
#'                                           "column" = as.numeric(columns[i])))
#'       if (i %% chunkSize == 0) {
#'         rl <- addRequest(rl,
#'                          list(rlist,
#'                               qcList[[(((i - 1)/(chunkSize)) %% length(qcList)) + 1]]))
#'       } else {
#'         rl <- addRequest(rl, list(rlist))
#'       }
#'     }
#'
#'     ### footer
#'     rl <- addRequest(rl, list(singleBlk, LTR7))
#'     rl <- addRequest(rl, list(doubleBlk, LTR7))
#'     rl <- addRequest(rl, list(cal8, cal7, cal6, cal5, cal4, cal3, cal2, cal1))
#'     rl <- addRequest(rl, list(doubleBlk, doubleBlk))
#'
#'     run <- printRequest(rl, list("assay" = "MS-Q300"))
#'     saveRun(run, currentRunName, methodName)
#'     plateCounter <- plateCounter + 1
#'     req <- c(req, list(requestList = rl, run = run))
#'   }
#'   return(req)
#' }
#'
#'
#' #' write run file for MS-BILE
#' #' @param selectedSamples - the selected samples for run
#' #' @param runName - the name of the run
#' #' @param projectName - the name of the project
#' #' @param matrixName - the id of the sample matrix
#' #' @param deviceName - the id of the device
#' #' @param methodName - the id of the method
#' #' @param LTR_NAME - the name of LTR sample
#' #' @param date - the date of the run
#' #' @return void
#' runMS_BILE <- function(selectedSamples, runName, projectName, matrixName, deviceName, methodName, LTR_NAME, date) {
#'
#'   plateList <- levels(factor(selectedSamples$plateID))
#'
#'   plateCounter <- 0
#'   req <- list()
#'   for (plate in plateList) {
#'
#'     currentRunName <- paste(runName,
#'                             plate,
#'                             sep = "_")
#'
#'     plateNames <- selectedSamples$plateID
#'
#'     sampleID <- selectedSamples$sampleID[plateNames == plate]
#'     tubeLabel <- selectedSamples$tubeLabel[plateNames == plate]
#'     positions <- selectedSamples$wellPos[plateNames == plate]
#'     RC <- posToRC(positions)
#'     columns <- RC$col
#'     rows <- RC$row
#'
#'     r <- new("request")
#'     runParam <- list("runName" = currentRunName,
#'                      "projectName" = projectName,
#'                      "methodName" = methodName,
#'                      "deviceName" = deviceName,
#'                      "matrixName" = matrixName,
#'                      "platePosition" = 1 + (plateCounter %% 2))
#'     rl <- new("requestList")
#'
#'     doubleBlk <- fillRequest(r, request = c(runParam, "sampleID" = "Double Blank", "row" = 0, "column" = 0, "sampleType" = "Blank"))
#'     cal1 <- fillRequest(r, request = c(runParam, "sampleID" = "CAL01", "row" = 8, "column" = 11, "sampleType" = "Standard", "options" = list(list("conc" = c(20,	40,	80,	100,	200,	400,	2000,	10000,	20000)))))
#'     cal2 <- fillRequest(r, request = c(runParam, "sampleID" = "CAL02", "row" = 7, "column" = 11, "sampleType" = "Standard", "options" = list(list("conc" = c(10,	20,	40,	50,	100,	200,	1000,	5000,	10000)))))
#'     cal3 <- fillRequest(r, request = c(runParam, "sampleID" = "CAL03", "row" = 6, "column" = 11, "sampleType" = "Standard", "options" = list(list("conc" = c(5,	10,	20,	25,	50,	100,	500,	2500, 5000)))))
#'     cal4 <- fillRequest(r, request = c(runParam, "sampleID" = "CAL04", "row" = 5, "column" = 11, "sampleType" = "Standard", "options" = list(list("conc" = c(3,	6,	12,	15,	30,	60,	300,	1500,	3000)))))
#'     cal5 <- fillRequest(r, request = c(runParam, "sampleID" = "CAL05", "row" = 4, "column" = 11, "sampleType" = "Standard", "options" = list(list("conc" = c(2,	4,	8,	10,	20,	40,	200,	1000,	2000)))))
#'     cal6 <- fillRequest(r, request = c(runParam, "sampleID" = "CAL06", "row" = 3, "column" = 11, "sampleType" = "Standard", "options" = list(list("conc" = c(0.8,	1.6,	3.2,	4,	8,	16,	80,	400,	800)))))
#'     cal7 <- fillRequest(r, request = c(runParam, "sampleID" = "CAL07", "row" = 2, "column" = 11, "sampleType" = "Standard", "options" = list(list("conc" = c(0.4,	0.8,	1.6,	2,	4,	8,	40,	200,	400)))))
#'     cal8 <- fillRequest(r, request = c(runParam, "sampleID" = "CAL08", "row" = 1, "column" = 11, "sampleType" = "Standard", "options" = list(list("conc" = c(0.2,	0.4,	0.8,	1,	2,	4,	20,	100,	200)))))
#'     QC4 <- fillRequest(r, request = c(runParam, "sampleID" = "QC04", "row" = 1, "column" = 12, "sampleType" = "QC", "options" = list(list("conc" = c(2, 4, 8, 10, 20, 40, 200, 1000, 2000)))))
#'     QC3 <- fillRequest(r, request = c(runParam, "sampleID" = "QC03", "row" = 2, "column" = 12, "sampleType" = "QC", "options" = list(list("conc" = c(8, 16, 32, 40, 80, 160, 800, 4000, 8000)))))
#'     QC2 <- fillRequest(r, request = c(runParam, "sampleID" = "QC02", "row" = 3, "column" = 12, "sampleType" = "QC", "options" = list(list("conc" = c(16, 32, 64, 80, 160, 320, 1600, 8000, 16000)))))
#'     QC1 <- fillRequest(r, request = c(runParam, "sampleID" = "QC01", "row" = 4, "column" = 12, "sampleType" = "QC", "options" = list(list("conc" = c(20, 40, 80, 100, 200, 400, 2000, 10000, 20000)))))
#'     LTR <- fillRequest(r, request = c(runParam, "sampleID" = LTR_NAME, "row" = 5, "column" = 12, "sampleType" = "Analyte"))
#'     LTR2 <- fillRequest(r, request = c(runParam, "sampleID" = LTR_NAME, "row" = 6, "column" = 12, "sampleType" = "Analyte"))
#'     LTR3 <- fillRequest(r, request = c(runParam, "sampleID" = LTR_NAME, "row" = 7, "column" = 12, "sampleType" = "Analyte"))
#'     LTR4 <- fillRequest(r, request = c(runParam, "sampleID" = LTR_NAME, "row" = 8, "column" = 12, "sampleType" = "Analyte"))
#'
#'     ### header
#'     rl <- addRequest(rl, list(doubleBlk))
#'     rl <- addRequest(rl, list(cal8, cal7, cal6, cal5, cal4, cal3, cal2, cal1))
#'     rl <- addRequest(rl, list(doubleBlk, QC4, QC3, QC2, QC1, doubleBlk))
#'
#'     qcList <- list(QC4, LTR2, QC1, LTR2, QC3, LTR3, LTR3, QC2)
#'     chunkSize <- floor(length(rows)/8)
#'     for (i in 1:length(rows)) {
#'       rlist <- fillRequest(r, request = c(runParam,
#'                                           "sampleType" = "Analyte",
#'                                           "sampleID" = paste0(sampleID[i], "_", tubeLabel[i]),
#'                                           "row" = as.numeric(rows[i]),
#'                                           "column" = as.numeric(columns[i])))
#'       if (i %% chunkSize == 0) {
#'         rl <- addRequest(rl,
#'                          list(rlist,
#'                               qcList[[(((i - 1)/(chunkSize)) %% length(qcList)) + 1]]))
#'       } else {
#'         rl <- addRequest(rl, list(rlist))
#'       }
#'     }
#'
#'     ### footer
#'     rl <- addRequest(rl, list(doubleBlk))
#'     rl <- addRequest(rl, list(cal8, cal7, cal6, cal5, cal4, cal3, cal2, cal1))
#'     rl <- addRequest(rl, list(doubleBlk))
#'
#'
#'     run <- printRequest(rl, list("assay" = "MS-BILE"))
#'     saveRun(run, currentRunName, methodName)
#'     plateCounter <- plateCounter + 1
#'     req <- c(req, list(requestList = rl, run = run))
#'   }
#'   return(req)
#' }
#'
#' #' write run file for MS AA
#' #' @param selectedSamples - the selected samples for run
#' #' @param runName - the name of the run
#' #' @param projectName - the name of the project
#' #' @param matrixName - the id of the sample matrix
#' #' @param deviceName - the id of the device
#' #' @param methodName - the id of the method
#' #' @param LTR_NAME - the name of LTR sample
#' #' @param date - the date of the run
#' #' @return void
#' runMS_AA <- function(selectedSamples, runName, projectName, matrixName, deviceName, methodName, LTR_NAME, date) {
#'
#'   plateList <- levels(factor(selectedSamples$plateID))
#'
#'   plateCounter <- 0
#'   req <- list()
#'   for (plate in plateList) {
#'
#'     currentRunName <- paste(runName,
#'                      plate,
#'                      sep = "_")
#'
#'     plateNames <- selectedSamples$plateID
#'
#'     sampleID <- selectedSamples$sampleID[plateNames == plate]
#'     tubeLabel <- selectedSamples$tubeLabel[plateNames == plate]
#'     positions <- selectedSamples$wellPos[plateNames == plate]
#'     RC <- posToRC(positions)
#'     columns <- RC$col
#'     rows <- RC$row
#'
#'     r <- new("request")
#'     runParam <- list("runName" = currentRunName,
#'                      "projectName" = projectName,
#'                      "methodName" = methodName,
#'                      "deviceName" = deviceName,
#'                      "matrixName" = matrixName,
#'                      "platePosition" = 1 + (plateCounter %% 2))
#'
#'     rl <- new("requestList")
#'
#'     doubleBlk <- fillRequest(r, request = c(runParam, "sampleID" = "Double Blank", "row" = 0, "column" = 12, "sampleType" = "Blank"))
#'     singleBlk <- fillRequest(r, request = c(runParam, "sampleID" = "Single Blank", "row" = 0, "column" = 12, "sampleType" = "Blank"))
#'     cal9 <- fillRequest(r, request = c(runParam, "sampleID" = "CAL09", "row" = 1, "column" = 12, "sampleType" = "Calibrant"))
#'     cal8 <- fillRequest(r, request = c(runParam, "sampleID" = "CAL08", "row" = 1, "column" = 11, "sampleType" = "Calibrant"))
#'     cal7 <- fillRequest(r, request = c(runParam, "sampleID" = "CAL07", "row" = 2, "column" = 11, "sampleType" = "Calibrant"))
#'     cal6 <- fillRequest(r, request = c(runParam, "sampleID" = "CAL06", "row" = 3, "column" = 11, "sampleType" = "Calibrant"))
#'     cal5 <- fillRequest(r, request = c(runParam, "sampleID" = "CAL05", "row" = 4, "column" = 11, "sampleType" = "Calibrant"))
#'     cal4 <- fillRequest(r, request = c(runParam, "sampleID" = "CAL04", "row" = 5, "column" = 11, "sampleType" = "Calibrant"))
#'     cal3 <- fillRequest(r, request = c(runParam, "sampleID" = "CAL03", "row" = 6, "column" = 11, "sampleType" = "Calibrant"))
#'     cal2 <- fillRequest(r, request = c(runParam, "sampleID" = "CAL02", "row" = 7, "column" = 11, "sampleType" = "Calibrant"))
#'     cal1 <- fillRequest(r, request = c(runParam, "sampleID" = "CAL01", "row" = 8, "column" = 11, "sampleType" = "Calibrant"))
#'     QC4 <- fillRequest(r, request = c(runParam, "sampleID" = "QC04", "row" = 1, "column" = 12, "sampleType" = "QC"))
#'     QC3 <- fillRequest(r, request = c(runParam, "sampleID" = "QC03", "row" = 2, "column" = 12, "sampleType" = "QC"))
#'     QC2 <- fillRequest(r, request = c(runParam, "sampleID" = "QC02", "row" = 3, "column" = 12, "sampleType" = "QC"))
#'     QC1 <- fillRequest(r, request = c(runParam, "sampleID" = "QC01", "row" = 4, "column" = 12, "sampleType" = "QC"))
#'     LTR <- fillRequest(r, request = c(runParam, "sampleID" = LTR_NAME, "row" = 5, "column" = 12, "sampleType" = "Sample"))
#'     LTR2 <- fillRequest(r, request = c(runParam, "sampleID" = LTR_NAME, "row" = 6, "column" = 12, "sampleType" = "Sample"))
#'     LTR3 <- fillRequest(r, request = c(runParam, "sampleID" = LTR_NAME, "row" = 7, "column" = 12, "sampleType" = "Sample"))
#'     LTR4 <- fillRequest(r, request = c(runParam, "sampleID" = LTR_NAME, "row" = 8, "column" = 12, "sampleType" = "Sample"))
#'
#'     ### header
#'     rl <- addRequest(rl, list(doubleBlk, QC1, doubleBlk))
#'     rl <- addRequest(rl, list(cal8, cal7, cal6, cal5, cal4, cal3, cal2, cal1))
#'     rl <- addRequest(rl, list(doubleBlk))
#'     rl <- addRequest(rl, list(QC4, QC3, QC2, QC1))
#'     rl <- addRequest(rl, list(LTR, doubleBlk, LTR))
#'
#'     qcList <- list(QC4, LTR2, QC1, LTR2, QC3, LTR3, LTR3, QC2)
#'     chunkSize <- floor(length(rows)/8)
#'     for (i in 1:length(rows)) {
#'       rlist <- fillRequest(r, request = c(runParam,
#'                                           "sampleType" = "Sample",
#'                                           "sampleID" = paste0(sampleID[i], "_", tubeLabel[i]),
#'                                           "row" = as.numeric(rows[i]),
#'                                           "column" = as.numeric(columns[i])))
#'       if (i %% chunkSize == 0) {
#'         rl <- addRequest(rl,
#'                          list(rlist,
#'                               qcList[[(((i - 1)/(chunkSize)) %% length(qcList)) + 1]]))
#'       } else {
#'         rl <- addRequest(rl, list(rlist))
#'       }
#'     }
#'     ### footer
#'     rl <- addRequest(rl, list(doubleBlk, LTR4))
#'     rl <- addRequest(rl, list(doubleBlk, LTR4))
#'     rl <- addRequest(rl, list(cal8, cal7, cal6, cal5, cal4, cal3, cal2, cal1))
#'     rl <- addRequest(rl, list(doubleBlk, doubleBlk))
#'
#'     run <- printRequest(rl, list("assay" = "MSBruker"))
#'     saveRun(run, currentRunName, methodName)
#'     plateCounter <- plateCounter + 1
#'     req <- c(req, list(requestList = rl, run = run))
#'   }
#'   return(req)
#' }
#'
#' #' write run file for MS MRMSP
#' #' @param selectedSamples - the selected samples for run
#' #' @param runName - the name of the run
#' #' @param projectName - the name of the project
#' #' @param matrixName - the id of the sample matrix
#' #' @param deviceName - the id of the device
#' #' @param methodName - the id of the method
#' #' @param LTR_NAME - the name of LTR sample
#' #' @param date - the date of the run
#' #' @return void
#' runMS_MRMSP <- function(selectedSamples, runName, projectName, matrixName, deviceName, methodName, LTR_NAME, date) {
#'   plateList <- levels(factor(selectedSamples$plateID))
#'   req <- list()
#'   for (plate in plateList) {
#'
#'     currentRunName <- paste(runName,
#'                             plate,
#'                             sep = "_")
#'
#'     plateNames <- selectedSamples$plateID
#'
#'     sampleID <- selectedSamples$sampleID[plateNames == plate]
#'     tubeLabel <- selectedSamples$tubeLabel[plateNames == plate]
#'     positions <- selectedSamples$wellPos[plateNames == plate]
#'     RC <- posToRC(positions)
#'     columns <- RC$col
#'     rows <- RC$row
#'
#'     r <- new("request")
#'     runParam <- list("runName" = currentRunName,
#'                      "projectName" = projectName,
#'                      "methodName" = methodName,
#'                      "deviceName" = deviceName,
#'                      "matrixName" = matrixName,
#'                      "sampleType" = "Sample")
#'
#'     rl <- new("requestList")
#'
#'     pqc1 <- fillRequest(r, request = c(runParam, "sampleID" = "PQC_Burnin1", "row" = 1, "column" = 1, "platePosition" =2, "sampleType" = "standard"))
#'     pqc2 <- fillRequest(r, request = c(runParam, "sampleID" = "PQC_Burnin2", "row" = 2, "column" = 1, "platePosition" =2, "sampleType" = "standard"))
#'     pqc3 <- fillRequest(r, request = c(runParam, "sampleID" = "PQC_Burnin3", "row" = 3, "column" = 1, "platePosition" =2, "sampleType" = "standard"))
#'     pqc4 <- fillRequest(r, request = c(runParam, "sampleID" = "PQC_Burnin4", "row" = 4, "column" = 1, "platePosition" =2, "sampleType" = "standard"))
#'     pqc5 <- fillRequest(r, request = c(runParam, "sampleID" = "PQC_Burnin5", "row" = 5, "column" = 1, "platePosition" =2, "sampleType" = "standard"))
#'     pqc6 <- fillRequest(r, request = c(runParam, "sampleID" = "PQC_Burnin6", "row" = 6, "column" = 1, "platePosition" =2, "sampleType" = "standard"))
#'     pqc7 <- fillRequest(r, request = c(runParam, "sampleID" = "PQC_Burnin7", "row" = 7, "column" = 1, "platePosition" =2, "sampleType" = "standard"))
#'     pqc8 <- fillRequest(r, request = c(runParam, "sampleID" = "PQC_Burnin8", "row" = 8, "column" = 1, "platePosition" =2, "sampleType" = "standard"))
#'     LTR <- fillRequest(r, request = c(runParam, "sampleID" = LTR_NAME, "row" = 8, "column" = 12))
#'
#'     casib1 <- fillRequest(r, request = c(runParam, "sampleID" = "CASI_Burnin1", "row" = 5, "column" = 4, "platePosition" =2, "sampleType" = "standard"))
#'     casib2 <- fillRequest(r, request = c(runParam, "sampleID" = "CASI_Burnin2", "row" = 6, "column" = 4, "platePosition" =2, "sampleType" = "standard"))
#'     casib3 <- fillRequest(r, request = c(runParam, "sampleID" = "CASI_Burnin3", "row" = 7, "column" = 4, "platePosition" =2, "sampleType" = "standard"))
#'     casib4 <- fillRequest(r, request = c(runParam, "sampleID" = "CASI_Burnin4", "row" = 8, "column" = 4, "platePosition" =2, "sampleType" = "standard"))
#'     ### header
#'     rl <- addRequest(rl, list(pqc1,
#'                               pqc2,
#'                               pqc3,
#'                               pqc4,
#'                               pqc5,
#'                               LTR <- setPosition(LTR, "A11"),
#'                               LTR <- setPosition(LTR, "B11"),
#'                               pqc6,
#'                               pqc7,
#'                               pqc8))
#'
#'
#'     for (i in 1:length(rows)) {
#'       rlist <- fillRequest(r, request = c(runParam, "sampleID" = paste0(sampleID[i], "_", tubeLabel[i]), "row" = rows[i], "column" = columns[i]))
#'
#'       if ((i - 1) %% 10 == 0) {
#'         pqc <- fillRequest(r, request = c(runParam, "sampleID" = "PQC", "row" = ((i - 1) %/% 10) + 1, "column" = 12, "platePosition" =1, "sampleType" = "pqc"))
#'         rl <- addRequest(rl, list(pqc, rlist))
#'       } else {
#'         rl <- addRequest(rl, list(rlist))
#'       }
#'     }
#'     ### footer
#'     rl <- addRequest(rl, list(pqc,
#'                               LTR <- setPosition(LTR, "C11"),
#'                               LTR <- setPosition(LTR, "D11")))
#'
#'     run <- printRequest(rl, list("assay" = "MS_MRMSP"))
#'     saveRun(run, currentRunName, methodName)
#'     req <- c(req, list(requestList = rl, run = run))
#'   }
#'   return(req)
#' }
#'
#' #' write run file for MS MRMSN
#' #' @param selectedSamples - the selected samples for run
#' #' @param runName - the name of the run
#' #' @param projectName - the name of the project
#' #' @param matrixName - the id of the sample matrix
#' #' @param deviceName - the id of the device
#' #' @param methodName - the id of the method
#' #' @param LTR_NAME - the name of LTR sample
#' #' @param date - the date of the run
#' #' @return void
#' runMS_MRMSN <- function(selectedSamples, runName, projectName, matrixName, deviceName, methodName, LTR_NAME, date) {
#'   plateList <- levels(factor(selectedSamples$plateID))
#'   req <- list()
#'   for (plate in plateList) {
#'
#'     currentRunName <- paste(runName,
#'                             plate,
#'                             sep = "_")
#'
#'     plateNames <- selectedSamples$plateID
#'
#'     sampleID <- selectedSamples$sampleID[plateNames == plate]
#'     tubeLabel <- selectedSamples$tubeLabel[plateNames == plate]
#'     positions <- selectedSamples$wellPos[plateNames == plate]
#'     RC <- posToRC(positions)
#'     columns <- RC$col
#'     rows <- RC$row
#'
#'     r <- new("request")
#'     runParam <- list("runName" = currentRunName,
#'                      "projectName" = projectName,
#'                      "methodName" = methodName,
#'                      "deviceName" = deviceName,
#'                      "matrixName" = matrixName,
#'                      "sampleType" = "Sample")
#'
#'     rl <- new("requestList")
#'
#'     pqc1 <- fillRequest(r, request = c(runParam, "sampleID" = "PQC_Burnin1", "row" = 1, "column" = 1, "platePosition" =2, "sampleType" = "standard"))
#'     pqc2 <- fillRequest(r, request = c(runParam, "sampleID" = "PQC_Burnin2", "row" = 2, "column" = 1, "platePosition" =2, "sampleType" = "standard"))
#'     pqc3 <- fillRequest(r, request = c(runParam, "sampleID" = "PQC_Burnin3", "row" = 3, "column" = 1, "platePosition" =2, "sampleType" = "standard"))
#'     pqc4 <- fillRequest(r, request = c(runParam, "sampleID" = "PQC_Burnin4", "row" = 4, "column" = 1, "platePosition" =2, "sampleType" = "standard"))
#'     pqc5 <- fillRequest(r, request = c(runParam, "sampleID" = "PQC_Burnin5", "row" = 5, "column" = 1, "platePosition" =2, "sampleType" = "standard"))
#'     pqc6 <- fillRequest(r, request = c(runParam, "sampleID" = "PQC_Burnin6", "row" = 6, "column" = 1, "platePosition" =2, "sampleType" = "standard"))
#'     pqc7 <- fillRequest(r, request = c(runParam, "sampleID" = "PQC_Burnin7", "row" = 7, "column" = 1, "platePosition" =2, "sampleType" = "standard"))
#'     pqc8 <- fillRequest(r, request = c(runParam, "sampleID" = "PQC_Burnin8", "row" = 8, "column" = 1, "platePosition" =2, "sampleType" = "standard"))
#'     LTR <- fillRequest(r, request = c(runParam, "sampleID" = LTR_NAME, "row" = 8, "column" = 12))
#'
#'     casib1 <- fillRequest(r, request = c(runParam, "sampleID" = "CASI_Burnin1", "row" = 5, "column" = 4, "platePosition" =2, "sampleType" = "standard"))
#'     casib2 <- fillRequest(r, request = c(runParam, "sampleID" = "CASI_Burnin2", "row" = 6, "column" = 4, "platePosition" =2, "sampleType" = "standard"))
#'     casib3 <- fillRequest(r, request = c(runParam, "sampleID" = "CASI_Burnin3", "row" = 7, "column" = 4, "platePosition" =2, "sampleType" = "standard"))
#'     casib4 <- fillRequest(r, request = c(runParam, "sampleID" = "CASI_Burnin4", "row" = 8, "column" = 4, "platePosition" =2, "sampleType" = "standard"))
#'     ### header
#'     rl <- addRequest(rl, list(pqc1,
#'                               pqc2,
#'                               pqc3,
#'                               pqc4,
#'                               pqc5,
#'                               LTR <- setPosition(LTR, "A11"),
#'                               LTR <- setPosition(LTR, "B11"),
#'                               pqc6,
#'                               pqc7,
#'                               pqc8))
#'
#'
#'     for (i in 1:length(rows)) {
#'       rlist <- fillRequest(r, request = c(runParam, "sampleID" = paste0(sampleID[i], "_", tubeLabel[i]), "row" = rows[i], "column" = columns[i]))
#'
#'       if ((i - 1) %% 10 == 0) {
#'         pqc <- fillRequest(r, request = c(runParam, "sampleID" = "PQC", "row" = ((i - 1) %/% 10) + 1, "column" = 12, "platePosition" =1, "sampleType" = "pqc"))
#'         rl <- addRequest(rl, list(pqc, rlist))
#'       } else {
#'         rl <- addRequest(rl, list(rlist))
#'       }
#'     }
#'     ### footer
#'     rl <- addRequest(rl, list(pqc,
#'                               LTR <- setPosition(LTR, "C11"),
#'                               LTR <- setPosition(LTR, "D11")))
#'
#'     run <- printRequest(rl, list("assay" = "MS_MRMSN"))
#'     saveRun(run, currentRunName, methodName)
#'     req <- c(req, list(requestList = rl, run = run))
#'   }
#'   return(req)
#' }
#'
#' #' write run file for TIMS EICOSANOIDS
#' #' @param selectedSamples - the selected samples for run
#' #' @param runName - the name of the run
#' #' @param projectName - the name of the project
#' #' @param matrixName - the id of the sample matrix
#' #' @param deviceName - the id of the device
#' #' @param methodName - the id of the method
#' #' @param LTR_NAME - the name of LTR sample
#' #' @param date - the date of the run
#' #' @return void
#' runMS_EICOS <- function(selectedSamples, runName, projectName, matrixName, deviceName, methodName, LTR_NAME, date) {
#'   plateList <- levels(factor(selectedSamples$plateID))
#'
#'   plateCounter <- 0
#'   req <- list()
#'   for (plate in plateList) {
#'
#'     currentRunName <- paste(runName,
#'                             plate,
#'                             sep = "_")
#'
#'     plateNames <- selectedSamples$plateID
#'
#'     sampleID <- selectedSamples$sampleID[plateNames == plate]
#'     tubeLabel <- selectedSamples$tubeLabel[plateNames == plate]
#'     positions <- selectedSamples$wellPos[plateNames == plate]
#'     RC <- posToRC(positions)
#'     columns <- RC$col
#'     rows <- RC$row
#'
#'     r <- new("request")
#'     runParam <- list("runName" = runName,
#'                      "methodName" = methodName,
#'                      "deviceName" = deviceName,
#'                      "matrixName" = matrixName,
#'                      "platePosition" = 1 + (plateCounter %% 2))
#'
#'     rl <- new("requestList")
#'
#'     conc <- list("a" = 1)
#'     CAL1 <- fillRequest(r, request = c(runParam, "sampleID" = "CAL01", "row" = 1, "column" = 11, "sampleType" = "Standard"))
#'     CAL2 <- fillRequest(r, request = c(runParam, "sampleID" = "CAL02", "row" = 2, "column" = 11, "sampleType" = "Standard"))
#'     CAL3 <- fillRequest(r, request = c(runParam, "sampleID" = "CAL03", "row" = 3, "column" = 11, "sampleType" = "Standard"))
#'     CAL4 <- fillRequest(r, request = c(runParam, "sampleID" = "CAL04", "row" = 4, "column" = 11, "sampleType" = "Standard"))
#'     CAL5 <- fillRequest(r, request = c(runParam, "sampleID" = "CAL05", "row" = 5, "column" = 11, "sampleType" = "Standard"))
#'     CAL6 <- fillRequest(r, request = c(runParam, "sampleID" = "CAL06", "row" = 6, "column" = 11, "sampleType" = "Standard"))
#'     CAL7 <- fillRequest(r, request = c(runParam, "sampleID" = "CAL07", "row" = 7, "column" = 11, "sampleType" = "Standard"))
#'     CAL8 <- fillRequest(r, request = c(runParam, "sampleID" = "CAL08", "row" = 8, "column" = 11, "sampleType" = "Standard"))
#'     QC1 <- fillRequest(r, request = c(runParam, "sampleID" = "QC01", "row" = 1, "column" = 12, "sampleType" = "Standard"))
#'     QC2 <- fillRequest(r, request = c(runParam, "sampleID" = "QC02", "row" = 2, "column" = 12, "sampleType" = "Standard"))
#'     QC3 <- fillRequest(r, request = c(runParam, "sampleID" = "QC03", "row" = 3, "column" = 12, "sampleType" = "Standard"))
#'     QC4 <- fillRequest(r, request = c(runParam, "sampleID" = "QC04", "row" = 4, "column" = 12, "sampleType" = "Standard"))
#'     doubleBlk <- fillRequest(r, request = c(runParam, "sampleID" = "DoubleBlank", "row" = 0, "column" = 11, "sampleType" = "Standard"))
#'     LTR1 <- fillRequest(r, request = c(runParam, "sampleID" = LTR_NAME, "row" = 5, "column" = 12))
#'     LTR2 <- fillRequest(r, request = c(runParam, "sampleID" = LTR_NAME, "row" = 6, "column" = 12))
#'     LTR3 <- fillRequest(r, request = c(runParam, "sampleID" = LTR_NAME, "row" = 7, "column" = 12))
#'     LTR4 <- fillRequest(r, request = c(runParam, "sampleID" = LTR_NAME, "row" = 8, "column" = 12))
#'     ### header
#'     rl <- addRequest(rl, list(doubleBlk,
#'                               CAL8,
#'                               CAL7,
#'                               CAL6,
#'                               CAL5,
#'                               CAL4,
#'                               CAL3,
#'                               CAL2,
#'                               CAL1,
#'                               doubleBlk,
#'                               LTR1,
#'                               QC4,
#'                               QC3,
#'                               QC2,
#'                               QC1,
#'                               doubleBlk,
#'                               LTR1))
#'
#'     qcList <- list(QC4, LTR2, QC1, LTR2, QC3, LTR3, LTR3, QC2)
#'     chunkSize <- floor(length(rows)/8)
#'     for (i in 1:length(rows)) {
#'       rlist <- fillRequest(r, request = c(runParam,
#'                                           "sampleType" = "Sample",
#'                                           "sampleID" = paste0(sampleID[i], "_", tubeLabel[i]),
#'                                           "row" = as.numeric(rows[i]),
#'                                           "column" = as.numeric(columns[i])))
#'       if (i %% chunkSize == 0) {
#'         rl <- addRequest(rl,
#'                          list(rlist,
#'                               qcList[[(((i - 1)/(chunkSize)) %% length(qcList)) + 1]]))
#'       } else {
#'         rl <- addRequest(rl, list(rlist))
#'       }
#'     }
#'     # footer
#'     rl <- addRequest(rl, list(LTR4, LTR4, doubleBlk))
#'     run <- printRequest(rl, list("assay" = "MS_EICOS"))
#'     saveRun(run, currentRunName, methodName)
#'     plateCounter <- plateCounter + 1
#'     req <- c(req, list(requestList = rl, run = run))
#'   }
#'   return(req)
#' }
#'
#' #' write run file for TIMS LIPIDS Positive
#' #' @param selectedSamples - the selected samples for run
#' #' @param runName - the name of the run
#' #' @param projectName - the name of the project
#' #' @param matrixName - the id of the sample matrix
#' #' @param deviceName - the id of the device
#' #' @param methodName - the id of the method
#' #' @param LTR_NAME - the name of LTR sample
#' #' @param date - the date of the run
#' #' @return void
#' runTIMS_LIPIDS_P <- function(selectedSamples, runName, projectName, matrixName, deviceName, methodName, LTR_NAME, date) {
#'   plateList <- levels(factor(selectedSamples$plateID))
#'   req <- list()
#'   for (plate in plateList) {
#'
#'     currentRunName <- paste(runName,
#'                             plate,
#'                             sep = "_")
#'
#'     plateNames <- selectedSamples$plateID
#'
#'     sampleID <- selectedSamples$sampleID[plateNames == plate]
#'     tubeLabel <- selectedSamples$tubeLabel[plateNames == plate]
#'     positions <- selectedSamples$wellPos[plateNames == plate]
#'     RC <- posToRC(positions)
#'     columns <- RC$col
#'     rows <- RC$row
#'
#'     r <- new("request")
#'     runParam <- list("runName" = currentRunName,
#'                      "projectName" = projectName,
#'                      "methodName" = methodName,
#'                      "deviceName" = deviceName,
#'                      "matrixName" = matrixName,
#'                      "sampleType" = "Sample")
#'
#'     rl <- new("requestList")
#'
#'     pqc1 <- fillRequest(r, request = c(runParam, "sampleID" = "PQC", "row" = 1, "column" = 12, "platePosition" = 1, "sampleType" = "standard"))
#'     LTR <- fillRequest(r, request = c(runParam, "sampleID" = LTR_NAME, "row" = 3, "column" = 1, "platePosition" = 0))
#'     sblk <- fillRequest(r, request = c(runParam, "sampleID" = "Blank", "row" = 2, "column" = 1, "platePosition" = 0, "sampleType" = "standard"))
#'     blk <- fillRequest(r, request = c(runParam, "sampleID" = "Double Blank", "row" = 1, "column" = 1, "platePosition" = 0, "sampleType" = "standard"))
#'
#'     ### header
#'     rl <- addRequest(rl, list(LTR,
#'                               LTR,
#'                               LTR,
#'                               LTR,
#'                               pqc1<- setPosition(pqc1, "A11"),
#'                               pqc1 <- setPosition(pqc1, "B11"),
#'                               pqc1 <- setPosition(pqc1, "C11")))
#'
#'     pqcList <- c(1:8)
#'     for (i in 1:length(rows)) {
#'       rlist <- fillRequest(r, request = c(runParam, "sampleID" = paste0(sampleID[i], "_", tubeLabel[i]), "row" = rows[i], "column" = columns[i]))
#'
#'       if ((i - 1) %% 10 == 0) {
#'         pqc <- fillRequest(r, request = c(runParam, "sampleID" = "PQC", "row" = pqcList[((i - 1) %/% 10) + 1], "column" = 12, "platePosition" = 1, "sampleType" = "pqc"))
#'         rl <- addRequest(rl, list(pqc, rlist))
#'       } else {
#'         rl <- addRequest(rl, list(rlist))
#'       }
#'     }
#'     if (length(rows) < 80){
#'       rl <- addRequest(rl, list(pqc))
#'     }
#'     ### footer
#'     rl <- addRequest(rl, list(pqc1 <- setPosition(pqc1, "D11"),
#'                               pqc1 <- setPosition(pqc1, "E11"),
#'                               LTR,
#'                               LTR,
#'                               sblk, sblk,
#'                               blk, blk, blk))
#'
#'     run <- printRequest(rl, list("assay" = "TIMS_LIPIDS_P"))
#'     saveRun(run, currentRunName, methodName)
#'     req <- c(req, list(requestList = rl, run = run))
#'   }
#'   return(req)
#' }
#'
#' #' write run file for TIMS LIPIDS Negative
#' #' @param selectedSamples - the selected samples for run
#' #' @param runName - the name of the run
#' #' @param projectName - the name of the project
#' #' @param matrixName - the id of the sample matrix
#' #' @param deviceName - the id of the device
#' #' @param methodName - the id of the method
#' #' @param LTR_NAME - the name of LTR sample
#' #' @param date - the date of the run
#' #' @return void
#' runTIMS_LIPIDS_N <- function(selectedSamples, runName, projectName, matrixName, deviceName, methodName, LTR_NAME, date) {
#'   plateList <- levels(factor(selectedSamples$plateID))
#'   req <- list()
#'   for (plate in plateList) {
#'
#'     currentRunName <- paste(runName,
#'                             plate,
#'                             sep = "_")
#'
#'     plateNames <- selectedSamples$plateID
#'
#'     sampleID <- selectedSamples$sampleID[plateNames == plate]
#'     tubeLabel <- selectedSamples$tubeLabel[plateNames == plate]
#'     positions <- selectedSamples$wellPos[plateNames == plate]
#'     RC <- posToRC(positions)
#'     columns <- RC$col
#'     rows <- RC$row
#'
#'     r <- new("request")
#'     runParam <- list("runName" = currentRunName,
#'                      "projectName" = projectName,
#'                      "methodName" = methodName,
#'                      "deviceName" = deviceName,
#'                      "matrixName" = matrixName,
#'                      "sampleType" = "Sample")
#'
#'     rl <- new("requestList")
#'
#'     pqc1 <- fillRequest(r, request = c(runParam, "sampleID" = "PQC", "row" = 1, "column" = 12, "platePosition" = 1, "sampleType" = "standard"))
#'     LTR <- fillRequest(r, request = c(runParam, "sampleID" = LTR_NAME, "row" = 3, "column" = 1, "platePosition" = 0))
#'     sblk <- fillRequest(r, request = c(runParam, "sampleID" = "Blank", "row" = 2, "column" = 1, "platePosition" = 0, "sampleType" = "standard"))
#'     blk <- fillRequest(r, request = c(runParam, "sampleID" = "Double Blank", "row" = 1, "column" = 1, "platePosition" = 0, "sampleType" = "standard"))
#'
#'     ### header
#'     rl <- addRequest(rl, list(LTR,
#'                               LTR,
#'                               LTR,
#'                               LTR,
#'                               pqc1<- setPosition(pqc1, "A11"),
#'                               pqc1 <- setPosition(pqc1, "B11"),
#'                               pqc1 <- setPosition(pqc1, "C11")))
#'
#'     pqcList <- c(1:8)
#'     for (i in 1:length(rows)) {
#'       rlist <- fillRequest(r, request = c(runParam, "sampleID" = paste0(sampleID[i], "_", tubeLabel[i]), "row" = rows[i], "column" = columns[i]))
#'
#'       if ((i - 1) %% 10 == 0) {
#'         pqc <- fillRequest(r, request = c(runParam, "sampleID" = "PQC", "row" = pqcList[((i - 1) %/% 10) + 1], "column" = 12, "platePosition" = 1, "sampleType" = "pqc"))
#'         rl <- addRequest(rl, list(pqc, rlist))
#'       } else {
#'         rl <- addRequest(rl, list(rlist))
#'       }
#'     }
#'     if (length(rows) < 80){
#'       rl <- addRequest(rl, list(pqc))
#'     }
#'     ### footer
#'     rl <- addRequest(rl, list(pqc1 <- setPosition(pqc1, "D11"),
#'                               pqc1 <- setPosition(pqc1, "E11"),
#'                               LTR,
#'                               LTR,
#'                               sblk, sblk,
#'                               blk, blk, blk))
#'
#'     run <- printRequest(rl, list("assay" = "TIMS_LIPIDS_N"))
#'     saveRun(run, currentRunName, methodName)
#'     req <- c(req, list(requestList = rl, run = run))
#'   }
#'   return(req)
#' }
#'
#' #' write run file for MS-URPP
#' #' @param selectedSamples - the selected samples for run
#' #' @param runName - the name of the run
#' #' @param projectName - the name of the project
#' #' @param matrixName - the id of the sample matrix
#' #' @param deviceName - the id of the device
#' #' @param methodName - the id of the method
#' #' @param LTR_NAME - the name of LTR sample
#' #' @param date - the date of the run
#' #' @return void
#' runMS_URPP <- function(selectedSamples, runName, projectName, matrixName, deviceName, methodName, LTR_NAME, date) {
#'
#'   plateList <- levels(factor(selectedSamples$plateID))
#'
#'   plateCounter <- 1
#'   req <- list()
#'   for (plate in plateList) {
#'
#'     currentRunName <- paste(runName,
#'                             plate,
#'                             sep = "_")
#'
#'     plateNames <- selectedSamples$plateID
#'
#'     sampleID <- selectedSamples$sampleID[plateNames == plate]
#'     tubeLabel <- selectedSamples$tubeLabel[plateNames == plate]
#'     positions <- selectedSamples$wellPos[plateNames == plate]
#'     RC <- posToRC(positions)
#'     columns <- RC$col
#'     rows <- RC$row
#'
#'     r <- new("request")
#'     runParam <- list("runName" = currentRunName,
#'                      "projectName" = projectName,
#'                      "methodName" = methodName,
#'                      "deviceName" = deviceName,
#'                      "matrixName" = matrixName,
#'                      "platePosition" = plateCounter,
#'                      "sampleType" = "Sample")
#'
#'     rl <- new("requestList")
#'
#'     doubleBlk <- fillRequest(r, request = c(runParam, "sampleID" = "Double Blank", "row" = 7, "column" = 12, "sampleType" = "Blank"))
#'     singleBlk <- fillRequest(r, request = c(runParam, "sampleID" = "Single Blank", "row" = 6, "column" = 12, "sampleType" = "Blank"))
#'     QC <- fillRequest(r, request = c(runParam, "sampleID" = "QC 1", "row" = 5, "column" = 12, "sampleType" = "QC"))
#'     LTR <- fillRequest(r, request = c(runParam, "sampleID" = LTR_NAME, "row" = 0, "column" = 0))
#'     LTR_cond <- fillRequest(r, request = c(runParam, "sampleID" = "LTR cond", "row" = 8, "column" = 12, "sampleType" = "QC"))
#'
#'     ### header
#'     rl <- addRequest(rl, list(doubleBlk,
#'                               LTR_cond,
#'                               LTR_cond,
#'                               LTR_cond,
#'                               LTR_cond,
#'                               LTR_cond,
#'                               LTR_cond,
#'                               LTR_cond,
#'                               LTR_cond,
#'                               LTR_cond,
#'                               LTR_cond,
#'                               LTR))
#'     LTR@column <- 3
#'     rl <- addRequest(rl, list(LTR))
#'     for (i in 1:length(rows)) {
#'       rlist <- fillRequest(r, request = c(runParam, "sampleID" = paste0(sampleID[i], "_", tubeLabel[i]), "row" = rows[i], "column" = columns[i]))
#'
#'       if (i %% 8 == 0) {
#'         LTR@column <- i %/% 8
#'         rl <- addRequest(rl, list(rlist, LTR))
#'       } else {
#'         rl <- addRequest(rl, list(rlist))
#'       }
#'     }
#'     if (length(rows) < 80){
#'       rl <- addRequest(rl, list(LTR))
#'     }
#'
#'     run <- printRequest(rl, list("assay" = "MS_URPP"))
#'     saveRun(run, currentRunName, methodName)
#'     plateCounter <- plateCounter + 1
#'     req <- c(req, list(requestList = rl, run = run))
#'   }
#'   return(req)
#' }
#'
#' #' write run file for MS-LIPIDS
#' #' @param selectedSamples - the selected samples for run
#' #' @param runName - the name of the run
#' #' @param projectName - the name of the project
#' #' @param matrixName - the id of the sample matrix
#' #' @param deviceName - the id of the device
#' #' @param methodName - the id of the method
#' #' @param LTR_NAME - the name of LTR sample
#' #' @param date - the date of the run
#' #' @return void
#' runMS_LIPIDS <- function(selectedSamples, runName, projectName, matrixName, deviceName, methodName, LTR_NAME, date) {
#'
#'   plateList <- levels(factor(selectedSamples$plateID))
#'
#'   plateCounter <- 0
#'   req <- list()
#'   for (plate in plateList) {
#'
#'     currentRunName <- paste(runName,
#'                             plate,
#'                             sep = "_")
#'
#'     plateNames <- selectedSamples$plateID
#'
#'     sampleID <- selectedSamples$sampleID[plateNames == plate]
#'     tubeLabel <- selectedSamples$tubeLabel[plateNames == plate]
#'     positions <- selectedSamples$wellPos[plateNames == plate]
#'     RC <- posToRC(positions)
#'     columns <- RC$col
#'     rows <- RC$row
#'
#'     r <- new("request")
#'     runParam <- list("runName" = currentRunName,
#'                      "methodName" = methodName,
#'                      "deviceName" = deviceName,
#'                      "matrixName" = matrixName,
#'                      "platePosition" = 1 + (plateCounter %% 5))
#'
#'     rl <- new("requestList")
#'
#'     conc <- list("a" = 1)
#'     COND1 <- fillRequest(r, request = c(runParam, "sampleID" = "COND01", "row" = -1, "column" = 11, "sampleType" = "Standard"))
#'     COND2 <- fillRequest(r, request = c(runParam, "sampleID" = "COND02", "row" = -1, "column" = 11, "sampleType" = "Standard"))
#'     COND3 <- fillRequest(r, request = c(runParam, "sampleID" = "COND03", "row" = -1, "column" = 11, "sampleType" = "Standard"))
#'     COND4 <- fillRequest(r, request = c(runParam, "sampleID" = "COND04", "row" = -1, "column" = 11, "sampleType" = "Standard"))
#'     COND5 <- fillRequest(r, request = c(runParam, "sampleID" = "COND05", "row" = -1, "column" = 11, "sampleType" = "Standard"))
#'     COND6 <- fillRequest(r, request = c(runParam, "sampleID" = "COND06", "row" = -1, "column" = 11, "sampleType" = "Standard"))
#'     COND7 <- fillRequest(r, request = c(runParam, "sampleID" = "COND07", "row" = -1, "column" = 11, "sampleType" = "Standard"))
#'     COND8 <- fillRequest(r, request = c(runParam, "sampleID" = "COND08", "row" = -1, "column" = 11, "sampleType" = "Standard"))
#'     LTR <- fillRequest(r, request = c(runParam, "sampleID" = LTR_NAME, "row" = 1, "column" = 11, "sampleType" = "Unknown"))
#'     posLTR <- c("A11", "B11", "C11", "D11", "E11", "F11", "G11", "H11",
#'                 "A12", "B12", "C12", "D12", "E12", "F12", "G12", "H12")
#'     ### header
#'     counter <- 1
#'     LTR <- setPosition(LTR, posLTR[counter])
#'     counter <- counter + 1
#'     rl <- addRequest(rl, list(COND1,
#'                               COND2,
#'                               COND3,
#'                               COND4,
#'                               COND5,
#'                               COND6,
#'                               COND7,
#'                               COND8,
#'                               LTR))
#'
#'     for (i in 1:length(rows)) {
#'       rlist <- fillRequest(r, request = c(runParam, "sampleType" = "Unknown" ,"sampleID" = paste0(sampleID[i], "_", tubeLabel[i]), "row" = as.numeric(rows[i]), "column" = as.numeric(columns[i])))
#'
#'       if (i %% 8 == 0) {
#'         LTR <- setPosition(LTR, posLTR[counter])
#'         counter <- counter + 1
#'         #LTR <- fillRequest(r, request = c(runParam, "sampleID" = LTR_NAME, "row" = 5 + counter %% 4, "column" = 12, "sampleType" = "Unknown"))
#'         rl <- addRequest(rl, list(rlist, LTR))
#'       } else {
#'         rl <- addRequest(rl, list(rlist))
#'       }
#'     }
#'
#'     # footer
#'     LTR <- setPosition(LTR, posLTR[counter])
#'     counter <- counter + 1
#'     rl <- addRequest(rl, list(LTR))
#'
#'     run <- printRequest(rl, list("assay" = "MSSciex"))
#'     saveRun(run, currentRunName, methodName)
#'     plateCounter <- plateCounter + 1
#'     req <- c(req, list(requestList = rl, run = run))
#'   }
#'   return(req)
#' }
#'
#' #' write run file for MS-CYT
#' #' @param selectedSamples - the selected samples for run
#' #' @param runName - the name of the run
#' #' @param projectName - the name of the project
#' #' @param matrixName - the id of the sample matrix
#' #' @param deviceName - the id of the device
#' #' @param methodName - the id of the method
#' #' @param LTR_NAME - the name of LTR sample
#' #' @param date - the date of the run
#' #' @return void
#' runCYT <- function(selectedSamples, runName, projectName, matrixName, deviceName, methodName, LTR_NAME, date) {
#'
#'   plateList <- levels(factor(selectedSamples$plateID))
#'
#'   plateCounter <- 0
#'   req <- list()
#'   for (plate in plateList) {
#'
#'     currentRunName <- paste(runName,
#'                             plate,
#'                             sep = "_")
#'
#'     plateNames <- selectedSamples$plateID
#'
#'     sampleID <- selectedSamples$sampleID[plateNames == plate]
#'     tubeLabel <- selectedSamples$tubeLabel[plateNames == plate]
#'     positions <- selectedSamples$wellPos[plateNames == plate]
#'     RC <- posToRC(positions)
#'     columns <- RC$col
#'     rows <- RC$row
#'
#'     r <- new("request")
#'     runParam <- list("runName" = currentRunName,
#'                      "methodName" = methodName,
#'                      "deviceName" = deviceName,
#'                      "matrixName" = matrixName,
#'                      "platePosition" = 1 + (plateCounter %% 5))
#'
#'     rl <- new("requestList")
#'
#'     conc <- list("a" = 1)
#'     COND1 <- fillRequest(r, request = c(runParam, "sampleID" = "COND01", "row" = -1, "column" = 11, "sampleType" = "Standard"))
#'     COND2 <- fillRequest(r, request = c(runParam, "sampleID" = "COND02", "row" = -1, "column" = 11, "sampleType" = "Standard"))
#'     COND3 <- fillRequest(r, request = c(runParam, "sampleID" = "COND03", "row" = -1, "column" = 11, "sampleType" = "Standard"))
#'     COND4 <- fillRequest(r, request = c(runParam, "sampleID" = "COND04", "row" = -1, "column" = 11, "sampleType" = "Standard"))
#'     COND5 <- fillRequest(r, request = c(runParam, "sampleID" = "COND05", "row" = -1, "column" = 11, "sampleType" = "Standard"))
#'     COND6 <- fillRequest(r, request = c(runParam, "sampleID" = "COND06", "row" = -1, "column" = 11, "sampleType" = "Standard"))
#'     COND7 <- fillRequest(r, request = c(runParam, "sampleID" = "COND07", "row" = -1, "column" = 11, "sampleType" = "Standard"))
#'     COND8 <- fillRequest(r, request = c(runParam, "sampleID" = "COND08", "row" = -1, "column" = 11, "sampleType" = "Standard"))
#'     LTR <- fillRequest(r, request = c(runParam, "sampleID" = LTR_NAME, "row" = 1, "column" = 11, "sampleType" = "Unknown"))
#'     posLTR <- c("A11", "B11", "C11", "D11", "E11", "F11", "G11", "H11",
#'                 "A12", "B12", "C12", "D12", "E12", "F12", "G12", "H12")
#'     ### header
#'     counter <- 1
#'     LTR <- setPosition(LTR, posLTR[counter])
#'     counter <- counter + 1
#'     rl <- addRequest(rl, list(COND1,
#'                               COND2,
#'                               COND3,
#'                               COND4,
#'                               COND5,
#'                               COND6,
#'                               COND7,
#'                               COND8,
#'                               LTR))
#'
#'     for (i in 1:length(rows)) {
#'       rlist <- fillRequest(r, request = c(runParam, "sampleType" = "Unknown" ,"sampleID" = paste0(sampleID[i], "_", tubeLabel[i]), "row" = as.numeric(rows[i]), "column" = as.numeric(columns[i])))
#'
#'       if (i %% 8 == 0) {
#'         LTR <- setPosition(LTR, posLTR[counter])
#'         counter <- counter + 1
#'         #LTR <- fillRequest(r, request = c(runParam, "sampleID" = LTR_NAME, "row" = 5 + counter %% 4, "column" = 12, "sampleType" = "Unknown"))
#'         rl <- addRequest(rl, list(rlist, LTR))
#'       } else {
#'         rl <- addRequest(rl, list(rlist))
#'       }
#'     }
#'
#'     # footer
#'     LTR <- setPosition(LTR, posLTR[counter])
#'     counter <- counter + 1
#'     rl <- addRequest(rl, list(LTR))
#'
#'     run <- printRequest(rl, list("assay" = "MSSciex"))
#'     saveRun(run, currentRunName, methodName)
#'     plateCounter <- plateCounter + 1
#'     req <- c(req, list(requestList = rl, run = run))
#'   }
#'   return(req)
#' }
#'
#' #' write run file for NMR
#' #' @param selectedSamples - the selected samples for run
#' #' @param runName - the name of the run
#' #' @param projectName - the name of the project
#' #' @param matrixName - the id of the sample matrix
#' #' @param deviceName - the id of the device
#' #' @param methodName - the id of the method
#' #' @param date - the date of the run
#' #' @return a list
#' runNMR <- function(selectedSamples, runName, projectName, matrixName, deviceName, methodName, LTR_NAME, date) {
#'   LTR <- 4
#'
#'   plateList <- levels(factor(selectedSamples$plateID))
#'   req <- list()
#'   plateCounter <- 1
#'   for (plate in plateList) {
#'     currentRunName <- paste(runName,
#'                      plate,
#'                      sep = "_")
#'
#'     plateNames <- selectedSamples$plateID
#'
#'     sampleID <- selectedSamples$sampleID[plateNames == plate]
#'     tubeLabel <- selectedSamples$tubeLabel[plateNames == plate]
#'     positions <- selectedSamples$wellPos[plateNames == plate]
#'     RC <- posToRC(positions)
#'     columns <- RC$col
#'     rows <- RC$row
#'
#'     plateLength <- length(positions)
#'
#'     if (plateLength < 51) {
#'       LTR_positionList <- seq(1,4, by = 1)[1:LTR]
#'     } else {
#'       LTR_positionList <- seq(2 ,8, by = 2)[1:LTR]
#'       # LTR_positionList <- seq(1 ,8, by = 1)[1:LTR] # LTR = 8
#'     }
#'
#'     if ("title" %in% colnames(selectedSamples)) {
#'       title <- selectedSamples$title[plateNames == plate]
#'     } else {
#'       title <- rep("", plateLength)
#'     }
#'
#'     plate1S <- data.frame("_sampleID" = paste0(sampleID, "_", tubeLabel),
#'                           "_matrixName" = rep(matrixName, plateLength),
#'                           "_runName" = rep(currentRunName, plateLength),
#'                           "_sampleType" = rep("S", plateLength),
#'                           "_methodName" = rep(methodName, plateLength),
#'                           "_deviceName" = rep(deviceName, plateLength),
#'                           "_projectName" = rep(projectName, plateLength),
#'                           "_platePosition" = rep(1, plateLength),
#'                           "row" = rows,
#'                           "column" = columns,
#'                           "_title" = title, check.names = FALSE)
#'
#'     plate1LTR <- data.frame("_sampleID" = rep(LTR_NAME, LTR),
#'                             "_matrixName" = rep(matrixName, LTR),
#'                             "_runName" = rep(currentRunName, LTR),
#'                             "_sampleType" = rep("LTR", LTR),
#'                             "_methodName" = rep(methodName, LTR),
#'                             "_deviceName" = rep(deviceName, LTR),
#'                             "_projectName" = rep(projectName, LTR),
#'                             "_platePosition" = rep(1, LTR),
#'                             "row" = LTR_positionList,
#'                             "column" = rep(c(12), LTR),
#'                             "_title" = rep("", LTR), check.names = FALSE)
#'
#'     plate1 <- rbind(plate1S, plate1LTR)
#'     # ordering plate by row
#'     F <- sort(plate1$col, index.return = TRUE)$ix
#'     plate1 <- plate1[F,]
#'     F <- sort(plate1$row, index.return = TRUE)$ix
#'     plate1 <- plate1[F,]
#'     plate1 <- makeBruker(plate1, methodName, deviceName, plateCounter)
#'     saveRun(plate1, currentRunName, methodName)
#'     req <- c(req, list(requestList = NA, run = plate1))
#'     plateCounter <- plateCounter + 1
#'   }
#'   return(req)
#' }
#'
#' makeBruker <- function(conf, methodName, deviceName, plateCounter) {
#'   method <- dbGetQuery(con, paste0("select * from methods
#'                        where method_name = '", methodName, "'"))
#'   method_request <- fromJSON(method$method_request)
#'   device <- dbGetQuery(con, paste0("select * from devices
#'                        where dev_name = '", deviceName, "'"))
#'   res <- list()
#'   counter = 1
#'   for (i in 1:nrow(conf)) {
#'     for (j in 1:length(method_request$experiments)) {
#'       holder <-  RCToNum(conf$row[i], conf$column[i]) + (100 * plateCounter)
#'       res[[counter]] <- data.frame(HOLDER = holder,
#'                                    USER = method_request$user,
#'                                    NAME = conf$`_runName`[i],
#'                                    TITLE = paste(conf$`_projectName`[i],
#'                                                  method$method_name,
#'                                                  conf$`_matrixName`[i],
#'                                                  conf$`_sampleID`[i],
#'                                                  paste0("R", conf$row[i],
#'                                                         "C", conf$column[i],
#'                                                         "\\n"),
#'                                                  sep = "_"),
#'                                    EXPNO = paste0(i, j - 1),
#'                                    SOLVENT = method_request$solvent,
#'                                    EXPERIMENT = method_request$experiments[j],
#'                                    PARAMETERS = paste0("usera2,",
#'                                                        conf$`_sampleID`[i]),
#'                                    DISK = device$dev_path
#'       )
#'       counter = counter + 1
#'     }
#'   }
#'   p <- data.frame(do.call("rbind", res))
#'   return(p)
#' }
#'
#' #' write run file
#' #' @param run - the selected samples for run
#' #' @param runName - the run name
#' #' @return write the file
#' #' @importFrom utils write.table
#' #' @importFrom DBI dbGetQuery dbDisconnect
#' #' @importFrom rolodex.samples openDb
#' saveRun <- function(run, runName, methodName){
#'   con <- openDb()
#'   method <- dbGetQuery(con, paste0("select * from methods
#'                        where method_name = '", methodName, "'"))
#'   dbDisconnect(con)
#'   fileFormat <- fromJSON(method$method_options)$fileFormat
#'   fileExtension <- fromJSON(method$method_options)$fileExtension
#'   if (fileFormat == "tsv") {
#'     sep = "\t"
#'   } else if (fileFormat == "csv") {
#'     sep = ","
#'   } else {
#'     stop("request >> getRun: unknown file extension")
#'   }
#'
#'   file <- paste0("configurationFiles/",
#'                  runName, ".",
#'                  fileExtension)
#'   write.table(run,
#'               file = file,
#'               sep = sep,
#'               dec = ".",
#'               row.names = FALSE,
#'               quote = TRUE)
#'   # if (grepl("_NMR-", runName)) {
#'   #     txt <- system(
#'   #       paste0(
#'   #         "~/git/cheminfo-js/getRunOrder/build/toNMR-macos -r configurationFiles/",
#'   #         runName,
#'   #         ".tsv"),
#'   #       intern = TRUE
#'   #       )
#'   #     fileConn <- file(paste0("configurationFiles/", runName, ".csv"))
#'   #     writeLines(txt, fileConn)
#'   #     close(fileConn)
#'   # }
#'   cat(crayon::green("request >> getRUN >> ", file, " written\n"))
#' }
