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
  types <- c("NMR", "T-MS")
  type <- which(type %in% types)
  switch(type,
         runNMR(selectedSamples,
                matrixID,
                deviceID,
                methodID,
                cohortName,
                projectName),
         runTMS())
}


runTMS <- function() {
  test <- 1
}

#' write run file for NMR
#' @param selectedSamples - the selected samples for run
#' @param matrixID - the id of the samples matrix
#' @param deviceID - the id of the device to run the experiments
#' @param methodID - the id of the method to be run
#' @param cohortName - the name of the cohort to run
#' @param projectName - the name of the project
#' @return write the file
#' @importFrom utils write.table
runNMR <- function(selectedSamples,
                   matrixID,
                   deviceID,
                   methodID,
                   cohortName,
                   projectName) {
  LTR <- 4
  deviceName <- getDevice(c(deviceID = deviceID))[[1]]
  methodName <- getDevice(c(methodID = methodID))[[1]]
  matrixName <- getDevice(c(matrixID = matrixID))[[1]]

  plateList <- levels(factor(selectedSamples$plateID))
  for (plate in plateList) {
    runName <- paste(projectName,
                     cohortName,
                     matrixName,
                     methodID,
                     deviceName,
                     plate,
                     format(Sys.time(), "%d%m%y"),
                     sep = "_")

    selectedPlateF <- selectedSamples$plateID == plate
    positions <- selectedSamples$wellPos[selectedPlateF]
    RC <- posToRC(positions)
    columns <- RC$col
    rows <- RC$row

    plateLength <- length(positions)

    sampleID <- selectedSamples$sampleID[selectedPlateF]
    sourceID <- selectedSamples$sourceID[selectedPlateF]

    plate1S <- data.frame("_sampleID" = paste0(sampleID, "_", sourceID),
                          "_matrixID" = rep(matrixID, plateLength),
                          "_runName" = rep(runName, plateLength),
                          "_sampleType" = rep("S", plateLength),
                          "_methodID" = rep(methodID, plateLength),
                          "_deviceID" = rep(deviceID, plateLength),
                          "_projectName" = rep(projectName, plateLength),
                          "_platePosition" = rep(1, plateLength),
                          "row" = rows,
                          "column" = columns, check.names = FALSE)

    plate1LTR <- data.frame("_sampleID" = rep("LTR_PLASMA", LTR),
                            "_matrixID" = rep(matrixID, LTR),
                            "_runName" = rep(runName, LTR),
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
    file <- paste0("configurationFiles/",
                   runName,
                   "_request.tsv")
    write.table(plate1,
                file = file,
                sep = "\t",
                dec = ".",
                row.names = FALSE)
  }
  cat(crayon::green("request >> getRUN >> ", file, " written\n"))
}
