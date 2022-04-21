#' request list class
#' @slot entry - a list of requests
#' @export
setClass("requestList",
         representation = representation(entry = "list"),
         prototype(entry = list()),
         contains = list("list")
)

#' adding request to list
#' @param r - the request list
#' @param entry - the new request
#' @export
setGeneric("addRequest", function(r, entry) {
  standardGeneric("addRequest")
})

#' adding request to list
#' @param r - the request list
#' @param entry - the new request
#' @export
setMethod("addRequest",
          c(r = "requestList", entry = "list"),
          function(r, entry) {
            r@entry <- c(r@entry, entry)
            return(r)
          }
)

#' printing a request list
#' @param r - the request list
#' @param options - options
#' @export
setGeneric("printRequest", function(r, options) {
  standardGeneric("printRequest")
})

#' printing a request list
#' @param r - the request list
#' @param options - options
#' @export
setMethod("printRequest",
          c(r = "requestList", options = "list"),
          function(r, options) {
            rlist <- list()
            for (j in 1:length(r@entry)) {
              i <- r@entry[[j]]
              row <- LETTERS[1:8]

              if (options$assay == "MSBruker") {
                # path <- "D:\\Data\\AA Methods\\Amino Acid\\"
                path <- paste0("D:\\lims\\", i@projectName, "\\Amino Acid\\")
                # methSet <- "D:\\Methods\\Amino Acid VALIDATED\\Amino acid MethodSet VALIDATED.m"
                sepMeth <- "D:\\Methods\\Amino Acid VALIDATED\\Amino acid LC method VALIDATED.m?HyStar_LC"
                MSMeth <- "D:\\Methods\\Amino Acid VALIDATED\\Amino acid MS method VALIDATED.m?microTOFQImpactControl"
                procMeth <- "D:\\Methods\\Amino Acid VALIDATED\\NaFA pos recal readout.m?DataAnalysis"
                injMeth <- "Standard"
                calLev <- switch(i@sampleID,
                                 "CAL08" = 1,
                                 "CAL07" = 2,
                                 "CAL06" = 4,
                                 "CAL05" = 10,
                                 "CAL04" = 20,
                                 "CAL03" = 100,
                                 "CAL02" = 200,
                                 "CAL01" = 400,
                                 "QC04" = 3,
                                 "QC03" = 15,
                                 "QC02" = 75,
                                 "QC01" = 300)
                if (is.null(calLev)) { calLev <- 0}

                if (i@row == 0){
                  sampleLocation <- "V:1"
                } else if (i@row == -1) {
                  sampleLocation <- "V:4"
                } else {
                  sampleLocation <- paste0(i@platePosition, ":", row[i@row], ",", i@column)
                }
                rlist[[j]] <- data.frame("Vial" = sampleLocation,
                                         "Sample ID" = paste0(i@runName, "_", i@sampleID, "_", j),
                                         # "Method Set" = methSet,
                                         "Separation Method" = sepMeth,
                                         "MS Method" = MSMeth,
                                         "Processing Method" = procMeth,
                                         "injMeth" = injMeth,
                                         "Sample Type" = i@sampleType,
                                         "Calib. Level" = calLev,
                                         "Inj." = 1,
                                         #"Volume [µl]" = 2,
                                         "Volume" = 2,
                                         "Data Path" = path,
                                         "Run Automated Processing" = "true", check.names = FALSE)
              } else if (options$assay == "MSWaters") {
                msFile <- "2020_Tryptophan_v5_0_AD"
                msTunFile <- "2020_Tryptophan_v1_0"
                inletFile <- "2020_Tryptophan_v1_0"
                injVol <- 10

                if (i@row == 0){
                  sampleLocation <- "V:1"
                } else if (i@row == -1) {
                  sampleLocation <- "V:4"
                } else {
                  sampleLocation <- paste0(i@platePosition, ":", row[i@row], ",", i@column)
                }

                if ("conc" %in% names(i@options)){
                  calLev = i@options$conc
                } else {
                  calLev = rep("", 9)
                }

                rlist[[j]] <- data.frame("Index" = "",
                                         "FILE_NAME" = paste0(i@runName,
                                                              "_",
                                                              gsub("\\.", "_", i@sampleID), "_",
                                                              j),
                                         "INJ_VOL" = injVol,
                                         "TYPE" = i@sampleType,
                                         "SAMPLE_LOCATION" = sampleLocation,
                                         "INLET_FILE" = inletFile,
                                         "MS_TUNE_FILE" = msTunFile,
                                         "MS_FILE" = msFile,
                                         "FILE_TEXT" = gsub("\\.", "_", i@sampleID),
                                         "CONC_A" = calLev[1],
                                         "CONC_B" = calLev[2],
                                         "CONC_C" = calLev[3],
                                         "CONC_D" = calLev[4],
                                         "CONC_E" = calLev[5],
                                         "CONC_F" = calLev[6],
                                         "CONC_G" = calLev[7],
                                         "CONC_H" = calLev[8],
                                         "CONC_I" = calLev[9], check.names = FALSE)
              } else if (options$assay == "MS-TRYE") {
                methodName <- "Tryptophan_method_optimization"
                injVol <- 20

                # if (i@row == 0){
                #   sampleLocation <- "V:1"
                # } else if (i@row == -1) {
                #   sampleLocation <- "V:4"
                # } else {
                  sampleLocation <- RCToPos(i@row, i@column) #paste0(i@platePosition, ":", row[i@row], ",", i@column)
                # }
                if (i@platePosition == 1) {
                  plateLocation <- "Left"
                } else if (i@platePosition == 2) {
                  plateLocation <- "Right"
                }

                calLev <- switch(i@sampleID,
                                 "CAL08" = 1,
                                 "CAL07" = 2,
                                 "CAL06" = 4,
                                 "CAL05" = 10,
                                 "CAL04" = 20,
                                 "CAL03" = 100,
                                 "CAL02" = 200,
                                 "CAL01" = 400,
                                 "QC04" = 3,
                                 "QC03" = 15,
                                 "QC02" = 75,
                                 "QC01" = 300)
                if (is.null(calLev)) { calLev <- 0}

                rlist[[j]] <- data.frame("Sample Position" = sampleLocation,
                                         "Sample Name" = paste0(i@runName,
                                                              "_",
                                                              gsub("\\.", "_", i@sampleID), "_",
                                                              j),
                                         "Sample Type" = i@sampleType,
                                         "Concentration Level" = calLev,
                                         "Injection Volume" = injVol,
                                         "Method Name" = methodName,
                                         "Plate" = plateLocation,
                                         "Comment" = NA,
                                         "Dilution" = 1, check.names = FALSE)
              } else if (options$assay == "MS-BILE") {
                msFile <- "BileAcids_MS_2022"
                msTunFile <- "BileAcids_Tune_2022"
                inletFile <- "BileAcids_LC_2022"
                injVol <- 5

                if (i@row == 0){
                  sampleLocation <- "2:F,8"
                } else if (i@row == -1) {
                  sampleLocation <- "V:4"
                } else {
                  sampleLocation <- paste0(i@platePosition, ":", row[i@row], ",", i@column)
                }

                calLev <- switch(i@sampleID,
                                 "CAL08" = 5,
                                 "CAL07" = 15,
                                 "CAL06" = 25,
                                 "CAL05" = 75,
                                 "CAL04" = 150,
                                 "CAL03" = 300,
                                 "CAL02" = 600,
                                 "CAL01" = 1000,
                                 "QC04" = 8,
                                 "QC03" = 32,
                                 "QC02" = 320,
                                 "QC01" = 800)
                if (is.null(calLev)) { calLev <- 0}

                rlist[[j]] <- data.frame("File Name" = paste0(i@runName,
                                                              "_",
                                                              gsub("\\.", "_", i@sampleID), "_",
                                                              j),
                                         "INLET_FILE" = inletFile,
                                         "MS_TUNE_FILE" = msTunFile,
                                         "MS_FILE" = msFile,
                                         "SAMPLE_LOCATION" = sampleLocation,
                                         "INJ_VOL" = injVol,
                                         "TYPE" = i@sampleType,
                                         "CONC_A" = calLev, check.names = FALSE)
              } else if (options$assay == "MS-Q300") {
                msFile <- "Q300-ver1"
                msTunFile <- "Q300_20210316"
                inletFile <- "20210209-Q300-1"
                injVol <- 5

                if (i@row == 0){
                  sampleLocation <- "V:1"
                } else if (i@row == -1) {
                  sampleLocation <- "V:4"
                } else {
                  sampleLocation <- paste0(i@platePosition, ":", row[i@row], ",", i@column)
                }

                calLev <- switch(i@sampleID,
                                 "CAL08" = NA,
                                 "CAL07" = NA,
                                 "CAL06" = NA,
                                 "CAL05" = NA,
                                 "CAL04" = NA,
                                 "CAL03" = NA,
                                 "CAL02" = NA,
                                 "CAL01" = NA)

                if (is.null(calLev)) { calLev <- 0}

                rlist[[j]] <- data.frame("File Name" = paste0(i@runName,
                                                              "_",
                                                              gsub("\\.", "_", i@sampleID), "_",
                                                              j),
                                         "INLET_FILE" = inletFile,
                                         "MS_TUNE_FILE" = msTunFile,
                                         "MS_FILE" = msFile,
                                         "SAMPLE_LOCATION" = sampleLocation,
                                         "INJ_VOL" = injVol,
                                         "TYPE" = i@sampleType,
                                         "CONC_A" = calLev, check.names = FALSE)
              } else if (options$assay == "MSSciex") {

                rackPos <- i@platePosition
                smplInjVol <- 5
                acqMethod <- "LM_RP_sMRM_LIPIDS"
                rackCode <- "Rack Order (Column)"
                plateCode <- "MTP 96"
                platePos <- i@platePosition

                if (i@row > 0){
                  sampleLocation <- RCToNum(i@row, i@column)
                } else if (i@row == -1) {
                  sampleLocation <- "20001"
                } else {
                  sampleLocation <- NA
                }

                rlist[[j]] <- data.frame("% header=SampleName" = paste0(i@sampleID, "_", j),
                                         "SampleId" = paste0(i@sampleID, "_", j),
                                         "RackCode" = rackCode,
                                         "Rackpos" = rackPos,
                                         "PlateCode" = plateCode,
                                         "SmplInjVol" = smplInjVol,
                                         "AcqMethod" = acqMethod,
                                         "PlatePos" = platePos,
                                         "VialPos" = sampleLocation,
                                         "OutputFile" = i@runName,
                                         "Type" = i@sampleType,
                                         check.names = FALSE)
              } else if (options$assay == "MS_EICOS") {
                rackPos <- 1
                smplInjVol <- 5
                acqMethod <- "Eicosanoids_Quantitation-2021"
                rackCode <- "Rack Order (Column)"
                plateCode <- "MTP 96"
                platePos <- i@platePosition

                if (i@row > 0){
                  sampleLocation <- RCToNum(i@row, i@column)
                } else {
                  sampleLocation <- 20001
                }

                rlist[[j]] <- data.frame("SampleName" = paste0(i@sampleID, "_", j),
                                         "Rack Code" = rackCode,
                                         "Rack position" = rackPos,
                                         "Plate Code" = plateCode,
                                         "PlatePos" = platePos,
                                         "VialPos" = sampleLocation,
                                         "Acquisition Method" = acqMethod,
                                         "Data File" = i@runName,
                                         "Injection Volume" = smplInjVol,
                                         check.names = FALSE)
              } else if (options$assay == "MS_URPP") {
                path <- paste0("D:\\lims\\", i@projectName, "\\URPP\\")
                methSet <- "D:\\Methods\\URPP\\URPP VALIDATED.m"
                if (i@row == 0){
                  col <- (i@column %% 4) + 1
                  sampleLocation <- paste0("V:", col)
                } else {
                  sampleLocation <- paste0(i@platePosition, ":", row[i@row], ",", i@column)
                }
                rlist[[j]] <- data.frame("Vial" = sampleLocation,
                                         "Sample ID" = paste0(i@runName, "_", i@sampleID, "_", j),
                                         "Method Set" = methSet,
                                         "Sample Type" = i@sampleType,
                                         "Calib. Level" = 0,
                                         "Inj." = 1,
                                         "Volume" = 2,
                                         "Data Path" = path,
                                         "Run Automated Processing" = "true", check.names = FALSE)
              } else if (options$assay == "MS_MRMSP") {
                path <- paste0("D:\\lims\\", i@projectName, "\\MRMSP\\")
                methSet <- "D:\\Methods\\FIA_Methods\\200802_Hexakis_Lockmass.m"
                sepMeth <- "D:\\Methods\\FIA_Methods\\201217_LC_method.m"
                msMeth <- "D:\\Methods\\FIA_Methods\\201217_MRMS_POS.m"

                rlist[[j]] <- data.frame("Vial" = paste0(1, ":", i@platePosition, ":", RCToNum(i@row, i@column)),
                                         "Sample ID" = paste0(i@runName, "_", i@sampleID, "_", j),
                                         "Method Set" = methSet,
                                         "Separation Method" = sepMeth,
                                         "MS Method" =  msMeth,
                                         "Sample Type" = i@sampleType,
                                         "Volume [µl]" = 20,
                                         "Data Path" = path,
                                         "Result Path" = "", check.names = FALSE)
              } else if (options$assay == "MS_MRMSN") {
                # path <- "D:\\Data\\AA Methods\\Amino Acid\\"
                path <- paste0("D:\\lims\\", i@projectName, "\\MRMSN\\")
                methSet <- "D:\\Methods\\FIA_Methods\\200802_Hexakis_Lockmass.m"
                sepMeth <- "D:\\Methods\\FIA_Methods\\201217_LC_method.m"
                msMeth <- "D:\\Methods\\FIA_Methods\\201217_MRMS_NEG.m"

                rlist[[j]] <- data.frame("Vial" = paste0(1, ":", i@platePosition, ":", RCToNum(i@row, i@column)),
                                         "Sample ID" = paste0(i@runName, "_", i@sampleID, "_", j),
                                         "Method Set" = methSet,
                                         "Separation Method" = sepMeth,
                                         "MS Method" = msMeth,
                                         "Sample Type" = i@sampleType,
                                         "Volume [µl]" = 20,
                                         "Data Path" = path,
                                         "Result Path" = "", check.names = FALSE)
              } else if (options$assay == "TIMS_LIPIDS_N") {
                path <- paste0("D:\\lims\\", i@projectName, "\\TIMS_LIPIDS_N\\")
                methSet <- ""
                sepMeth <- "D:\\Methods\\CORTECS\ T3\ Lipidomics\\2020\ 0921_CORTECS\ T3_A2B2_Lipidomics_400uLmin_15min_PLNO_v1.m"
                msMeth <- "D:\\Methods\\CRC-lipid_CSH_C18\\2020 0831-4D-Lipidomics_ANPC_v1.m"
                procMeth <- "D:\\Data\\NaFA\ pos\ recal\ readout.m"
                injMeth <- "Standard"

                if (i@platePosition == 0){
                  sampleLocation <- paste0("V:", i@row)
                } else {
                  sampleLocation <- paste0(i@platePosition, ":", row[i@row], ",", i@column)
                }

                rlist[[j]] <- data.frame("Vial" = sampleLocation,
                                         "Sample ID" = paste0(i@runName, "_", i@sampleID, "_", j),
                                         "Method Set" = methSet,
                                         "Separation Method" = sepMeth,
                                         "Injection Method" = injMeth,
                                         "MS Method" = msMeth,
                                         "Processing Method" = procMeth,
                                         "Sample Type" = i@sampleType,
                                         "Inj." = 1,
                                         "Volume [µl]" = 7.5,
                                         "Data Path" = path,
                                         "Run Automated Processing" = "TRUE", check.names = FALSE)
              } else if (options$assay == "TIMS_LIPIDS_P") {
                path <- paste0("D:\\lims\\", i@projectName, "\\TIMS_LIPIDS_P\\")
                methSet <- ""
                sepMeth <- "D:\\Methods\\CORTECS\ T3\ Lipidomics\\2020\ 0921_CORTECS\ T3_A2B2_Lipidomics_400uLmin_15min_PLNO_v1.m"
                msMeth <- "D:\\Methods\\CRC-lipid_CSH_C18\\2020 0831-4D-Lipidomics_ANPC_v1.m"
                procMeth <- "D:\\Data\\NaFA\ pos\ recal\ readout.m"
                injMeth <- "Standard"

                if (i@platePosition == 0){
                  sampleLocation <- paste0("V:", i@row)
                } else {
                  sampleLocation <- paste0(i@platePosition, ":", row[i@row], ",", i@column)
                }

                rlist[[j]] <- data.frame("Vial" = sampleLocation,
                                         "Sample ID" = paste0(i@runName, "_", i@sampleID, "_", j),
                                         "Method Set" = methSet,
                                         "Separation Method" = sepMeth,
                                         "Injection Method" = injMeth,
                                         "MS Method" = msMeth,
                                         "Processing Method" = procMeth,
                                         "Sample Type" = i@sampleType,
                                         "Inj." = 1,
                                         "Volume [µl]" = 7.5,
                                         "Data Path" = path,
                                         "Run Automated Processing" = "TRUE", check.names = FALSE)
              } else {
                rlist[[j]] <- data.frame("_sampleID" = i@sampleID,
                                         "_matrixID" = i@matrixID,
                                         "_runName" = i@runName,
                                         "_sampleType" = i@sampleType,
                                         "_methodID" = i@methodID,
                                         "_deviceID" = i@deviceID,
                                         "_projectName" = i@projectName,
                                         "_platePosition" = i@platePosition,
                                         "row" = i@row,
                                         "column" = i@column, check.names = FALSE)
              }
              df <- do.call("rbind", rlist)
            }

            # adding shutdown blank
            if (options$assay == "MSBruker") {
              last <- nrow(df)
              cat(crayon::blue("request >> classRequestList >> ", last, " samples were added \n"))
              path <- "D:\\Methods\\System startup\\LC Methods\\"
              methSet <- "D:\\Methods\\Amino Acid VALIDATED\\Amino acid LC method SHUTDOWN.m"
              sepMeth <- "Amino acid LC method SHUTDOWN"
              injMeth <- "Standard"
              MSMeth <- ""
              procMeth <- ""
              df$`Sample ID`[last] <- paste0(i@runName, "_Blank Shutdown_", last)
              df$`Separation Method`[last] <- sepMeth
            } else if (options$assay == "MSWaters") {
              last <- nrow(df)
              cat(crayon::blue("request >> classRequestList >> ", last, " samples were added \n"))
              inletFile <- "AAA end"
              df$`INLET_FILE`[last] <- inletFile
              df$`FILE_TEXT`[last] <- "Blank Shutdown"
              df$`TYPE`[last] <- "Blank"
              df$FILE_NAME[last] <- paste0(i@runName, "_BlankShutdown_", last)
            } else if (options$assay == "MS-BILE") {
              last <- nrow(df)
              cat(crayon::blue("request >> classRequestList >> ", last, " samples were added \n"))
              df$`File Name`[last] <- paste0(i@runName, "_BlankShutdown_", last)
            } else {
              last <- nrow(df)
              cat(crayon::blue("request >> classRequestList >> ", last, " samples were added \n"))
            }

            return(df)
          }
)
