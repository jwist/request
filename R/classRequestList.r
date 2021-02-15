#' request list class
#'
#' @export
setClass("requestList",
         representation = representation(entry = "list"),
         prototype(entry = list()),
         contains = list("list")
)

#' adding request to list
#'
#' @export
setGeneric("addRequest", function(r, entry) {
  standardGeneric("addRequest")
})

#' adding request to list
#'
#' @export
setMethod("addRequest",
          c(r = "requestList", entry = "list"),
          function(r, entry) {
            r@entry <- c(r@entry, entry)
            return(r)
          }
)

#' printing a request list
#'
#' @export
setGeneric("printRequest", function(r, options) {
  standardGeneric("printRequest")
})

#' printing a request list
#'
#' @export
setMethod("printRequest",
          c(r = "requestList", options = "list"),
          function(r, options) {
            rlist <- list()
            for (j in 1:length(r@entry)) {
              i <- r@entry[[j]]
              row <- LETTERS[1:8]
              if (options$assay == "MSBruker") {
                path <- "D:\\Data\\AA Methods\\Amino Acid\\"
                methSet <- "D:\\Methods\\Amino Acid VALIDATED\\Amino acid MethodSet VALIDATED.m"
                # sepMeth <- paste0(path, "Amino_Acid_Methodset_v3.m?HyStar_LC")
                # injMeth <- paste0(path, "Amino_Acid_Methodset_v3.m?HyStar_Autosampler")
                # MSMeth <- paste0(path, "Amino_Acid_Methodset_v3.m?microTOFQImpactControl")
                # procMeth <- paste0(path, "Amino_Acid_Methodset_v3.m?DataAnalysis")

                calLev <- switch(i@sampleID,
                                 "Cal 9" = 1,
                                 "Cal 8" = 2,
                                 "Cal 7" = 4,
                                 "Cal 6" = 10,
                                 "Cal 5" = 20,
                                 "Cal 4" = 40,
                                 "Cal 3" = 100,
                                 "Cal 2" = 200,
                                 "Cal 1" = 400,
                                 "QC 4" = 3,
                                 "QC 3" = 15,
                                 "QC 2" = 75,
                                 "QC 1" = 300)
                if (is.null(calLev)) { calLev <- 0}

                rlist[[j]] <- data.frame("Vial" = paste0(2, ":", row[i@row], ",", i@column),
                                         "Sample ID" = paste0(runName, "_", i@sampleID, "_", j),
                                         "Method Set" = methSet,
                                         #"Separation Method" = sepMeth,
                                         #"Injection Method" = injMeth,
                                         #"MS Method" = MSMeth,
                                         #"Processing Method" = procMeth,
                                         "Sample Type" = i@sampleType,
                                         "Calib. Level" = calLev,
                                         "Inj." = 1,
                                         "Volume" = 2,
                                         "Data Path" = path,
                                         "Run Automated Processing" = "true", check.names = FALSE)
              } else if (options$assay == "MSWaters") {
                msFile <- "2020_Tryptophan_V2_0"
                msTunFile <- "2020_Tryptophan_v1_0"
                inletFile <- "2020_Tryptophan_v1_0"
                injVol <- 10

                if (i@row == 0){
                  sampleLocation <- "V:1"
                } else if (i@row == -1) {
                  sampleLocation <- "V:4"
                } else {
                  sampleLocation <- paste0(2, ":", row[i@row], ",", i@column)
                }

                if ("conc" %in% names(i@options)){
                  calLev = i@options$conc
                } else {
                  calLev = rep("", 9)
                }

                rlist[[j]] <- data.frame("Index" = "",
                                         "FILE_NAME" = paste0(runName,
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
              } else if (options$assay == "MSSciex") {
                rackPos <- 1
                smplInjVol <- 5
                acqMethod <- "LM_RP_sMRM_LIPIDS"
                rackCode <- "Rack Order (Column)"
                plateCode <- "MTP 96, 1.5mL"
                platePos <- 1

                sampleLocation <- RCToNum(row[i@row], i@column)

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
              print(last)
              path <- "D:\\Methods\\System startup\\LC Methods\\"
              methSet <- "D:\\Methods\\Amino Acid VALIDATED\\Amino acid LC method SHUTDOWN.m"
              sepMeth <- paste0(path, "A1B1_600ulmin_10min_shutdown.m?HyStar_LC")
              injMeth <- paste0(path, "A1B1_600ulmin_10min_shutdown.m?HyStar_Autosampler")
              MSMeth <- ""
              procMeth <- ""
              df$`Sample ID`[last] <- paste0(runName, "_Blank Shutdown_", j)
              df$`Method Set`[last] <- methSet
              #df$`Separation Method`[last] <- sepMeth
              #df$`Injection Method`[last] <- injMeth
              #df$`MS Method`[last] <- MSMeth
              #df$`Processing Method`[last] <- procMeth
            } else if (options$assay == "MSWaters") {
              last <- nrow(df)
              print(last)
              inletFile <- "AAA end"
              df$`INLET_FILE`[last] <- inletFile
              df$`FILE_TEXT`[last] <- "Blank Shutdown"
              df$`TYPE`[last] <- "Blank"
              df$FILE_NAME[last] <- paste0(runName, "_BlankShutdown")
            }

            return(df)
          }
)
