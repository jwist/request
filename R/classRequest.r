#' An S4 class for request
#'
#' @slot .Data the data matrix
#' @slot varName a vector containing the name of each variable
#' @slot obsDescr a data.frame containing experimental
#' conditions and a field called sampleID that MUST be unique.
#' @slot type type can be NMR, MSU, MST, ANN
#' @return a dataElement
#' @examples
#'
#' new("request")
#'
#' @export
setClass("request",
         representation = representation(sampleID = "character",
                                         matrixID = "numeric",
                                         runName = "character",
                                         sampleType = "character",
                                         methodID = "numeric",
                                         deviceID = "numeric",
                                         projectName = "character",
                                         platePosition = "numeric",
                                         row = "numeric",
                                         column = "numeric",
                                         options = "list"),
         validity = function(object) {
           if (identical(object@row, numeric(0))) {
             "invalid number of rows"
           } else {
             TRUE
           }
         }
)

# use validObject(new("request")) to check validity of object

#' initialize request
#'
#' @export
setMethod("initialize", "request",
          function(.Object){
            .Object@sampleType = "sample"
            .Object@projectName = "project"
            .Object@platePosition = 1
            .Object@options = list()
            return(.Object)
          }
)

#' filling requests
#'
#' @export
setGeneric("fillRequest", function(r, request) {
  standardGeneric("fillRequest")
})

#' filling requests
#'
#' @export
setMethod("fillRequest",
          c(r = "request", request = "list"),
          function(r, request) {
            if ("sampleID" %in% names(request)){
              r@sampleID <- request$sampleID
            }
            if ("matrixID" %in% names(request)){
              r@matrixID <- request$matrixID
            }
            if ("sampleType" %in% names(request)){
              r@sampleType <- request$sampleType
            }
            if ("platePosition" %in% names(request)){
              r@platePosition <- request$platePosition
            }
            if ("runName" %in% names(request)){
              r@runName <- request$runName
            }
            if ("methodID" %in% names(request)){
              r@methodID <- request$methodID
            }
            if ("deviceID" %in% names(request)){
              r@deviceID <- request$deviceID
            }
            if ("projectName" %in% names(request)){
              r@projectName <- request$projectName
            }
            if ("row" %in% names(request)){
              r@row <- request$row
            }
            if ("column" %in% names(request)){
              r@column <- request$column
            }
            if ("options" %in% names(request)){
              r@options <- c(r@options, request$options)
            }

            return(r)
          }
)
