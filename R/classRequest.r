#' An S4 class for request
#'
#' @slot sampleID sampleID
#' @slot sampleType sample type
#' @slot matrixID id of sample matrix
#' @slot methodID id of the method
#' @slot deviceID id of the device
#' @slot projectName name of the project
#' @slot runName name of the cohort
#' @slot platePosition position of the plate
#' @slot row row of the sample
#' @slot column column of the sample
#' @slot options options
#' @return a object with request
#' @examples
#'
#' new("request")
#'
#' @export
setClass("request",
         representation = representation(sampleID = "character",
                                         sampleType = "character",
                                         matrixID = "numeric",
                                         methodID = "numeric",
                                         deviceID = "numeric",
                                         projectName = "character",
                                         runName = "character",
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
#' @param .Object the request object
setMethod("initialize", "request",
          function(.Object){
            .Object@projectName = "project"
            .Object@platePosition = 1
            .Object@options = list()
            return(.Object)
          }
)

#' filling requests
#' @param r the request
#' @param request the parameters to fill
#' @export
setGeneric("fillRequest", function(r, request) {
  standardGeneric("fillRequest")
})

#' filling requests
#' @param r the request
#' @param request the parameters to fill
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


#' set position of requests
#' @param r the request
#' @param position the parameters to fill
#' @export
setGeneric("setPosition", function(r, position) {
  standardGeneric("setPosition")
})


#' set position of requests
#' @param r the request
#' @param position the parameters to fill
#' @export
setMethod("setPosition",
          c(r = "request", position = "character"),
          function(r, position) {
            RC <- posToRC(position)
            r@row <- as.numeric(RC[1])
            r@column <- as.numeric(RC[2])
            return(r)
          }
)
