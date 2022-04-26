#' retrieve device name from ID using googlesheet
#' @param what - what to retrive c(deviceID = 1)
#' @return the name of the device
#' @export
#' @importFrom utils read.table
getDevice <- function(what) {

  res = list()
  if("deviceID" %in% names(what)) {
    deviceListURL <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vQs_oa0dMN6RE837QQ6GgQHFQr85SnjJZExxBarrWhEClCqHWEjEEp_bhtNwpbAlWpucbda9RhrkWTp/pub?gid=1146951709&single=true&output=tsv"

    url <- deviceListURL
    ID <- what[which(names(what)=="deviceID")]
    if (!is.numeric(ID)) {stop("request >> getDevice >> ID should be numeric")}

    deviceList <- read.table(url,
                             header = TRUE,
                             sep = "\t",
                             dec = ".")[,1:10]
    cat(crayon::blue(nrow(deviceList), ": devices found\n"))
    if(ID > 0) {
      idx <- which(deviceList$dev_id == ID)
      res <- c(res, deviceList$dev_name[idx])
    } else {
      cat(crayon::blue(paste(deviceList$dev_name, "\n")), '\n')
    }
  }
  if("methodID" %in% names(what)) {
    methodListURL <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vQs_oa0dMN6RE837QQ6GgQHFQr85SnjJZExxBarrWhEClCqHWEjEEp_bhtNwpbAlWpucbda9RhrkWTp/pub?gid=1000620284&single=true&output=tsv"

    url <- methodListURL
    ID <- what[which(names(what)=="methodID")]
    if (!is.numeric(ID)) {stop("request >> getDevice >> ID should be numeric")}

    methodList <- read.table(url,
                             header = TRUE,
                             sep = "\t",
                             dec = ".")[, 1:7]
    cat(crayon::blue(nrow(methodList), ": methods found\n"))
    if(ID > 0) {
      idx <- which(methodList$method_id == ID)
      res <- c(res, methodList$method_name[idx])
    } else {
      cat(crayon::blue(paste(methodList$method_name, "\n")), '\n')
    }
  }
  if("matrixID" %in% names(what)) {
    matrixListURL <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vQs_oa0dMN6RE837QQ6GgQHFQr85SnjJZExxBarrWhEClCqHWEjEEp_bhtNwpbAlWpucbda9RhrkWTp/pub?gid=481322438&single=true&output=tsv"

    url <- matrixListURL
    ID <- what[which(names(what)=="matrixID")]
    if (!is.numeric(ID)) {stop("request >> getDevice >> ID should be numeric")}

    matrixList <- read.table(url,
                             header = TRUE,
                             sep = "\t",
                             dec = ".")[, 1:4]
    cat(crayon::blue(nrow(matrixList), ": matrices found\n"))
    if(ID > 0) {
      idx <- which(matrixList$matrix_id == ID)
      res <- c(res, matrixList$matrix_name[idx])
    } else {
      cat(crayon::blue(paste(matrixList$matrix_name, "\n")), '\n')
    }
  }
  return(res)

}
