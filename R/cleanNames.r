#' cleaning names for automation (no underscore is allowed as
#' it is a separator in long names)
#' @param names - a name or an array of
#' @return clean name(s)
#' @examples
#' originalID <- c("ddd.aaa", "dd_aa", "dad*", "ddd uuu", "ddd+aaa", "ddd*yyy")
#' cleanNames(originalID)
#' @export
cleanNames <- function(names) {
  names <- tolower(names)
  names <-gsub("[*]$", "-S", originalID)
  names <-gsub("[*]", "T", originalID)
  names <-gsub("[+]", "P", originalID)
  names <-gsub("[^A-Za-z0-9]", "-", originalID)
  return(names)
}

