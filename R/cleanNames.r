#' cleaning names for automation (no underscore is allowed as
#' it is a separator in long names)
#' @param names - a name or an array of
#' @return clean name(s)
#' @examples
#' originalID <- c("ddd.aaa", "ddd uuu", "ddd+aaa", "ddd*yyy", "ddd#dd", "ddd_fff")
#' originalID <-c(originalID, "ddd$ddd", "ddd@ddd", "dd_aa", "dad*")
#' cleanNames(originalID)
#' @export
cleanNames <- function(names) {
  names <- tolower(names)
  make.unique(names, sep = "#")
  names <-gsub("[*]$", "-S", names)
  names <-gsub("[*]", "T", names)
  names <-gsub("[+]", "P", names)
  # we remove all except # for replicates
  names <-gsub("[^A-Za-z0-9\\W#]", "-", names)
  # names <-gsub("[\\W_]", "-", names)
  # names <-gsub("[\\W ]", "-", names)
  return(names)
}


# cleanNames <- function(names) {
#   # names <- tolower(names)
#   make.unique(names, sep = "#")
#   names <-gsub("[*]$", "-S", names)
#   names <-gsub("[*]", "T", names)
#   names <-gsub("[+]", "P", names)
#   # we remove all except # for replicates
#   names <-gsub(" ", "_", names)
#   names <-gsub("[^A-Za-z0-9\\W#]", "_", names)
#   # names <-gsub("[\\W_]", "-", names)
#   # names <-gsub("[\\W ]", "-", names)
#   return(names)
# }
