

sdlHeaders <-  c("sampleID",
                 "projectName",
                 "cohortName",
                 "receptionDate",
                 "sampleMatrixType",
                 "sampleVolume",
                 "tubeLabel",
                 "sourceID",
                 "sampleTimePoint",
                 "sampleAliquots",
                 "boxName",
                 "boxType",
                 "tubePosition",
                 "row",
                 "col",
                 "collectionTube",
                 "comments")

sslHeaders <- c("plateID",
                "wellPos",
                "wellRC", sdlHeaders)

checkHeaders <- function(df, type = "sdl") {
  minHeaders <- switch(type,
                       "sdl" = sdlHeaders,
                       "ssl" = sslHeaders)
  #check minHeaders
  fi <- minHeaders %in% colnames(df)
  if (sum(fi) < length(minHeaders)) {
    cat(crayon::red(paste("request >> missing headers", minHeaders[!fi], "\n")))
  }
  # check for additional headers
  hdr <- setdiff(colnames(df), minHeaders)
  if (length(hdr) > 0) {
    cat(crayon::blue("request >> aditional headers:\n"), paste0(hdr, "\n"))
  }
}


checkForWeirdCharacters <- function(df) {
  for (i in 1:ncol(df)) {
    wc <- grep("[^A-Za-z0-9\\W#]", df[,i])
    if (length(wc) == 0) {
      # cat(crayon::green("no weird characters found\n"))
    } else {
      cat(crayon::yellow(paste("weird character found in", colnames(df)[i], "\n")))
    }
  }
}
