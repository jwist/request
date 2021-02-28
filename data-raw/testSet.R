load("./data-raw/selectedSamples.rda")
nmr <- getRun(selectedSamples, "NMR", 1, 4, 19, "cambridgeFollowUP", "covid19", options = list(date = "280221"))
dire <- getRun(selectedSamples, "NMR", 1, 5, 24, "cambridgeFollowUP", "covid19", options = list(date = "280221"))
try <- getRun(selectedSamples, "MS-TRY", 1, 16, 25, "cambridgeFollowUP", "covid19", options = list(date = "280221"))
aa <- getRun(selectedSamples, "MS-AA", 1, 11, 17, "cambridgeFollowUP", "covid19", options = list(date = "280221"))
urpp <-getRun(selectedSamples, "MS-URPP", 1, 21, 26, "cambridgeFollowUP", "covid19", options = list(date = "280221"))
lipids <- getRun(selectedSamples, "MS-LIPIDS", 1, 30, 27, "cambridgeFollowUP", "covid19", options = list(date = "280221"))
testSet <- list(selectedSamples = selectedSamples,
                        nmr = nmr,
                        dire = dire,
                        try = try,
                        aa = aa,
                        urpp = urpp,
                        lipids = lipids)
usethis::use_data(testSet, overwrite = TRUE, internal = TRUE)
