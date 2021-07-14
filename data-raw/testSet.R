load("./data-raw/selectedSamples.rda")
nmr <- getRun(selectedSamples, "NMR", 1, 4, 19, "cambridgeFollowUP", "covid19", options = list(date = "280221"))
dire <- getRun(selectedSamples, "NMR", 1, 5, 24, "cambridgeFollowUP", "covid19", options = list(date = "280221"))
try <- getRun(selectedSamples, "MS-TRY", 1, 16, 25, "cambridgeFollowUP", "covid19", options = list(date = "280221"))
bile <- getRun(selectedSamples, "MS-BILE", 1, 22, 15, "cambridgeFollowUP", "covid19", options = list(date = "280221"))
aa <- getRun(selectedSamples, "MS-AA", 1, 11, 17, "cambridgeFollowUP", "covid19", options = list(date = "280221"))
urpp <-getRun(selectedSamples, "MS-URPP", 1, 21, 26, "cambridgeFollowUP", "covid19", options = list(date = "280221"))
lipids <- getRun(selectedSamples, "MS-LIPIDS",
                 matrixID = 1,
                 deviceID = 30,
                 methodID = 27,
                 "cambridgeFollowUP",
                 "covid19",
                 options = list(date = "280221"))
eicosanoids <- getRun(selectedSamples, "MS-EICOS",
                      matrixID = 1,
                      deviceID = 29,
                      methodID = 16,
                      "cambridgeFollowUP",
                      "covid19",
                      options = list(date = "280221"))
mrmsn <- getRun(selectedSamples, "MS-MRMSN", 1, 27, 29, "cambridgeFollowUP", "covid19", options = list(date = "280221"))
mrmsp <- getRun(selectedSamples, "MS-MRMSP", 1, 27, 28, "cambridgeFollowUP", "covid19", options = list(date = "280221"))
timsLipidsP <- getRun(selectedSamples, "TIMS-LIPIDS-P", 1, 25, 30, "cambridgeFollowUP", "covid19", options = list(date = "280221"))
timsLipidsN <- getRun(selectedSamples, "TIMS-LIPIDS-N", 1, 25, 31, "cambridgeFollowUP", "covid19", options = list(date = "280221"))
testSet <- list(selectedSamples = selectedSamples,
                nmr = nmr,
                dire = dire,
                try = try,
                bile = bile,
                aa = aa,
                urpp = urpp,
                lipids = lipids,
                eicosanoids = eicosanoids,
                mrmsn = mrmsn,
                mrmsp = mrmsp,
                timsLipidsN = timsLipidsN,
                timsLipidsP = timsLipidsP)
usethis::use_data(testSet, overwrite = TRUE, internal = TRUE)
