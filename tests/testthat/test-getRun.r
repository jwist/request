test_that("get run for NMR", {
  run <- getRun(testSet$selectedSamples, "NMR", 1, 4, 19, "cambridgeFollowUP", "covid19", options = list(date = "280221"))
  nmr <- testSet$nmr
  expect_equal(run, nmr)
})

test_that("get run for dire", {
  run <- getRun(testSet$selectedSamples, "NMR", 1, 5, 24, "cambridgeFollowUP", "covid19", options = list(date = "280221"))
  nmr <- testSet$dire
  expect_equal(run, nmr)
})

test_that("get run for try", {
  run <- getRun(testSet$selectedSamples, "MS-TRY", 1, 16, 25, "cambridgeFollowUP", "covid19", options = list(date = "280221"))
  nmr <- testSet$try
  expect_equal(run, nmr)
  expect_equal(run[[2]]$SAMPLE_LOCATION[4], "1:A,11")
  expect_equal(run[[4]]$SAMPLE_LOCATION[4], "2:A,11")
})

test_that("get run for aa", {
  run <- getRun(testSet$selectedSamples, "MS-AA", 1, 11, 17, "cambridgeFollowUP", "covid19", options = list(date = "280221"))
  nmr <- testSet$aa
  expect_equal(run, nmr)
  expect_equal(run[[2]]$Vial[2], "1:D,12")
  expect_equal(run[[4]]$Vial[2], "2:D,12")
})

test_that("get run for urpp", {
  run <-getRun(testSet$selectedSamples, "MS-URPP", 1, 21, 26, "cambridgeFollowUP", "covid19", options = list(date = "280221"))
  nmr <- testSet$urpp
  expect_equal(run, nmr)
})

test_that("get run for lipids", {
  run <- getRun(testSet$selectedSamples, "MS-LIPIDS", 1, 30, 27, "cambridgeFollowUP", "covid19", options = list(date = "280221"))
  nmr <- testSet$lipids
  expect_equal(run, nmr)
  expect_equal(run[[2]]$PlatePos[1], 1)
  expect_equal(run[[4]]$PlatePos[1], 2)
})

test_that("get run for eicosanoids", {
  run <- getRun(testSet$selectedSamples, "MS-EICOS", 1, 29, 16, "cambridgeFollowUP", "covid19", options = list(date = "280221"))
  nmr <- testSet$eicosanoids
  expect_equal(run, nmr)
  expect_equal(run[[2]]$PlatePos[1], 1)
  expect_equal(run[[4]]$PlatePos[1], 2)
})

test_that("get run for MRMSN", {
  run <- getRun(testSet$selectedSamples, "MS-MRMSN", 1, 27, 29, "cambridgeFollowUP", "covid19", options = list(date = "280221"))
  nmr <- testSet$mrmsn
  expect_equal(run, nmr)
})

test_that("get run for MRMSP", {
  run <- getRun(testSet$selectedSamples, "MS-MRMSP", 1, 27, 28, "cambridgeFollowUP", "covid19", options = list(date = "280221"))
  nmr <- testSet$mrmsp
  expect_equal(run, nmr)
})

test_that("get run for TIMS-LIPIDS-P", {
  run <- getRun(testSet$selectedSamples, "TIMS-LIPIDS-P", 1, 25, 30, "cambridgeFollowUP", "covid19", options = list(date = "280221"))
  nmr <- testSet$timsLipidsP
  expect_equal(run, nmr)
})

test_that("get run for TIMS-LIPIDS-N", {
  run <- getRun(testSet$selectedSamples, "TIMS-LIPIDS-N", 1, 25, 31, "cambridgeFollowUP", "covid19", options = list(date = "280221"))
  nmr <- testSet$timsLipidsN
  expect_equal(run, nmr)
})
