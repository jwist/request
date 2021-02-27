test_that("device name is correct", {
  expect_equal(getDevice(c(deviceID = 4))[[1]], "IVDR04")
})

test_that("device list is correct", {
  expect_equal(getDevice(c(deviceID = 0)), list())
})

test_that("method list is correct", {
  expect_equal(getDevice(c(methodID = 0)), list())
})

test_that("method is correct", {
  expect_equal(getDevice(c(methodID = 1))[[1]], "Lip")
})
