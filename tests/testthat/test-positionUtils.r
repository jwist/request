test_that("check getPlatePos", {
  pos <- getPlatePos()
  expect_equal(pos[1], "A1")
  expect_equal(pos[12], "A12")
  expect_equal(pos[13], "B1")
  expect_equal(pos[96], "H12")
  expect_length(pos, 96)
})

test_that("check getPlatePos by col", {
  pos <- getPlatePos(by = "col")
  expect_equal(pos[1], "A1")
  expect_equal(pos[8], "H1")
  expect_equal(pos[9], "A2")
  expect_equal(pos[96], "H12")
  expect_length(pos, 96)
})

test_that("check posToRC", {
  pos <- getPlatePos()
  RC <- posToRC(pos)
  expect_equal(RC$row, rep(c(1:8), each = 12))
  expect_equal(RC$col, rep(1:12, times = 8))
})

test_that("check posToNum", {
  pos <- getPlatePos()
  num <- posToNum(pos)
  expect_length(num, 96)
  expect_equal(num, seq(1,96))
})

test_that("check posToNum by col", {
  pos <- getPlatePos(by = "col")
  num <- posToNum(pos)
  expect_length(num, 96)
  expect_equal(num[2], 13)
  expect_equal(num[9], 2)
})

test_that("check posToNum with other dimensions", {
  pos <- getPlatePos(boxDim = c(9, 9))
  num <- posToNum(pos, boxDim = c(9, 9))
  expect_length(num, 81)
  expect_equal(num, seq(1,81))
})

test_that("check numToPos", {
  pos <- getPlatePos()
  num <- posToNum(pos)
  newPos <- numToPos(num, boxDim = c(8, 12))
  expect_length(newPos, 96)
  expect_equal(pos, newPos)
})

test_that("check numToPos by col", {
  pos <- getPlatePos(by = "col")
  num <- posToNum(pos)
  newPos <- numToPos(num, boxDim = c(8, 12))
  expect_length(newPos, 96)
  expect_equal(pos, newPos)
})

test_that("check numToPos with other dimensions", {
  pos <- getPlatePos(boxDim = c(9, 9))
  num <- posToNum(pos, boxDim = c(9, 9))
  newPos <- numToPos(num, boxDim = c(9, 9))
  expect_length(newPos, 81)
  expect_equal(pos, newPos)
})

test_that("check RCtoPos", {
  pos <- getPlatePos()
  RC <- posToRC(pos)
  newPos <- RCToPos(RC$row, RC$col)
  expect_equal(pos, newPos)
  expect_equal(pos[96], "H12")
  expect_length(newPos, 96)
})

test_that("check RCtoPos with other boxDim", {
  pos <- getPlatePos(boxDim = c(9, 9))
  RC <- posToRC(pos)
  pos <- RCToPos(RC$row, RC$col)
  expect_equal(pos[1], "A1")
  expect_equal(pos[9], "A9")
  expect_equal(pos[10], "B1")
  expect_equal(pos[81], "I9")
  expect_length(pos, 81)
})
