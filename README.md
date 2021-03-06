
# request

<!-- badges: start -->
[![Codecov test coverage](https://codecov.io/gh/jwist/request/branch/master/graph/badge.svg)](https://codecov.io/gh/jwist/request?branch=master)
[![Travis build status](https://travis-ci.com/jwist/request.svg?branch=master)](https://travis-ci.com/jwist/request)
<!-- badges: end -->

The goal of request is to ...

## Installation

You can install the released version of request from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("request")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(request)
## basic example code

sampleList$sampleID
sampleList$samplePositionInBox
sampleList$boxID

requestList <- makeRL(sampleList, "method")

  slots <- getSampleSlots(numberOfSamples, "method")
  slots$plateNumber
  slots$slotPosition

  newPositions <- getPositions(sampleList, slots)
  newPositions$samplePositionOnPlate
  
requestList$sampleID
requestList$samplePositionInBox
requestList$boxID
requestList$samplePositionOnPlate
requestList$plateNumber

runOrder <- makeRO(requestList, "method")


method@availableSlots = list()
method@methodSpecificSamples = list(sample, position)

```

