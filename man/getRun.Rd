% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getRun.r
\name{getRun}
\alias{getRun}
\title{create configuration files}
\usage{
getRun(
  selectedSamples,
  type,
  matrixID,
  deviceID,
  methodID,
  cohortName,
  projectName,
  options = list()
)
}
\arguments{
\item{selectedSamples}{\itemize{
\item the selected samples for run
}}

\item{type}{\itemize{
\item the type of run
}}

\item{matrixID}{\itemize{
\item the id of the samples matrix
}}

\item{deviceID}{\itemize{
\item the id of the device to run the experiments
}}

\item{methodID}{\itemize{
\item the id of the method to be run
}}

\item{cohortName}{\itemize{
\item the name of the cohort to run
}}

\item{projectName}{\itemize{
\item the name of the project
}}

\item{options}{\itemize{
\item options
}}
}
\value{
the name of the device
}
\description{
create configuration files
}
