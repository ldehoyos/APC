% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/runSCwr.R
\name{runSCwr}
\alias{runSCwr}
\title{Compute and compare the structural covariance matrices & output the text files.}
\usage{
runSCwr(data1, data2, fname1, fname2)
}
\arguments{
\item{data1}{Data frame 1 with information to be converted into a SC matrix. Do not input subject ID, only metrics values.}

\item{data2}{Data frame 2 with information to be converted into a SC matrix. Do not input subject ID, only metrics values.}

\item{fname1}{Character string with the name of the file to save the SC matrix of data1.}

\item{fname2}{Character string with the name of the file to save the SC matrix of data2.}
}
\value{
Matrix of p-values.
}
\description{
This function creates the structural covariance (SC) matrices from the two input datasets (data1, data2) and then compares them using Z-scores. The function then outputs a matrix with p-values that indicate which differences between the two matrices are significant. To do the comparison, the function substracts data1 to data2.
}
\author{
Lucia de Hoyos <luciadeh@gmail.com>
}
