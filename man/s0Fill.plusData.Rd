% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RHSNetCreation.r
\name{s0Fill.plusData}
\alias{s0Fill.plusData}
\title{Return a function with data used to zero-fill a single-wave network}
\usage{
s0Fill.plusData(hids, mm, sidhid)
}
\arguments{
\item{hids}{(vector) The unique house IDs applicable to the zero-fill
operation in progress. Normally this will be obtained from one of the
columns (2,...w+1) of an object as created by 'makeSIDInAnyWave'.}

\item{mm}{(matrix) The adjacency matrix to be zero-filled.}

\item{sidhid}{(tbl) A two-column tbl (tbl_df) of SIDs and their HIDs for
the current wave. There should be no missing values; anyone missing a
HID would need to have been removed prior to calling this function.}
}
\value{
(function) A closure, which has the values of the input parameters
  as part of its execution environment. It is designed to be called
  repeatedly by 's0Fill', once for each wave to be zero-filled, each time
  returning a zero-filled matrix otherwise equivalent to the input 'mm'.
NOT EXPORTED <<
}
\description{
Return a function with data used to zero-fill a single-wave network
}
