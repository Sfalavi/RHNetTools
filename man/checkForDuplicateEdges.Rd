% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RHSConChecksFixes.r
\name{checkForDuplicateEdges}
\alias{checkForDuplicateEdges}
\title{Checks long-format data (output from 'makeLongNet') for duplicate edges}
\usage{
checkForDuplicateEdges(rlpNet, pWave = 0)
}
\arguments{
\item{rlpNet}{(tbl_df) Long-format edge list of the form 'longNet' or
'rlpNet.}

\item{pWave}{(scaler, default=0) The wave to select. If default, function
assumes rlpNet is for 1 wave only, & that is the wave to use. Otherwise
the function will throw an error.}
}
\value{
Outputs a tbl_df showing duplicated rows, in the same format
   as 'rlpNet'
}
\description{
Checks long-format data (output from 'makeLongNet') for duplicate edges
}
