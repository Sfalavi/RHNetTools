% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RHSNetCreation.r
\name{sid2rid}
\alias{sid2rid}
\title{Transforms vector of SIDs into vector of Row IDs}
\usage{
sid2rid(SIDVec, SIDRID)
}
\arguments{
\item{SIDVec}{(numerical vector) The SIDs to be transformed into Row IDs.
Normally this should be sorted in ascending numerical order, but
'row id's' will be assigned based on the vector elements' order. If any
of these values have no SID value in the 'SIDHID' table, the function
fails and returns an error message.}

\item{SIDRID}{(tbl_df) The SID-Row ID crosswalk to be used to assign
RIDs to the SIDs in SIDVec. The two columns must be named SID and RID,
respectively.}
}
\value{
(numerical vector) A vector of of the same length as SIDVec, with
  the SIDs replaced by Row IDs. The Row IDs are simply the order number
  of the SID element, sorted ascending.
}
\description{
This function is just a light wrapper on the 'match' function, and a little
easier to read.
}
