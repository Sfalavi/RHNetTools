% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RHSConChecksFixes.r
\name{checkForNoAlters}
\alias{checkForNoAlters}
\title{Check individuals who were only egos in a house (never alters).}
\usage{
checkForNoAlters(rlpNet, pWave = 0)
}
\arguments{
\item{rlpNet}{(tbl_df) A tbl in 'longNet' or 'rlpNet' format. Can include
multiple waves but only one will be processed (see 'pWave' doc).}

\item{pWave}{(scaler, default=0) The wave to select. If default, function
assumes rlpNet is for 1 wave only, & that is the wave to use. Otherwise
the function will throw an error.}
}
\value{
A tbl_df containing SID, HID, and WID for any individuals &
   waves where the individual did a survey, but never appears as an
   alter (AltID) in the input table.
}
\description{
Check individuals who were only egos in a house (never alters).
}
\details{
Because house residents are asked to rate all other residents on
   study relationship criteria, there should never be a case where someone
   did a survey, but was not named as an alter by anyone. This check also
   covers the situation where only 1 individual was surveyed in a house
   (in that instance, there will be just 1 ego, though possibly
   multiple alters)
}
\note{
Only within a wave does it make sense to assume that all egos should
   be chosen by at least some (same-house) alters.
}
