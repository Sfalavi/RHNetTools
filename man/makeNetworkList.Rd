% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RHSNetCreation.r
\name{makeNetworkList}
\alias{makeNetworkList}
\title{Creates a w length list of 'Matrix' class networks,
    plus (if requested) the sidHID dataframe as the w+1st element.}
\usage{
makeNetworkList(
  rlpNet,
  pRlpName,
  pWavVec = c(1),
  pTypOut = c("MX"),
  includeAltID = FALSE,
  not_active_code = 10
)
}
\arguments{
\item{rlpNet}{A 5-column tbl_df Columns are SID, HID, WID, AltID, and
a final column with the name of one of the 6 types of relationships
measured in this study. Legal values are friend, loan, help, conv,
advc, rate. This dataframe should have edges for all of the waves
included in the parameter 'pWavVec', but if it does not have any
for a particular wave, that element of the output list will simply
be empty.}

\item{pRlpName}{The name of the relationship measured by this set of
networks; see 'makeSparse' for the allowed values. This column
normally will contain scored (0/1) values, though there is no
requirement in this function for this to be true.}

\item{pWavVec}{(default c(1)) A w-length vector with the numbers of
the waves to create list elemenets for.}

\item{pTypOut}{(default c("MX")) Character sting: MX for matrix, NT for
class 'network'.}

\item{includeAltID}{(default FALSE)}

\item{not_active_code}{Default=10. This is the code put into the adjacency
matrix if the individual represented by that row & column did not
contribute network data during wave 'pWave'}
}
\value{
Returns a list of networks in matrix form, one element per
    unique wave appearing in the WID column of pinTB
}
\description{
w is the number of waves, with one network per wave. Elements of the list
     are named "wv1", "wv2" (the specific wave numbers that appear in
     pinTB$WID).
}
\note{
For internal use only (called by 'makeNetworkSet'
}
