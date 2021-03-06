% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RHSNetCreation.r
\name{makeLongNet}
\alias{makeLongNet}
\title{Create long-format tibble from DT converted from SPSS file}
\usage{
makeLongNet(pInTB)
}
\arguments{
\item{pInTB}{A 'wide' tibble created by 'inputSPSS', or equivalent format.}
}
\value{
A tibble with multiple rows per 'chooser' SID, one for each
    chosen alter. The row format is:
    SID -- chooser's (ego's) ID
    HID -- chooser & chosen's House ID
    WID -- wave ID
    AltID -- chosen (alter's) ID
    friend -- 'friend' relationship (1=close,...5=advesary)
    loan -- 'would loan $' relationahip (1=$0,...,5=$500)
    help -- 'would help' relationship (1=very likely,...5=no)
    conv -- 'freq conversation' relationship (1=daily,...5=almost never)
    advc -- 'receive advice' relationship (1=very often,...5=never)
    rate -- 'strength' of relationship (1=very strong,...5=negative)
}
\description{
This tibble is an edge-list format of choosers x chosen (for each of the
   relationship criteria friend, loan, help, conv, advc, and rate.
}
