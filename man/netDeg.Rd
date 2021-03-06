% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RHSDescriptives.r
\name{netDeg}
\alias{netDeg}
\title{Calculate average in + out ("Freeman") degrees across a whole network}
\usage{
netDeg(pNet)
}
\arguments{
\item{pNet}{('network') A 'network' class object (from pkg 'network').
Could be binary or non-binary.}
}
\value{
(numeric) A numeric variable that averages.
}
\description{
Calculate average in + out ("Freeman") degrees across a whole network
}
\details{
Wraps the sna function 'degree', which gives a vector of
the total in + out-degree values by node.
}
