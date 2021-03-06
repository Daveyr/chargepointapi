% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ncr.R
\name{ncr_count}
\alias{ncr_count}
\title{Fetches the number of charge points  within a search radius}
\usage{
ncr_count(lat, long, radius = 1, units = "mi")
}
\arguments{
\item{lat}{latitude in decimal degrees}

\item{long}{longitude in decimal degrees}

\item{radius}{search radius}

\item{units}{unit of radius (default is miles, the alternative is "km")}
}
\description{
Fetches the number of charge points  within a search radius
}
\examples{
ncr_count(51.545581, -0.077301)
ncr_count(51.545581, -0.077301, radius = 2.5, units = "km")
}
