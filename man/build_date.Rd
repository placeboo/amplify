% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/build_date.R
\name{build_date}
\alias{build_date}
\title{Generate a continuous sequence dates}
\usage{
build_date(start.date, end.date)
}
\arguments{
\item{start.date}{A number represents a starting date}

\item{end.date}{A number represents a ending date}
}
\value{
A data frame with one column called date, and the date column displays a continouse sequence of dates from the starting date to the ending date.
}
\description{
Generate a continuous sequence dates
}
\examples{
build_date(20190101, 20190301)
}
