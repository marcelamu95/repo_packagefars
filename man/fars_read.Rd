% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions.R
\name{fars_read}
\alias{fars_read}
\title{Title 1 fars_read}
\source{
extdate/accident_year.csv.bz2
}
\usage{
fars_read(filename)
}
\arguments{
\item{filename}{a file path that may or may not exist}
}
\value{
a dplyr tbl_df of the data or an error message if the file path does not exist
}
\description{
This function receives a .csv file and returns tbl_df of the data
This function print an error message when  .csv file does not exist
}
\examples{

x <- setwd(system.file("extdata", package = "packagefars"))
fars_read("accident_2013.csv.bz2")
setwd(x)
}
