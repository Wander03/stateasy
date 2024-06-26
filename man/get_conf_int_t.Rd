% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/T-Test.R
\name{get_conf_int_t}
\alias{get_conf_int_t}
\title{Calculate a confidence interval for a one-sample t-test}
\usage{
get_conf_int_t(data_vec, conf_level)
}
\arguments{
\item{data_vec}{A numeric vector representing the sample data.}

\item{conf_level}{A number between 0 and 1 specifying the confidence level
for the interval (default is 0.95).}
}
\value{
A dataframe with columns for the confidence level, lower bound, and
upper bound of the confidence interval.
}
\description{
This function calculates a confidence interval for the population mean of
the specified data vector using a one-sample t-test.
}
