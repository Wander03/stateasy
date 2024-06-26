% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/proportion-test.R
\name{get_test_result}
\alias{get_test_result}
\title{Calculate the result of a one-sample z test for a population proportion}
\usage{
get_test_result(data_vec, hypo_p, hypo_direction)
}
\arguments{
\item{data_vec}{A vector of TRUE/FALSE or 0/1 data representing the sample}

\item{hypo_p}{The hypothesized value of the proportion}

\item{hypo_direction}{The direction of the alternative hypothesis}
}
\value{
A data frame containing the test statistic and p-value
}
\description{
Calculate the result of a one-sample z test for a population proportion
}
