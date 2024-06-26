% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/slr.R
\name{create_scatter_plot}
\alias{create_scatter_plot}
\title{This function creates scatter plot for simple linear regression.}
\usage{
create_scatter_plot(data, x, y, intercept = TRUE)
}
\arguments{
\item{data}{A dataframe containing the data.}

\item{x}{Predictor variable}

\item{y}{Response variable}

\item{intercept}{True if the intercept should be used, False otherwise}
}
\value{
Plotly scatter plot for provided lm model.
}
\description{
This function creates scatter plot for simple linear regression.
}
