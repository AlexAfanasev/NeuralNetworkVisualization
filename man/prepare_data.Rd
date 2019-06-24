% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotting_data_preparation.R
\name{prepare_data}
\alias{prepare_data}
\title{Prepares the data used for plotting.}
\usage{
prepare_data(neural_net, predictor, probs = c(0.05, 0.95))
}
\arguments{
\item{neural_net}{Fitted NeuralNetwork, see: \code{\link{NeuralNetwork}}}

\item{predictor}{Predictor for which to prepare the plotting data
(predictor should be transformed with sym)}

\item{probs}{Vector of 2 probabilities for the confidence intervals}
}
\value{
Data.frame with the prepared data. This data will be used for
  plotting.
}
\description{
\code{prepare_data} Returns the prepared data used for plotting.
}
\keyword{internal}