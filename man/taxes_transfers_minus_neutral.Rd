% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/contributions.R
\name{taxes_transfers_minus_neutral}
\alias{taxes_transfers_minus_neutral}
\title{Subtract counterfactual taxes and transfers from realized taxes and transfers}
\usage{
taxes_transfers_minus_neutral(df)
}
\arguments{
\item{df}{}
}
\value{
Difference between realized and counterfactual paths for taxes and transfers.
We assume that taxes and transfers would grow with potential gdp. Therefore,
the counterfactual growth is 'neutral' since it wouldn't deviate from the
potential growth of the economy.
}
\description{
Subtract counterfactual taxes and transfers from realized taxes and transfers
}
