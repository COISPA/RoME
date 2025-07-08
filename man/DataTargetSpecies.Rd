\name{DataTargetSpecies}
\alias{DataTargetSpecies}
\docType{data}
\title{
Length and weight ranges for relevant species
}
\description{
Data frame containing information about length and individual weight ranges for relevant species, including quantiles and various statistics. It also contains metadata about faunistic category and period of relevance as target species.
}
\usage{
data("DataTargetSpecies")
}
\format{
A data frame with 392 observations on the following 26 variables:
\describe{
  \item{\code{SPECIES}}{Rubincode species identifier (character).}
  \item{\code{WMIN}}{Minimum observed individual weight (numeric).}
  \item{\code{WMIN05}}{5th percentile of individual weight (numeric).}
  \item{\code{WMIN10}}{10th percentile of individual weight (numeric).}
  \item{\code{WMIN25}}{25th percentile of individual weight (numeric).}
  \item{\code{WMAX_75}}{75th percentile of individual weight (numeric).}
  \item{\code{WMAX90}}{90th percentile of individual weight (numeric).}
  \item{\code{WMAX95}}{95th percentile of individual weight (numeric).}
  \item{\code{WMAX}}{Maximum observed individual weight (numeric).}
  \item{\code{LMIN}}{Minimum observed length (numeric, in mm).}
  \item{\code{LMIN01}}{1st percentile of length (numeric, in mm).}
  \item{\code{LMIN05}}{5th percentile of length (numeric, in mm).}
  \item{\code{LMIN10}}{10th percentile of length (numeric, in mm).}
  \item{\code{LMIN25}}{25th percentile of length (numeric, in mm).}
  \item{\code{LMAX75}}{75th percentile of length (numeric, in mm).}
  \item{\code{LMAN90}}{90th percentile of length calculated via mean or other method (numeric, in mm).}
  \item{\code{LMAX95}}{95th percentile of length (numeric, in mm).}
  \item{\code{LMAX99}}{99th percentile of length (numeric, in mm).}
  \item{\code{LMAX}}{Maximum observed length (numeric, in mm).}
  \item{\code{obs_in_TC}}{Number of observations found in TC data (numeric).}
  \item{\code{FAUNISTIC_CATEGORY}}{Old faunistic categories (character).}
  \item{\code{START_YEAR}}{Year when the species started to be considered target (numeric).}
  \item{\code{END_YEAR}}{Year when the species stopped being considered target (numeric).}
  \item{\code{GROUP}}{Species group or classification (character).}
}
}
\source{
Literature and other data sources.
}
\examples{
data(DataTargetSpecies)
head(DataTargetSpecies)
}
\keyword{datasets}
