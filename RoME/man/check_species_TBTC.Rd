\name{check_species_TBTC}
\alias{check_species_TBTC}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function checking if all the target species in the catch data table (TB) are in Biological data table (TC)}
}
\description{
This function verifies the presence of the target species (that is a subset of the all the species caught, reported in TB), in the TC table, where additional information (apart from number and weight) are collected.}
\usage{
check_species_TBTC(ResultTB,ResultTC,DataTargetSpecies,wd,suffix)
}
\arguments{
\item{ResultDataTB}{Catch data table(TB).}
\item{ResultDataTC}{Biological data table (TC).}
\item{DataTargetSpecies}{Information related to target species.}
\item{wd}{Working directory selected by the user.}
\item{suffix}{Suffix string of the Logfile.}
}
}
\details{
This function gives a warning message, thus the execution is not stopped when some target species are lacking in TC; the user is informed in the Logfile.
}
\value{
The function returns always TRUE, because the outcome of the function is a warning that does not lock the execution of the RoME checks.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x)
{
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
