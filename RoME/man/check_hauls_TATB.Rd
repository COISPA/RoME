\name{check_hauls_TATB}
\alias{check_hauls_TATB}
\title{Check of TB hauls in TA}
\description{
The function check the presence of the TB (catch data table) hauls in the TA (haul data table)
}
\usage{
check_hauls_TATB(DataTA,DataTB,wd,suffix)
}
\arguments{
  \item{DataTA}{Haul data table according to MEDITS protocol (TA)}
  \item{DataTB}{Catch data table according to MEDITS protocol (TB)}
  \item{wd}{working directory path defined by the user}
  \item{suffix}{Suffix string of the Logfile}
}
\details{The function check the presence of the TB (catch data table) hauls in the TA (haul data table)}
\value{
The function returns TRUE if no error occurs, while FALSE is returned when there are differences in the AREA code among the TX tables.
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
