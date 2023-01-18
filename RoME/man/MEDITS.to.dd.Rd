\name{MEDITS.to.dd}
\alias{MEDITS.to.dd}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Conversion of MEDITS format coordinates in decimal degrees format}
\description{
Conversion of MEDITS format coordinates in decimal degrees format
}
\usage{MEDITS.to.dd(data)}

\arguments{
  \item{data}{data frame of the hauls data (TA, table A) in MEDITS format}
}

\value{
The function returns the data frame of the TA table with the coordinates expressed as decimal degrees.
}
\author{Walter Zupa}

\examples{MEDITS.to.dd(TA)}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{coordinates}% use one of  RShowDoc("KEYWORDS")
\keyword{TA}% __ONLY ONE__ keyword per line
