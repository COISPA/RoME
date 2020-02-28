\name{check_sex_inversion}
\alias{check_sex_inversion}
\title{
Function to verify the consistency of sex information about hermaphrodite species.
}
\description{
The user will be informed if information about sex-inversion size for hermaphrodite species (at the moment only for Pagellus spp. and Spicara spp.) is stored in Maturity parameters dataframe.}
\usage{
check_sex_inversion(DataTC,Maturity_parameters)
}
\arguments{
\item{DataTC}{alternatively: Biological data table (TC) and Individual data table (TE).}
\item{wd}{Working directory selected by the user.}
\item{suffix}{Suffix string of the Logfile.}
}
\details{
This last check is performed only for proteroginic and proterandrous target species; the user will be only informed if information about sex-inversion size for hermaphrodite species (at the moment only for Pagellus spp. and Spicara spp.) is present in Maturity parameters.csv (folder Tables) in order to be used by the user to check the sex data of these species.
}
\value{
The function returns always TRUE, because the outcome of the function is a warning that does not lock the execution of the RoME checks.
}
\references{
Anonymus. 2017. MEDITS-Handbook. Version n. 9. MEDITS Working Group. 106 pp. http://www.sibm.it/MEDITS 2011/principaledownload.htm
}
\author{
I. Bitetto, W. Zupa
}
\examples{
library(MEDITS)
library(RoME)
wd=tempdir()
suffix= "27-02-2020 18:30"
check_sex_inversion(TC,Maturity_parameters)
}
\keyword{sex inversion, hermaphrodite}
