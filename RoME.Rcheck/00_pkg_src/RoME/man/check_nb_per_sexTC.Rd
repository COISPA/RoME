\name{check_nb_per_sexTC}
\alias{check_nb_per_sexTC}
\title{Consistency check of number of individuals}
\description{
The function checks the consistency of the number of individuals by sex measured (NO_OF_INDIVIDUAL_OF_THE_ABOVE_SEX_MEASURED field in the biological data table, TC) with the sum of the individuals by sex, length class and maturity stage (NUMBER_OF_INDIVIDUALS_IN_THE_LENGTH_CLASS_AND_MATURITY_STAGE field in TC)}
\usage{
check_nb_per_sexTC(DataTC, wd, suffix)
}
\arguments{
    \item{DataTC}{Biological data table according to MEDITS protocol (TC)}
    \item{wd}{working directory path defined by the user}
    \item{suffix}{Suffix string of the Logfile}
}
\details{The function checks the consistency of the number of individuals by sex measured (NO_OF_INDIVIDUAL_OF_THE_ABOVE_SEX_MEASURED field in the biological data table, TC) with the sum of the individuals by sex, length class and maturity stage (NUMBER_OF_INDIVIDUALS_IN_THE_LENGTH_CLASS_AND_MATURITY_STAGE field in TC)}
\value{
The function returns TRUE if no error occurs, while FALSE is returned when there inconsistences between the following biological data table (TC): NO_OF_INDIVIDUAL_OF_THE_ABOVE_SEX_MEASURED and NUMBER_OF_INDIVIDUALS_IN_THE_LENGTH_CLASS_AND_MATURITY_STAGE.If the field number per sex is found completely empty, the routine will stop and will produce automatically a .csv file (TC_file_with_computed_nb_per_sex.csv) with the nb per sex column filled in. The user will have to copy and paste the column in the original file and run again the code.
}
\references{Anonymus. 2017. MEDITS-Handbook. Version n. 9. MEDITS Working Group. 106 pp.https://www.sibm.it/MEDITS 2011/principaledownload.htm}
\author{I. Bitetto, W. Zupa}
\examples{
    library(RoME)
    wd <- tempdir()
    suffix="27-02-2020 18:30"
    DataTC = MEDITS::TC
    check_nb_per_sexTC(DataTC,wd,suffix)
}
\keyword{error}
