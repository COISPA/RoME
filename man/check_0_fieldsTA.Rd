\name{check_0_fieldsTA}
\alias{check_0_fieldsTA}
\title{Checks the presence of 0 fields in TA}
\description{
The function checks the presence of 0 fields in the following haul data table (TA, according to MEDITS protocol) fields: WING_OPENING, WARP_DIAMETER  and VERTICAL_OPENING}
\usage{check_0_fieldsTA(DataTA,wd,suffix,year)}
\arguments{
    \item{DataTA}{Haul data table according to MEDITS protocol (TA)}
    \item{wd}{working directory path defined by the user}
    \item{suffix}{Suffix string of the Logfile}
    \item{year}{reference year for the analysis}
    }

\value{The function returns a boolean value. It is FALSE in case 0 values are detected in the TA table's fields}
\references{Anonymus (2017) "MEDITS-Handbook. Version n. 9. MEDITS Working Group" \href{http://www.sibm.it/MEDITS 2011/principaledownload.htm}{http://www.sibm.it/MEDITS 2011/principaledownload.htm}}
\author{Isabella Bitetto [aut,cre] <bitetto@coispa.it>;
        Walter Zupa [aut, cre] <zupa@coispa.it>}
\examples{
wd <- tempdir()
suffix="2020-03-05_time_h17m44s55"
check_0_fieldsTA(RoME::TA,wd,suffix, year=2007)
}

\keyword{ error }

