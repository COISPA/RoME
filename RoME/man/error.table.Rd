\name{error.table}
\alias{error.table}
\title{
Summary table of the errors
}
\description{
Function generating the error summary table.
}
\usage{
error.table(check.df,check_without_errors,
check_without_warnings,checkName,table,Field,yea)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{check.df}{data frame of the checks}
  \item{check_without_errors}{boolean variable reporting if errors were detected}
  \item{check_without_warnings}{boolean variable reporting if warnings were detected}
  \item{checkName}{string of the check name}
  \item{table}{reference table checked by the function}
  \item{Field}{field checked by check dictionary}
  \item{yea}{reference year}
}
\details{
The function generate the summary table of the errors detected by the \code{RoMEcc} function.
}
\value{
The output of the function is the data frame \code{check.df} updated with the result of the check.
}
\references{Anonymus. 2017. MEDITS-Handbook. Version n. 9. MEDITS Working Group. 106 pp.https://www.sibm.it/MEDITS 2011/principaledownload.htm}
\author{I. Bitetto, W. Zupa}
