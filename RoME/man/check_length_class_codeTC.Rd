\name{check_length_class_codeTC}
\alias{check_length_class_codeTC}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
check_length_class_codeTC(DataTC, Specieslist = NA, wd, suffix)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{DataTC}{
%%     ~~Describe \code{DataTC} here~~
}
  \item{Specieslist}{
%%     ~~Describe \code{Specieslist} here~~
}
  \item{wd}{
%%     ~~Describe \code{wd} here~~
}
  \item{suffix}{
%%     ~~Describe \code{suffix} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
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
function (DataTC, Specieslist = NA, wd, suffix) 
{
    if (FALSE) {
        library(MEDITS)
        wd <- tempdir()
        Specieslist = NA
        suffix = paste(as.character(Sys.Date()), format(Sys.time(), 
            "_time h\%Hm\%Ms\%OS0"), sep = "")
        DataTC = read.csv("~/GitHub/RoME/data/TC_GSA18_1994-2018.csv", 
            sep = ";")
        DataTC <- DataTC[DataTC$YEAR == 2018, ]
    }
    if (!file.exists(paste(wd, "Logfiles", sep = "/"))) {
        dir.create(file.path(wd, "Logfiles"), showWarnings = FALSE)
    }
    numberError = 0
    if (!exists("suffix")) {
        suffix = paste(as.character(Sys.Date()), format(Sys.time(), 
            "_time h\%Hm\%Ms\%OS0"), sep = "")
    }
    Errors <- paste(wd, "/Logfiles/Logfile_", suffix, ".dat", 
        sep = "")
    ResultData = DataTC
    write(paste("\n----------- check correctness of LENGTH_CLASSES_CODE in TC - ", 
        ResultData$YEAR[1]), file = Errors, append = TRUE)
    if (is.na(Specieslist)) {
        ResultSpecies <- RoME::TM_list
    }
    else {
        ResultSpecies <- Specieslist
    }
    if (nrow(ResultData) != 0) {
        j = 1
        for (j in 1:nrow(ResultData)) {
            FoundSpecies = ResultSpecies[as.character(ResultSpecies$MeditsCode) == 
                paste(as.character(ResultData$GENUS[j]), as.character(ResultData$SPECIES[j]), 
                  sep = ""), ]
            if (length(FoundSpecies$CODLON[1]) != 0) {
                if (as.character(FoundSpecies$CODLON[1]) != as.character(ResultData$LENGTH_CLASSES_CODE[j])) {
                  write(paste("Haul", ResultData$HAUL_NUMBER[j], 
                    ": code species", as.character(ResultData$GENUS[j]), 
                    as.character(ResultData$SPECIES[j]), "wrong LENGTH_CLASSES_CODE according to MEDITS FM list in Tables directory"), 
                    file = Errors, append = TRUE)
                  numberError = numberError + 1
                }
            }
            else {
                write(paste("Warning: Haul", ResultData$HAUL_NUMBER[j], 
                  ": code species", as.character(ResultData$GENUS[j]), 
                  as.character(ResultData$SPECIES[j]), " species not present in TM list: LENGTH_CLASSES_CODE not verified."), 
                  file = Errors, append = TRUE)
            }
        }
    }
    if (numberError == 0) {
        write(paste("No error occurred"), file = Errors, append = TRUE)
    }
    if (numberError == 0) {
        return(TRUE)
    }
    else {
        return(FALSE)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
