\name{check_nb_per_sexTC}
\alias{check_nb_per_sexTC}
\title{
Consistency check between sum of NB_LON and NB_SEX}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
check_nb_per_sexTC(DataTC, wd, suffix)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{DataTC}{
%%     ~~Describe \code{DataTC} here~~
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
function (DataTC, wd, suffix)
{
    if (FALSE) {
        wd <- tempdir()
        DataSpecies = NA
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
    Result = DataTC
    write(paste("\n----------- check consistency of number per sex in TC - ",
        Result$YEAR[1]), file = Errors, append = TRUE)
    table1 <- aggregate(Result$NUMBER_OF_INDIVIDUALS_IN_THE_LENGTH_CLASS_AND_MATURITY_STAGE,
        by = list(Result$TYPE_OF_FILE, Result$COUNTRY, Result$AREA,
            Result$VESSEL, Result$YEAR, Result$HAUL_NUMBER, Result$CODEND_CLOSING,
            Result$PART_OF_THE_CODEND, Result$GENUS, Result$SPECIES,
            Result$LENGTH_CLASSES_CODE, Result$WEIGHT_OF_THE_FRACTION,
            Result$WEIGHT_OF_THE_SAMPLE_MEASURED, Result$SEX,
            Result$NO_OF_INDIVIDUAL_OF_THE_ABOVE_SEX_MEASURED),
        FUN = "sum")
    colnames(table1) <- c("TYPE_OF_FILE", "COUNTRY", "AREA",
        "VESSEL", "YEAR", "HAUL_NUMBER", "CODEND_CLOSING", "PART_OF_THE_CODEND",
        "GENUS", "SPECIES", "LENGTH_CLASSES_CODE", "WEIGHT_OF_THE_FRACTION",
        "WEIGHT_OF_THE_SAMPLE_MEASURED", "SEX", "NO_OF_INDIVIDUAL_OF_THE_ABOVE_SEX_MEASURED",
        "Sum")
    if (!all(is.na(table1$NO_OF_INDIVIDUAL_OF_THE_ABOVE_SEX_MEASURED))) {
        errori = (which(table1$NO_OF_INDIVIDUAL_OF_THE_ABOVE_SEX_MEASURED !=
            table1$Sum))
        if (length(errori) != 0) {
            for (j in 1:length(errori)) {
                write(paste("Haul", table1$HAUL_NUMBER[errori[j]],
                  table1$GENUS[errori[j]], table1$SPECIES[errori[j]],
                  table1$SEX[errori[j]], "number per sex not consistent with the sum of individuals"),
                  file = Errors, append = TRUE)
                numberError = numberError + 1
            }
        }
    }
    else {
        write("The column nb per sex has been found empty, then it was computed automatically.",
            file = Errors, append = TRUE)
        print("The column nb per sex has been found empty, then it was computed automatically and saved in the TC_file_with_computed_nb_per_sex.csv.Please, fill in the nb per sex field in the original file, using the file produced and run again the code.",
            quote = FALSE)
        for (k in 1:nrow(table1)) {
            ResultData[which(ResultData$GENUS == table1$GENUS[k] &
                ResultData$SPECIES == table1$SPECIES[k] & ResultData$HAUL_NUMBER ==
                table1$HAUL_NUMBER[k] & ResultData$COUNTRY ==
                table1$COUNTRY[k] & ResultData$codedsex == table1$codedsex[k] &
                ResultData$WEIGHT_OF_THE_FRACTION == table1$WEIGHT_OF_THE_FRACTION[k] &
                ResultData$WEIGHT_OF_THE_SAMPLE_MEASURED == table1$WEIGHT_OF_THE_SAMPLE_MEASURED[k]),
                which(names(ResultData) == "NO_OF_INDIVIDUAL_OF_THE_ABOVE_SEX_MEASURED")] = table1$Sum[k]
        }
        write.table(ResultData[, 1:ncol(ResultData) - 1], file = paste("TC_file_with_computed_nb_per_sex.csv",
            sep = ""), sep = ";", row.names = FALSE)
        numberError = numberError + 1
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
