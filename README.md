
# RoME

**Authors**: I. Bitetto, W. Zupa

COISPA Tecnologia & Ricerca, Stazione Sperimentale per lo Studio delle
Risorse del Mare

**RoME** package integrates a list of common quality checks on survey
data. The main function *RoME()* calls all the functions built in the
package in an ordered way to perform a complete quality check of TX data
available. The order of the checks in *RoME* was implemented in a
defined sequence to avoid cascade errors due to the correction of a
previous error. The function does not correct the data itself, but it
detects the errors, warning the user that there is the possibility of
one or more errors, specifying the type of the error and easing the data
correction. RoME stops if an error occurs; then the user has to correct
the error and run again the code to continue with the other checks. The
function can be used on a complete time series dataset, checking year
after year, until the end of the time series. After the checks of the
mandatory fields and the controlled vocabulary, that are carried out for
all the TX tables, the specific checks on each kind of TX table are
performed. Finally, *RoME* provides a list of cross checks aimed to
guarantee the consistency among the data tables. If parameter
*verbose=TRUE* returns a series of text output in console to let the
user to trace the state of the checks. All the output of the functions
are stored in the user defined working directory *wd* and in the
sub-directory there resident. In the Lofile subfolder are stored the
logfiles of each run of the function.

## Installation

You can install the released version of *RoME* package from GitHub with:

``` r
library(remotes)
remotes::install_git("https://github.com/COISPA/RoME")
```

## References

Anonymus. 2017. MEDITS-Handbook. Version n.9. MEDITS Working Group. 106
pp.[https://www.sibm.it/MEDITS
2011/principaledownload.htm](https://www.sibm.it/MEDITS%202011/principaledownload.htm)

## Use of RoME function

``` r
 library(RoME)

    wd <- tempdir()
    suffix=NA
    DataTA = data.frame(RoME::TA[RoME::TA$YEAR==2012 ,])
    DataTB = data.frame(RoME::TB[RoME::TB$YEAR==2012 ,])
    DataTC = data.frame(RoME::TC[RoME::TC$YEAR==2012 ,])
    DataTE = NA
    DataTL = NA
    RoME(DataTA, DataTB,DataTC,DataTE,DataTL, wd, suffix, verbose=FALSE)
```

## Use of RoMEcc function

``` r
 library(RoME)

    wd <- tempdir()
    suffix=NA
    DataTA = data.frame(RoME::TA[RoME::TA$YEAR==2012 ,])
    DataTB = data.frame(RoME::TB[RoME::TB$YEAR==2012 ,])
    DataTC = data.frame(RoME::TC[RoME::TC$YEAR==2012 ,])
    DataTE = NA
    DataTL = NA
    RoME(DataTA, DataTB,DataTC,DataTE,DataTL, wd, suffix, verbose=FALSE)
```
