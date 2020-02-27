pkgname <- "RoME"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
base::assign(".ExTimings", "RoME-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('RoME')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("check_0_fieldsTA")
### * check_0_fieldsTA

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: check_0_fieldsTA
### Title: Checks the presence of 0 fields in TA
### Aliases: 'Checks 0 fields in TA'
### Keywords: error

### ** Examples

library(MEDITS)
wd <- tempdir()
suffix <- "27-02-2020 18:30"
ResultDataTA = read.csv("~/GitHub/RoME/data/TA_GSA18_1994-2018.csv", sep=";")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("check_0_fieldsTA", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("check_area")
### * check_area

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: check_area
### Title: Check if TX files have the same area
### Aliases: check_area
### Keywords: error

### ** Examples

    wd <- tempdir()
    suffix="27-02-2020 18:30"
    DataTA = MEDITS::TA
    DataTB = MEDITS::TB
    DataTC = MEDITS::TC
    DataTD = NA
    DataTE = NA
    DataTT = NA
    DataTL = NA

    check_area(DataTA, DataTB,DataTC,DataTD=NA,DataTT=NA,DataTE=NA,DataTL=NA, wd, suffix)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("check_area", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("check_quadrant")
### * check_quadrant

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: check_quadrant
### Title: Check start and end quadrant for each haul
### Aliases: check_quadrant
### Keywords: quadrant

### ** Examples

library(MEDITS)
library(RoME)
wd=tempdir()
suffix= "27-02-2020 18:30"
check_quadrant(ResultDataTA,wd,suffix)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("check_quadrant", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("check_quasiidentical_records")
### * check_quasiidentical_records

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: check_quasiidentical_records
### Title: Function checking the presence of quasi-identical records.
### Aliases: check_quasiidentical_records
### Keywords: quasi-identical

### ** Examples

library(MEDITS)
library(RoME)
wd=tempdir()
suffix= "27-02-2020 18:30"
check_quasiidentical_records(ResultDataTA,wd,suffix)
check_quasiidentical_records(ResultDataTB,wd,suffix)
check_quasiidentical_records(ResultDataTC,wd,suffix)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("check_quasiidentical_records", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
