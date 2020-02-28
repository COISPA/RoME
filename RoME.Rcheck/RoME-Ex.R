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
nameEx("DataTargetSpecies")
### * DataTargetSpecies

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: DataTargetSpecies
### Title: Length and weight ranges
### Aliases: DataTargetSpecies
### Keywords: datasets

### ** Examples

data(DataTargetSpecies)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("DataTargetSpecies", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("Maturity_parameters")
### * Maturity_parameters

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: Maturity_parameters
### Title: Maturity parameters
### Aliases: Maturity_parameters
### Keywords: datasets

### ** Examples

data(Maturity_parameters)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("Maturity_parameters", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("TM_list")
### * TM_list

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: TM_list
### Title: TM list
### Aliases: TM_list
### Keywords: datasets

### ** Examples

data(TM_list)
## maybe str(TM_list) ; plot(TM_list) ...



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("TM_list", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("assTL")
### * assTL

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: assTL
### Title: TL association between categories and sub-categories
### Aliases: assTL
### Keywords: datasets

### ** Examples

data(assTL)
## maybe str(assTL) ; plot(assTL) ...



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("assTL", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("check_0_fieldsTA")
### * check_0_fieldsTA

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: check_0_fieldsTA
### Title: Checks the presence of 0 fields in TA
### Aliases: check_0_fieldsTA
### Keywords: error

### ** Examples

library(MEDITS)
wd <- tempdir()
suffix <- "27-02-2020 18:30"
check_0_fieldsTA(MEDITS::TA,wd,suffix)



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
nameEx("check_associations_category_TL")
### * check_associations_category_TL

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: check_associations_category_TL
### Title: Check corretness of TL categories
### Aliases: check_associations_category_TL
### Keywords: error

### ** Examples




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("check_associations_category_TL", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("check_bridles_length")
### * check_bridles_length

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: check_bridles_length
### Title: check of bridles length correctness
### Aliases: check_bridles_length
### Keywords: error

### ** Examples

library(MEDITS)
wd <- tempdir()
suffix <- "27-02-2020 18:30"
check_bridles_length(MEDITS::TA, wd, suffix)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("check_bridles_length", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("check_consistencyTA_distance")
### * check_consistencyTA_distance

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: check_consistencyTA_distance
### Title: Consistency check of distance in TA
### Aliases: check_consistencyTA_distance
### Keywords: error

### ** Examples

library(MEDITS)
wd <- tempdir()
suffix <- "27-02-2020 18:30"
check_consistencyTA_distance(MEDITS::TA,wd,suffix)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("check_consistencyTA_distance", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("check_consistencyTA_duration")
### * check_consistencyTA_duration

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: check_consistencyTA_duration
### Title: Consistency check of hauls duration in TA
### Aliases: check_consistencyTA_duration
### Keywords: error

### ** Examples

library(MEDITS)
wd <- tempdir()
suffix <- "27-02-2020 18:30"
check_consistencyTA_duration(MEDITS::TA,wd,suffix)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("check_consistencyTA_duration", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
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
check_quadrant(TA,wd,suffix)



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
check_quasiidentical_records(TA,wd,suffix)
check_quasiidentical_records(TB,wd,suffix)
check_quasiidentical_records(TC,wd,suffix)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("check_quasiidentical_records", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("check_raising")
### * check_raising

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: check_raising
### Title: Function checking if, in case of sub-sampling in TC, the Total
###   number and the number per sex in TB is raised correctly
### Aliases: check_raising
### Keywords: raising, sub-sampling

### ** Examples

library(MEDITS)
library(RoME)
wd=tempdir()
suffix= "27-02-2020 18:30"
check_raising(TB,TC,wd,suffix)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("check_raising", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("check_rubincode")
### * check_rubincode

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: check_rubincode
### Title: Function checking the correctness of species MEDITS code and
###   faunistic category according to TM reference list
### Aliases: check_rubincode
### Keywords: rubincode,TM list

### ** Examples

library(MEDITS)
library(RoME)
wd=tempdir()
suffix= "27-02-2020 18:30"
check_rubincode(TB,wd,suffix)
check_rubincode(TC,wd,suffix)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("check_rubincode", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("check_sex_inversion")
### * check_sex_inversion

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: check_sex_inversion
### Title: Function to verify the consistency of sex information about
###   hermaphrodite species.
### Aliases: check_sex_inversion
### Keywords: sex inversion, hermaphrodite

### ** Examples

library(MEDITS)
library(RoME)
wd=tempdir()
suffix= "27-02-2020 18:30"
check_sex_inversion(TC,Maturity_parameters,wd,suffix)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("check_sex_inversion", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("check_smallest_mature")
### * check_smallest_mature

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: check_smallest_mature
### Title: Function to verify the consistency of maturity information
###   respect to the smallest mature individual observed in literature.
### Aliases: check_smallest_mature
### Keywords: smallest mature

### ** Examples

library(MEDITS)
library(RoME)
wd=tempdir()
suffix= "27-02-2020 18:30"
check_smallest_mature(TC,Maturity_parameters,wd,suffix)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("check_smallest_mature", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
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
