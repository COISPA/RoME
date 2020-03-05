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
nameEx("LW")
### * LW

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: LW
### Title: Table of the Length-Weight parameters
### Aliases: LW
### Keywords: datasets

### ** Examples

data(LW)
## maybe str(LW) ; plot(LW) ...



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("LW", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
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
nameEx("MedSea")
### * MedSea

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: MedSea
### Title: Shapefile of Mediterranean and Black Sea area
### Aliases: MedSea
### Keywords: datasets

### ** Examples

data(MedSea)
## maybe str(MedSea) ; plot(MedSea) ...



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("MedSea", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("TE_provisional")
### * TE_provisional

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: TE_provisional
### Title: Provisional example of TE table!!!!
### Aliases: TE_provisional
### Keywords: datasets

### ** Examples

data(TE_provisional)
str(TE_provisional)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("TE_provisional", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
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
nameEx("checkHeader")
### * checkHeader

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: checkHeader
### Title: Function to check the correctness of the headers.
### Aliases: checkHeader
### Keywords: headers

### ** Examples

library(MEDITS)
library(RoME)
wd=tempdir()
suffix= "27-02-2020 18:30"
checkHeader(TA,"TA")
checkHeader(TB,"TB")
checkHeader(TC,"TC")




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("checkHeader", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
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
nameEx("check_G1_G2")
### * check_G1_G2

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: check_G1_G2
### Title: Check of length measurements for G1 and G2 species
### Aliases: check_G1_G2
### Keywords: warning

### ** Examples

library(RoME)
library(MEDITS)
wd <- tempdir()
suffix <- "27-02-2020 18:30"
check_G1_G2(MEDITS::TC,wd,suffix)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("check_G1_G2", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("check_TE_TC")
### * check_TE_TC

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: check_TE_TC
### Title: Function to verify the onsistency between individual data table
###   (TE) and biological data table (TC) respect to numbe rof individuals.
### Aliases: check_TE_TC
### Keywords: cross-check TETC

### ** Examples

library(MEDITS)
library(RoME)
wd=tempdir()
suffix= "27-02-2020 18:30"
check_TE_TC(TC,ResultDataTE,wd,suffix)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("check_TE_TC", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
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
    DataTE = NA
    DataTL = NA

    check_area(DataTA, DataTB,DataTC,DataTE=NA,DataTL=NA, wd, suffix)



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
nameEx("check_date_haul")
### * check_date_haul

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: check_date_haul
### Title: Check of date consistency
### Aliases: check_date_haul
### Keywords: error

### ** Examples

library(RoME)
    wd <- tempdir()
    suffix="27-02-2020 18:30"
    DataTA = MEDITS::TA
    Data = MEDITS::TB
    check_date_haul(DataTA, Data, wd, suffix)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("check_date_haul", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("check_depth")
### * check_depth

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: check_depth
### Title: Check between start depth and end depth
### Aliases: check_depth
### Keywords: warning

### ** Examples

library(RoME)
library(MEDITS)
wd <- tempdir()
suffix <- "27-02-2020 18:30"
check_depth(MEDITS::TA,wd,suffix)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("check_depth", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("check_dictionary")
### * check_dictionary

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: check_dictionary
### Title: Check of the dictionary of specific fields
### Aliases: check_dictionary
### Keywords: error

### ** Examples

    wd <- tempdir()
    suffix="27-02-2020 18:30"
    Field = "COURSE"
    Values = c("R","N")
    DataTA = MEDITS::TA
    check_dictionary(ResultData = DataTA, Field, Values, wd, suffix)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("check_dictionary", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("check_distance")
### * check_distance

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: check_distance
### Title: Check of distance consistency
### Aliases: check_distance
### Keywords: warning

### ** Examples

library(RoME)
library(MEDITS)
wd <- tempdir()
suffix <- "27-02-2020 18:30"
check_distance(MEDITS::TA,wd,suffix)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("check_distance", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("check_dm")
### * check_dm

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: check_dm
### Title: Check of "WING_OPENING" and "VERTICAL_OPENING" fields
### Aliases: check_dm
### Keywords: error

### ** Examples

library(MEDITS)
wd <- tempdir()
suffix <- "27-02-2020 18:30"
check_dm(MEDITS::TA,wd,suffix)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("check_dm", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("check_haul_species_TCTB")
### * check_haul_species_TCTB

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: check_haul_species_TCTB
### Title: Check species of TC in TB
### Aliases: check_haul_species_TCTB
### Keywords: warning

### ** Examples

library(RoME)
library(MEDITS)
wd <- tempdir()
suffix <- "27-02-2020 18:30"
check_haul_species_TCTB(MEDITS::TB, MEDITS::TC, wd, suffix)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("check_haul_species_TCTB", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("check_hauls_TATB")
### * check_hauls_TATB

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: check_hauls_TATB
### Title: Check of TA hauls in TB
### Aliases: check_hauls_TATB
### Keywords: error

### ** Examples

    library(RoME)
    library(MEDITS)
    wd <- tempdir()
    suffix="27-02-2020 18:30"
    DataTA <- MEDITS::TA
    DataTA <- DataTA[DataTA$YEAR ==2018 , ]
    DataTB <- MEDITS::TB
    DataTB <- DataTB[DataTB$YEAR ==2018 , ]
    check_hauls_TATB(DataTA,DataTB,wd,suffix)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("check_hauls_TATB", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("check_hauls_TATL")
### * check_hauls_TATL

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: check_hauls_TATL
### Title: Check presence of TA hauls in TL
### Aliases: check_hauls_TATL
### Keywords: warning

### ** Examples




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("check_hauls_TATL", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("check_hauls_TBTA")
### * check_hauls_TBTA

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: check_hauls_TBTA
### Title: Check of TB hauls in TA
### Aliases: check_hauls_TBTA
### Keywords: error

### ** Examples

    library(RoME)
    library(MEDITS)
    wd <- tempdir()
    suffix="27-02-2020 18:30"
    DataTA <- MEDITS::TA
    DataTA <- DataTA[DataTA$YEAR ==2018 , ]
    DataTB <- MEDITS::TB
    DataTB <- DataTB[DataTB$YEAR ==2018 , ]
    check_hauls_TBTA(DataTA,DataTB,wd,suffix)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("check_hauls_TBTA", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("check_hauls_TLTA")
### * check_hauls_TLTA

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: check_hauls_TLTA
### Title: Check presence of TL hauls in TA
### Aliases: check_hauls_TLTA
### Keywords: error

### ** Examples




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("check_hauls_TLTA", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("check_identical_records")
### * check_identical_records

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: check_identical_records
### Title: Check of identical records in TX tables
### Aliases: check_identical_records
### Keywords: error

### ** Examples

    wd <- tempdir()
    suffix="27-02-2020 18:30"
    Data = MEDITS::TA
    check_identical_records(Data, wd, suffix)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("check_identical_records", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("check_individual_weightTC")
### * check_individual_weightTC

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: check_individual_weightTC
### Title: Check of observed and estimated total weight in the haul
### Aliases: check_individual_weightTC
### Keywords: warning

### ** Examples

    wd <- tempdir()
    suffix="27-02-2020 18:30"
    TC = MEDITS::TC
    check_individual_weightTC(DataTC=TC, wd=wd, suffix=suffix)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("check_individual_weightTC", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("check_individual_weightTE")
### * check_individual_weightTE

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: check_individual_weightTE
### Title: Consistency of individual weights (according to length-weight
###   relationship)
### Aliases: check_individual_weightTE
### Keywords: warning

### ** Examples




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("check_individual_weightTE", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("check_length")
### * check_length

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: check_length
### Title: Check of length classes in TC
### Aliases: check_length
### Keywords: warning

### ** Examples

library(MEDITS)
library(RoME)
wd=tempdir()
DataTC <- MEDITS::TC
suffix= "27-02-2020 18:30"
check_length(DataTC,DataSpecies,wd,suffix)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("check_length", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("check_length_class_codeTC")
### * check_length_class_codeTC

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: check_length_class_codeTC
### Title: Consistency check of LENGTH_CLASS
### Aliases: check_length_class_codeTC
### Keywords: error warning

### ** Examples

library(MEDITS)
library(RoME)
DataTC <- MEDITS::TC
wd=tempdir()
suffix= "27-02-2020 18:30"
check_length_class_codeTC(DataTC,Specieslist=NA,wd,suffix)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("check_length_class_codeTC", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("check_mat_stages")
### * check_mat_stages

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: check_mat_stages
### Title: Consistency of maturity stages
### Aliases: check_mat_stages
### Keywords: error warning

### ** Examples

library(MEDITS)
library(RoME)
DataTC <- MEDITS::TC
wd=tempdir()
suffix= "27-02-2020 18:30"
check_mat_stages(Result, wd, suffix)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("check_mat_stages", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("check_nb_TE")
### * check_nb_TE

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: check_nb_TE
### Title: Consistency of number of individuals sampled for weight and
###   ageing in TE
### Aliases: check_nb_TE
### Keywords: error

### ** Examples




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("check_nb_TE", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("check_nb_per_sexTC")
### * check_nb_per_sexTC

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: check_nb_per_sexTC
### Title: Consistency check of number of individuals
### Aliases: check_nb_per_sexTC
### Keywords: error

### ** Examples

    library(RoME)
    wd <- tempdir()
    suffix="27-02-2020 18:30"
    DataTC = MEDITS::TC
    check_nb_per_sexTC(DataTC,wd,suffix)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("check_nb_per_sexTC", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("check_nbtotTB")
### * check_nbtotTB

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: check_nbtotTB
### Title: Check total number of individuals in TB
### Aliases: check_nbtotTB
### Keywords: error

### ** Examples

    wd <- tempdir()
    suffix="27-02-2020 18:30"
    DataTB = MEDITS::TB
    check_nbtotTB(DataTB, wd, suffix)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("check_nbtotTB", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("check_position")
### * check_position

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: check_position
### Title: Plot of haul positions
### Aliases: check_position
### Keywords: warning plot

### ** Examples

library(MEDITS)
wd <- tempdir()
suffix <- "27-02-2020 18:30"
check_position(MEDITS::TA,wd,suffix)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("check_position", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("check_position_in_Med")
### * check_position_in_Med

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: check_position_in_Med
### Title: Check of haul position in Mediterranean Sea
### Aliases: check_position_in_Med
### Keywords: error

### ** Examples

library(MEDITS)
wd <- tempdir()
suffix <- "27-02-2020 18:30"
check_position_in_Med(MEDITS::TA,wd,suffix)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("check_position_in_Med", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
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
check_rubincode(TB,TM_list,wd,suffix)
check_rubincode(TC,TM_list,wd,suffix)



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
check_smallest_mature(TC,Maturity_parameters,DataTargetSpecies,wd,suffix)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("check_smallest_mature", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("check_spawning_period")
### * check_spawning_period

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: check_spawning_period
### Title: Function to check the consistency of the maturity stages
###   according to the spawning period.
### Aliases: check_spawning_period
### Keywords: smallest mature

### ** Examples

library(MEDITS)
library(RoME)
wd=tempdir()
suffix= "27-02-2020 18:30"
check_spawning_period(TA,TC,Maturity_parameters,DataTargetSpecies,wd,suffix)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("check_spawning_period", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("check_species_TBTC")
### * check_species_TBTC

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: check_species_TBTC
### Title: Function checking if all the target species in the catch data
###   table (TB) are in Biological data table (TC)
### Aliases: check_species_TBTC
### Keywords: cross-check TBTC

### ** Examples

library(MEDITS)
library(RoME)
wd=tempdir()
suffix= "27-02-2020 18:30"
check_species_TBTC(TB,TC,DataTargetSpecies,wd,suffix)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("check_species_TBTC", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("check_step_length_distr")
### * check_step_length_distr

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: check_step_length_distr
### Title: Function the verifies that in TC the length measures are
###   repoerted with the correct precision.
### Aliases: check_step_length_distr
### Keywords: length, step

### ** Examples

library(MEDITS)
library(RoME)
wd=tempdir()
suffix= "27-02-2020 18:30"
check_step_length_distr(TC,wd,suffix)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("check_step_length_distr", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("check_stratum")
### * check_stratum

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: check_stratum
### Title: Function that checks the consistency between start and end depth
###   according to the stratum.
### Aliases: check_stratum
### Keywords: stratum

### ** Examples

library(MEDITS)
library(RoME)
wd=tempdir()
suffix= "27-02-2020 18:30"
check_stratum(TA,wd,suffix)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("check_stratum", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("check_stratum_code")
### * check_stratum_code

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: check_stratum_code
### Title: Function to check the correct codification of the strata in haul
###   data table (TA).
### Aliases: check_stratum_code
### Keywords: stratum code

### ** Examples

library(MEDITS)
library(RoME)
wd=tempdir()
suffix= "27-02-2020 18:30"
check_stratum_code(TA,stratification_scheme,wd,suffix)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("check_stratum_code", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("check_subsampling")
### * check_subsampling

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: check_subsampling
### Title: Function to warng the user about the presence of subsamples <0.1
###   of the total catch.
### Aliases: check_subsampling
### Keywords: sub-sampling

### ** Examples

library(MEDITS)
library(RoME)
wd=tempdir()
suffix= "27-02-2020 18:30"
check_subsampling(TC,wd,suffix)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("check_subsampling", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("check_temperature")
### * check_temperature

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: check_temperature
### Title: Function to check the consistency of the temperature data stored
###   in haul data table (TA).
### Aliases: check_temperature
### Keywords: temperature

### ** Examples

library(MEDITS)
library(RoME)
wd=tempdir()
suffix= "27-02-2020 18:30"
check_temperature(TA,wd,suffix)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("check_temperature", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("check_unique_valid_haul")
### * check_unique_valid_haul

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: check_unique_valid_haul
### Title: Function checking that smong hauls with the same code, only one
###   must be valid.
### Aliases: check_unique_valid_haul
### Keywords: valid hauls

### ** Examples

library(MEDITS)
library(RoME)
wd=tempdir()
suffix= "27-02-2020 18:30"
check_unique_valid_haul(TA,wd,suffix)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("check_unique_valid_haul", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("check_weight")
### * check_weight

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: check_weight
### Title: Function that checks if number of individuals and total weight
###   collected in the haul are consistent.
### Aliases: check_weight
### Keywords: weight

### ** Examples

library(MEDITS)
library(RoME)
wd=tempdir()
suffix= "27-02-2020 18:30"
check_weight(TB,DataTargetSpecies,wd,suffix)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("check_weight", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("create_catch")
### * create_catch

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: create_catch
### Title: Function to create the R-sufi file capt.
### Aliases: create_catch
### Keywords: R-sufi,capt

### ** Examples

library(MEDITS)
library(RoME)
wd <- tempdir()
suffix="27-02-2020 18:30"
create_catch(TB,wd)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("create_catch", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("create_haul")
### * create_haul

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: create_haul
### Title: Function to create R-sufi file containing haul data.
### Aliases: create_haul
### Keywords: R-sufi,traits

### ** Examples

library(MEDITS)
library(RoME)
wd <- tempdir()
suffix="27-02-2020 18:30"
create_haul(TA,wd)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("create_haul", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("create_length")
### * create_length

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: create_length
### Title: Function to create the R-sufi file taille.
### Aliases: create_length
### Keywords: R-sufi,taille

### ** Examples

library(MEDITS)
library(RoME)
wd <- tempdir()
suffix="27-02-2020 18:30"
create_length(TC,TM_list,wd)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("create_length", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("create_strata")
### * create_strata

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: create_strata
### Title: Function to create R-sufi file containing strata surface data.
### Aliases: create_strata
### Keywords: R-sufi,strata

### ** Examples

library(MEDITS)
library(RoME)
wd <- tempdir()
suffix="27-02-2020 18:30"
create_strata(stratification_scheme,"18",wd)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("create_strata", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("haul_at_sea")
### * haul_at_sea

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: haul_at_sea
### Title: Check of haul position on sea area
### Aliases: haul_at_sea
### Keywords: list

### ** Examples

library(MEDITS)
wd <- tempdir()
suffix <- "27-02-2020 18:30"
haul_at_sea(MEDITS::TA, seas = MedSea, verbose = TRUE)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("haul_at_sea", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("list_g1_g2")
### * list_g1_g2

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: list_g1_g2
### Title: List of G1 and G2 species
### Aliases: list_g1_g2
### Keywords: datasets

### ** Examples

data(list_g1_g2)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("list_g1_g2", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("mat_stages")
### * mat_stages

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: mat_stages
### Title: Table of maturity stages
### Aliases: mat_stages
### Keywords: datasets

### ** Examples

data(mat_stages)
## maybe str(mat_stages) ; plot(mat_stages) ...



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("mat_stages", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("printError")
### * printError

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: printError
### Title: Management of the error in logfile.
### Aliases: printError
### Keywords: error

### ** Examples




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("printError", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("templateTA")
### * templateTA

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: templateTA
### Title: Template haul data table (TA).
### Aliases: templateTA
### Keywords: datasets

### ** Examples

data(templateTA)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("templateTA", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("templateTB")
### * templateTB

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: templateTB
### Title: Template catch data table (TB).
### Aliases: templateTB
### Keywords: datasets

### ** Examples

data(templateTB)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("templateTB", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("templateTC")
### * templateTC

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: templateTC
### Title: Template biological data table (TC).
### Aliases: templateTC
### Keywords: datasets

### ** Examples

data(templateTC)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("templateTC", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("templateTE")
### * templateTE

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: templateTE
### Title: Template individual data table (TE).
### Aliases: templateTE
### Keywords: datasets

### ** Examples

data(templateTE)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("templateTE", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("templateTL")
### * templateTL

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: templateTL
### Title: TL table template
### Aliases: templateTL
### Keywords: datasets

### ** Examples

data(templateTL)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("templateTL", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
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
