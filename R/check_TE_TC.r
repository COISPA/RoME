############################################################################################################################
#   RoME: R code to perform multiple checks on MEDITS Survey data (TA, TB, TC and TE files)                                #
#   Authors: I. Bitetto, W. Zupa, M.T. Spedicato                                                                           #
#   Coispa Tecnologia & Ricerca - Stazione sperimentale per lo Studio delle Risorse del Mare                               #
#   If you have any comments or suggestions please contact the following e-mail address: bitetto@coispa.it, zupa@coispa.it #
#   January 2022                                                                                                           #
############################################################################################################################


# Check about the consistency of the number of individuals by length, sex and stage between TC and TE

if (FALSE){
  # ResultDataTC = RoME::TC
  # ResultDataTE = RoME::TE
  ResultDataTC = read.table("D:\\OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L\\RDB\\RoME_RDBFIS\\data\\TC GSA18 2017-2020.csv", sep=";",header=TRUE)
  ResultDataTE = read.table("D:\\OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L\\RDB\\RoME_RDBFIS\\data\\TE GSA18 2017-2020.csv", sep=";",header=TRUE)
  year=2017
  wd = "D:\\OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L\\RDB\\RoME_RDBFIS\\check_RoME\\Logfiles"
  suffix= NA

  check_TE_TC(ResultDataTC,ResultDataTC,year,wd,suffix)
}

check_TE_TC <- function (ResultDataTC,ResultDataTE,year,wd,suffix){

  Format="from_2012"

  if (!file.exists(file.path(wd, "Logfiles"))){
    dir.create(file.path(wd, "Logfiles"), recursive = TRUE, showWarnings = FALSE)
  }
  if (!file.exists(file.path(wd,"Graphs"))){
    dir.create(file.path(wd, "Graphs"), showWarnings = FALSE)
  }
  if (!exists("suffix")){
    suffix=paste(as.character(Sys.Date()),format(Sys.time(), "_time_h%Hm%Ms%OS0"),sep="")
  }
  numberError = 0
  Errors <- file.path(wd,"Logfiles",paste("Logfile_",suffix,".dat",sep=""))
  if (!file.exists(Errors)){
    file.create(Errors)
  }


  ### FILTERING DATA FOR THE SELECTED YEAR
  arg <- "year"
  if (!exists(arg)) {
    stop(paste0("'", arg, "' argument should be provided"))
  } else if (length(year) != 1) {
    stop(paste0("only one value should be provided for '", arg, "' argument"))
  } else if (is.na(year)) {
    stop(paste0(arg, " argument should be a numeric value"))
  }
  ResultDataTC <- ResultDataTC[ResultDataTC$YEAR == year, ]
  ResultDataTE <- ResultDataTE[ResultDataTE$YEAR == year, ]
  ########################################

  id<- seq(1:nrow(ResultDataTC))
  ResultDataTC <- cbind(id,ResultDataTC)

  id <- seq(1:nrow(ResultDataTE))
  ResultDataTE <- cbind(id,ResultDataTE)

  TC = ResultDataTC
  TE = ResultDataTE

  TC$MATURITY <- as.character(TC$MATURITY)
  TC$MATSUB <- as.character(TC$MATSUB)
  TC[is.na(TC$MATSUB) | TC$MATSUB =="","MATSUB"] <- "ND"

  TE$MATURITY <- as.character(TE$MATURITY)
  TE$MATSUB <- as.character(TE$MATSUB)
  TE[is.na(TE$MATSUB) | TE$MATSUB =="","MATSUB"] <- "ND"
  TE[TE$MATSUB =="O","MATSUB"] <- "ND"

  write(paste("\n----------- check consistency nb of individuals TC and TE - ",TC$YEAR[1]), file = Errors, append = TRUE)
  i=1
  for (i in 1:nrow(TE)){
    TE$SEX[i]=ifelse(as.character(TE$SEX[i])=="FALSE","F",as.character(TE$SEX[i]))
  }

  # # modifica 17/06/2020
  # TC$MATSUB <- as.character(TC$MATSUB)
  # TE$MATSUB <- as.character(TE$MATSUB)
  # TC[is.na(TC$MATSUB), "MATSUB"] <- "NA"
  # TE[is.na(TE$MATSUB), "MATSUB"] <- "NA"
  #########
  i=1
  for (i in 1:nrow(TE)){
  # check if the individual in TE is in TC
    TC_temp = TC[which((TC$HAUL_NUMBER==TE$HAUL_NUMBER[i]) &
                         (as.character(TC$GENUS)==as.character(TE$GENUS[i])) &
                         (as.character(TC$SPECIES)==as.character(TE$SPECIES[i])) &
                         (as.character(TC$SEX)==as.character(TE$SEX[i])) &
                         (TC$LENGTH_CLASS==TE$LENGTH_CLASS[i]) &
                         (as.character(TC$MATURITY)==as.character(TE$MATURITY[i])) &
                         (as.character(TC$MATSUB)==as.character(TE$MATSUB[i])))  ,   ]
    nb_TC= sum(TC_temp[,ncol(TC)])

    if (nrow(TC_temp)==0) { # record not present in TC
  write(paste("Haul ",TE$HAUL_NUMBER[i],as.character(TE$GENUS[i]),as.character(TE$SPECIES[i]),", sex ",ifelse(as.character(TE$SEX[i])=="FALSE","F",as.character(TE$SEX[i])),", length ",TE$LENGTH_CLASS[i],"mm, maturity",as.character(TE$MATURITY[i]),as.character(ResultDataTE[ResultDataTE$id==TE$id[i],"MATSUB"])," : record not present in TC"), file = Errors, append = TRUE)
  numberError=numberError+1

  } else { # record present: check on the number (must be <= the number in TC)
  # sum of individuals in TE:
    nb_TE = nrow(TE[which((TE$HAUL_NUMBER==TE$HAUL_NUMBER[i])& (as.character(TE$GENUS)==as.character(TE$GENUS[i]))& (as.character(TE$SPECIES)==as.character(TE$SPECIES[i]))& (as.character(TE$SEX)==as.character(TE$SEX[i]))& (TE$LENGTH_CLASS==TE$LENGTH_CLASS[i])& (as.character(TE$MATURITY)==as.character(TE$MATURITY[i]))& (as.character(TE$MATSUB)==as.character(TE$MATSUB[i]))),])
   if (nb_TC<nb_TE){
     write(paste("Haul ",TE$HAUL_NUMBER[i],as.character(TE$GENUS[i]),as.character(TE$SPECIES[i]),", sex ",ifelse(as.character(TE$SEX[i])=="FALSE","F",as.character(TE$SEX[i])),", length ",TE$LENGTH_CLASS[i],"mm, maturity",as.character(TE$MATURITY[i]),as.character(ResultDataTE[ResultDataTE$id==TE$id[i],"MATSUB"])," : the number of individuals in TE (=",nb_TE,") is greater than the number reported in TC(=",nb_TC,")"), file = Errors, append = TRUE)
    numberError=numberError+1
   }
  }

  }
      if (numberError ==0) {
    write(paste("No error occurred"), file = Errors, append = TRUE)
      }
   if (file.exists(file.path(tempdir(), "Logfiles"))){
  unlink(file.path(tempdir(),"Logfiles"),recursive=T)
  }
  if (file.exists(file.path(tempdir(), "Graphs"))){
  unlink(file.path(tempdir(),"Graphs"),recursive=T)
    }
	if (file.exists(file.path(tempdir(), "files R-Sufi"))){
  unlink(file.path(tempdir(),"files R-Sufi"),recursive=T)
    }

  if (numberError ==0) {
    return(TRUE)
   } else { return(FALSE)
   }

}
