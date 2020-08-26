############################################################################################################################
#   RoME: R code to perform multiple checks on MEDITS Survey data (TA, TB, TC and TE files - old and new MEDITS formats)   #
#   Authors: I. Bitetto, W. Zupa, M.T. Spedicato                                                                           #
#   Coispa Tecnologia & Ricerca - Stazione sperimentale per lo Studio delle Risorse del Mare                               #
#   If you have any comments or suggestions please contact the following e-mail address: bitetto@coispa.it, zupa@coispa.eu #
#   March 2020                                                                                                             #
############################################################################################################################
# Check if in TB there are the total number, number of females, males and undetermined for species G1

check_nm_TB<- function (DataTB, DataTC,wd,suffix){

  if (FALSE){
    library(RoME)
    wd <- tempdir()
    suffix=paste(as.character(Sys.Date()),format(Sys.time(), "_time h%Hm%Ms%OS0"),sep="")
    DataTB = read.csv("~/GitHub/RoME/data/TB_GSA18_1994-2018.csv", sep=";")
    DataTB = DataTB[DataTB$YEAR == 2012 ,   ]
    DataTC = read.csv("~/GitHub/RoME/data/TC_GSA18_1994-2018.csv", sep=";")
    DataTC = DataTC[DataTC$YEAR == 2012 ,   ]
    # check_nm_TB(DataTB,DataTC, wd, suffix)
  }
  if (!file.exists(file.path(wd, "Logfiles"))){
    dir.create(file.path(wd, "Logfiles"), showWarnings = FALSE)
  }
  if (!file.exists(file.path(wd,"Graphs"))){
    dir.create(file.path(wd, "Graphs"), showWarnings = FALSE)
  }
  if (!exists("suffix")){
    suffix=paste(as.character(Sys.Date()),format(Sys.time(), "_time h%Hm%Ms%OS0"),sep="")
  }
  numberError = 0
  Errors <- file.path(wd,"Logfiles",paste("Logfile_",suffix,".dat",sep=""))

  TB = DataTB
  TC = DataTC

  write(paste("\n----------- check presence of number of individuals for species G1 - ",TB$YEAR[1]), file = Errors, append = TRUE)


  # SELECTION OF SPECIES G1
  TB=TB   [(as.character(TB$FAUNISTIC_CATEGORY)=="Ae" )|
          (as.character(TB$GENUS)=="MERL" & as.character(TB$SPECIES)=="MERL") |
          (as.character(TB$GENUS)=="MULL" ) |
          (as.character(TB$GENUS)=="ARIS" & as.character(TB$SPECIES)=="FOL") |
          (as.character(TB$GENUS)=="ARIT" & as.character(TB$SPECIES)=="ANT") |
          (as.character(TB$GENUS)=="ILLE" & as.character(TB$SPECIES)=="COI") |
          (as.character(TB$GENUS)=="LOLI" & as.character(TB$SPECIES)=="VUL") |
          (as.character(TB$GENUS)=="NEPR" & as.character(TB$SPECIES)=="NOR") |
          (as.character(TB$GENUS)=="PAPE" & as.character(TB$SPECIES)=="LON") ,]

  i=1
  for (i in 1:nrow(TB)){
    if ((as.character(TB$NB_OF_FEMALES[i])=="0")&(as.character(TB$NB_OF_MALES[i])=="0")){
       TC_temp = TC[(TC$HAUL_NUMBER== TB$HAUL_NUMBER[i])
               & (as.character(TC$GENUS)== as.character(TB$GENUS[i]))
               & (as.character(TC$SPECIES)== as.character(TB$SPECIES[i])),]

       if (!(all(as.character(TC_temp$SEX)=="N" | as.character(TC_temp$SEX)=="I"))){
        write(paste("Haul", TB$HAUL_NUMBER[i],TB$GENUS[i],TB$SPECIES[i], "for the fields related to numbers of individuals, the value 0 is not allowed from 2012 for species G1"), file = Errors, append = TRUE)
        numberError =   numberError +1
        }
    }
  }

    if (numberError ==0) {
      write(paste("No error occurred"), file = Errors, append = TRUE)
    }

  unlink(file.path(tempdir(),"Logfiles"),recursive=T)
  unlink(file.path(tempdir(),"Graphs"),recursive=T)
  #unlink(file.path(tempdir(),"files R-Sufi"),recursive=T)

    if (numberError ==0) {
      return(TRUE)
    } else { return(FALSE) }


}
