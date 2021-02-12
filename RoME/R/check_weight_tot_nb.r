

check_weight_tot_nb<-function(ResultDataTB,wd,suffix){

if (FALSE){
  wd <- "C:\\Users\\walte\\Documents\\GitHub\\RoME\\Test Loredana"
  ResultDataTB = read.table(file=paste(wd, "\\TB.csv",sep=""), sep=";", header=T)
  #ResultDataTB= MEDITS::TB # ResultDataTB[ResultDataTB$YEAR==2017,]

  # wd <- tempdir() # "C:/Users/Bitetto Isabella/OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L/Rome/ROME/temp"
  suffix=NA # paste(as.character(Sys.Date()),format(Sys.time(), "_time_h%Hm%Ms%OS0"),sep="")
  #load("C:/Users/Bitetto Isabella/OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L/Rome/ROME//RoME//data//DataTargetSpecies.rda")
  #load("C:/Users/Bitetto Isabella/OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L/Rome/ROME//RoME//data//Maturity_parameters.rda")
  #load("C:/Users/Bitetto Isabella/OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L/Rome/ROME//RoME//data//TM_list.rda")
  check_weight_tot_nb(ResultDataTB,wd,suffix)

}

   oldpar <- par(no.readonly = TRUE)
   on.exit(suppressWarnings(par(oldpar)))

  if (!file.exists(file.path(wd,"Logfiles"))){
    dir.create(file.path(wd, "Logfiles"), recursive = TRUE, showWarnings = FALSE)
  }

  if (!exists("suffix")){
    suffix=paste(as.character(Sys.Date()),format(Sys.time(), "_time_h%Hm%Ms%OS0"),sep="")
  }

  if (!file.exists(file.path(wd,"Graphs"))){
    dir.create(file.path(wd, "Graphs"), showWarnings = FALSE)
  }


  Errors <- file.path(wd,"/Logfiles",paste("Logfile_",suffix,".dat",sep=""))
  if (!file.exists(Errors)){
    file.create(Errors)
  }

  numberError = 0
  ResultData = ResultDataTB #read.csv(paste(DataTB,".csv",sep=""), sep=";", header=TRUE)
  write(paste("
              ----------- check consistency between not null weight and not null total number in TB - ",ResultData$YEAR[1]), file = Errors, append = TRUE)

  for (i in 1:nrow(ResultData)){

      if ((ResultData$TOTAL_WEIGHT_IN_THE_HAUL[i]==0) & (ResultData$TOTAL_NUMBER_IN_THE_HAUL[i]!=0)){

        write(paste("Warning: Haul ",ResultData$HAUL_NUMBER[i]," species ",ResultData$GENUS[i],ResultData$SPECIES[i]," Total weight equals 0, but total number is not null ", sep=""), file = Errors, append = TRUE)
      }



    if ((ResultData$TOTAL_NUMBER_IN_THE_HAUL[i]==0) & (ResultData$TOTAL_WEIGHT_IN_THE_HAUL[i]!=0) &
          (   ((str_extract(as.character(ResultData$FAUNISTIC_CATEGORY[i]),"[A-Z]"))!="E") &
                (  (str_extract(as.character(ResultData$FAUNISTIC_CATEGORY[i]),"[A-Z]"))!="D") &
                (  (str_extract(as.character(ResultData$FAUNISTIC_CATEGORY[i]),"[A-Z]"))!="V") &
                (  (str_extract(as.character(ResultData$FAUNISTIC_CATEGORY[i]),"[A-Z]"))!="G") &
                (  (str_extract(as.character(ResultData$FAUNISTIC_CATEGORY[i]),"[A-Z]"))!="H")  )){

      write(paste("Warning: Haul ",ResultData$HAUL_NUMBER[i]," species ",ResultData$GENUS[i],ResultData$SPECIES[i]," Total number equals 0, but total weight is not null", sep=""), file = Errors, append = TRUE)
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
  } else { return(FALSE) }

}

################################################################################
