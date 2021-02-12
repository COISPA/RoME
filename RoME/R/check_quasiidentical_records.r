
if (FALSE){
    Result = read.csv("C:/Users/Bitetto Isabella/OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L/Rome/ROME/data/TA_GSA18_1994-2018.csv", sep=";")
    Result = read.csv("C:/Users/Bitetto Isabella/OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L/Rome/ROME/data/TB_GSA18_1994-2018.csv", sep=";")
    Result = read.table(file=paste(wd, "\\2019 GSA18 TA.csv",sep=""), sep=";", header=T) # read.csv("C:/Users/Bitetto Isabella/OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L/Rome/ROME/data/TC_GSA18_1994-2018.csv", sep=";")

    wd <- "C:\\Users\\walte\\Documents\\GitHub\\RoME\\data TEST Neglia" # "C:/Users/Bitetto Isabella/OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L/Rome/ROME/temp"
    suffix= NA # "C:\\Users\\walte\\Documents\\GitHub\\RoME\\data TEST Neglia" # paste(as.character(Sys.Date()),format(Sys.time(), "_time_h%Hm%Ms%OS0"),sep="")
    check_quasiidentical_records(Result,wd,suffix)
}

check_quasiidentical_records<-function(Result,wd,suffix){

  Table=as.character(Result[1,1])
  check_without_errors = FALSE

  if (!file.exists(file.path(wd, "Logfiles"))){
    dir.create(file.path(wd, "Logfiles"), recursive = TRUE, showWarnings = FALSE)
  }

  if (!exists("suffix")){
    suffix=paste(as.character(Sys.Date()),format(Sys.time(), "_time_h%Hm%Ms%OS0"),sep="")
  }
  numberError = 0
  Errors <- file.path(wd,"Logfiles",paste("Logfile_",suffix,".dat",sep=""))
  if (!file.exists(Errors)){
    file.create(Errors)
  }

  #### CHECK TL FIELDS ####
  {
    if ("LITTER_SUB.CATEGORY" %in% colnames(Result)){
      colnames(Result)[which(colnames(Result)=="LITTER_SUB.CATEGORY")] <- "LITTER_SUB-CATEGORY"
    }
    if ("TOTAL_WEIGHT_IN_THE_SUB.CATEGORY_HAUL" %in% colnames(Result)){
      colnames(Result)[which(colnames(Result)=="TOTAL_WEIGHT_IN_THE_SUB.CATEGORY_HAUL")] <- "TOTAL_WEIGHT_IN_THE_SUB-CATEGORY_HAUL"
    }
    if ("TOTAL_NUMBER_IN_THE_SUB.CATEGORY_HAUL" %in% colnames(Result)){
      colnames(Result)[which(colnames(Result)=="TOTAL_NUMBER_IN_THE_SUB.CATEGORY_HAUL")] <- "TOTAL_NUMBER_IN_THE_SUB-CATEGORY_HAUL"
    }
  }
  #### CHECK TL FIELDS - END ####

  # ------------------------------------------------------------------ TA

  if (Table == "TA"){
    write(paste("
                ----------- check quasi identical records - ",Result$YEAR[1]), file = Errors, append = TRUE)
    write(paste("TA:"), file = Errors, append = TRUE)

     #Matrix=sqldf("select count(*) as count, TYPE_OF_FILE, AREA, VESSEL, GEAR, RIGGING, DOORS, YEAR from Result Group by TYPE_OF_FILE, AREA, VESSEL, GEAR, RIGGING, DOORS, YEAR")
     Matrix=stats::aggregate(Result$TYPE_OF_FILE,by=list(Result$TYPE_OF_FILE, Result$AREA, Result$VESSEL, Result$GEAR, Result$RIGGING, Result$DOORS, Result$YEAR),FUN="length")
     colnames(Matrix)=c("TYPE_OF_FILE", "AREA", "VESSEL", "GEAR", "RIGGING", "DOORS","YEAR","x")

    if (nrow(Matrix)>1){
      Max=max(Matrix$x)
      Matrix2=Matrix[Matrix$x!=Max,]

      ResultData = Result[!is.na(Result$HAUL_NUMBER),]
        #sqldf("select * from Result where HAUL_NUMBER is not null")

      for (i in 1:nrow(Matrix2)){
        Err = ResultData[as.character(ResultData$TYPE_OF_FILE) == as.character(Matrix2$TYPE_OF_FILE[i]) &
                           as.character(ResultData$AREA) == as.character(Matrix2$AREA[i]) &
                           as.character(ResultData$VESSEL) == as.character(Matrix2$VESSEL[i]) &
                           as.character(ResultData$GEAR) == as.character(Matrix2$GEAR[i]) &
                           as.character(ResultData$RIGGING) == as.character(Matrix2$RIGGING[i]) &
                           as.character(ResultData$DOORS) == as.character(Matrix2$DOORS[i]) &
                           as.character(ResultData$YEAR) == as.character(Matrix2$YEAR[i]),]


        for( k in 1:nrow(Err)){
          write(paste("Haul ",Err$HAUL_NUMBER[k],
                      "there is an inconsistent value in one or more of the fields that should be always identical in",
                      Matrix$TYPE_OF_FILE[1]), file = Errors, append = TRUE)
        }
      }
    } else {
      write(paste("No error occurred"), file = Errors, append = TRUE)
      check_without_errors = TRUE
    }


  } else if (Table=="TB" | Table=="TT" | Table=="TD" ) {
    # ------------------------------------------------------------------ TB
    write(paste("TB:"), file = Errors, append = TRUE)

     # Matrix=sqldf("select count(*) as count, TYPE_OF_FILE, AREA, VESSEL, YEAR from Result Group by TYPE_OF_FILE,  AREA, VESSEL, YEAR")
    Matrix=stats::aggregate(Result$TYPE_OF_FILE,by=list(Result$TYPE_OF_FILE, Result$AREA, Result$VESSEL, Result$YEAR),FUN="length")
colnames(Matrix)=c("TYPE_OF_FILE", "AREA", "VESSEL", "YEAR","x")
ResultData = Result[!is.na(Result$HAUL_NUMBER),]

    if (nrow(Matrix)>1){
      Max=max(Matrix$x)
      Matrix2=Matrix[Matrix$x!=Max,]
      for (i in 1:nrow(Matrix2)){

        Err = ResultData[as.character(ResultData$TYPE_OF_FILE) == as.character(Matrix2$TYPE_OF_FILE[i]) &
                           as.character(ResultData$AREA) == as.character(Matrix2$AREA[i]) &
                           as.character(ResultData$VESSEL) == as.character(Matrix2$VESSEL[i]) &
                           as.character(ResultData$YEAR) == as.character(Matrix2$YEAR[i]),]

        for( k in 1:nrow(Err)){
          if (Err$YEAR[k] == Matrix2$YEAR[i]){
            write(paste("Haul ",Err$HAUL_NUMBER[k],Err$GENUS[k],Err$SPECIES[k],"there is an inconsistent value in one or more of the fields that should be always identical in", Matrix$TYPE_OF_FILE[1]), file = Errors, append = TRUE)
          }
        }
      }
    } else
    {
      write(paste("No error occurred"), file = Errors, append = TRUE)
      check_without_errors = TRUE
    }

  } else if (Table=="TC"){
    # ------------------------------------------------------------------ TC
    write(paste("TC:"), file = Errors, append = TRUE)

 # Matrix=sqldf("select count(*) as count, TYPE_OF_FILE, AREA, VESSEL, YEAR from Result Group by TYPE_OF_FILE, AREA, VESSEL, YEAR")
    Matrix=stats::aggregate(Result$TYPE_OF_FILE,by=list(Result$TYPE_OF_FILE, Result$AREA, Result$VESSEL, Result$YEAR),FUN="length")
colnames(Matrix)=c("TYPE_OF_FILE", "AREA", "VESSEL", "YEAR","x")
  ResultData = Result[!is.na(Result$HAUL_NUMBER),]


    if (nrow(Matrix)>1){
      Max=max(Matrix$x)
      Matrix2=Matrix[Matrix$x!=Max,]
      for (i in 1:nrow(Matrix2)){

        Err = ResultData[as.character(ResultData$TYPE_OF_FILE) == as.character(Matrix2$TYPE_OF_FILE[i]) &
                           as.character(ResultData$AREA) == as.character(Matrix2$AREA[i]) &
                           as.character(ResultData$VESSEL) == as.character(Matrix2$VESSEL[i]) &
                           as.character(ResultData$YEAR) == as.character(Matrix2$YEAR[i]),]

        for( k in 1:nrow(Err)){
          if (Err$YEAR[k] == Matrix2$YEAR[i]){
            write(paste("Haul",Err$HAUL_NUMBER[k],Err$GENUS[k],Err$SPECIES[k],ifelse(Err$SEX[k]=="FALSE","F", "M"),"length",Err$LENGTH_CLASS[k],"there is an inconsistent value in one or more of the fields that should be always identical in", Matrix$TYPE_OF_FILE[1]), file = Errors, append = TRUE)
          }
        }
      }
    } else
    {
      write(paste("No error occurred"), file = Errors, append = TRUE)
      check_without_errors = TRUE
    }


  } else if (Table=="TL"){
    # ------------------------------------------------------------------ TC
    write(paste("TL:"), file = Errors, append = TRUE)

    # Matrix=sqldf("select count(*) as count, TYPE_OF_FILE, AREA, VESSEL, YEAR from Result Group by TYPE_OF_FILE, AREA, VESSEL, YEAR")
    Matrix=stats::aggregate(Result$TYPE_OF_FILE,by=list(Result$TYPE_OF_FILE, Result$AREA, Result$VESSEL, Result$YEAR),FUN="length")
    colnames(Matrix)=c("TYPE_OF_FILE", "AREA", "VESSEL", "YEAR","x")

    ResultData = Result[!is.na(Result$HAUL_NUMBER),]


    if (nrow(Matrix)>1){
      Max=max(Matrix$x)
      Matrix2=Matrix[Matrix$x!=Max,]
      for (i in 1:nrow(Matrix2)){

        Err = ResultData[as.character(ResultData$TYPE_OF_FILE) == as.character(Matrix2$TYPE_OF_FILE[i]) &
                           as.character(ResultData$AREA) == as.character(Matrix2$AREA[i]) &
                           as.character(ResultData$VESSEL) == as.character(Matrix2$VESSEL[i]) &
                           as.character(ResultData$YEAR) == as.character(Matrix2$YEAR[i]),]

        for( k in 1:nrow(Err)){
          if (Err$YEAR[k] == Matrix2$YEAR[i]){
            write(paste("Haul",Err$HAUL_NUMBER[k],Err$LITTER_CATEGORY[k],Err[k,"LITTER_SUB-CATEGORY"],"there is an inconsistent value in one or more of the fields that should be always identical in", Matrix$TYPE_OF_FILE[1]), file = Errors, append = TRUE)

            }
        }
      }
    } else
    {
      write(paste("No error occurred"), file = Errors, append = TRUE)
      check_without_errors = TRUE
    }


  } else if (Table=="TE"){
    # ------------------------------------------------------------------ TC
    write(paste("TE:"), file = Errors, append = TRUE)

    # Matrix=sqldf("select count(*) as count, TYPE_OF_FILE, AREA, VESSEL, YEAR from Result Group by TYPE_OF_FILE, AREA, VESSEL, YEAR")
    Matrix=stats::aggregate(Result$TYPE_OF_FILE,by=list(Result$TYPE_OF_FILE, Result$AREA, Result$VESSEL, Result$YEAR),FUN="length")
    colnames(Matrix)=c("TYPE_OF_FILE", "AREA", "VESSEL", "YEAR","x")

    ResultData = Result[!is.na(Result$HAUL_NUMBER),]


    if (nrow(Matrix)>1){
      Max=max(Matrix$x)
      Matrix2=Matrix[Matrix$x!=Max,]
      for (i in 1:nrow(Matrix2)){

        Err = ResultData[as.character(ResultData$TYPE_OF_FILE) == as.character(Matrix2$TYPE_OF_FILE[i]) &
                           as.character(ResultData$AREA) == as.character(Matrix2$AREA[i]) &
                           as.character(ResultData$VESSEL) == as.character(Matrix2$VESSEL[i]) &
                           as.character(ResultData$YEAR) == as.character(Matrix2$YEAR[i]),]

        for( k in 1:nrow(Err)){
          if (Err$YEAR[k] == Matrix2$YEAR[i]){
            write(paste("Haul",Err$HAUL_NUMBER[k],Err$GENUS[k],Err$SPECIES[k],ifelse(Err$SEX[k]=="FALSE","F", "M"),"length",Err$LENGTH_CLASS[k],Err$INDIVIDUAL_WEIGHT[k],"there is an inconsistent value in one or more of the fields that should be always identical in", Matrix$TYPE_OF_FILE[1]), file = Errors, append = TRUE)

          }
        }
      }
    } else
    {
      write(paste("No error occurred"), file = Errors, append = TRUE)
      check_without_errors = TRUE
    }


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

return(check_without_errors)
}
