############################################################################################################################
#   RoME: R code to perform multiple checks on MEDITS Survey data (TA, TB, TC and TE files)                                #
#   Authors: I. Bitetto, W. Zupa, M.T. Spedicato                                                                           #
#   Coispa Tecnologia & Ricerca - Stazione sperimentale per lo Studio delle Risorse del Mare                               #
#   If you have any comments or suggestions please contact the following e-mail address: bitetto@coispa.it, zupa@coispa.it #
#   January 2022                                                                                                           #
############################################################################################################################


# Check if weights and numbers in TB are consistent

if (FALSE){
    wd <- tempdir()
    ResultDataTB = read.table("D:\\OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L\\QualiTrain\\Task 2\\Data\\MEDITS\\medits_tb_1.csv",sep=";",header=TRUE)# tb # RoME::TB
    # ResultDataTB$TOTAL_NUMBER_IN_THE_HAUL[27]=0
    year=2012
    DataTargetSpecies <- RoME::DataTargetSpecies
    suffix=NA
    check_weight(ResultDataTB, year, RoME::DataTargetSpecies, wd, suffix)
  }


check_weight<-function(ResultDataTB,year,DataTargetSpecies=DataTargetSpecies,wd,suffix){

  oldpar <- par()
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
  ResultDataTB <- ResultDataTB[ResultDataTB$YEAR == year, ]
  ########################################

   numberError = 0
   Result = ResultDataTB
  write(paste("\n----------- check consistency of weight and number TB - ",Result$YEAR[1]), file = Errors, append = TRUE)


  Weight=DataTargetSpecies #read.csv(file=paste(DataTargetSpecies,".csv",sep=""),sep=";",header=TRUE)

  #queryData= paste("SELECT TYPE_OF_FILE, HAUL_NUMBER, GENUS, SPECIES,TOTAL_WEIGHT_IN_THE_HAUL,TOTAL_NUMBER_IN_THE_HAUL from Result where HAUL_NUMBER is not null order by HAUL_NUMBER", sep="")
  ResultData = Result[order(Result$HAUL_NUMBER),c("TYPE_OF_FILE", "HAUL_NUMBER", "GENUS", "SPECIES","TOTAL_WEIGHT_IN_THE_HAUL","TOTAL_NUMBER_IN_THE_HAUL")] #sqldf(queryData)

  ResultData$species=paste(ResultData$GENUS,ResultData$SPECIES,sep="")


    ResultData =  ResultData[ResultData$TOTAL_NUMBER_IN_THE_HAUL!=0 & !is.na(ResultData$TOTAL_NUMBER_IN_THE_HAUL),]
    ResultData$mean_weight=round(ResultData$TOTAL_WEIGHT_IN_THE_HAUL/ResultData$TOTAL_NUMBER_IN_THE_HAUL,3)

    if (nrow(ResultData)>0){


    #queryData2= paste("SELECT count(*) as occurrence, GENUS, SPECIES from Result where HAUL_NUMBER is not null group by GENUS, SPECIES", sep="")
    present= aggregate(Result$GENUS,by=list(Result$GENUS, Result$SPECIES),FUN="length")
    colnames(present)=c("GENUS","SPECIES","occurrence")
    #sqldf(queryData2)

  present$species=paste(present$GENUS,present$SPECIES,sep="")
  present$present=FALSE
#   odbcClose(channelData)
  nb_graphs=0
  nb_graphs_to_be_printed=0

  i=26
  for (i in 1:nrow(ResultData)){
    FoundInTable=Weight[as.character(Weight$SPECIES)==as.character(ResultData$species[i]),]
    FoundInTable=FoundInTable[is.na(FoundInTable$MIN_WEIGHT)==FALSE ,]
    if (nrow(FoundInTable)!=0){
      if (((ResultData$mean_weight[i]<FoundInTable$MIN_WEIGHT[1]) | (ResultData$mean_weight[i]>FoundInTable$MAX_WEIGHT[1]))==TRUE)
      {
        write(paste("Warning: Haul ",ResultData$HAUL_NUMBER[i]," ",ResultData$species[i]," : mean weight= ", ResultData$mean_weight[i]," out of boundaries (",FoundInTable$MIN_WEIGHT[1],",",FoundInTable$MAX_WEIGHT[1],") in ", ResultData$TYPE_OF_FILE[1],sep=""), file = Errors, append = TRUE)
      }
    } else {
      if((present[present$species==ResultData$species[i],]$present==FALSE) & (present[present$species==ResultData$species[i],]$occurrence>=10)){
        nb_graphs_to_be_printed = nb_graphs_to_be_printed + 1
        present[present$species==ResultData$species[i],]$present=TRUE

        xlabels <- ResultData[ResultData$species==ResultData$species[i] & !is.infinite(ResultData$mean_weight),]$HAUL_NUMBER
        X= 1:length(ResultData[ResultData$species==ResultData$species[i] & !is.infinite(ResultData$mean_weight),]$HAUL_NUMBER)
        Y=ResultData[ResultData$species==ResultData$species[i]& !is.infinite(ResultData$mean_weight),]$mean_weight


        if (length(X)>=10) {
          if ( nb_graphs<20) {

              dev.new(width=60, height=60)

            plot(X,Y,main=paste(ResultData$species[i],"-",Result$YEAR[1]),xlab="HAUL number",ylab="mean weight",type="b",pch=".")
            text(X,Y,labels=xlabels)
            nb_graphs= nb_graphs+1
          }
        }
      }

    }

  }

  if (nb_graphs_to_be_printed >20){
    write(paste("Too many graphs to be displayed: all the graphs have been saved (up to a max of 120) in .tif file stored in Graphs directory",sep=""), file = Errors, append = TRUE)
    print(paste("Too many graphs to be displayed: all the graphs have been saved (up to a max of 120) in .tif file stored in Graphs directory",sep=""),quote=FALSE)
  }

  if (nb_graphs!=0){
    present_true = present[present$present == TRUE, ]



    x=nb_graphs_to_be_printed
    m=6
    t1<-floor(x/m)

    if ((x-t1*m) ==0){
      nb_sheets= as.integer(nb_graphs_to_be_printed/6)
    }  else {
      nb_sheets= as.integer(nb_graphs_to_be_printed/6)+1
    }

    # number of plots in the current .tif
    i=1
    for (i in 1:nb_sheets) {

      if ((6*i)>nrow(present_true) ){
        nb_loops = nrow(present_true)
      }  else {
        nb_loops = 6*i
      }

      if (i<=20){
        # tiff(filename=file.path(wd,"Graphs",paste("check_mean_weight_",Result$AREA[1],"_",Result$YEAR[1],"_", i,".tif",sep="")),width=12, height=8, bg="white", units="in", res=300, compression = 'lzw', pointsize = 1/300)
		jpeg(filename=file.path(wd,"Graphs",paste("check_mean_weight_",Result$AREA[1],"_",Result$YEAR[1],"_", i,".jpeg",sep="")),width=12, height=8, bg="white", units="in", res=300,quality=80)
        par(mfrow=c(3,2), mai=c(0.6,0.6,0.6,0.6), omi=c(0.8,0.8,1,0.8))
        for (m in (6*i-5):(nb_loops)) {
          xlabels <- ResultData[ResultData$species==ResultData$species[m] & !is.infinite(ResultData$mean_weight),]$HAUL_NUMBER
          X= 1:length(ResultData[ResultData$species==ResultData$species[m] & !is.infinite(ResultData$mean_weight),]$HAUL_NUMBER)
          Y=ResultData[ResultData$species==ResultData$species[m]& !is.infinite(ResultData$mean_weight),]$mean_weight
          if (length(X)!=0){
            plot(X,Y,main=paste(ResultData$species[m],"-",Result$YEAR[1]),xlab="HAUL number",ylab="mean weight",type="b",pch=".")
            text(X,Y,labels=xlabels)
          }
        }


      }
      dev.off()
    }

  }


  if (nb_graphs> 0) {
    write(paste("Warning: See graphs generated and saved in working directory about the species without mean weight range"), file = Errors, append = TRUE)
  }
 }
  if (numberError ==0) {
    write(paste("No error occurred"), file = Errors, append = TRUE)
  }



#    if (file.exists(file.path(tempdir(), "Logfiles"))){
#   unlink(file.path(tempdir(),"Logfiles"),recursive=T)
#   }
#   if (file.exists(file.path(tempdir(), "Graphs"))){
#   unlink(file.path(tempdir(),"Graphs"),recursive=T)
#     }
# 	if (file.exists(file.path(tempdir(), "files R-Sufi"))){
#   unlink(file.path(tempdir(),"files R-Sufi"),recursive=T)
#     }

  if (numberError ==0) {
    return(TRUE)
  } else { return(FALSE) }

}

###############################################################################

