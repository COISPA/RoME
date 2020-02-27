###########################################################################################################################
#   RoME: R code to perform multiple checks on MEDITS Survey data (TA, TB, TC and TE files - old and new MEDITS formats)                                   #                   
#   Authors: I. Bitetto, M.T. Facchini, M.T. Spedicato                                                                    #
#   Coispa Tecnologia & Ricerca - Stazione sperimentale per lo Studio delle Risorse del Mare                              #
#   If you have any comments or suggestions please contact the following e-mail address: bitetto@coispa.it                #                                                                                  
#   March 2013                                                                                                            #
################################################################################
# Check if weights and numbers in TB are consistent

check_weight<-function(ResultDataTB,DataTargetSpecies){
  numberError = 0
   Result = ResultDataTB #read.csv(paste(Data,".csv",sep=""), sep=";", header=TRUE)
  write(paste("
              ----------- check consistency of weight and number TB - ",Result$YEAR[1]), file = Errors, append = TRUE)
 
  
  if (Type_of_files==".csv"){
#     write.xls(Result,file=paste(Data,".xls", sep = ""))}
  }
#   channelData <- odbcConnectExcel(paste(Data,".xls", sep = ""))
  Weight=read.csv(file=paste(DataTargetSpecies,".csv",sep=""),sep=";",header=TRUE)
  if (Format=="before_2012") {
    queryData= paste("SELECT TYPE_OF_FILE, HAUL_NUMBER, GENUS, SPECIES,TOTAL_WEIGHT_IN_HAUL,TOTAL_NUMBER_IN_HAUL from Result where HAUL_NUMBER is not null order by HAUL_NUMBER", sep="")
  } else {
    queryData= paste("SELECT TYPE_OF_FILE, HAUL_NUMBER, GENUS, SPECIES,TOTAL_WEIGHT_IN_THE_HAUL,TOTAL_NUMBER_IN_THE_HAUL from Result where HAUL_NUMBER is not null order by HAUL_NUMBER", sep="")  
  }
  ResultData=sqldf(queryData)
  ResultData$species=paste(ResultData$GENUS,ResultData$SPECIES,sep="")
  
  if (Format=="before_2012"){
    ResultData = ResultData[ResultData$TOTAL_NUMBER_IN_HAUL !=0,]
    ResultData$mean_weight=round(ResultData$TOTAL_WEIGHT_IN_HAUL/ResultData$TOTAL_NUMBER_IN_HAUL,3)
  } else {
    ResultData =  ResultData[ResultData$TOTAL_NUMBER_IN_THE_HAUL!=0,]
    ResultData$mean_weight=round(ResultData$TOTAL_WEIGHT_IN_THE_HAUL/ResultData$TOTAL_NUMBER_IN_THE_HAUL,3) 
  }
  
  queryData2= paste("SELECT count(*) as occurrence, GENUS, SPECIES from Result where HAUL_NUMBER is not null group by GENUS, SPECIES", sep="")
  present=sqldf(queryData2)
  present$species=paste(present$GENUS,present$SPECIES,sep="")
  present$present=FALSE
#   odbcClose(channelData)
  nb_graphs=0
  nb_graphs_to_be_printed=0
  
  for (i in 1:nrow(ResultData)){
    FoundInTable=Weight[as.character(Weight$SPECIES)==as.character(ResultData$species[i]),]
    FoundInTable=FoundInTable[is.na(FoundInTable$MIN_WEIGHT)==FALSE,]
    if (nrow(FoundInTable)!=0){
      if (((ResultData$mean_weight[i]<FoundInTable$MIN_WEIGHT[1]) | (ResultData$mean_weight[i]>FoundInTable$MAX_WEIGHT[1]))==TRUE)
      {
        write(paste("Warning: Haul ",ResultData$HAUL_NUMBER[i]," ",ResultData$species[i]," : mean weight= ", ResultData$mean_weight[i]," out of boundaries (",FoundInTable$MIN_WEIGHT[1],",",FoundInTable$MAX_WEIGHT[1],") in ", ResultData$TYPE_OF_FILE[1],sep=""), file = Errors, append = TRUE) 
      }
    } else { 
      if((present[present$species==ResultData$species[i],]$present==FALSE) & (present[present$species==ResultData$species[i],]$occurrence>=10)){
        nb_graphs_to_be_printed = nb_graphs_to_be_printed + 1
        present[present$species==ResultData$species[i],]$present=TRUE
        
        X=ResultData[ResultData$species==ResultData$species[i] & !is.infinite(ResultData$mean_weight),]$HAUL_NUMBER
        Y=ResultData[ResultData$species==ResultData$species[i]& !is.infinite(ResultData$mean_weight),]$mean_weight
        
        
        if (length(X)>=10) {  
          if ( nb_graphs<20) {
            if(.Platform$OS.type=="windows") {
              windows(width=60, height=60)
            } #else if(.Platform$OS.type=="unix")
#            {
#              X11(width=60, height=60)
#            }else{
#              quartz(width=60, height=60)
#            }
            plot(X,Y,main=paste(ResultData$species[i],"-",Result$YEAR[1]),xlab="HAUL number",ylab="mean weight",type="b",pch=".")
            text(X,Y,labels=X) 
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
    
    if (.mod(nb_graphs_to_be_printed,6) ==0){
      nb_sheets= as.integer(nb_graphs_to_be_printed/6)
    }  else {
      nb_sheets= as.integer(nb_graphs_to_be_printed/6)+1
    }
    
    # number of plots in the current .tif
    for (i in 1:nb_sheets) {
      
      if ((6*i)>nrow(present_true) ){
        nb_loops = nrow(present_true)
      }  else {
        nb_loops = 6*i
      }
      
      if (i<=20){
        tiff(file=paste(getwd(),"/Graphs/check_mean_weight_",Result$AREA[1],"_",Result$YEAR[1],"_", i,".tif",sep=""), width=21, height=29.7, bg="white", units="cm", compression="none",res=200)
        par(mfrow=c(3,2), mai=c(0.6,0.6,0.6,0.6), omi=c(0.8,0.8,1,0.8))
        for (m in (6*i-5):(nb_loops)) {     
          X=ResultData[ResultData$species==present_true$species[m] & !is.infinite(ResultData$mean_weight),]$HAUL_NUMBER
          Y=ResultData[ResultData$species==present_true$species[m] & !is.infinite(ResultData$mean_weight),]$mean_weight
          if (length(X)!=0){
            plot(X,Y,main=paste(present_true$species[m],"-",Result$YEAR[1]),xlab="HAUL number",ylab="mean weight",type="b",pch=".")
            text(X,Y,labels=X) 
          }
        }
        
        
      }    
      dev.off()
    }  
    
  }
  
  
  if (nb_graphs> 0) {
    write(paste("Warning: See graphs generated and saved in working directory about the species without mean weight range"), file = Errors, append = TRUE) 
  }  
  
  if (numberError ==0) {
    write(paste("No error occurred"), file = Errors, append = TRUE)
  }  
  
  if (Type_of_files==".csv"){
#     unlink(paste(Data,".xls", sep = ""))
  }
  
  if (numberError ==0) {
    return(TRUE)
  } else { return(FALSE) }
  
}

###############################################################################           
.mod<-function(x,m)
{
  t1<-floor(x/m)
  return(x-t1*m)
}