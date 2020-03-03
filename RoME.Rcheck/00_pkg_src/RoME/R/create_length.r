###########################################################################################################################
#   RoME: R code to perform multiple checks on MEDITS Survey data (TA, TB, TC and TE files - old and new MEDITS formats)                                   #                   
#   Authors: I. Bitetto, M.T. Facchini, M.T. Spedicato                                                                    #
#   Coispa Tecnologia & Ricerca - Stazione sperimentale per lo Studio delle Risorse del Mare                              #
#   If you have any comments or suggestions please contact the following e-mail address: bitetto@coispa.it                #                                                                                  
#   March 2013                                                                                                            #
###########################################################################################################################

# Creation of R-SUFI files:
# tailles.csv
create_length<-function(ResultData_){
  ResultData = ResultData_ #read.csv(paste(Data,".csv",sep=""), sep=";", header=TRUE, stringsAsFactors=FALSE)
  ResultData= ResultData[as.character(ResultData$MATURITY)!="ND",]
  
  if (any(!is.na(ResultData$MATSUB)) & (any(as.character(ResultData$MATSUB)=="A" )| any(as.character(ResultData$MATSUB)=="B")| any(as.character(ResultData$MATSUB)=="C")| any(as.character(ResultData$MATSUB)=="D")| any(as.character(ResultData$MATSUB)=="E"))) {
  mat_scale="new"
  } else {
  mat_scale="old"}
  
  
  taille=matrix(nrow=nrow(ResultData),ncol=10)
  colnames(taille)=(c("Survey",  "Year",	"Haul",	"Species",	"Sex", "Maturity", "Longeur",	"Nb",	"Weight", "Age") )
  ResultData$species=paste(ResultData$GENUS,ResultData$SPECIES,sep="")

  if(Format=="before_2012"){
  cat_fau=read.csv(file=paste(DataTargetSpecies,".csv",sep=""),sep=";",header=TRUE, stringsAsFactors=FALSE)
  cat_fau=cat_fau[cat_fau$FAUNISTIC_CATEGORY!="",]
  } else {
  cat_fau=read.csv(file=paste(DataSpecies,".csv",sep=""),sep=";",header=TRUE, stringsAsFactors=FALSE)
  cat_fau=cat_fau[cat_fau$CATFAU!="",]  
  }
  
  ResultData$catfau = NA
   if(Format=="before_2012"){
   for (i in 1:nrow(ResultData)){     
      if (length(cat_fau$FAUNISTIC_CATEGORY[cat_fau$SPECIES==ResultData$species[i]])!=0) {
      ResultData$catfau[i]= as.character(cat_fau$FAUNISTIC_CATEGORY[cat_fau$SPECIES==ResultData$species[i]])}  
  } 
  }  else {
   for (i in 1:nrow(ResultData)){   
   if (length(cat_fau$CATFAU[cat_fau$MeditsCode==ResultData$species[i]])!=0) {
   ResultData$catfau[i]= as.character(cat_fau$CATFAU[cat_fau$MeditsCode==ResultData$species[i]])
   }
   } 
  
  }  
   
  
  taille[,1]=paste("MEDITS-GSA",as.character(ResultData$AREA[1]),sep="")
  taille[,2]=as.character(ResultData$YEAR[1])
  taille[,3]=ResultData$HAUL_NUMBER
  taille[,4]=paste(ResultData$GENUS,ResultData$SPECIES,sep="")
  for (i in 1:nrow(ResultData)){
    taille[i,5]=ifelse(ResultData$SEX[i]=="M","m",ifelse(ResultData$SEX[i]=="F","f","i"))         # sex
  }
  
  for (j in 1:nrow(taille)){
    if (!is.na(ResultData$catfau[j])) {
    if (ResultData$catfau[j]=="B")      { 
          if (ResultData$SEX[j]=="M") {
               if (ResultData$YEAR[j]<2006) {         # before 2006 no stage for male cristaceans
               taille[j,6]=NA 
               } else {                  
                   if (mat_scale=="old") {                     # male after 2006
                   taille[j,6]=ifelse(as.numeric(as.character(ResultData$MATURITY[j]))>=2,"m","i")
                   } else {
                   if(as.numeric(as.character(ResultData$MATURITY[j]))==0 | as.numeric(as.character(ResultData$MATURITY[j]))==1 | ((as.numeric(as.character(ResultData$MATURITY[j]))==2) & (as.character(ResultData$MATSUB[j]))=="A")) {
                   taille[j,6]=  "i"
                   } else {
                    taille[j,6]=  "m"
                    }
                    }
               }
           } else {     # females
           if  (ResultData$YEAR[j]<2006) {   # females before 2006
           taille[j,6]=ifelse(as.numeric(as.character(ResultData$MATURITY[j]))>=2,"m","i")
           }else {
                   if (mat_scale=="old") {                     # females after 2006
                   taille[j,6]=ifelse(as.numeric(as.character(ResultData$MATURITY[j]))>=2,"m","i")
                   } else {
                   if(as.numeric(as.character(ResultData$MATURITY[j]))==0 | as.numeric(as.character(ResultData$MATURITY[j]))==1 |((as.numeric(as.character(ResultData$MATURITY[j]))==2) & (as.character(ResultData$MATSUB[j]))=="A")) {
                   taille[j,6]=  "i"
                   } else {
                    taille[j,6]=  "m"
                   }
                   }
           }
           }
          } else { # other species 
           if ((ResultData$catfau[j]!="S")& (ResultData$catfau[j]!="Ae")){ #no selachians  
           if  (ResultData$YEAR[j]<2006) {  
           taille[j,6]=ifelse(as.numeric(as.character(ResultData$MATURITY[j]))>=3,"m","i")
           }  else {
                   if (mat_scale=="old") {                     # females after 2006
                   taille[j,6]=ifelse(as.numeric(as.character(ResultData$MATURITY[j]))>=3,"m","i")
                   } else {
                   if(as.numeric(as.character(ResultData$MATURITY[j]))==0 | as.numeric(as.character(ResultData$MATURITY[j]))==1 |((as.numeric(as.character(ResultData$MATURITY[j]))==2) & (as.character(ResultData$MATSUB[j]))=="A")) {
                   taille[j,6]=  "i"
                   } else {
                    taille[j,6]=  "m"
                   }
                   }
           }  # if on the year <2006 
               
           } else { # if on selachians
           taille[j,6]=ifelse(as.numeric(as.character(ResultData$MATURITY[j]))>=3,"m","i") 
           } 
           }
          
            
      }
  }
  taille[,7]=ResultData$LENGTH_CLASS/10
  if ((Format=="before_2012")| (DataTE=="")) {
  taille[,8]=ResultData$NUMBER_OF_INDIVIDUALS_IN_THE_LENGTH_CLASS_AND_MATURITY_STAGE
  } else if (DataTE!=""){
  taille[,8]=1
  }
  if ((Format=="before_2012")| (DataTE=="")) {
  taille[,9] ="NA"
  taille[,10] = "NA"
  } else if (DataTE!=""){
  taille[,9] =ResultData$INDIVIDUAL_WEIGHT
  
  ResultData$AGE[which(is.na(ResultData$AGE) | as.character(ResultData$AGE)=="UR" | as.character(ResultData$AGE)=="NR" | as.character(ResultData$AGE) == "-1") | as.character(ResultData$AGE) == "" ] <- NA
  
  taille[,10] <- ResultData$AGE
  }
  if ((Format!="from_2012")| (DataTE=="")){
  #write.xlsx(taille,file="length.xls", sheetName="Sheet1" , col.names=TRUE)
  
  #taille <- taille[,-1]
  #print(length(taille))
  
  write.table(taille,file="length.csv",sep=";", col.names=TRUE, row.names=FALSE)
  
  
  # eliminate duplicated records
  TableT <- read.csv("length.csv",sep=";", header=TRUE, stringsAsFactors=FALSE)
  #channel <- odbcConnectExcel(Table)
  #query="select Survey,	Year,	Haul,	Species,	Sex, Maturity, Longeur,	sum(Nb), Weight, Age  from Table Group by Survey,	Year,	Haul,	Species,	Sex, Maturity, Longeur,	Weight, Age"
  #query="select Survey,	Year,	Haul,	Species,	Sex, Maturity, Longeur,	Nb, Weight, Age  from Table Group by Survey,	Year,	Haul,	Species,	Sex, Maturity, Longeur, Weight, Age"
  query="select Survey, Year, Haul, Species, Sex, Maturity, Longeur, Nb, Weight, Age  from TableT Group by Survey, Year, Haul, Species, Sex, Maturity, Longeur, Nb, Weight, Age"
  
  Matrix=sqldf(query)
  #odbcClose(channel)
  # unlink("length.xls")
  unlink("length.csv")
   } else {

 
  #write.csv(taille,file="length.csv",sep=";", col.names=TRUE, row.names=FALSE)
  write.table(taille, file="length.csv" ,row.names=FALSE, col.names=TRUE, sep=";")
  # eliminate duplicated records
  #channel <- odbcConnectExcel(Table)
  
  
  
  TableT <- read.csv("length.csv" ,sep=";", header=TRUE, stringsAsFactors=FALSE)
  #Table <- read.xlsx("length.xls", sheetName="Sheet1")
  query="select Survey, Year, Haul, Species, Sex, Maturity, Longeur, Nb, Weight, Age from TableT"
  Matrix=sqldf(query)
  #odbcClose(channel)
  # unlink("length.xls")
  unlink("length.csv")
  
  #######
  #  write.xlsx(taille,file="length.xls",colNames=TRUE)
  # eliminate duplicated records
  #Table="length.xls"
  #channel <- odbcConnectExcel(Table)
  #query=paste("select Survey,	Year,	Haul,	Species,	Sex, Maturity, Longeur,	Nb, Weight, Age  from [Sheet1$]", sep="") 
  #Matrix=sqlQuery(channel, query)
  #odbcClose(channel)
  # unlink("length.xls")
  #######

   }
  colnames(Matrix)=(c("Survey",	"Year",	"Haul",	"Species",	"Sex", "Maturity", "Length",	"Number",	"Weight", "Age") )
  

  rSufiString <- paste("files R-Sufi/taille_",ResultData$YEAR[1],"_GSA",ResultData$AREA[1],".csv",sep="")
#   rSufiString <- paste("taille_",Year,"_GSA",ResultData$AREA[1],".csv",sep="")
  
  if(file.exists(paste(getwd(),rSufiString,sep=""))) file.remove(paste(getwd(),rSufiString,sep=""))
  
  
  write.table(Matrix,file=rSufiString, col.names=TRUE, row.names=FALSE, quote=FALSE, sep=";", append=FALSE) 
  
  #write.csv(Matrix,file=rSufiString, row.names=FALSE, quote=FALSE, sep=";")
  }

###########################################################################################################################
