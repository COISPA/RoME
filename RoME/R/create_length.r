###########################################################################################################################
#   RoME: R code to perform multiple checks on MEDITS Survey data (TA, TB, TC and TE files - old and new MEDITS formats)                                   #
#   Authors: I. Bitetto, W. Zupa, M.T. Spedicato                                                                    #
#   Coispa Tecnologia & Ricerca - Stazione sperimentale per lo Studio delle Risorse del Mare                              #
#   If you have any comments or suggestions please contact the following e-mail address: bitetto@coispa.it                #
#   March 2020                                                                                                            #
###########################################################################################################################
if (FALSE){
  Result = read.csv("C:/Users/Bitetto Isabella/OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L/Rome/ROME/data/TC_GSA18_1994-2018.csv", sep=";")
  Result =Result[Result$YEAR==1994,]
  wd <- "C:/Users/Bitetto Isabella/OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L/Rome/ROME/temp"
  #suffix=paste(as.character(Sys.Date()),format(Sys.time(), "_time_h%Hm%Ms%OS0"),sep="")
  #DataTE=""
  create_length(Result,TM_list,wd)
}


# Creation of R-SUFI files:
# tailles.csv

create_length<-function(ResultData,DataSpecies=RoME::TM_list,wd){

  #if (is.na(DataSpecies)){
   # DataSpecies=RoME::TM_list
  #}

  if (!file.exists(file.path(wd,"files R-Sufi"))){
    dir.create(file.path(wd, "files R-Sufi"), showWarnings = FALSE)
  }

  ResultData= ResultData[as.character(ResultData$MATURITY)!="ND" & as.character(ResultData$MATURITY)!="",]

  if (any(!is.na(ResultData$MATSUB)) & (any(as.character(ResultData$MATSUB)=="A" )| any(as.character(ResultData$MATSUB)=="B")| any(as.character(ResultData$MATSUB)=="C")| any(as.character(ResultData$MATSUB)=="D")| any(as.character(ResultData$MATSUB)=="E"))) {
  mat_scale="new"
  } else {
  mat_scale="old"}


  taille=matrix(nrow=nrow(ResultData),ncol=10)
  colnames(taille)=(c("Survey",  "Year",	"Haul",	"Species",	"Sex", "Maturity", "Longeur",	"Nb",	"Weight", "Age") )
  ResultData$species=paste(ResultData$GENUS,ResultData$SPECIES,sep="")


  cat_fau=DataSpecies #read.csv(file=paste(,".csv",sep=""),sep=";",header=TRUE, stringsAsFactors=FALSE)
  cat_fau=cat_fau[cat_fau$CATFAU!="",]


  ResultData$catfau = NA

   for (i in 1:nrow(ResultData)){
   if (length(cat_fau$CATFAU[cat_fau$MeditsCode==ResultData$species[i]])!=0) {
   ResultData$catfau[i]= as.character(cat_fau$CATFAU[cat_fau$MeditsCode==ResultData$species[i]])

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
 if (as.character(ResultData[1,1])=="TE"){
  taille[,8]=1
   taille[,9] =ResultData$INDIVIDUAL_WEIGHT

  ResultData$AGE[which(is.na(ResultData$AGE) | as.character(ResultData$AGE)=="UR" | as.character(ResultData$AGE)=="NR" | as.character(ResultData$AGE) == "-1") | as.character(ResultData$AGE) == "" ] <- NA

  taille[,10] <- ResultData$AGE
  } else {

  taille[,8]=ResultData$NO_OF_INDIVIDUAL_OF_THE_ABOVE_SEX_MEASURED
}

  if (as.character(ResultData[1,1])=="TC"){

  #write.table(taille,file="length.csv",sep=";", col.names=TRUE, row.names=FALSE)


  # eliminate duplicated records
  TableT <-data.frame(taille) # read.csv("length.csv",sep=";", header=TRUE, stringsAsFactors=FALSE)
  class(TableT$Age) ="character"
  TableT$Age= "NA"
  class(TableT$Weight) ="character"
  TableT$Weight = "NA"
  #channel <- odbcConnectExcel(Table)
  #query="select Survey,	Year,	Haul,	Species,	Sex, Maturity, Longeur,	sum(Nb), Weight, Age  from Table Group by Survey,	Year,	Haul,	Species,	Sex, Maturity, Longeur,	Weight, Age"
  #query="select Survey,	Year,	Haul,	Species,	Sex, Maturity, Longeur,	Nb, Weight, Age  from Table Group by Survey,	Year,	Haul,	Species,	Sex, Maturity, Longeur, Weight, Age"
  #query="select Survey, Year, Haul, Species, Sex, Maturity, Longeur, Nb, Weight, Age  from TableT Group by Survey, Year, Haul, Species, Sex, Maturity, Longeur, Nb, Weight, Age"

  Matrix= aggregate(as.numeric(TableT$Nb),by=list(TableT$Survey, TableT$Year, TableT$Haul, TableT$Species, TableT$Sex, TableT$Maturity, TableT$Longeur, TableT$Weight, TableT$Age),FUN="sum") #sqldf(query)
  colnames(Matrix)=c("Survey", "Year", "Haul", "Species", "Sex", "Maturity", "Longeur", "Weight", "Age","Nb")
  #odbcClose(channel)
  # unlink("length.xls")
  #unlink("length.csv")
  Matrix=Matrix[,c(1:7,10,8:9)]
   } else if(ResultData[1,1]=="TE") {


  #write.csv(taille,file="length.csv",sep=";", col.names=TRUE, row.names=FALSE)
  #write.table(taille, file="length.csv" ,row.names=FALSE, col.names=TRUE, sep=";")
  # eliminate duplicated records
  #channel <- odbcConnectExcel(Table)



  TableT <- data.frame(taille) #read.csv("length.csv" ,sep=";", header=TRUE, stringsAsFactors=FALSE)
  TableT <-data.frame(taille) # read.csv("length.csv",sep=";", header=TRUE, stringsAsFactors=FALSE)
  class(TableT$Age) ="character"
  TableT$Age= "NA"
  class(TableT$Weight) ="character"
  TableT$Weight = "NA"

  #Table <- read.xlsx("length.xls", sheetName="Sheet1")
  #query="select Survey, Year, Haul, Species, Sex, Maturity, Longeur, Nb, Weight, Age from TableT"
  #Matrix=sqldf(query)

  Matrix= aggregate(as.numeric(TableT$Nb),by=list(TableT$Survey, TableT$Year, TableT$Haul, TableT$Species, TableT$Sex, TableT$Maturity, TableT$Longeur, TableT$Weight, TableT$Age),FUN="sum") #sqldf(query)
  colnames(Matrix)=c("Survey", "Year", "Haul", "Species", "Sex", "Maturity", "Longeur", "Weight", "Age","Nb")
  Matrix=Matrix[,c(1:7,10,8:9)]
  #odbcClose(channel)
  # unlink("length.xls")
  #unlink("length.csv")

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


  rSufiString <- file.path(wd,"files R-Sufi",paste("taille_",ResultData$YEAR[1],"_GSA",ResultData$AREA[1],".csv",sep=""))
#   rSufiString <- paste("taille_",Year,"_GSA",ResultData$AREA[1],".csv",sep="")

  if(file.exists(paste(rSufiString,sep=""))) file.remove(paste(rSufiString,sep=""))


  write.table(Matrix,file=rSufiString, col.names=TRUE, row.names=FALSE, quote=FALSE, sep=";", append=FALSE)
  if (file.exists(file.path(tempdir(),"files R-Sufi"))){
  unlink(file.path(tempdir(),"files R-Sufi"),recursive=T)
}
  #write.csv(Matrix,file=rSufiString, row.names=FALSE, quote=FALSE, sep=";")
  }
