###########################################################################################################################
#   RoME: R code to perform multiple checks on MEDITS Survey data (TA, TB, TC and TE files - old and new MEDITS formats)                                   #
#   Authors: I. Bitetto, W. Zupa, M.T. Spedicato                                                                    #
#   Coispa Tecnologia & Ricerca - Stazione sperimentale per lo Studio delle Risorse del Mare                              #
#   If you have any comments or suggestions please contact the following e-mail address: bitetto@coispa.it, zupa@coispa.eu                #
#   March 2020                                                                                                            #
###########################################################################################################################
# Creation of R-SUFI files:

# traits.csv

if (FALSE){
  ResultDataTA = read.csv("C:/Users/Bitetto Isabella/OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L/Rome/ROME/data/TA_GSA18_1994-2018.csv", sep=";")

  wd <- "C:/Users/Bitetto Isabella/OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L/Rome/ROME/temp"
  suffix=paste(as.character(Sys.Date()),format(Sys.time(), "_time h%Hm%Ms%OS0"),sep="")
  create_haul(Result,wd,suffix)
}


create_haul<-function(ResultDataTA,wd,suffix){

   Format="from_2012"
  if (!file.exists(paste(wd,"Logfiles",sep="/"))){
    dir.create(file.path(wd, "Logfiles"), showWarnings = FALSE)
  }

  if (!exists("suffix")){
    suffix=paste(as.character(Sys.Date()),format(Sys.time(), "_time h%Hm%Ms%OS0"),sep="")
  }

  if (!file.exists(paste(wd,"Graphs",sep="/"))){
    dir.create(file.path(wd, "Graphs"), showWarnings = FALSE)
  }

   if (!file.exists(paste(wd,"files R-Sufi",sep="/"))){
     dir.create(file.path(wd, "files R-Sufi"), showWarnings = FALSE)
   }


  Errors <- paste(wd,"/Logfiles/Logfile_",suffix,".dat",sep="")


  ResultData = ResultDataTA

  ResultData=ResultData[ResultData$VALIDITY=="V",]
  ResultData=MEDITS.to.dd(ResultData)
  traits=matrix(nrow=nrow(ResultData),ncol=9)
  colnames(traits)=(c("Survey",  "Year",	"Haul", "Month", "Stratum",	"SweptSurface",	"Lat",	"Long","Depth") )

  traits[,1]=paste("MEDITS-GSA",as.character(ResultData$AREA[1]),sep="")
  traits[,2]=as.character(ResultData$YEAR[1])
  traits[,3]=ResultData$HAUL_NUMBER
  traits[,4]=ResultData$MONTH
  traits[,7]=round((ResultData$SHOOTING_LATITUDE+ResultData$HAULING_LATITUDE)/2,5)
  traits[,8]=round((ResultData$SHOOTING_LONGITUDE+ResultData$HAULING_LONGITUDE)/2,5)
  traits[,6]=round((ResultData$DISTANCE*(ResultData$WING_OPENING/10))/1000000,3)
  ResultData$mean_depth=(ResultData$SHOOTING_DEPTH +ResultData$HAULING_DEPTH)/2
  traits[,9]=ResultData$mean_depth
  for (i in 1:nrow(ResultData)){

    if ((ResultData$mean_depth[i]>=0)  &  (ResultData$mean_depth[i]<=50))
    { traits[i,5]=1}
    else
    {if ((ResultData$mean_depth[i]>=50)  &  (ResultData$mean_depth[i]<=100))
    { traits[i,5]=2    }
     else
     {if  ((ResultData$mean_depth[i]>=100)  &  (ResultData$mean_depth[i]<=200))
     {traits[i,5]=3}
      else
      {if  ((ResultData$mean_depth[i]>=200)  &  (ResultData$mean_depth[i]<=500))
      {traits[i,5]=4}
       else
       {if  (ResultData$mean_depth[i]>500)
       {traits[i,5]=5}
       }
      }
     }
    }
  }
  write.table(traits,file=paste(wd,"/files R-Sufi/traits_",ResultData$YEAR[1],"_GSA",ResultData$AREA[1],".csv",sep=""),row.names=FALSE,quote=FALSE,sep=";")
}

###########################################################################################################################
