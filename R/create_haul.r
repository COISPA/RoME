############################################################################################################################
#   RoME: R code to perform multiple checks on MEDITS Survey data (TA, TB, TC and TE files)                                #
#   Authors: I. Bitetto, W. Zupa, M.T. Spedicato                                                                           #
#   Coispa Tecnologia & Ricerca - Stazione sperimentale per lo Studio delle Risorse del Mare                               #
#   If you have any comments or suggestions please contact the following e-mail address: bitetto@coispa.it, zupa@coispa.it #
#   January 2022                                                                                                           #
############################################################################################################################
# Creation of R-SUFI files:

# traits.csv




create_haul<-function(ResultDataTA,year,wd,save=TRUE){

   if (FALSE){
     ResultDataTA = RoME::TA
     year=2007
     wd <- tempdir()
     create_haul(ResultDataTA,year,wd,save=FALSE)
    }

   Format="from_2012"





   if (!file.exists(file.path(wd,"files R-Sufi"))){
     dir.create(file.path(wd, "files R-Sufi"), showWarnings = FALSE)
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
   ResultDataTA <- ResultDataTA[ResultDataTA$YEAR == year, ]
   ########################################

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

  if(save) {
    write.table(traits,file=file.path(wd,"files R-Sufi",paste("traits_",ResultData$YEAR[1],"_GSA",ResultData$AREA[1],".csv",sep="")),row.names=FALSE,quote=FALSE,sep=";")
    # if (file.exists(file.path(tempdir(),"files R-Sufi"))){
    #   unlink(file.path(tempdir(),"files R-Sufi"),recursive=T)
    # }
  } else {
    return(as.data.frame(traits))
  }



  }
