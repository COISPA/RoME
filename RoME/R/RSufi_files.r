

if (FALSE){

  wd <- "C:/Users/Utente/OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L/Rome/ROME/temp"
  RSufi_files(1994,1994,"18",wd)
}



RSufi_files<-function(Year_start,Year_end,AREA,wd){

  traitsString <- file.path(wd,"files R-Sufi",paste("traits_",Year_start,"_GSA",AREA,".csv",sep=""))
  if(file.exists(traitsString))
  {
    traits=read.csv(file=traitsString,sep=";",header=TRUE)
  }else{
    print(paste("File: ",traitsString, "NOT found" ))
  }


  taillesString <- file.path(wd,"files R-Sufi",paste("taille_",Year_start,"_GSA",AREA,".csv",sep=""))
  if(file.exists(taillesString))
  {
    tailles=read.csv(file=taillesString,sep=";",header=TRUE)
  }else{
    print(paste("File: ",taillesString, "NOT found" ))
  }


  capturesString <- file.path(wd,"files R-Sufi",paste("captures_",Year_start,"_GSA",AREA,".csv",sep=""))
  if(file.exists(capturesString))
  {
    captures=read.csv(file=capturesString,sep=";",header=TRUE)
  }else{
    print(paste("File: ",capturesString, "NOT found"))
  }

  Year_start=as.numeric(Year_start)
  Year_end=as.numeric(Year_end)
  if (Year_start< Year_end){
  for (i in ((Year_start+1):Year_end)){


    traits2String <- file.path(wd,"files R-Sufi",paste("traits_",i,"_GSA",AREA,".csv",sep=""))
    if(file.exists(traits2String))
    {
      traits2=read.csv(file=traits2String,sep=";")
      traits=rbind(traits,traits2)
    }else{
      print(paste("File: ",traits2String, "NOT found"))
    }


    tailles2String <- file.path(wd,"files R-Sufi",paste("taille_",i,"_GSA",AREA,".csv",sep=""))
    if(file.exists(tailles2String))
    {
      tailles2=read.csv(file=tailles2String,sep=";")
      tailles=rbind(tailles,tailles2)
    }else{
      print(paste("File: ",tailles2String, "NOT found"))
    }


    captures2String <- file.path(wd,"files R-Sufi",paste("captures_",i,"_GSA",AREA,".csv",sep=""))
    if(file.exists(captures2String))
    {
      captures2=read.csv(file=captures2String,sep=";")
      captures=rbind(captures,captures2)
    }else{
      print(paste("File: ",captures2String, "NOT found"))
    }



  }
  }
  tryCatch({
    write.table(traits,file=file.path(wd,"files R-Sufi",paste("traits_GSA",AREA,"_",Year_start,"-",Year_end,".csv",sep="")),sep=";",row.names=FALSE)
    write.table(captures,file=file.path(wd,"files R-Sufi",paste("captures_GSA",AREA,"_",Year_start,"-",Year_end,".csv",sep="")),sep=";",row.names=FALSE)
    write.table(tailles,file=file.path(wd,"files R-Sufi",paste("taille_GSA",AREA,"_",Year_start,"-",Year_end,".csv",sep="")),sep=";",row.names=FALSE)
  }, error = function(e)
    {
    print(e)
  })

  #unlink(file.path(tempdir(),"Logfiles"),recursive=T)
  #unlink(file.path(tempdir(),"Graphs"),recursive=T)

	if (file.exists(file.path(tempdir(), "Graphs"))){
  unlink(file.path(tempdir(),"files R-Sufi"),recursive=T)
    }

  }
