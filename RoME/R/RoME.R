###########################################################################################################################
#   RoME: R code to perform multiple checks on MEDITS Survey data (TA, TB, TC, TT, TD and TE files                        #
#   - old and new MEDITS formats)                                                                                         #
#   Authors: I. Bitetto, M.T. Facchini, M.T. Spedicato                                                                    #
#   Coispa Tecnologia & Ricerca - Stazione sperimentale per lo Studio delle Risorse del Mare                              #
#   If you have any comments or suggestions please contact the following e-mail address: bitetto@coispa.it                #
#   March 2013                                                                                                            #
###########################################################################################################################

# IMPORTANT: set the name of your TA, TB, TC, TD, TT, TE (that have to be in your working directory)

# VAR INIZIALIZING --------------------------------------------------------






# VAR END --------------------------------------------------------

RoME_open <- function()
{
  stringsAsFactors=FALSE

  assign("AREA",value=NA,envir=.GlobalEnv)
  assign("Format",value=NA,envir=.GlobalEnv)
  assign("Errors",value=NA,envir=.GlobalEnv)
  assign("DataTA",value=NA,envir=.GlobalEnv)
  assign("DataTB",value=NA,envir=.GlobalEnv)
  assign("DataTC",value=NA,envir=.GlobalEnv)
  assign("DataTD",value=NA,envir=.GlobalEnv)
  assign("DataTE",value=NA,envir=.GlobalEnv)
  assign("DataTT",value=NA,envir=.GlobalEnv)
  assign("DataTL",value=NA,envir=.GlobalEnv)

#   assign("Type_of_files",value=NA,envir=.GlobalEnv)
  assign("Type_of_files",value=".csv",envir=.GlobalEnv)
  assign("Length_weight",value=NA,envir=.GlobalEnv)
  assign("DataTargetSpecies",value=NA,envir=.GlobalEnv)
  assign("Year",value=NA,envir=.GlobalEnv)
  assign("DataSpecies",value=NA,envir=.GlobalEnv)
  assign("channelTA",value=NA,envir=.GlobalEnv)
  assign("sqlQuery",value=NA,envir=.GlobalEnv)
  assign("Stratification",value=NA,envir=.GlobalEnv)
  assign("DataMatStages",value=NA,envir=.GlobalEnv)
  assign("start_temp",value=NA,envir=.GlobalEnv)
  assign("end_temp",value=NA,envir=.GlobalEnv)
  assign("Year_start",value=NA,envir=.GlobalEnv)
  assign("Year_end",value=NA,envir=.GlobalEnv)
  assign("working_tables",value=NA,envir=.GlobalEnv)
  assign("ResultDataTA",value=NA,envir=.GlobalEnv)
  assign("ResultDataTB",value=NA,envir=.GlobalEnv)
  assign("ResultDataTC",value=NA,envir=.GlobalEnv)
  assign("ResultDataTT",value=NA,envir=.GlobalEnv)
  assign("ResultDataTD",value=NA,envir=.GlobalEnv)
  assign("ResultDataTE",value=NA,envir=.GlobalEnv)
  assign("ResultDataTL",value=NA,envir=.GlobalEnv)
  assign("AssociationsTL",value=NA,envir=.GlobalEnv)




#   if(!is.na(Format)){ #exists("DataTA") & exists("DataTB") & exists("DataTC") &
  if(!is.na(Format) & !is.na(DataTA) & !is.na(DataTB) & !is.na(DataTC)){ #exists("DataTA") & exists("DataTB") & exists("DataTC") &
    print("Are the names of the files and format changed respect to the following settings:? ",quote=F)
    print(DataTA)
    print(DataTB)
    print(DataTC)

    if (Format=="before_2012") {
      print(DataTT)
      print(DataTD)
    } else {
      print(DataTE)
      print(DataTL)
    }
    response <<- readline("Y/N --> ")

    if (as.character(response)=="Y"){
      DataTA <<- as.character(readline(prompt="Enter name of TA file, without extention: ")) #"TA"
      DataTB <<- as.character(readline(prompt="Enter name of TB file, without extention: ")) #"TB"
      DataTC <<- as.character(readline(prompt="Enter name of TC file, without extention: ")) #"TC"
      DataTT <<- ""
      DataTD <<- ""
      DataTE <<- ""
      DataTL <<- ""
      if (Format=="before_2012") {

        DataTT <<- as.character(readline(prompt="Enter name of TT file, without extention: ")) # ""
        DataTD <<- as.character(readline(prompt="Enter name of TD file, without extention: ")) # ""
      } else {
        DataTE <<- as.character(readline(prompt="Enter name of TE file, without extention: ")) #"TE"
        DataTL <<- as.character(readline(prompt="Enter name of TL file, without extention: ")) #"TL"
      }

     # Type_of_files <<- as.character(readline(prompt="Enter the extention of the selected files ('.csv' or '.xls'): ") ) #".csv" #or ".xls"

    }
  } else {
    Format <<- as.character(readline(prompt="Enter the format to be checked ('from_2012' or 'before_2012'): ") ) #"from_2012"   #or "before_2012"
    #Year= as.character(readline(prompt="Enter the year: " ) )
    DataTA <<- as.character(readline(prompt="Enter name of TA file, without extention: ")) #"TA"
    DataTB <<- as.character(readline(prompt="Enter name of TB file, without extention: ")) #"TB"
    DataTC <<- as.character(readline(prompt="Enter name of TC file, without extention: ")) #"TC"
    DataTT <<- ""
    DataTD <<- ""
    DataTE <<- ""
    DataTL <<- ""
    if (Format=="before_2012") {
      DataTT <<- as.character(readline(prompt="Enter name of TT file, without extention: ")) # ""
      DataTD <<- as.character(readline(prompt="Enter name of TD file, without extention: ")) # ""
    } else {
      DataTE <<- as.character(readline(prompt="Enter name of TE file, without extention: ")) #"TE"
      DataTL <<- as.character(readline(prompt="Enter name of TL file, without extention: ")) #"TL"
    }

#     Type_of_files <<- as.character(readline(prompt="Enter the extention of the selected files ('.csv' or '.xls'): ") ) #".csv" #or ".xls"

    ## Convert xls files to csv
#     if(Type_of_files==".xls") {
# #
# #       source("convertToCsv.R")
# #
#       convertToCsv(DataTA)
#       convertToCsv(DataTB)
#       convertToCsv(DataTC)
#       #convertToCsv(DataTT)
#       #convertToCsv(DataTD)
#       convertToCsv(DataTE)
#
#       Type_of_files <<- ".csv"
#     }

  }

  #-------------------------------------------------------------------------------
  # Creation of singles RSufi files

  # IMPORTANT: set "create_RSufi_files" variable to 1 and "AREA" variable for creating RSufi files at
  # the end of multiple checks procedure

  create_RSufi_files = as.character(readline(prompt= "Do you want create R-Sufi files for this year of data ('Y', 'N')?  ") )#1
  if(as.character(create_RSufi_files)=="Y"){
    AREA<<- as.character(readline(prompt="Enter the GSA: " )) #10
    # Year<<- as.character(readline(prompt="Enter the year: " ) ) #2012       #***************************+
  }



  #-------------------------------------------------------------------------------
  # Creation of RSufi files for all the years that you need.
  # ATTENTION: set this variable to 1 only if all the R-Sufi files have been created from Year_start to Year_end and if they are ALL in the files R-Sufi directory!!!

  print("ATTENTION: insert 'Y'if all the R-Sufi files have been created from Year_start to Year_end ",quote=F) # 0
  print("and they are ALL in the files R-Sufi directory!!! Otherwise, insert 'N'",quote=F)
  create_global_RSufi_files = as.character(readline(prompt= "Do you want create R-Sufi files for more than one year ('Y', 'N')?  "))
  if (as.character(create_global_RSufi_files)=="Y") {
    AREA<<- as.character(readline(prompt="Enter the GSA: " ))
    Year_start<<-as.character(readline(prompt= "Enter the start year ")) #1994
    Year_end<<-as.character(readline(prompt= "Enter the end year ") )#2012
  }

# IMPORTANT: Don't change this part of the code ----------
  require(stringr)
   require(RODBC)
   require(timeDate)
   require(graphics)
   require(mapdata)
   require(maps)

   require(xlsx)
   require(sqldf)
require(time)
source("load_functions.r")


  ## Verifico la presenza della cartella Logfiles e Graphs
  if(!file.exists(paste(getwd(),"/Logfiles",sep="")))
  {
    dir.create("Logfiles")
  }

  if(!file.exists(paste(getwd(),"/Graphs",sep="")))
  {
    dir.create("Graphs")
  }

if(!file.exists(paste(getwd(),"/files R-Sufi",sep="")))
  {
    dir.create("files R-Sufi")
  }

  # Copio le tabelle nella working directory
if(!file.exists(paste(getwd(),"/Tables",sep="")))
{
#   copyTables()
  dir.create(paste(getwd(),"/Tables",sep=""))
flist <- list.files(paste(path.package("RoME"),"/extdata/Tables",sep=""), "*.csv", full.names = TRUE)
  file.copy(flist, paste(getwd(),"/Tables/",sep=""))
}

working_tables <<- paste(getwd(),"/Tables",sep="")

if(!file.exists(paste(getwd(),"/templates",sep="")))
{
  #   copyTables()
  dir.create(paste(getwd(),"/templates",sep=""))
  flist <- list.files(paste(path.package("RoME"),"/extdata/templates",sep=""), "*.csv", full.names = TRUE)
  file.copy(flist, paste(getwd(),"/templates/",sep=""))

}

  # Copio il manuale pdf
 # copyPDFmanual()



  Errors <<- paste("Logfiles/Logfile_",as.character(Sys.Date()),format(Sys.time(), "_time h%Hm%Ms%OS0"),".dat",sep="")
  write(paste("
              ",date(),sep=""), file = Errors, append = TRUE)

#   DataSpecies <<- ifelse (Format=="before_2012",paste(path.package("RoME"),"/extdata/FM_list_old",sep=""),
#                           paste(path.package("RoME"),"/extdata/FM_list",sep=""))
#   DataTargetSpecies <<- paste(path.package("RoME"),"/extdata/Species_LEN_WEIGHT",sep="")
#   Stratification <<- paste(path.package("RoME"),"/extdata/Stratification_Scheme",sep="")

  DataSpecies <<- ifelse (Format=="before_2012",paste(working_tables,"/FM_list_old",sep=""),
                          paste(working_tables,"/TM_list",sep=""))
  DataTargetSpecies <<- paste(working_tables,"/Species_LEN_WEIGHT",sep="")
  Stratification <<- paste(working_tables,"/Stratification_Scheme",sep="")


  if (Type_of_files == ".xls"){

#     ResultDataTA<<-read.xlsx(normalizePath(paste(DataTA,".xls",sep="")) ,colClasses="character",sheetIndex=1)
#     ResultDataTB<<-read.xlsx(normalizePath(paste(DataTB,".xls",sep="")),colClasses="character",sheetIndex=1)
#     ResultDataTC<<-read.xlsx(normalizePath(paste(DataTC,".xls",sep="")),colClasses="character",sheetIndex=1)

    ResultDataTA<<-read.xlsx2(normalizePath(paste(DataTA,".xls",sep="")) ,colClasses="character",sheetIndex=1)
    ResultDataTB<<-read.xlsx2(normalizePath(paste(DataTB,".xls",sep="")),colClasses="character",sheetIndex=1)
    ResultDataTC<<-read.xlsx2(normalizePath(paste(DataTC,".xls",sep="")),colClasses="character",sheetIndex=1)


    if (Format=="before_2012"){
      ResultDataTC[which(is.na(ResultDataTC[,11])),11]="m"
      ResultDataTC[which(is.na(ResultDataTC[,18])),18]=""
    } else {
      ResultDataTC[which(is.na(ResultDataTC[,14])),14]="m"
      ResultDataTC[which(is.na(ResultDataTC[,20])),20]="ND"
    }
    if (as.character(DataTT)!=""){
#       ResultDataTT<<-read.xlsx(normalizePath(paste(DataTT,".xls",sep="")),sheetIndex=1)
      ResultDataTT<<-read.xlsx2(normalizePath(paste(DataTT,".xls",sep="")),sheetIndex=1)
      write.table(ResultDataTT,file=paste(DataTT,".csv",sep=""),quote=FALSE,sep=";",row.names=FALSE)
    }

    if (as.character(DataTD)!=""){
#       ResultDataTD<<-read.xlsx(normalizePath(paste(DataTD,".xls",sep="")),sheetIndex=1)
      ResultDataTD<<-read.xlsx2(normalizePath(paste(DataTD,".xls",sep="")),sheetIndex=1)
      write.table(ResultDataTD,file=paste(DataTD,".csv",sep=""),quote=FALSE,sep=";",row.names=FALSE)
    }
    if (Format=="from_2012"){
      if (as.character(DataTE)!=""){
#         ResultDataTE<<-read.xlsx(normalizePath(paste(DataTE,".xls",sep="")),sheetIndex=1)
        ResultDataTE<<-read.xlsx2(normalizePath(paste(DataTE,".xls",sep="")),sheetIndex=1)
        ResultDataTE[which(is.na(ResultDataTE[,12])),12]="m"
        ResultDataTE$INDIVIDUAL_WEIGHT[which(is.na(ResultDataTE$INDIVIDUAL_WEIGHT))] ="ND"
        write.table(ResultDataTE,file=paste(DataTE,".csv",sep=""),quote=FALSE,sep=";",row.names=FALSE)
      }
    }
    write.table(ResultDataTA,file=paste(DataTA,".csv",sep=""),quote=FALSE,sep=";",row.names=FALSE)
    write.table(ResultDataTB,file=paste(DataTB,".csv",sep=""),quote=FALSE,sep=";",row.names=FALSE)
    write.table(ResultDataTC,file=paste(DataTC,".csv",sep=""),quote=FALSE,sep=";",row.names=FALSE)

  }else{
    ## apro i file CSV
    if(Format=="before_2012")
    {
      ResultDataTA <<- read.table(paste(DataTA,".csv",sep=""),sep=";",header=T)
      ResultDataTB <<- read.table(paste(DataTB,".csv",sep=""),sep=";",header=T)
      ResultDataTC <<- read.table(paste(DataTC,".csv",sep=""),sep=";",header=T)
	    ResultDataTA_bkp <<- ResultDataTA
      ResultDataTB_bkp <<- ResultDataTB
      ResultDataTC_bkp <<- ResultDataTC

      if(DataTT=="")
      {
        # nothing
      }else{
        ResultDataTT <<- read.table(paste(DataTT,".csv",sep=""),sep=";",header=T)
		    ResultDataTT_bkp <<- ResultDataTT
      }

      if(DataTD=="")
      {
        # nothing

      }else{
        ResultDataTD <<- read.table(paste(DataTD,".csv",sep=""),sep=";",header=T)
		    ResultDataTD_bkp <<- ResultDataTD
      }

    }else{
      ResultDataTA <<- read.table(paste(DataTA,".csv",sep=""),sep=";",header=T)
      ResultDataTB <<- read.table(paste(DataTB,".csv",sep=""),sep=";",header=T)
      ResultDataTC <<- read.table(paste(DataTC,".csv",sep=""),sep=";",header=T)
      ResultDataTA_bkp <<- ResultDataTA
      ResultDataTB_bkp <<- ResultDataTB
      ResultDataTC_bkp <<- ResultDataTC

      # Il TE non è necessario per from_2012
      if(as.character(DataTE) != "")
      {
        ResultDataTE <<- read.table(paste(DataTE,".csv",sep=""),sep=";",header=T)
		    ResultDataTE_bkp <<- ResultDataTE
      }

      if(as.character(DataTL) != "")
      {
        ResultDataTL <<- read.table(paste(DataTL,".csv",sep=""),sep=";",header=T)
        ResultDataTL_bkp <<- ResultDataTL
      }


    }

  }

  #ResultDataTA = read.csv(paste(DataTA,".csv",sep=""),sep=";")
#   DataMatStages <<- ifelse (Format=="before_2012", paste(path.package("RoME"),"/extdata/MATURITY_STAGES",sep=""), paste(path.package("RoME"),"/extdata/MATURITY_STAGES_from_2012",sep=""))
#   print(DataMatStages)
#   Length_weight <<- paste(path.package("RoME"),"/extdata/L-W",sep="")

  DataMatStages <<- ifelse (Format=="before_2012", paste(working_tables,"/MATURITY_STAGES",sep=""), paste(working_tables,"/MATURITY_STAGES_from_2012",sep=""))
  #print(DataMatStages)
  Length_weight <<- paste(working_tables,"/L-W",sep="")

  stop_ = FALSE
#   suppressWarnings(warning(source("main.r")))


# START -------------------------------------------------------------------

initializeErrors()

check_without_errors = TRUE

# check degli header ------------------------------------------------------

if(Format=="before_2012")
{
  checkHeader(ResultDataTA,"TA")
  checkHeader(ResultDataTB,"TB")
  checkHeader(ResultDataTC,"TC")
  if(DataTT=="")
  {
  	DataTT_bkp = DataTT
    # nothing
  }else{
    checkHeader(ResultDataTT,"TT")
	DataTT_bkp = DataTT
  }

  if(DataTD=="")
  {
  	DataTD_bkp = DataTD
    # nothing
  }else{
    checkHeader(ResultDataTD,"TD")
	DataTD_bkp = DataTD
  }

}else{
  checkHeader(ResultDataTA,"TA")
  checkHeader(ResultDataTB,"TB")
  checkHeader(ResultDataTC,"TC")
  # Non è necessario TE per il from_2012

  if(as.character(DataTE) != "")
  {
    checkHeader(ResultDataTE,"TE")
	DataTE_bkp = DataTE

  }   else {
  DataTE_bkp = DataTE
  }

  if(as.character(DataTL) != "")
  {
    checkHeader(ResultDataTL,"TL")
    DataTL_bkp = DataTL

  }   else {
    DataTL_bkp = DataTL
  }
}

years = unique (ResultDataTA_bkp$YEAR)

for (yea in years) {
if (check_without_errors == TRUE) {
print(paste("Checking year ",yea ),quote=F)
}
#------------------
if(Format=="before_2012") {
      ResultDataTA <<-ResultDataTA_bkp[ResultDataTA_bkp$YEAR == yea,]
      ResultDataTB <<-ResultDataTB_bkp[ResultDataTB_bkp$YEAR == yea,]
      ResultDataTC <<-ResultDataTC_bkp[ResultDataTC_bkp$YEAR == yea,]



      if(DataTT=="")
      {
        # nothing
      }else{
        ResultDataTT <<- ResultDataTT_bkp[ResultDataTT_bkp$YEAR==yea,]
		if (nrow(ResultDataTT)==0){
		DataTT = ""
		}
      }

      if(DataTD=="")
      {
        # nothing

      }else{
        ResultDataTD <<- ResultDataTD_bkp[ResultDataTD_bkp$YEAR==yea,]
		if (nrow(ResultDataTD)==0){
		DataTD = ""
		}
      }

    }else{
      ResultDataTA <<-ResultDataTA_bkp[ResultDataTA_bkp$YEAR == yea,]
      ResultDataTB <<-ResultDataTB_bkp[ResultDataTB_bkp$YEAR == yea,]
      ResultDataTC <<-ResultDataTC_bkp[ResultDataTC_bkp$YEAR == yea,]


      # Il TE non è necessario per from_2012
      if(as.character(DataTE) != "")
      {
        ResultDataTE <<- ResultDataTE_bkp[ResultDataTE_bkp$YEAR==yea,]
		if (nrow(ResultDataTE)==0){
		DataTE = ""
		}
      }

      if(as.character(DataTL) != "")
      {
        ResultDataTL <<- ResultDataTL_bkp[ResultDataTL_bkp$YEAR==yea,]
        if (nrow(ResultDataTL)==0){
          DataTL = ""
        }
      }
    }


#---------------------


checkName = "Check identical record TA"
if (check_without_errors == TRUE) {
  print(paste(checkName,"in progress..."), quote = FALSE)
  check_without_errors = check_identical_records(ResultDataTA,"TA")
}
stop_ = printError(checkName,check_without_errors, stop_)
if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                       unlink(paste(DataTB,".csv",sep=""))
                                       unlink(paste(DataTC,".csv",sep=""))}


checkName= "Check identical record TB"
if (check_without_errors == TRUE) {
  print(paste(checkName,"in progress..."), quote = FALSE)
  check_without_errors = check_identical_records(ResultDataTB,"TB")
}
stop_ = printError(checkName,check_without_errors, stop_)
if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                       unlink(paste(DataTB,".csv",sep=""))
                                       unlink(paste(DataTC,".csv",sep=""))}

checkName = "Check identical record TC"
if (check_without_errors == TRUE) {
  print(paste(checkName,"in progress..."), quote = FALSE)
  check_without_errors = check_identical_records(ResultDataTC,"TC")
}
stop_ = printError(checkName,check_without_errors, stop_)
if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                       unlink(paste(DataTB,".csv",sep=""))
                                       unlink(paste(DataTC,".csv",sep=""))}

checkName = "Check quasi-identical record in TA"
if (check_without_errors == TRUE) {
   print(paste(checkName, "in progress..."), quote = FALSE)
   check_without_errors = check_quasiidentical_records(ResultDataTA,"TA")
}
stop_ = printError(checkName,check_without_errors, stop_)
if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
unlink(paste(DataTB,".csv",sep=""))
unlink(paste(DataTC,".csv",sep=""))}

checkName = "Check quasi-identical record in TB"
if (check_without_errors == TRUE) {
   print(paste(checkName,"in progress..."), quote = FALSE)
check_without_errors = check_quasiidentical_records(ResultDataTB,"TB")
}
stop_ = printError(checkName,check_without_errors, stop_)
if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
unlink(paste(DataTB,".csv",sep=""))
unlink(paste(DataTC,".csv",sep=""))}

checkName = "Check quasi-identical record in TC"
if (check_without_errors == TRUE) {
   print(paste(checkName,"in progress..."), quote = FALSE)
check_without_errors = check_quasiidentical_records(ResultDataTC,"TC")
}
stop_ = printError(checkName,check_without_errors, stop_)
if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
unlink(paste(DataTB,".csv",sep=""))
unlink(paste(DataTC,".csv",sep=""))}

checkName = "Check consistency of area and year TA, TB and TC"
if (check_without_errors == TRUE) {
  print(paste(checkName,"in progress..."), quote = FALSE)
  check_without_errors = check_area_year()
}
stop_ = printError(checkName,check_without_errors, stop_)
if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                       unlink(paste(DataTB,".csv",sep=""))
                                       unlink(paste(DataTC,".csv",sep=""))}


#--------------------------------------------------
# Dictionary checks
#--------------------------------------------------


#TA
checkName = "Check dictionary for field:"

Field = "VALIDITY"
Values = c("V","I")
if (check_without_errors == TRUE) {
  print(paste(checkName,Field,"in progress..."), quote = FALSE)
  check_without_errors = check_dictionary(ResultDataTA, Field, Values)
}
stop_ = printError(paste(checkName,Field),check_without_errors, stop_)
if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                       unlink(paste(DataTB,".csv",sep=""))
                                       unlink(paste(DataTC,".csv",sep=""))}

Field = "COURSE"
Values = c("R","N")
if (check_without_errors == TRUE) {
  print(paste(checkName,Field,"in progress..."), quote = FALSE)
  check_without_errors = check_dictionary(ResultDataTA, Field, Values)
}
stop_ = printError(paste(checkName,Field),check_without_errors, stop_)
if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                       unlink(paste(DataTB,".csv",sep=""))
                                       unlink(paste(DataTC,".csv",sep=""))}

Field = "CODEND_CLOSING"
Values = c("S","C")
if (check_without_errors == TRUE) {
  print(paste(checkName,Field,"in progress..."), quote = FALSE)
  check_without_errors = check_dictionary(ResultDataTA, Field, Values)
}
stop_ = printError(paste(checkName,Field),check_without_errors, stop_)
if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                       unlink(paste(DataTB,".csv",sep=""))
                                       unlink(paste(DataTC,".csv",sep=""))}

if (Format=="from_2012"){
  Field = "PART_OF_THE_CODEND"
  Values = c("A","M","P","S")
  if (check_without_errors == TRUE) {
    print(paste(checkName,Field,"in progress..."), quote = FALSE)
    check_without_errors = check_dictionary(ResultDataTA, Field, Values)
  }
  stop_ = printError(paste(checkName,Field),check_without_errors, stop_)
  if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                         unlink(paste(DataTB,".csv",sep=""))
                                         unlink(paste(DataTC,".csv",sep=""))}
}

Field = "GEOMETRICAL_PRECISION"
Values = c("M","E")
if (check_without_errors == TRUE) {
  print(paste(checkName,Field,"in progress..."), quote = FALSE)
  check_without_errors = check_dictionary(ResultDataTA, Field, Values)
}
stop_ = printError(paste(checkName,Field),check_without_errors, stop_)
if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                       unlink(paste(DataTB,".csv",sep=""))
                                       unlink(paste(DataTC,".csv",sep=""))}

Field = "GEAR"
Values = c("GOC73")
if (check_without_errors == TRUE) {
  print(paste(checkName,Field,"in progress..."), quote = FALSE)
  check_without_errors = check_dictionary(ResultDataTA, Field, Values)
}
stop_ = printError(paste(checkName,Field),check_without_errors, stop_)
if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                       unlink(paste(DataTB,".csv",sep=""))
                                       unlink(paste(DataTC,".csv",sep=""))}

Field = "RIGGING"
Values = c("GC73")
if (check_without_errors == TRUE) {
  print(paste(checkName,Field,"in progress..."), quote = FALSE)
  check_without_errors = check_dictionary(ResultDataTA, Field, Values)
}
stop_ = printError(paste(checkName,Field),check_without_errors, stop_)
if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                       unlink(paste(DataTB,".csv",sep=""))
                                       unlink(paste(DataTC,".csv",sep=""))}

Field = "DOORS"
Values = c("WHS8")
if (check_without_errors == TRUE) {
  print(paste(checkName,Field,"in progress..."), quote = FALSE)
  check_without_errors = check_dictionary(ResultDataTA, Field, Values)
}
stop_ = printError(paste(checkName,Field),check_without_errors, stop_)
if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                       unlink(paste(DataTB,".csv",sep=""))
                                       unlink(paste(DataTC,".csv",sep=""))}

Field = "SHOOTING_QUADRANT"
Values = c("1", "3", "5", "7")
if (check_without_errors == TRUE) {
  print(paste(checkName,Field,"in progress..."), quote = FALSE)
  check_without_errors = check_dictionary(ResultDataTA, Field, Values)
}
stop_ = printError(paste(checkName,Field),check_without_errors, stop_)
if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                       unlink(paste(DataTB,".csv",sep=""))
                                       unlink(paste(DataTC,".csv",sep=""))}

Field = "HAULING_QUADRANT"
Values = c("1", "3", "5", "7")
if (check_without_errors == TRUE) {
  print(paste(checkName,Field,"in progress..."), quote = FALSE)
  check_without_errors = check_dictionary(ResultDataTA, Field, Values)
}
stop_ = printError(paste(checkName,Field),check_without_errors, stop_)
if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                       unlink(paste(DataTB,".csv",sep=""))
                                       unlink(paste(DataTC,".csv",sep=""))}

if (Format=="from_2012"){

  Field = "MEASURING_SYSTEM"
  Values = c("VA","SO","XA","SA","SI","CT","SB")
  if (check_without_errors == TRUE) {
    print(paste(checkName,Field,"in progress..."), quote = FALSE)
    check_without_errors = check_dictionary(ResultDataTA, Field, Values)
  }
  stop_ = printError(paste(checkName,Field),check_without_errors, stop_)
  if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                         unlink(paste(DataTB,".csv",sep=""))
                                         unlink(paste(DataTC,".csv",sep=""))}
} else {
  if (as.character(DataTD)!=""){
    Field = "METHOD"
    Values = c("VA","XA","SA","CTD")
    if (check_without_errors == TRUE) {
      print(paste(checkName,Field,"in progress..."), quote = FALSE)
      check_without_errors = check_dictionary(ResultDataTD, Field, Values)
    }
    stop_ = printError(paste(checkName,Field),check_without_errors, stop_)
    if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                           unlink(paste(DataTB,".csv",sep=""))
                                           unlink(paste(DataTC,".csv",sep=""))
                                           unlink(paste(DataTD,".csv",sep=""))  }



  }
}

#TB
Field = "CODEND_CLOSING"
Values = c("S","C")
if (check_without_errors == TRUE) {
  print(paste(checkName,Field,"in progress..."), quote = FALSE)
  check_without_errors = check_dictionary(ResultDataTB, Field, Values)
}
stop_ = printError(paste(checkName,Field),check_without_errors, stop_)
if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                       unlink(paste(DataTB,".csv",sep=""))
                                       unlink(paste(DataTC,".csv",sep=""))}

Field = "PART_OF_THE_CODEND"
Values = c("A", "M", "P", "S")
if (check_without_errors == TRUE) {
  print(paste(checkName,Field,"in progress..."), quote = FALSE)
  check_without_errors = check_dictionary(ResultDataTB, Field, Values)
}
stop_ = printError(paste(checkName,Field),check_without_errors, stop_)
if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                       unlink(paste(DataTB,".csv",sep=""))
                                       unlink(paste(DataTC,".csv",sep=""))}



#TC
Field = "CODEND_CLOSING"
Values = c("S","C")
if (check_without_errors == TRUE) {
  print(paste(checkName,Field,"in progress..."), quote = FALSE)
  check_without_errors = check_dictionary(ResultDataTC, Field, Values)
}
stop_ = printError(paste(checkName,Field),check_without_errors, stop_)
if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                       unlink(paste(DataTB,".csv",sep=""))
                                       unlink(paste(DataTC,".csv",sep=""))}

Field = "PART_OF_THE_CODEND"
Values = c("A", "M", "P", "S")
if (check_without_errors == TRUE) {
  print(paste(checkName,Field,"in progress..."), quote = FALSE)
  check_without_errors = check_dictionary(ResultDataTC, Field, Values)
}
stop_ = printError(paste(checkName,Field),check_without_errors, stop_)
if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                       unlink(paste(DataTB,".csv",sep=""))
                                       unlink(paste(DataTC,".csv",sep=""))}

Field = "SEX"
Values = c("M", "F", "I", "N")
if (check_without_errors == TRUE) {
  print(paste(checkName,Field,"in progress..."), quote = FALSE)
  check_without_errors = check_dictionary(ResultDataTC, Field, Values)
}
stop_ = printError(paste(checkName,Field),check_without_errors, stop_)
if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                       unlink(paste(DataTB,".csv",sep=""))
                                       unlink(paste(DataTC,".csv",sep=""))}


if(Format=="before_2012"){
  Field = "LENGTH_CLASSES_CODE"
  Values = c("0","1","m")
  if (check_without_errors == TRUE) {
    print(paste(checkName,Field,"in progress..."), quote = FALSE)
    check_without_errors = check_dictionary(ResultDataTC, Field, Values)
  }
  stop_ = printError(paste(checkName,Field),check_without_errors, stop_)
  if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                         unlink(paste(DataTB,".csv",sep=""))
                                         unlink(paste(DataTC,".csv",sep=""))}
} else {
  Field = "LENGTH_CLASSES_CODE"
  Values = c("0","m")
  if (check_without_errors == TRUE) {
    print(paste(checkName,Field,"in progress..."), quote = FALSE)
    check_without_errors = check_dictionary(ResultDataTC, Field, Values)
  }
  stop_ = printError(paste(checkName,Field),check_without_errors, stop_)
  if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                         unlink(paste(DataTB,".csv",sep=""))
                                         unlink(paste(DataTC,".csv",sep=""))}

  if (DataTE!=""){
    Field = "LENGTH_CLASSES_CODE"
    Values = c("0","m")
    if (check_without_errors == TRUE) {
      print(paste(checkName,Field,"in progress..."), quote = FALSE)
      check_without_errors = check_dictionary(ResultDataTE, Field, Values)
    }
    stop_ = printError(paste(checkName,Field),check_without_errors, stop_)
    if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                           unlink(paste(DataTB,".csv",sep=""))
                                           unlink(paste(DataTC,".csv",sep=""))
                                           unlink(paste(DataTE,".csv",sep=""))}



    Field = "SEX"
    Values = c("M", "F", "I", "N")
    if (check_without_errors == TRUE) {
      print(paste(checkName,Field,"in progress..."), quote = FALSE)
      check_without_errors = check_dictionary(ResultDataTE, Field, Values)
    }
    stop_ = printError(paste(checkName,Field),check_without_errors, stop_)
    if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                           unlink(paste(DataTB,".csv",sep=""))
                                           unlink(paste(DataTC,".csv",sep=""))
                                           unlink(paste(DataTE,".csv",sep=""))}
  }
}
# End dictionary checks
#--------------------------------------------------

checkName = "Check no empty fields TA"
if (check_without_errors == TRUE) {
  print(paste(checkName,"in progress..."), quote = FALSE)
  check_without_errors = check_no_empty_fields(ResultDataTA, "TA")
}
stop_ = printError(checkName,check_without_errors, stop_)
if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                       unlink(paste(DataTB,".csv",sep=""))
                                       unlink(paste(DataTC,".csv",sep=""))}

checkName =  "Check no empty fields TB"
if (check_without_errors == TRUE) {
  print(paste(checkName,"in progress..."), quote = FALSE)
  check_without_errors = check_no_empty_fields(ResultDataTB, "TB")
}
stop_ = printError(checkName,check_without_errors, stop_)
if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                       unlink(paste(DataTB,".csv",sep=""))
                                       unlink(paste(DataTC,".csv",sep=""))}

checkName = "Check no empty fields TC"
if (check_without_errors == TRUE) {
  print(paste(checkName,"in progress..."), quote = FALSE)
  check_without_errors = check_no_empty_fields(ResultDataTC, "TC")
}
stop_ = printError(checkName,check_without_errors, stop_)
if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                       unlink(paste(DataTB,".csv",sep=""))
                                       unlink(paste(DataTC,".csv",sep=""))}

if ((Format=="from_2012")& (DataTE!="")){

  checkName = "Check no empty fields TE"
  if (check_without_errors == TRUE) {
    print(paste(checkName,"in progress..."), quote = FALSE)
    check_without_errors = check_no_empty_fields(ResultDataTE, "TE")
  }
  stop_ = printError(checkName,check_without_errors, stop_)
  if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                         unlink(paste(DataTB,".csv",sep=""))
                                         unlink(paste(DataTC,".csv",sep=""))
                                         unlink(paste(DataTE,".csv",sep=""))}
}
# checks on TA

checkName = "Check 0 fields TA"
if (check_without_errors == TRUE) {
  print(paste(checkName,"in progress..."), quote = FALSE)
  check_without_errors = check_0_fieldsTA(ResultDataTA)
}
stop_ = printError(checkName,check_without_errors, stop_)
if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                       unlink(paste(DataTB,".csv",sep=""))
                                       unlink(paste(DataTC,".csv",sep=""))}

checkName = "Check dm TA"
if (check_without_errors == TRUE) {
  print(paste(checkName,"in progress..."), quote = FALSE)
  check_without_errors = check_dm(ResultDataTA)
}
stop_ = printError(checkName,check_without_errors, stop_)
if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                       unlink(paste(DataTB,".csv",sep=""))
                                       unlink(paste(DataTC,".csv",sep=""))}

checkName = "Check consistency between duration and time TA"
if (check_without_errors == TRUE) {
  print(paste(checkName,"in progress..."), quote = FALSE)
  check_without_errors = check_consistencyTA_duration(ResultDataTA)
}
stop_ = printError(checkName,check_without_errors, stop_)
if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                       unlink(paste(DataTB,".csv",sep=""))
                                       unlink(paste(DataTC,".csv",sep=""))}

checkName = "Check consistency between distance and duration of the haul TA"
if (check_without_errors == TRUE) {
  print(paste(checkName,"in progress..."), quote = FALSE)
  check_without_errors = check_consistencyTA_distance(ResultDataTA)
}
stop_ = printError(checkName,check_without_errors, stop_)
if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                       unlink(paste(DataTB,".csv",sep=""))
                                       unlink(paste(DataTC,".csv",sep=""))}

checkName = "Check consistency of the hauls coordinates with the distance(difference not greater than 30%)"
if (check_without_errors == TRUE) {
  print(paste(checkName,"in progress..."), quote = FALSE)
  check_without_errors = check_distance(ResultDataTA)
}
stop_ = printError(checkName,check_without_errors, stop_)
if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                       unlink(paste(DataTB,".csv",sep=""))
                                       unlink(paste(DataTC,".csv",sep=""))}

checkName = "Check consistency of bridles length TA"
if (check_without_errors == TRUE) {
  print(paste(checkName,"in progress..."), quote = FALSE)
  check_without_errors = check_bridles_length(ResultDataTA)
}
stop_ =printError(checkName,check_without_errors, stop_)
if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                       unlink(paste(DataTB,".csv",sep=""))
                                       unlink(paste(DataTC,".csv",sep=""))}

checkName = "Check difference between start depth and end depth (not greater than 20%) in TA"
if (check_without_errors == TRUE) {
  print(paste(checkName,"in progress..."), quote = FALSE)
  check_without_errors = check_depth(ResultDataTA)
}
stop_ = printError(checkName,check_without_errors, stop_)
if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                       unlink(paste(DataTB,".csv",sep=""))
                                       unlink(paste(DataTC,".csv",sep=""))}

checkName = "Check start depth and end depth in the same stratum TA"
if (check_without_errors == TRUE) {
  print(paste(checkName,"in progress..."), quote = FALSE)
  check_without_errors = check_stratum(ResultDataTA)
}
stop_ = printError(checkName,check_without_errors, stop_)
if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                       unlink(paste(DataTB,".csv",sep=""))
                                       unlink(paste(DataTC,".csv",sep=""))}

if ((as.character(DataTT)!="" )| (Format=="from_2012")){
  checkName = "Check consistency of stratum code in TA or TT"
  if (check_without_errors == TRUE) {
    print(paste(checkName,"in progress..."), quote = FALSE)
    check_without_errors = check_stratum_code()
  }
  stop_ = printError(checkName,check_without_errors, stop_)
  if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                         unlink(paste(DataTB,".csv",sep=""))
                                         unlink(paste(DataTC,".csv",sep=""))}

}
checkName = "Check start quadrant and end quadrant TA"
if (check_without_errors == TRUE) {
  print(paste(checkName,"in progress..."), quote = FALSE)
  check_without_errors = check_quadrant(ResultDataTA)
}
stop_ = printError(checkName,check_without_errors, stop_)
if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                       unlink(paste(DataTB,".csv",sep=""))
                                       unlink(paste(DataTC,".csv",sep=""))}

checkName = "Check uniqueness of valid hauls TA"
if (check_without_errors == TRUE) {
  print(paste(checkName,"in progress..."), quote = FALSE)
  check_without_errors = check_unique_valid_haul(ResultDataTA)
}
stop_ =printError(checkName,check_without_errors, stop_)
if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                       unlink(paste(DataTB,".csv",sep=""))
                                       unlink(paste(DataTC,".csv",sep=""))}

checkName = "Visual check of the haul positions"
if (check_without_errors == TRUE) {
  print(paste(checkName,"in progress..."), quote = FALSE)
check_position(ResultDataTA)
}
stop_ = printError(checkName,check_without_errors, stop_)
if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                       unlink(paste(DataTB,".csv",sep=""))
                                       unlink(paste(DataTC,".csv",sep=""))}


checkName = "Relation between shooting depth e warp length and between warp length e wing opening"
if (check_without_errors == TRUE) {
  print(paste(checkName,"in progress..."), quote = FALSE)
  graphs_TA(ResultDataTA)
}
stop_ = printError(checkName,check_without_errors, stop_)
if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                       unlink(paste(DataTB,".csv",sep=""))
                                       unlink(paste(DataTC,".csv",sep=""))}

checkName = "Check if the coordinates are in the Mediterranean Sea"
if (check_without_errors == TRUE) {
  print(paste(checkName,"in progress..."), quote = FALSE)
  check_without_errors = check_position_in_Med(ResultDataTA)
}
stop_ = printError(checkName,check_without_errors, stop_)
if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                       unlink(paste(DataTB,".csv",sep=""))
                                       unlink(paste(DataTC,".csv",sep=""))}


checkName = "Check on temperature by haul"
if (check_without_errors == TRUE) {
  print(paste(checkName,"in progress..."), quote = FALSE)
#   if((Format=="before_2012")){if (DataTD != ""){.check_without_errors = .check_temperature(ResultDataTD)}} else {check_without_errors = .check_temperature(ResultDataTA)}
  if((Format=="before_2012")){if (DataTD != ""){check_without_errors = check_temperature()}} else {check_without_errors = check_temperature()}
}
stop_ = printError(checkName,check_without_errors, stop_)
if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                       unlink(paste(DataTB,".csv",sep=""))
                                       unlink(paste(DataTC,".csv",sep=""))}


# checks on TB

checkName = "Check correctness of species codes TB"
if (check_without_errors == TRUE) {
  print(paste(checkName,"in progress..."), quote = FALSE)
  check_without_errors = check_rubincode(DataSpecies,ResultDataTB)
}
stop_ = printError(checkName,check_without_errors, stop_)
if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                       unlink(paste(DataTB,".csv",sep=""))
                                       unlink(paste(DataTC,".csv",sep=""))}

checkName = "Check consistency of NB_TOTAL and number per sex TB"
if (check_without_errors == TRUE) {
  print(paste(checkName,"in progress..."), quote = FALSE)
  check_without_errors = check_nbtotTB(ResultDataTB)
}
stop_ = printError(checkName,check_without_errors, stop_)
if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                       unlink(paste(DataTB,".csv",sep=""))
                                       unlink(paste(DataTC,".csv",sep=""))}

if (Format=="from_2012"){
  checkName = "Check presence of NB_TOTAL and number per sex TB for species G1"
  if (check_without_errors == TRUE) {
    print(paste(checkName,"in progress..."), quote = FALSE)
    check_without_errors = check_nm_TB()
  }
  stop_ = printError(checkName,check_without_errors, stop_)
  if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                         unlink(paste(DataTB,".csv",sep=""))
                                         unlink(paste(DataTC,".csv",sep=""))}

}

checkName = "Check consistency between not null weight and not null total number"
if (check_without_errors == TRUE) {
  print(paste(checkName,"in progress..."), quote = FALSE)
  check_without_errors = check_weight_tot_nb(ResultDataTB)
}
stop_ = printError(checkName,check_without_errors, stop_)
if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                       unlink(paste(DataTB,".csv",sep=""))
                                       unlink(paste(DataTC,".csv",sep=""))}

checkName = "Check consistency of weight and number TB"
if (check_without_errors == TRUE) {
  print(paste(checkName,"in progress..."), quote = FALSE)
  check_without_errors = check_weight(ResultDataTB, DataTargetSpecies)
}
stop_ = printError(checkName,check_without_errors, stop_)
if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                       unlink(paste(DataTB,".csv",sep=""))
                                       unlink(paste(DataTC,".csv",sep=""))}

# checks on TC

graphics.off()
checkName = "Check correctness of LENGTH_CLASSES_CODE TC"
if (check_without_errors == TRUE) {
  print(paste(checkName,"in progress..."), quote = FALSE)
  check_without_errors = check_length_class_codeTC(ResultDataTC,DataSpecies)
}
stop_ = printError(checkName,check_without_errors, stop_)
if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                       unlink(paste(DataTB,".csv",sep=""))
                                       unlink(paste(DataTC,".csv",sep=""))}

#if(Format=="before_2012"){

checkName = "Check correctness of species codes TC"
if (check_without_errors == TRUE) {
  print(paste(checkName,"in progress..."), quote = FALSE)
  check_without_errors = check_rubincode(DataSpecies,ResultDataTC)
}
stop_ = printError(checkName,check_without_errors, stop_)
if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                       unlink(paste(DataTB,".csv",sep=""))
                                       unlink(paste(DataTC,".csv",sep=""))}
#}

checkName = "Check consistency of length classes TC"
if (check_without_errors == TRUE) {
  print(paste(checkName,"in progress..."), quote = FALSE)
  check_without_errors = check_length(ResultDataTC,DataTargetSpecies)
}
stop_ = printError(checkName,check_without_errors, stop_)
if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                       unlink(paste(DataTB,".csv",sep=""))
                                       unlink(paste(DataTC,".csv",sep=""))}



checkName = "Check correctness of number per sex in TC"
if (check_without_errors == TRUE) {
  print(paste(checkName,"in progress..."), quote = FALSE)
  check_without_errors = check_nb_per_sexTC(ResultDataTC)
}
stop_ = printError(checkName,check_without_errors, stop_)
if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                       unlink(paste(DataTB,".csv",sep=""))
                                       unlink(paste(DataTC,".csv",sep=""))}

checkName = "Check consistency of maturity stages TC"
if (check_without_errors == TRUE) {
  print(paste(checkName,"in progress..."), quote = FALSE)
  check_without_errors = check_mat_stages(ResultDataTC,DataTargetSpecies)
}
stop_ = printError(checkName,check_without_errors, stop_)
if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                       unlink(paste(DataTB,".csv",sep=""))
                                       unlink(paste(DataTC,".csv",sep=""))}

checkName = "Check sub-sampling"
if (check_without_errors == TRUE) {
  print(paste(checkName,"in progress..."), quote = FALSE)
  check_without_errors = check_subsampling(ResultDataTC)
}
stop_ = printError(checkName,check_without_errors, stop_)
if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                       unlink(paste(DataTB,".csv",sep=""))
                                       unlink(paste(DataTC,".csv",sep=""))}


checkName = "Check consistency of maturity stages TC by the comparison with the length of smallest mature individuals reported in bibliography"
if (check_without_errors == TRUE) {
  print(paste(checkName,"in progress..."), quote = FALSE)
  check_without_errors = check_smallest_mature(ResultDataTC)
}
stop_ = printError(checkName,check_without_errors, stop_)
if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                       unlink(paste(DataTB,".csv",sep=""))
                                       unlink(paste(DataTC,".csv",sep=""))}

checkName = "Check consistency of sex TC by means of spawning period"
if (check_without_errors == TRUE) {
  print(paste(checkName,"in progress..."), quote = FALSE)
  check_without_errors = check_spawning_period(ResultDataTA,ResultDataTC)
}
stop_ = printError(checkName,check_without_errors, stop_)
if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                       unlink(paste(DataTB,".csv",sep=""))
                                       unlink(paste(DataTC,".csv",sep=""))}

checkName = "Check consistency of sex data TC by means of sex-inversion size"
if (check_without_errors == TRUE) {
  print(paste(checkName,"in progress..."), quote = FALSE)
  check_without_errors = check_sex_inversion(ResultDataTC)
}
stop_ = printError(checkName,check_without_errors, stop_)
if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                       unlink(paste(DataTB,".csv",sep=""))
                                       unlink(paste(DataTC,".csv",sep=""))}

checkName = "Check consistency of length distribution TC"
if (check_without_errors == TRUE) {
  print(paste(checkName,"in progress..."), quote = FALSE)
  check_without_errors = check_step_length_distr(ResultDataTC)
}
stop_ = printError(checkName,check_without_errors, stop_)
if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                       unlink(paste(DataTB,".csv",sep=""))
                                       unlink(paste(DataTC,".csv",sep=""))}

checkName = "Check total weight in the haul in TC"
if (check_without_errors == TRUE) {
  print(paste(checkName,"in progress..."), quote = FALSE)
  check_without_errors = check_individual_weightTC()
}
stop_ = printError(checkName,check_without_errors, stop_)
if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                       unlink(paste(DataTB,".csv",sep=""))
                                       unlink(paste(DataTC,".csv",sep=""))}


checkName = "Check correctness of species codes TC"
if (check_without_errors == TRUE) {
  print(paste(checkName,"in progress..."), quote = FALSE)
  check_without_errors = check_rubincode(DataSpecies,ResultDataTC)
}
stop_ = printError(checkName,check_without_errors, stop_)
if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                       unlink(paste(DataTB,".csv",sep=""))
                                       unlink(paste(DataTC,".csv",sep=""))}
#}
if (Format=="from_2012") {
  checkName = "Check presence of lengths for G1 and G2 Medits species in TC"
  if (check_without_errors == TRUE) {
    print(paste(checkName,"in progress..."), quote = FALSE)
    check_without_errors = check_G1_G2()
  }
  stop_ = printError(checkName,check_without_errors, stop_)

  if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                         unlink(paste(DataTB,".csv",sep=""))
                                         unlink(paste(DataTC,".csv",sep=""))}
}
# cross checks

checkName = "Check presence in TB of TA hauls"
if (check_without_errors == TRUE) {
  print(paste(checkName,"in progress..."), quote = FALSE)
  check_without_errors = check_hauls_TATB(ResultDataTA,ResultDataTB)
}
stop_ = printError(checkName,check_without_errors, stop_)
if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                       unlink(paste(DataTB,".csv",sep=""))
                                       unlink(paste(DataTC,".csv",sep=""))}

checkName = "Check presence in TA of TB hauls"
if (check_without_errors == TRUE) {
  print(paste(checkName,"in progress..."), quote = FALSE)
  check_without_errors = check_hauls_TBTA(ResultDataTA,ResultDataTB)
}
stop_ = printError(checkName,check_without_errors, stop_)
if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                       unlink(paste(DataTB,".csv",sep=""))
                                       unlink(paste(DataTC,".csv",sep=""))}

checkName = "Check presence in TC of TB target species"
if (check_without_errors == TRUE) {
  print(paste(checkName,"in progress..."), quote = FALSE)
  check_without_errors = check_species_TBTC(DataTargetSpecies,ResultDataTB,ResultDataTC)
}
stop_ = printError(checkName,check_without_errors, stop_)
if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                       unlink(paste(DataTB,".csv",sep=""))
                                       unlink(paste(DataTC,".csv",sep=""))}

checkName = "Check presence in TB of TC species"
if (check_without_errors == TRUE) {
  print(paste(checkName,"in progress..."), quote = FALSE)
  check_without_errors = check_haul_species_TCTB(ResultDataTB,ResultDataTC)
}
stop_ = printError(checkName,check_without_errors, stop_)
if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                       unlink(paste(DataTB,".csv",sep=""))
                                       unlink(paste(DataTC,".csv",sep=""))}

checkName = "Check correctness of the number per sex in TB in case of sub-sampling in TC  "
if (check_without_errors == TRUE) {
  print(paste(checkName,"in progress..."), quote = FALSE)
  check_without_errors = check_raising(ResultDataTB,ResultDataTC)
}
stop_ = printError(checkName,check_without_errors, stop_)
if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                       unlink(paste(DataTB,".csv",sep=""))
                                       unlink(paste(DataTC,".csv",sep=""))}

if (Format=="from_2012"){

  checkName = "Check on date by haul TB"
  if (check_without_errors == TRUE) {
    print(paste(checkName,"in progress..."), quote = FALSE)
    check_without_errors = check_date_haul(ResultDataTB)
  }
  stop_ =printError(checkName,check_without_errors, stop_)
  if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                         unlink(paste(DataTB,".csv",sep=""))
                                         unlink(paste(DataTC,".csv",sep=""))
                                         unlink(paste(DataTE,".csv",sep=""))}

  checkName = "Check on date by haul TC"
  if (check_without_errors == TRUE) {
    print(paste(checkName,"in progress..."), quote = FALSE)
    check_without_errors = check_date_haul(ResultDataTC)
  }
  stop_ = printError(checkName,check_without_errors, stop_)
  if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                         unlink(paste(DataTB,".csv",sep=""))
                                         unlink(paste(DataTC,".csv",sep=""))
                                         unlink(paste(DataTE,".csv",sep=""))}

  checkName = "Check on date by haul TE"
  if (DataTE!=""){
    if (check_without_errors == TRUE) {
      print(paste(checkName,"in progress..."), quote = FALSE)
      check_without_errors = check_date_haul(ResultDataTE)
    }
    stop_ = printError(checkName,check_without_errors, stop_)
    if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                           unlink(paste(DataTB,".csv",sep=""))
                                           unlink(paste(DataTC,".csv",sep=""))
                                           unlink(paste(DataTE,".csv",sep=""))}


    checkName = "Cross check on number between TC and TE"
    if (check_without_errors == TRUE) {
      print(paste(checkName,"in progress..."), quote = FALSE)
      check_without_errors = check_TE_TC()
    }
    stop_ = printError(checkName,check_without_errors, stop_)
    if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                           unlink(paste(DataTB,".csv",sep=""))
                                           unlink(paste(DataTC,".csv",sep=""))
                                           unlink(paste(DataTE,".csv",sep=""))}





    #Check on TE

    checkName = "Check consistency of maturity stages TE"
    if (as.character(DataTE)!=""){
      if (check_without_errors == TRUE) {
        print(paste(checkName,"in progress..."), quote = FALSE)
        check_without_errors = check_mat_stages(ResultDataTE,DataTargetSpecies)
      }
      stop_ = printError(checkName,check_without_errors, stop_)
      if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                             unlink(paste(DataTB,".csv",sep=""))
                                             unlink(paste(DataTC,".csv",sep=""))
                                             unlink(paste(DataTE,".csv",sep=""))}
    }

    checkName = "Check consistency of maturity stages TE by the comparison with the length of smallest mature individuals reported in bibliography"
    if (check_without_errors == TRUE) {
      print(paste(checkName,"in progress..."), quote = FALSE)
      check_without_errors = check_smallest_mature(ResultDataTE)
    }
    stop_ = printError(checkName,check_without_errors, stop_)
    if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                           unlink(paste(DataTB,".csv",sep=""))
                                           unlink(paste(DataTC,".csv",sep=""))
                                           unlink(paste(DataTE,".csv",sep=""))}

    checkName = "Check consistency of maturity stages in TE by means of spawning period"
    if (check_without_errors == TRUE) {
      print(paste(checkName,"in progress..."), quote = FALSE)
      check_without_errors = check_spawning_period(ResultDataTA,ResultDataTE)
    }
    stop_ = printError(checkName,check_without_errors, stop_)
    if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                           unlink(paste(DataTB,".csv",sep=""))
                                           unlink(paste(DataTC,".csv",sep=""))
                                           unlink(paste(DataTE,".csv",sep=""))}

    checkName = "Check individual weight in TE"
    if (check_without_errors == TRUE) {
      print(paste(checkName,"in progress..."), quote = FALSE)
      check_without_errors = check_individual_weightTE()
    }
    stop_ = printError(checkName,check_without_errors, stop_)
    if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                           unlink(paste(DataTB,".csv",sep=""))
                                           unlink(paste(DataTC,".csv",sep=""))
                                           unlink(paste(DataTE,".csv",sep=""))}

    checkName = "Check correctness of species codes TE"
    if (check_without_errors == TRUE) {
      print(paste(checkName,"in progress..."), quote = FALSE)
      check_without_errors = check_rubincode(DataSpecies,ResultDataTE)
    }
    stop_ = printError(checkName,check_without_errors, stop_)
    if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                           unlink(paste(DataTB,".csv",sep=""))
                                           unlink(paste(DataTC,".csv",sep=""))
                                           unlink(paste(DataTE,".csv",sep=""))}


    checkName = "Check consistency TE check-fields"
    if (check_without_errors == TRUE) {
      print(paste(checkName,"in progress..."), quote = FALSE)
      check_without_errors = check_nb_TE()
    }
    stop_ = printError(checkName,check_without_errors, stop_)
    if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                           unlink(paste(DataTB,".csv",sep=""))
                                           unlink(paste(DataTC,".csv",sep=""))
                                           unlink(paste(DataTE,".csv",sep=""))}

    checkName = "Summary individual data sampling"
    if (check_without_errors == TRUE) {
      print(paste(checkName,"in progress..."), quote = FALSE)
      check_without_errors = scheme_individual_data()
    }
    stop_ = printError(checkName,check_without_errors, stop_)
    if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                           unlink(paste(DataTB,".csv",sep=""))
                                           unlink(paste(DataTC,".csv",sep=""))
                                           unlink(paste(DataTE,".csv",sep=""))}

  }
}

   if (as.character(DataTL)!=""){
   # Checks on TL
checkName = "Check allowed values for category on Litter data"
Field = "LITTER_CATEGORY"
Values = c("L0","L1","L2","L3","L4","L5","L6","L7")


if (check_without_errors == TRUE) {
  print(paste(checkName,"in progress..."), quote = FALSE)
  check_without_errors = check_dictionary(ResultDataTL,Field, Values)
}
stop_ = printError(checkName,check_without_errors, stop_)
if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
  unlink(paste(DataTB,".csv",sep=""))
  unlink(paste(DataTC,".csv",sep=""))
  unlink(paste(DataTE,".csv",sep=""))
  unlink(paste(DataTL,".csv",sep=""))
  }

checkName = "Check allowed values for category on Litter data"
Field = "LITTER_SUB-CATEGORY"
Values = c("0","a","b","c","d","e","f","g","h","i","j")


if (check_without_errors == TRUE) {
  print(paste(checkName,"in progress..."), quote = FALSE)
  check_without_errors = check_dictionary(ResultDataTL,Field, Values)
}
stop_ = printError(checkName,check_without_errors, stop_)
if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
  unlink(paste(DataTB,".csv",sep=""))
  unlink(paste(DataTC,".csv",sep=""))
  unlink(paste(DataTE,".csv",sep=""))
  unlink(paste(DataTL,".csv",sep=""))
}


checkName = "Check correctness of associations between category and sub-category on Litter data"

AssociationsTL=read.table("Tables/Associations_cat_TL.csv",sep=";",header=T)

if (check_without_errors == TRUE) {
  print(paste(checkName,"in progress..."), quote = FALSE)
  check_without_errors = check_associations_category_TL(ResultDataTL,AssociationsTL)
}
stop_ = printError(checkName,check_without_errors, stop_)
if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
  unlink(paste(DataTB,".csv",sep=""))
  unlink(paste(DataTC,".csv",sep=""))
  unlink(paste(DataTE,".csv",sep=""))
  unlink(paste(DataTL,".csv",sep=""))
}

checkName = "Check if the number is always filled in on Litter data"


if (check_without_errors == TRUE) {
  print(paste(checkName,"in progress..."), quote = FALSE)
  check_without_errors = check_no_empty_fields(ResultDataTL,"TL")
}
stop_ = printError(checkName,check_without_errors, stop_)
if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
  unlink(paste(DataTB,".csv",sep=""))
  unlink(paste(DataTC,".csv",sep=""))
  unlink(paste(DataTE,".csv",sep=""))
  unlink(paste(DataTL,".csv",sep=""))
}


checkName = "Check if the date in TL is consistent with TA"


if (check_without_errors == TRUE) {
  print(paste(checkName,"in progress..."), quote = FALSE)
  check_without_errors = check_date_haul(ResultDataTL)
}
stop_ = printError(checkName,check_without_errors, stop_)
if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
  unlink(paste(DataTB,".csv",sep=""))
  unlink(paste(DataTC,".csv",sep=""))
  unlink(paste(DataTE,".csv",sep=""))
  unlink(paste(DataTL,".csv",sep=""))
}

checkName = "Check if the hauls in TL are present in TA"


if (check_without_errors == TRUE) {
  print(paste(checkName,"in progress..."), quote = FALSE)
  check_without_errors = check_hauls_TLTA(ResultDataTA,ResultDataTL)
}
stop_ = printError(checkName,check_without_errors, stop_)
if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
  unlink(paste(DataTB,".csv",sep=""))
  unlink(paste(DataTC,".csv",sep=""))
  unlink(paste(DataTE,".csv",sep=""))
  unlink(paste(DataTL,".csv",sep=""))
}

checkName = "Check if the hauls in TA are present in TL"


if (check_without_errors == TRUE) {
  print(paste(checkName,"in progress..."), quote = FALSE)
  check_without_errors = check_hauls_TATL(ResultDataTA,ResultDataTL)
}
stop_ = printError(checkName,check_without_errors, stop_)
if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
  unlink(paste(DataTB,".csv",sep=""))
  unlink(paste(DataTC,".csv",sep=""))
  unlink(paste(DataTE,".csv",sep=""))
  unlink(paste(DataTL,".csv",sep=""))
}
}


if (!stop_) {
  print("All the checks have been performed!",quote=FALSE)
}



# Create files for R_Sufi

if ((!stop_) & as.character(create_RSufi_files)=="Y"){
  create_strata(Stratification,AREA)
  create_haul(ResultDataTA)
  create_catch(ResultDataTB)
  if ((Format=="before_2012")| (DataTE=="") ){
    create_length(ResultDataTC)
  } else if (DataTE!=""){
    create_length(ResultDataTE)
  }
  print(paste("R-Sufi files have been created for the ",yea, "and the GSA selected! They have been stored in files R-Sufi directory."),quote=FALSE)
}

#----- bkp
if(Format=="before_2012"){
DataTT = DataTT_bkp
DataTD = DataTD_bkp
} else {
DataTE = DataTE_bkp
}


#------ bkp

} # ciclo years

# END ---------------------------------------------------------------------




  # -------------------------------------------------------
  # -------------------------------------------------------
  # -------------------------------------------------------
  # -------------------------------------------------------


  if ((!stop_) & as.character(create_global_RSufi_files)=="Y"){
    RSufi_files()
    print("R-Sufi files have been created for the Years and the GSA selected! They have been stored in files R-Sufi directory.",quote=FALSE)
  }
  # -------------------------------------------------------
} # funzione RoME

