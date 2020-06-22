

library(RoME)

wd <- "C:\\Users\\walte\\Documents\\GitHub\\RoME\\data TEST Neglia"
suffix=NA  # non modificare
ta <- read.table(file=paste(wd, "\\2019 GSA18 TA.csv",sep=""), sep=";", header=T)
tb <- read.table(file=paste(wd, "\\2019 GSA18 TB.csv",sep=""), sep=";", header=T)
tc <- read.table(file=paste(wd, "\\2019 GSA18 TC.csv",sep=""), sep=";", header=T)
te <- read.table(file=paste(wd, "\\2019 GSA18 TE.csv",sep=""), sep=";", header=T) # puoi mettere NA
tl <- read.table(file=paste(wd,"\\2019 GSA18 TL.csv",sep=""), sep=";", header=T) # puoi mettere NA
#
# colnames(tl)[which(colnames(tl)=="TOTAL_NUMBER_IN_THE_SUB.CATEGORY_HAUL")] <- "TOTAL_NUMBER_IN_THE_SUB-CATEGORY_HAUL"
# tl$`TOTAL_WEIGHT_IN_THE_CATEGORY_HAUL` [1] <- ""
# tb$TYPE_OF_FILE <- as.character(tb$TYPE_OF_FILE)

te$MATURITY [2] <- 5
# te$MATSUB[2] <- "D"
# ta$WING_OPENING [12] <- 1165

Stratification= MEDITS::stratification_scheme # oppure = read.table(file=paste(wd,"\\Tables\\Stratification_Scheme.csv",sep=""), sep=";", header=T)
TM_list= RoME::TM_list #read.csv(file=paste(wd,"\\Tables\\TM_list.csv",sep=""), sep=";", header=T)  # oppure RoME::TM_list
DataTargetSpecies=RoME::DataTargetSpecies # oppure = read.table(file=paste(wd,"\\Tables\\DataTargetSpecies.csv",sep=""), sep=";", header=T)
Maturity_parameters=RoME::Maturity_parameters # oppure = read.table(file=paste(wd,"\\Tables\\Maturity_parameters.csv",sep=""), sep=";", header=T)
stages=RoME::mat_stages
assTL=RoME::assTL

verbose = TRUE
create_RSufi_files=FALSE # metti FALSE se non vuoi effettuare la produzione dei file Rsufi
create_global_RSufi_files=FALSE # metti FALSE se non vuoi effettuare la produzione dei file Rsufi
Year_start=2019 # deve essere impostato se vuoi effettuare l'analisi R-sufi
Year_end=2019 # deve essere impostato se vuoi effettuare l'analisi R-sufi

################ NON MODIFICARE #############

RoME(TA=ta,
     TB=tb,
     TC=tc,
     TE=te,
     TL=tl,
     wd=wd,
     suffix=suffix,
     create_RSufi_files=create_RSufi_files,
     create_global_RSufi_files=create_global_RSufi_files,
     Year_start=Year_start,
     Year_end=Year_end,
     verbose=verbose,
     Stratification=Stratification,
     TM_list=TM_list,
     DataTargetSpecies=DataTargetSpecies,
     Maturity_parameters=Maturity_parameters,
     stages=stages,
     assTL=assTL)
