library(RoME)

wd <- tempdir()
suffix=NA  # non modificare
ta <- read.table(file="C:\\Users\\walte\\Documents\\GitHub\\RoME\\data\\GSA18-test\\TA_GSA18_1994-2018.csv", sep=";", header=T)
tb <- read.table(file="C:\\Users\\walte\\Documents\\GitHub\\RoME\\data\\GSA18-test\\TB_GSA18_1994-2018.csv", sep=";", header=T)
tc <- read.table(file="C:\\Users\\walte\\Documents\\GitHub\\RoME\\data\\GSA18-test\\TC_GSA18_1994-2018.csv", sep=";", header=T)
te <- NA # puoi mettere NA
tl <- NA # puoi mettere NA

ta <- ta[ta$YEAR==1996,]
tb <- tb[tb$YEAR==1996,]
tc <- tc[tc$YEAR==1996,]


verbose = TRUE
create_RSufi_files=FALSE # metti FALSE se non vuoi effettuare la produzione dei file Rsufi
create_global_RSufi_files=FALSE # metti FALSE se non vuoi effettuare la produzione dei file Rsufi
Year_start=NA   # deve essere impostato se vuoi effettuare l'analisi R-sufi
Year_end=NA     # deve essere impostato se vuoi effettuare l'analisi R-sufi

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
     verbose=verbose)
