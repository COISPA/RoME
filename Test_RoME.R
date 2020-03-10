library(RoME)

wd <- "metti il percorso della cartella (esistente) in cui vuoi salvare i risultati (usa \\ come separatore)"
suffix=NA  # non modificare
ta <- read.table(file="metti qui il nome del file con il percorso", sep=";", header=T)
tb <- read.table(file="metti qui il nome del file con il percorso", sep=";", header=T)
tc <- read.table(file="metti qui il nome del file con il percorso", sep=";", header=T)
te <- read.table(file="metti qui il nome del file con il percorso", sep=";", header=T) # puoi mettere NA
tl <- read.table(file="metti qui il nome del file con il percorso", sep=";", header=T) # puoi mettere NA

verbose = TRUE
create_RSufi_files=TRUE # metti FALSE se non vuoi effettuare la produzione dei file Rsufi
create_global_RSufi_files=TRUE # metti FALSE se non vuoi effettuare la produzione dei file Rsufi
Year_start=2007 # deve essere impostato se vuoi effettuare l'analisi R-sufi
Year_end=2016# deve essere impostato se vuoi effettuare l'analisi R-sufi

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
