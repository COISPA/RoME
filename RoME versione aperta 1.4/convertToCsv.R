 
convertToCsv <- function(stringa)
{
  
  print("Converting xls to csv")
  x <- read.xlsx(paste(getwd(),"/",stringa,".xls",sep=""), 1)
  write.table(x,file=paste(stringa,".csv",sep=""), row.names=FALSE, col.names=TRUE, quote=FALSE, sep=";", append=FALSE)
  
}