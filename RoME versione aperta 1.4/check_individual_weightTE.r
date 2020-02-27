###########################################################################################################################
#   RoME: R code to perform multiple checks on MEDITS Survey data (TA, TB, TC and TE files - old and new MEDITS formats)  #                   
#   Authors: I. Bitetto, M.T. Facchini, M.T. Spedicato                                                                    #
#   Coispa Tecnologia & Ricerca - Stazione sperimentale per lo Studio delle Risorse del Mare                              #
#   If you have any comments or suggestions please contact the following e-mail address: bitetto@coispa.it                #                                                                                  
#   March 2013                                                                                                            #
################################################################################
# Check if weight in TE are consistent with length-weight relationship

check_individual_weightTE<- function (){
  TE = ResultDataTE #read.csv(paste(DataTE,".csv",sep=""), sep=";",header=TRUE)
  write(paste("
              ----------- check consistency individual weights in TE - ",TE$YEAR[1]), file = Errors, append = TRUE)
  numberError = 0
  numberError_ = 0
  

  
  TE_ND=TE[as.character(TE$INDIVIDUAL_WEIGHT)=="ND",] # selection on the weight ND
  TE_ND$Species = paste(TE_ND$GENUS,TE_ND$SPECIES)
  species = unique(TE_ND$Species)
#   list_g1_g2 = read.table("Tables/MEDITS_G1_G2.csv",sep=";",header=T)
#   list_g1_g2 = read.table(paste(path.package("RoME"),"/extdata/MEDITS_G1_G2.csv",sep=""),sep=";",header=T)
  list_g1_g2 = read.table(paste(working_tables,"/MEDITS_G1_G2.csv",sep=""),sep=";",header=T)
  G1 =  data.frame(as.character(list_g1_g2$CODE[!is.na(list_g1_g2$MEDITS_G1)]))

  for (spe in  species){
  if ((spe %in% G1[,1])){
  write(paste("For G1 species the individual weight in TE is mandatory. Please check,",spe), file = Errors, append = TRUE)
  numberError_ = numberError_ +1
  }
  }
  
  TE=TE[as.character(TE$INDIVIDUAL_WEIGHT)!="ND",] # selection on the weight different from ND
  
  LW = read.csv(paste(Length_weight,".csv",sep=""), sep=";",header=TRUE)
  TE$mean_weight = NA
  TE$perc_diff = NA
  
  species_to_plot = as.character(unique(LW[LW$AREA == TE$AREA[1],]$SPECIES))
  
  for (i in 1:nrow(TE)){
    ab=LW[(LW$SPECIES== paste(TE$GENUS[i],TE$SPECIES[i])) &(as.character(LW$SEX)==as.character(TE$SEX[i])) & (LW$AREA==TE$AREA[1]),]
    
    if (nrow(ab)!=0){
    A= ab$a[1]
    B= ab$b[1]
    if(as.character(TE$LENGTH_CLASSES_CODE[i])=="m"){
      mean_length =TE$LENGTH_CLASS[i] +0.5  
      } else if (as.character(TE$LENGTH_CLASSES_CODE[i])=="0"){ # step: 0.5 cm
      mean_length =(TE$LENGTH_CLASS[i] +2.5)/10 
    } else if (as.character(TE$LENGTH_CLASSES_CODE[i])=="1"){ # step: 1 cm
      mean_length =(TE$LENGTH_CLASS[i] +5)/10 
    } 
     mean_weight = A*mean_length^B
    
    TE$mean_length[i]=mean_length
    TE$mean_weight[i] = mean_weight # estimated weight
    TE$perc_diff[i] = (as.numeric(as.character(TE$INDIVIDUAL_WEIGHT[i])) -TE$mean_weight[i] )/as.numeric(as.character(TE$mean_weight[i])) *100
    
    if (abs(TE$perc_diff[i])>20){
      numberError=numberError+1 
    # write(paste("Haul",TE$HAUL_NUMBER[i],as.character(TE$GENUS[i]),as.character(TE$SPECIES[i]),as.character(TE$SEX[i]),TE$LENGTH_CLASS[i],as.character(TE$MATURITY[i]), as.character(TE$MATSUB[i]),": the difference between estimated and observed individual weight is ",round(TE$perc_diff[i],1),"%"), file = Errors, append = TRUE)
   
    }
  }
    
  }
  
  # scatter plots
for (ii in 1: length(species_to_plot)){
TE_temp1 = TE[paste(TE$GENUS,TE$SPECIES)==species_to_plot[ii] ,]
if (nrow(TE_temp1)!=0){
tiff(file=paste(getwd(),"/Graphs/check_individual_weight_",species_to_plot[ii],"_",TE$YEAR[1],".tif",sep=""), width=21, height=21, bg="white", units="cm", compression="none",res=200)
par(mfrow=c(3,1))
}
for (sex in c("M","F","I")){
TE_temp = TE[paste(TE$GENUS,TE$SPECIES)==species_to_plot[ii] & as.character(TE$SEX)== sex ,]
if (nrow(TE_temp)!=0){
ab=LW[(LW$SPECIES== paste(TE_temp$GENUS[1],TE_temp$SPECIES[1])) &(as.character(LW$SEX)==as.character(TE_temp$SEX[1])) & (LW$AREA==TE$AREA[1]),]
if (nrow(ab)!=0){
    A= ab$a[1]
    B= ab$b[1]
}
xx = as.numeric(as.character(unique(TE_temp$mean_length[order(as.numeric(as.character(TE_temp$mean_length)))])))
yy = A*xx^B
if (as.character(TE_temp$LENGTH_CLASSES_CODE[1])=="m"){
plot(as.numeric(as.character(TE_temp$mean_length)),as.numeric(as.character(TE_temp$INDIVIDUAL_WEIGHT)), main = paste("Length-weight relationship ", paste(TE_temp$GENUS[1],TE_temp$SPECIES[1])," " ,sex," - ",TE_temp$YEAR[1]), xlab ="length (mm)", ylab="weight (g)")
} else {
plot(as.numeric(as.character(TE_temp$mean_length)),as.numeric(as.character(TE_temp$INDIVIDUAL_WEIGHT)), main = paste("Length-weight relationship ", paste(TE_temp$GENUS[1],TE_temp$SPECIES[1])," " ,sex," - ",TE_temp$YEAR[1]), xlab ="length (cm)", ylab="weight (g)")
}
lines(xx,yy, col="blue")
}

}# fine sex
if (nrow(TE_temp1)!=0){
dev.off()
}
}#fine ciclo for
  
  if (numberError ==0) {
    write(paste("No error occurred"), file = Errors, append = TRUE)
  } else {
    
    write.table(TE,file=paste("TE_with_estimated_weights_",TE$YEAR[1],".csv",sep=""),sep=";",row.names=F)
    
    write("For some records the difference between estimated and observed individual weight is greater than 20%. Please verify in the file TE_with_estimated_weights.csv automatically produced in the working directory", file = Errors, append = TRUE)
  }
  
  if (numberError_==0){
  return(TRUE)
  } else {
  return(FALSE)
  }
  
}