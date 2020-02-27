###########################################################################################################################
#   RoME: R code to perform multiple checks on MEDITS Survey data (TA, TB, TC and TE files - old and new MEDITS formats)                                   #                   
#   Authors: I. Bitetto, M.T. Facchini, M.T. Spedicato                                                                    #
#   Coispa Tecnologia & Ricerca - Stazione sperimentale per lo Studio delle Risorse del Mare                              #
#   If you have any comments or suggestions please contact the following e-mail address: bitetto@coispa.it                #                                                                                  
#   March 2013                                                                                                            #
###########################################################################################################################
# 27. Start depth and end depth of each haul should be in the same stratum 

check_stratum<-function(ResultData){
  numberError = 0
  #ResultData = read.csv(paste(Data,".csv",sep=""), sep=";", header=TRUE)
  write(paste("
              ----------- check start depth and end depth in the same stratum TA - ",ResultData$YEAR[1]), file = Errors, append = TRUE)
  
  
  ResultData=ResultData[ResultData$VALIDITY=="V",]
  for (i in 1:nrow(ResultData)){
    if ((ResultData$SHOOTING_DEPTH[i]>=10) & (ResultData$SHOOTING_DEPTH[i]<=50))
    {ResultData$stratum_s[i]="10-50"
    } else {
      if ((ResultData$SHOOTING_DEPTH[i]>50) & (ResultData$SHOOTING_DEPTH[i]<=100)){
        ResultData$stratum_s[i]="51-100"
      } else {
        if ((ResultData$SHOOTING_DEPTH[i]>100) & (ResultData$SHOOTING_DEPTH[i]<=200)){
          ResultData$stratum_s[i]="101-200"
        } else {
          if ((ResultData$SHOOTING_DEPTH[i]>200) & (ResultData$SHOOTING_DEPTH[i]<=500)){
            ResultData$stratum_s[i]="201-500"
          } else {
            if (ResultData$SHOOTING_DEPTH[i]>=500 ){
              ResultData$stratum_s[i]="501-800"
            }
            
            
          }
          
        }
      }
    }
  }
  for (j in 1:nrow(ResultData)){
    if ((ResultData$HAULING_DEPTH[j]>=10) & (ResultData$HAULING_DEPTH[j]<=50))
    {ResultData$stratum_e[j]="10-50"
    } else {
      if ((ResultData$HAULING_DEPTH[j]>50) & (ResultData$HAULING_DEPTH[j]<=100)){
        ResultData$stratum_e[j]="51-100"
      } else {
        if ((ResultData$HAULING_DEPTH[j]>100) & (ResultData$HAULING_DEPTH[j]<=200)){
          ResultData$stratum_e[j]="101-200"
        } else {
          if ((ResultData$HAULING_DEPTH[j]>200) & (ResultData$HAULING_DEPTH[j]<=500)){
            ResultData$stratum_e[j]="201-500"
          } else {
            if ((ResultData$HAULING_DEPTH[j]>500)){
              ResultData$stratum_e[j]="501-800"
            }
          }             
        }
      }
    }
  }
  for (k in 1:nrow(ResultData)){
    if (ResultData$stratum_s[k]!= ResultData$stratum_e[k]){
      write(paste("Warning: Haul",ResultData$HAUL_NUMBER[k]," starts in the stratum",ResultData$stratum_s[k],"(",ResultData$SHOOTING_DEPTH[k],"m ) and finishes in the stratum", ResultData$stratum_e[k],"(",ResultData$HAULING_DEPTH[k],"m ) in",ResultData$TYPE_OF_FILE[k]), file = Errors, append = TRUE)}
  }
  
  if (numberError ==0) {
    write(paste("No error occurred"), file = Errors, append = TRUE)
  }  
  
  if (numberError ==0) {
    return(TRUE)
  } else { return(FALSE) }
  
  
}

################################################################################