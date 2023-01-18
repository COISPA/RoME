error.table <- function(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea) {

  if (FALSE) {
    check.df <- data.frame(matrix(ncol=6,nrow=0))
    colnames(check.df) <- c("errors","table","check type","variable","year") # ,"warnings"
    check_without_errors <- TRUE
    check_without_warnings <- TRUE
    checkName = "Check YEAR"
    table <- "ALL"
    Field <- NA
    yea <- NA

    error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
  }


  nline <- nrow(check.df)+1
  # check_without_warnings <- FALSE
  if (!check_without_errors) {
    check.df[nline,"errors"] <- "X"
  } else {
    check.df[nline,"errors"] <- ""
  }
  # if (!check_without_warnings) {
  #   check.df[nline,"warnings"] <- "X"
  # } else {
  #   check.df[nline,"warnings"] <- NA
  # }

  check.df[nline,"check type"] <- checkName
  if (exists("yea")) {
    check.df[nline,"year"] <- yea
  }
  if (exists("Field")) {
    check.df[nline,"variable"] <- Field
  } else {
    check.df[nline,"variable"] <- NA
  }
  check.df[nline,"table"] <- table
  check_without_errors = TRUE
  check_without_warnings = TRUE
  return(list(check.df,check_without_errors,check_without_warnings))
}
