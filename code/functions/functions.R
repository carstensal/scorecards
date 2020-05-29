#' Bin numeric variable
#'
#' @param x Character string indicating which variable is being binned 
#' @param y Binary response variable 
#' @param dftrain Data frame containing the training data
#' @param dftest Data frame containing the test data
#' @param cuts Vector of pre-determined cut points for binning
#' @param type Whether to bin using conditional inference trees or use quantiles
#'
#' @return A list of two components, the first component is the binned variable
#'     in the training data and the second component is the binning applied on
#'     the test data
#'     
#'     
binning_numeric <- function(x, y, dftrain, dftest, cuts = NULL, type = "CIT") {
    if (is.null(cuts) && type == "CIT") {
        bin_train <- smbinning(dftrain, y, x)
    } else if (is.null(cuts) && type != "CIT") {
        cuts <- unique(c(min(dftrain[[x]], na.rm = TRUE), quantiles = quantile(dftrain[[x]], probs = seq(0.1, 1, by = 0.1), 
            na.rm = TRUE)))
        bin_train <- smbinning.custom(dftrain, y, x, cuts)
    } else {
        bin_train <- smbinning.custom(dftrain, y, x, cuts)
    }
    if (class(bin_train) != "character") {
        bin_test <- smbinning.custom(dftest, y, x, bin_train$cuts)
    } else {
        bin_test <- bin_train
    }
    list(bin_train = bin_train, bin_test = bin_test)
}



#' IV Calculation for all variables
#'
#' @param df Dataframe containing dataset loaded from input function
#' @param y Binary response variable (0,1). Integer (int) is required. Name of y must not have a dot. 
#'        Name "default" is not allowed.
#'
#' @return Table with IV of all variables based on full dataset(to check if required to only run on train). 
#' Goal is to provide view for variable filtering and not model building. 
#' 
#' If required could add input for category levels
#' Change to apply (pbapply) for tracking time with parallel 
#'     
iv_calculation_cit2 <- function (df, y, categories = 16) 
{
  if (!is.data.frame(df)) {
    return("Data not a data.frame")
  }
  ncol = ncol(df)
  sumivt = data.frame(matrix(ncol = 0, nrow = 0))
  options(warn = -1)
  cat("", "\n")
  pb = txtProgressBar(min = 0, max = 1, initial = 0, style = 3, char = "-", width = 50)
  for (i in 1:ncol) {
    smbnum = smbinning(df, y, colnames(df[i]))
    smbfac = smbinning.factor(df, y, colnames(df[i]), maxcat = categories)
    if (colnames(df[i]) != y) {
      if (is.numeric(df[, i]) & is.list(smbnum)) {
        sumivt = rbind(sumivt, data.frame(Char = colnames(df[i]), 
                                          IV = smbnum$iv, Process = "Numeric binning OK"))
      }
      else if (is.numeric(df[, i]) & !is.list(smbnum)) {
        sumivt = rbind(sumivt, data.frame(Char = colnames(df[i]), 
                                          IV = NA, Process = smbnum))
      }
      else if (is.factor(df[, i]) & is.list(smbfac)) {
        sumivt = rbind(sumivt, data.frame(Char = colnames(df[i]), 
                                          IV = smbfac$iv, Process = "Factor binning OK"))
      }
      else if (is.factor(df[, i]) & !is.list(smbfac)) {
        sumivt = rbind(sumivt, data.frame(Char = colnames(df[i]), 
                                          IV = NA, Process = smbfac))
      }
      else {
        sumivt = rbind(sumivt, data.frame(Char = colnames(df[i]), 
                                          IV = NA, Process = "Not numeric nor factor"))
      }
    }
    setTxtProgressBar(pb, i/ncol)
  }
  close(pb)
  options(warn = 0)
  iv_table <<- sumivt[with(sumivt, order(-IV)), ]
  cat("", "\n")
  return(iv_table)
}




iv_calculation_cit <- function (df, y, categories = 16) 
{
  if (!is.data.frame(df)) {
    return("Data not a data.frame")
  }
  else {
    options(warn = -1)
    cat("", "\n")
    #pb = txtProgressBar(min = 0, max = 1, initial = 0, style = 3, char = "-", width = 50)
    iv_table = t(pbsapply(1:ncol(df), function(i){
      #setTxtProgressBar(pb, i/ncol(df))
      
      if (colnames(df[i]) != y) {
        
        # FOR NUMERICAL VARIABLES
        if(is.numeric(df[,i])){
          smbnum = smbinning(df, y, colnames(df[i]))
          
          if (is.list(smbnum)) 
            c(colnames(df[i]), smbnum$iv, "Numeric binning OK")
          else 
            c(colnames(df[i]), NA, smbnum)
        }
        
        # FOR FACTOR VARIABLES
        else{
          if(is.factor(df[, i])){
            smbfac = smbinning.factor(df, y, colnames(df[i]), maxcat = categories)
            if (is.list(smbfac))
              c(colnames(df[i]), smbfac$iv, "Factor binning OK")
            
            else
              c(colnames(df[i]),  NA, smbfac)
            
          }
          else
            c(colnames(df[i]), NA, "Not numeric nor factor")
          
        }
      } 
      else
        c(NA,NA,NA)
      
    }))
    
    iv_table = iv_table[pbapply(iv_table,1,function(arg){sum(is.na(arg))<3}),]
    iv_table = iv_table[order(iv_table[,2],decreasing=T),]
    iv_table = data.frame(Char=iv_table[,1], IV=iv_table[,2], Process=iv_table[,3])
    options(warn = 0)
    cat("", "\n")
   # close(pb)
    return(iv_table)
  }
}

