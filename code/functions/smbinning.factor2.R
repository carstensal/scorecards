#' Bin categorical variable
#'
#' @param x Character string indicating which variable is being binned 
#' @param y Binary response variable 
#' @param dftrain Data frame containing the training data


#' Updated from base by using dplyr rather than loops, categorical binning on 300k sample down from 78 seconds to <5s
#'    

smbinning.factor2 <- function (df, y, x, maxcat = 20) 
{
  
  if (!is.data.frame(df)) {
    return("Data not a data.frame")
  }
  else if (is.numeric(y) | is.numeric(x)) {
    return("Column name not string")
  }
  else if (grepl("[.]", y) | grepl("[.]", x)) {
    return("Column name with a dot [.]")
  }
  else i = which(names(df) == y)
  j = which(names(df) == x)
  if (!is.numeric(df[, i])) {
    return("Target (y) not found or it is not numeric")
  }
  else if (max(df[, i], na.rm = T) != 1) {
    return("Maximum not 1")
  }
  else if (any(grepl(",", df[, j]))) {
    return("Values contain comma")
  }
  else if (tolower(y) == "default") {
    return("Field name 'default' not allowed")
  }
  else if (fn$sqldf("select count(*) from df where cast($x as text)='Inf' or cast($x as text)='-Inf'") > 
           0) {
    return("Characteristic (x) with an 'Inf' value (Divided by Zero). Replace by NA")
  }
  else if (min(df[, i], na.rm = T) != 0) {
    return("Minimum not 0")
  }
  else if (!is.factor(df[, j])) {
    return("Characteristic (x) not found or it is not a factor")
  }
  else if (length(unique(df[, j])) <= 1) {
    return("Characteristic (x) requires at leats 2 uniques categories")
  }
  else if (length(unique(df[, j])) > maxcat) {
    return("Too many categories")
  }
  else {
    
    #split select because need to run filter for all NA values again for Missing calculation
    df_selected <<- select(df, all_of(x), all_of(y))
    
    #could change to parse from function entry, need to check rlang?, below works, but in storing easier to name now
    #df_no_na <<- filter(df_selected, !is.na(!!as.name(x) ))
    
    colnames(df_selected) <- c("X_Var", "Y_Var")
    
    df_no_na <- filter(df_selected, !is.na(X_Var))
    
    result <- df_no_na %>% group_by(X_Var) %>%
      mutate( Good = sum(Y_Var == 1), Bad = sum(Y_Var == 0)) %>%
      count(Good, Bad) %>%
      arrange(desc(n))
    
    ivt = cbind.data.frame(Cutpoint = paste("=", result$X_Var)
                           , CntRec = result$n
                           , CntGood = result$Good
                           , CntBad = result$Bad
                           , CntCumRec = 0
                           , CntCumGood = 0
                           , CntCumBad = 0
                           , PctRec = 0
                           , GoodRate = 0
                           , BadRate = 0
                           , Odds = 0
                           , LnOdds = 0
                           , WoE = 0
                           , IV = 0
    )
    
    
    cutvct = as.vector(as.matrix(result[,1]))
    
    if (length(cutvct) < 1) {return("No Bins")}   
    
    # Entry for Missing Data
    if(sum(is.na(df_selected))>0){
      df_na <- filter(df_selected, is.na(X_Var)) %>%
        group_by(Y_Var) %>%
        count()
      
      CntRecNA <- sum(df_na)-1
      CntGoodNA <- df_na$n[2]
      CntBadNA <- df_na$n[1]
      
      ivt = rbind(ivt,
                  c("Missing", CntRecNA, CntGoodNA, CntBadNA, NA, NA, NA, NA, NA, NA, NA, NA, NA))
    } 
    else{
      ivt = rbind(ivt,
                  c("Missing",0,0,0,0,0,0,NA,NA,NA,NA,NA,NA))}
    
    # Total data entry
    df_total <- as.data.frame(group_by(df_selected, Y_Var) %>%
                                count())
    
    CntRecTotal <- sum(df_total)-1
    CntGoodTotal <- df_total$n[2]
    CntBadTotal <- df_total$n[1]
    ivt = rbind(ivt,
                c("Total", CntRecTotal, CntGoodTotal, CntBadTotal, NA, NA, NA, NA, NA, NA, NA, NA, NA))
    
    
    options(warn = -1)
    ncol = ncol(ivt)
    
    #changed for loop numeric change to apply
    ivt[, 2:ncol] <- sapply(ivt[, 2:ncol], as.numeric)
    
    options(warn = 0)
    ivt[1, 5] = ivt[1, 2]
    ivt[1, 6] = ivt[1, 3]
    ivt[1, 7] = ivt[1, 4]
    n = nrow(ivt) - 2
    for (i in 2:n) {
      ivt[i, 5] = ivt[i, 2] + ivt[i - 1, 5]
      ivt[i, 6] = ivt[i, 3] + ivt[i - 1, 6]
      ivt[i, 7] = ivt[i, 4] + ivt[i - 1, 7]
    }
    ivt[2, 5] = ivt[2, 2] + ivt[1, 5]
    ivt[2, 6] = ivt[2, 3] + ivt[1, 6]
    ivt[2, 7] = ivt[2, 4] + ivt[1, 7]
    ivt[i + 1, 5] = ivt[i, 5] + ivt[i + 1, 2]
    ivt[i + 1, 6] = ivt[i, 6] + ivt[i + 1, 3]
    ivt[i + 1, 7] = ivt[i, 7] + ivt[i + 1, 4]
    options(scipen = 999)
    ivt[, 8] = round(ivt[, 2]/ivt[i + 2, 2], 4)
    ivt[, 9] = round(ivt[, 3]/ivt[, 2], 4)
    ivt[, 10] = round(ivt[, 4]/ivt[, 2], 4)
    ivt[, 11] = round(ivt[, 3]/ivt[, 4], 4)
    ivt[, 12] = round(log(ivt[, 3]/ivt[, 4]), 4)
    G = ivt[i + 2, 3]
    B = ivt[i + 2, 4]
    LnGB = log(G/B)
    ivt[, 13] = round(log(ivt[, 3]/ivt[, 4]) - LnGB, 4)
    ivt[, 14] = round(ivt[, 13] * (ivt[, 3]/G - ivt[, 4]/B), 
                      4)
    ivt[i + 2, 14] = 0
    for (k in 1:(nrow(ivt) - 1)) {
      if (is.finite(ivt[k, 14])) {
        mgiv = ivt[k, 14]
      }
      else {
        mgiv = 0
      }
      ivt[i + 2, 14] = ivt[i + 2, 14] + mgiv
    }
    iv = ivt[i + 2, 14]
  }
  
  
  list(ivtable = ivt, iv = iv, x = x, col_id = j, cuts = cutvct)
}
