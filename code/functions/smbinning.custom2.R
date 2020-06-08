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
#'   Updated from normal by using dplyr - faster
#'   If using input in scorecard app additional > level added which caused break
#'   
smbinning.custom2 <- function (df, y, x, cuts) 
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
  else if (!is.numeric(df[, j])) {
    return("Characteristic (x) not found or it is not a number")
  }
  else if (length(unique(df[, j])) < 5) {
    return("Uniques values < 5")
  }
  else {
    cutvct = data.frame(matrix(ncol = 0, nrow = 0))
    n = length(cuts)
    if (n < 1) {
      return("No Bins")
    }
    for (i in 1:n) {
      cutvct = rbind(cutvct, cuts[i])
    }
    cutvct = cutvct[order(cutvct[, 1]), ]
    cutvct = ifelse(cutvct < 0, trunc(10000 * cutvct)/10000, 
                    ceiling(10000 * cutvct)/10000)
    #ivt = data.frame(matrix(ncol = 0, nrow = 0))
    n = length(cutvct)
    
    cuts <- data.frame(Cutpoint = paste("<=", cutvct), CutStart = cutvct, stringsAsFactors=FALSE)
    
    # Add maximum as another level, changes only output in text of Cutpoints from > final cut  to <= maximum in description
    cutpoint = max(df[, j], na.rm = T)
    cutpoint = ifelse(cutpoint < 0, trunc(10000 * cutpoint)/10000, ceiling(10000 * cutpoint)/10000)
    
    #split select because need to run filter for all NA values again for Missing calculation
    df_selected <- select(df, x, y)
    
    #could change to parse from function entry
    colnames(df_selected) <- c("X_Var", "Y_Var")
    
    
    df_no_na <- filter(df_selected, !is.na(x))
    
    result <- cuts %>% group_by(Cutpoint) %>% 
      mutate(
        CntRec = 0,
        CntGood = 0,
        CntBad = 0,
        CntCumRec  = length(which(df_no_na$X_Var <= CutStart )), 
        CntCumGood  = length(which(df_no_na$X_Var <= CutStart & df_no_na$Y_Var == 1)),
        CntCumBad  = length(which(df_no_na$X_Var <= CutStart & df_no_na$Y_Var == 0)),
        PctRec = 0,
        GoodRate = 0,
        BadRate = 0,
        Odds = 0,
        LnOdds = 0,
        WoE = 0,
        IV = 0
      )
    ivt <- as.data.frame(select(result, -CutStart))
    
    maxcutpoint=max(cutvct) # Calculte Max cut point
    mincutpoint=min(df[,j],na.rm=T) # Calculte Min without Missing for later usage
    mincutpoint=ifelse(mincutpoint<0,trunc(10000*mincutpoint)/10000,ceiling(10000*mincutpoint)/10000) 
    # Round to 4 dec. to avoid borderline cases 
    
    
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
                  c("Missing",0,0,0,NA,NA,NA,NA,NA,NA,NA,NA,NA))}
    
    # Total data entry
    df_total <- as.data.frame(group_by(df_selected, Y_Var) %>%
                                count())
    
    CntRecTotal <- sum(df_total)-1
    CntGoodTotal <- df_total$n[2]
    CntBadTotal <- df_total$n[1]
    ivt = rbind(ivt,
                c("Total", CntRecTotal, CntGoodTotal, CntBadTotal, NA, NA, NA, NA, NA, NA, NA, NA, NA))
    
    
    # Covert to table numeric
    options(warn = -1)
    ncol = ncol(ivt)
    
    #changed for loop numeric change to apply
    ivt[, 2:ncol] <- sapply(ivt[, 2:ncol], as.numeric)
    
    
    
    options(warn = 0)
    ivt[1, 2] = ivt[1, 5]
    ivt[1, 3] = ivt[1, 6]
    ivt[1, 4] = ivt[1, 7]
    n = nrow(ivt) - 2
    for (i in 2:n) {
      ivt[i, 2] = ivt[i, 5] - ivt[i - 1, 5]
      ivt[i, 3] = ivt[i, 6] - ivt[i - 1, 6]
      ivt[i, 4] = ivt[i, 7] - ivt[i - 1, 7]
    }
    ivt[2, 2] = ivt[2, 5] - ivt[1, 5]
    ivt[2, 3] = ivt[2, 6] - ivt[1, 6]
    ivt[2, 4] = ivt[2, 7] - ivt[1, 7]
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
  bands = append(mincutpoint, cutvct)
  bands = append(bands, cutpoint)
  list(ivtable = ivt, iv = iv, bands = bands, x = x, col_id = j, 
       cuts = cutvct)
}
