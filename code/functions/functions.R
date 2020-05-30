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
#' Changes from smbinning.sumiv function 
#' If required could add input for category levels
#' Change to apply functions rather than loop
#' Added parallel for speedup and pbapply for progress bar. Initial setup slows down, but big improvement on large datasets
#'     

iv_calculation_cit <- function (df, y, categories = 16) 
{
  #setup parallel 
  nr_cores <- max(1, detectCores() - 1)
  cl <- makeCluster(nr_cores, type = "PSOCK") #change to FORK on linux
  
  #clusterExport(cl, "base")
  clusterEvalQ(cl, library(smbinning))
  if (!is.data.frame(df)) {
    return("Data not a data.frame")
  }
  else {
    options(warn = -1)
    cat("", "\n")
    pboptions(type="txt", char=":)")
    iv_table = t(pbsapply(1:ncol(df), function(i){

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
      
    }, cl = cl))

    #disable second progressbar
    pbo <- pboptions(type = "none")
    iv_table = iv_table[pbapply(iv_table, 1, function(arg) {sum(is.na(arg))<3}, cl = cl),]
    stopCluster(cl)
    iv_table = iv_table[order(iv_table[,2],decreasing=T),]
    iv_table = data.frame(Char = as.character(iv_table[,1])
                          , IV = as.numeric(as.character(iv_table[,2]))
                          , Process=iv_table[,3])
    
    iv_table <- mutate(iv_table, Predictiveness = case_when(
      IV > 0.5 ~ "Suspicious",
      IV >= 0.3 ~ "Strong",
      IV >= 0.1 ~ "Medium",
      IV >= 0.02 ~ "Weak",
      IV < 0.02 ~ "Not Useful",
      IV == NA ~ "NA"
    ))
    
    
    options(warn = 0)
    t1 <<- iv_table
    return(iv_table)
 
    
  }
}


#' Plot Information Value Summary
#'
#' It gives the user the ability to plot the Information Value by characteristic.
#' The chart only shows characteristics with a valid IV. 
#' Example shown on \code{smbinning.sumiv} section.
#' @param sumivt A data frame saved after \code{smbinning.sumiv}.
#' @param cex Optional parameter for the user to control the font size of the characteristics
#' displayed on the chart. The default value is 0.9
#' @return The command \code{smbinning.sumiv.plot} returns a plot that shows the IV
#' for each numeric and factor characteristic in the dataset.
#' Updated from package smbinning to flag suspiciously high IV scores

smbinning.sumiv.plot=function(sumivt, cex=0.9){
  if (!is.data.frame(sumivt)){ # Check if data.frame
    return("Data not a data.frame")
  } else if (names(sumivt[1])!="Char" | names(sumivt[2])!="IV") {
    return("Not from smbinning.sumiv")}
  sumivtplot=sumivt
  sumivtplot=sumivtplot[complete.cases(sumivtplot$IV),]
  sumivtplot=sumivtplot[order(sumivtplot$IV),]
  sumivtplot=cbind(sumivtplot,Desc=ifelse(sumivtplot$IV>=0.3,"1:Strong",ifelse(sumivtplot$IV>=0.1,"2:Medium","3:Weak")))
  unique(sumivtplot$Desc)
  sumivtplot$Desc=as.factor(sumivtplot$Desc)
  smbsumivplot=dotchart(sumivtplot$IV, 
                        main="Information Value",
                        labels=sumivtplot$Char,
                        pch=ifelse(sumivtplot$IV>=0.3,21,ifelse(sumivtplot$IV>=0.1,21,1)),
                        color=ifelse(sumivtplot$IV>=0.3,"black",ifelse(sumivtplot$IV>=0.1,"black","black")),
                        bg=ifelse(sumivtplot$IV>=0.3,"black",ifelse(sumivtplot$IV>=0.1,"gray75","white")),
                        groups=sumivtplot$Desc,
                        cex=cex)
}


#' Binning based on CIT
#'
#' Updated rom base smbinning function to speed up sql loop by using dplyr functions. 
#' 
#' Example shown on \code{smbinning.sumiv} section.
#' @param sumivt A data frame saved after \code{smbinning.sumiv}.
#' @param cex Optional parameter for the user to control the font size of the characteristics
#' displayed on the chart. The default value is 0.9
#' @return The command \code{smbinning.sumiv.plot} returns a plot that shows the IV
#' for each numeric and factor characteristic in the dataset.
#' Updated from package smbinning to flag suspiciously high IV scores

smbinning2 = function(df,y,x,p=0.05){
  # Check data frame and formats
  ptm <<- proc.time()
  
  if (!is.data.frame(df)){ # Check if data.frame
    return("Data not a data.frame")
  } else if (is.numeric(y) | is.numeric(x)){ # Check if target variable is numeric
    return("Column name not string")
  } else if (grepl("[.]",y) | grepl("[.]",x)){ # Check if there is a dot
    return("Column name with a dot [.]")
  } else 
    i=which(names(df)==y) # Find Column for dependant
  j=which(names(df)==x) # Find Column for independant
  if (!is.numeric(df[,i])){ 
    return("Target (y) not found or it is not numeric")
  } else if (max(df[,i],na.rm=T)!=1){
    return("Maximum not 1")
  } else if (tolower(y)=="default"){
    return("Field name 'default' not allowed")
  } else if (fn$sqldf("select count(*) from df where cast($x as text)='Inf' or cast($x as text)='-Inf'")>0){
    return("Characteristic (x) with an 'Inf' value (Divided by Zero). Replace by NA")  
  } else if (min(df[,i],na.rm=T)!=0){
    return("Minimum not 0")
  } else if (p<=0 | p>0.5){
    return("p must be greater than 0 and lower than 0.5 (50%)")
  } else if (!is.numeric(df[,j])){
    return("Characteristic (x) not found or it is not a number")
  } else if (length(unique(df[,j]))<5){
    return("Uniques values < 5")  
  } else { 
    ctree=ctree(formula(paste(y,"~",x)),
                data=df, 
                na.action=na.exclude,
                control=ctree_control(minbucket=ceiling(round(p*nrow(df)))))
    bins=width(ctree)
    
    cat(proc.time() - ptm)
    cat("Ctree done \n")
    
    if (bins<2){return("No significant splits")}
    # Append cutpoinstop()ts in a table (Automated)
    cutvct=data.frame(matrix(ncol=0,nrow=0)) # Shell
    n=length(ctree) # Number of nodes
    for (i in 1:n) {
      cutvct=rbind(cutvct,ctree[i]$node$split$breaks)
    }
    
    cat(proc.time() - ptm)
    cat("Ctree loop \n")
    
    cutvct=cutvct[order(cutvct[,1]),] # Sort / converts to a ordered vector (asc)
    cutvct=ifelse(cutvct<0,trunc(10000*cutvct)/10000,ceiling(10000*cutvct)/10000) # Round to 4 dec. to avoid borderline cases
    # Build Information Value Table #############################################
    # Counts per not missing cutpoint
    # ivt=data.frame(matrix(ncol=0,nrow=0)) # Empty table
    # n=length(cutvct) # Number of cutpoits
    # for (i in 1:n) {
    # cutpoint=cutvct[i]
    # ivt=rbind(ivt,
    # fn$sqldf(
    # "select '<= $cutpoint' as Cutpoint,
    # NULL as CntRec,
    # NULL as CntGood,
    # NULL as CntBad,
    # sum(case when $x <= $cutpoint and $y in (1,0) then 1 else 0 end) as CntCumRec,
    # sum(case when $x <= $cutpoint and $y=1 then 1 else 0 end) as CntCumGood,
    # sum(case when $x <= $cutpoint and $y=0 then 1 else 0 end) as CntCumBad,
    # NULL as PctRec,
    # NULL as GoodRate,
    # NULL as BadRate,
    # NULL as Odds,
    # NULL as LnOdds,
    # NULL as WoE,
    # NULL as IV
    # from df where $x is not NULL and $y is not NULL")
    # )
    # }
    cutvct <<- cutvct
    ds1 <- select(df, x, y)
    colnames(ds1) <- 
      
      ds2 <<- ds1
    cuts <- data.frame(Cutpoint = paste("<=", cutvct), CutStart = cutvct)
    
    result <- cuts %>% group_by(Cutpoint) %>% 
      mutate(
        CntRec = NA,
        CntGood = NA,
        CntBad = NA,
        CntCumRec  = length(which(ds1$x <= CutStart )), 
        CntCumGood  = length(which(ds1$x <= CutStart & ds1$y == 1)),
        CntCumBad  = length(which(ds1$x <= CutStart & ds1$y == 0)),
        PctRec = NA,
        GoodRate = NA,
        BadRate = NA,
        Odds = NA,
        LnOdds = NA,
        WoE = NA,
        IV = NA
      )
    result <<- result
    ivt <- as.data.frame(select(result, -CutStart))
    
    
    cat(proc.time() - ptm)
    cat("first loop \n")
    
    ivt2 <<- ivt
    
    cutpoint=max(df[,j],na.rm=T) # Calculte Max without Missing
    cutpoint=ifelse(cutpoint<0,trunc(10000*cutpoint)/10000,ceiling(10000*cutpoint)/10000) # Round to 4 dec. to avoid borderline cases
    maxcutpoint=max(cutvct) # Calculte Max cut point
    mincutpoint=min(df[,j],na.rm=T) # Calculte Min without Missing for later usage
    mincutpoint=ifelse(mincutpoint<0,trunc(10000*mincutpoint)/10000,ceiling(10000*mincutpoint)/10000) # Round to 4 dec. to avoid borderline cases 
    ivt=rbind(ivt,
              fn$sqldf(
                "select '> $maxcutpoint' as Cutpoint,
                      NULL as CntRec,
                      NULL as CntGood,
                      NULL as CntBad,
                      sum(case when $x <= $cutpoint and $y in (1,0) then 1 else 0 end) as CntCumRec,
                      sum(case when $x <= $cutpoint and $y=1 then 1 else 0 end) as CntCumGood,
                      sum(case when $x <= $cutpoint and $y=0 then 1 else 0 end) as CntCumBad,
                      NULL as PctRec,
                      NULL as GoodRate,
                      NULL as BadRate,
                      NULL as Odds,
                      NULL as LnOdds,
                      NULL as WoE,
                      NULL as IV
                      from df where $x is not NULL and $y is not NULL")
    )
    
    cat(proc.time() - ptm)
    cat("RBIND SQL \n")
    
    # Missing Data
    x.na=fn$sqldf("select count(*) from df where $x is null")  
    y.na=fn$sqldf("select count(*) from df where $y is null")
    if(x.na>0){
      ivt=rbind(ivt,
                fn$sqldf(
                  "select 'Missing' as Cutpoint,
                          sum(case when $x is NULL and $y in (1,0) then 1 else 0 end) as CntRec,
                          sum(case when $x is NULL and $y=1 then 1 else 0 end) as CntGood,
                          sum(case when $x is NULL and $y=0 then 1 else 0 end) as CntBad,
                          NULL as CntCumRec,
                          NULL as CntCumGood,
                          NULL as CntCumBad,
                          NULL as PctRec,
                          NULL as GoodRate,
                          NULL as BadRate,
                          NULL as Odds,
                          NULL as LnOdds,
                          NULL as WoE,
                          NULL as IV
                          from df where $y is not NULL")
      )
    } 
    
    else {
      ivt=rbind(ivt,
                c("Missing",0,0,0,NA,NA,NA,NA,NA,NA,NA,NA,NA))}
    
    cat(proc.time() - ptm)
    cat("NA Rbind \n")
    
    # Total
    ivt=rbind(ivt,
              fn$sqldf(
                "select 'Total' as Cutpoint,
                      count(*) as CntRec,
                      sum(case when $y=1 then 1 else 0 end) as CntGood,
                      sum(case when $y=0 then 1 else 0 end) as CntBad,
                      NULL as CntCumRec,
                      NULL as CntCumGood,
                      NULL as CntCumBad,
                      NULL as PctRec,
                      NULL as GoodRate,
                      NULL as BadRate,
                      NULL as Odds,
                      NULL as LnOdds,
                      NULL as WoE,
                      NULL as IV
                      from df where $y is not NULL")
    )
    
    cat(proc.time() - ptm)
    cat("Total Rbind \n")
    
    # Covert to table numeric
    options(warn=-1)
    ncol=ncol(ivt)
    for (i in 2:ncol){
      ivt[,i]=as.numeric(ivt[,i])
    }
    options(warn=0)
    
    cat(proc.time() - ptm)
    cat("Numeric conversion loop \n")
    
    # Complete Table 
    ivt[1,2]=ivt[1,5] # Nbr Records
    ivt[1,3]=ivt[1,6] # Nbr Goods
    ivt[1,4]=ivt[1,7] # Nbr Bads
    
    # From 2nd row
    n=nrow(ivt)-2
    for (i in 2:n){ivt[i,2]=ivt[i,5]-ivt[i-1,5]
    ivt[i,3]=ivt[i,6]-ivt[i-1,6]
    ivt[i,4]=ivt[i,7]-ivt[i-1,7]}
    
    cat(proc.time() - ptm)
    cat("Calc loop \n")
    
    ivt[2,2]=ivt[2,5]-ivt[1,5]
    ivt[2,3]=ivt[2,6]-ivt[1,6]
    ivt[2,4]=ivt[2,7]-ivt[1,7]
    
    # Missing row.  Update: Added "if" statement
    ivt[i+1,5]=ivt[i,5]+ivt[i+1,2]
    ivt[i+1,6]=ivt[i,6]+ivt[i+1,3]
    ivt[i+1,7]=ivt[i,7]+ivt[i+1,4]
    
    # Calculating metrics
    options(scipen=999) # Remove Scientific Notation
    ivt[,8]=round(ivt[,2]/ivt[i+2,2],4) # PctRec
    ivt[,9]=round(ivt[,3]/ivt[,2],4) # GoodRate
    ivt[,10]=round(ivt[,4]/ivt[,2],4) # BadRate
    ivt[,11]=round(ivt[,3]/ivt[,4],4) # Odds
    ivt[,12]=round(log(ivt[,3]/ivt[,4]),4) # LnOdds
    G=ivt[i+2,3]
    B=ivt[i+2,4]
    LnGB=log(G/B) # IV Part 1
    ivt[,13]=round(log(ivt[,3]/ivt[,4])-LnGB,4) # WoE
    ivt[,14]=round(ivt[,13]*(ivt[,3]/G-ivt[,4]/B),4) # Mg IV
    # ivt[i+2,14]=round(sum(ivt[,13]*(ivt[,3]/G-ivt[,4]/B),na.rm=T),4) -- Old Calculation
    # Calculates Information Value even with undefined numbers
    ivt[i+2,14]=0.0000
    
    cat(proc.time() - ptm)
    cat("Calculations \n")
    
    for (k in 1:(nrow(ivt)-1))
    {
      if(is.finite(ivt[k,14])) {mgiv=ivt[k,14]} else {mgiv=0.0000}
      ivt[i+2,14]=ivt[i+2,14]+mgiv
    }
    iv=ivt[i+2,14]
    cat(proc.time() - ptm)
    cat("Func done \n")
    
    # End Inf. Value Table ###################################################### 
  }
  bands=append(mincutpoint,cutvct)
  bands=append(bands,cutpoint)
  iv=ivt[i+2,14]
  
  cat(proc.time() - ptm)
  cat("smbin  done \n")
  
  list(ivtable=ivt,iv=iv,ctree=ctree,bands=bands,x=x,col_id=j,cuts=cutvct)
  
}

