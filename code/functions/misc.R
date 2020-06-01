install.packages("formatR")

library(formatR)
?tidy_dir
formatR::tidy_dir("C:/Users/CP318189/Documents/Repos/scorecards/code/functions")

library(smbinning)
library(funModeling)
library(dplyr)

?order()

variable_importance = var_rank_info(smbsimdf1, "fgood")



application_train <- read.csv(file = 'C:\\Users\\CP318189\\Documents\\Repos\\application_train.csv')
application_train$OCCUPATION_TYPE <- as.factor(application_train$OCCUPATION_TYPE)

application_train$TARGET <- as.numeric(application_train$TARGET)

##misc
col_names <- sapply(application_train, function(col) length(unique(col)) < 10)
application_train[ , col_names] <- lapply(application_train[ , col_names] , factor)
application_train$TARGET <- as.numeric(application_train$TARGET)-1

str(application_train)

bin1 <- smbinning(application_train[1:10000,], "TARGET", "AMT_CREDIT")
bin2 <- smbinning2(application_train, "TARGET", "AMT_CREDIT")

bin1$ivtable
bin2$ivtable

#with NA vals
bin1 <- smbinning.factor(application_train[1:50000,], "TARGET", "OCCUPATION_TYPE", maxcat = 20)
bin2 <- smbinning2(application_train[1:50000,], "TARGET", "OCCUPATION_TYPE")

bin1$ivtable 
bin2$ivtable


#Baseline ~ 45.96 AMT_CREDIT
#Baseline ~ 24.97 APARTMENTS_AVG
library(smbinning)
library(dplyr)

options(editor = "C:\\Program Files\\Notepad++\\notepad++.exe")

application_train <- read.csv(file = 'C:\\Users\\CP318189\\Documents\\Repos\\application_train.csv')
application_train$OCCUPATION_TYPE <- as.factor(application_train$OCCUPATION_TYPE)

application_train$TARGET <- as.numeric(application_train$TARGET)

str(application_train$TARGET)
# AMT_CREDIT
#v1 16
#v2 12
#v3 9
#v4 5

#APARTMENTS_AVG
#v1 18
#v2 16
#v3 6
#v4 3

system.time(smbinning(application_train, "TARGET", "APARTMENTS_AVG"))

system.time(smbinning.factor(application_train[1:100000,], "TARGET", "OCCUPATION_TYPE", maxcat = 20))

system.time(smbinning.factor2(application_train[1:50000,], "TARGET", "OCCUPATION_TYPE", maxcat = 20))

system.time(smbinning2(application_train[1:50000], "TARGET", "OCCUPATION_TYPE"))

colnames(df_selected) <- c("X_Var", "Y_Var")

#categorical, full = 78, orig 50k = 12

bin1 <- smbinning.factor(application_train[1:50000,], y = "TARGET", x = "OCCUPATION_TYPE", maxcat = 20)
bin2 <- smbinning.factor2(application_train[1:50000,], y = "TARGET", x = "OCCUPATION_TYPE", maxcat = 20)

b2 <- bin1$ivtable %>% arrange(desc(CntRec))
bin2$ivtable

df2 <- select(application_train[1:50000,], c("TARGET", "OCCUPATION_TYPE"))


# What it is all about IV calculation check timer
# Original on 50k entries == 458.7 
system.time(smbinning.sumiv(application_train[1:50000,], 'TARGET'))

# Optim smb, smb.factor, apply IV and parallel. == 40
system.time(iv_calculation_cit(application_train[1:50000,], 'TARGET'))
# Full == 237.67 
system.time(iv_calculation_cit(application_train, 'TARGET'))



