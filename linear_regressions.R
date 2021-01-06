library(plyr)
library(dplyr)
library(caret)
library(caTools)
library(stats)
library(MASS)
library(pROC)
library(brglm2)
library(leaps)

#loading data-----------------------------------------------------------------------------------------
df_clean <- read.csv("ADNID_Baseline_MasterFile_clean.csv", stringsAsFactors = FALSE)
df_clean <- df_clean %>% dplyr::select(b_ECOG_Total_Avg, b_Informant_ECOG_Total_Avg, b_Age, BiRace, Gender,
                                     EducationYears, ECOG_Imp, INFORMECOG_Imp, CogScore, b_HAMD17Total, 
                                     b_GAD7_Total, b_PSS_Total, b_DSSI_Total, b_GSIS_Total)

#data transformation----------------------------------------------------------------------------------

#coding categorical variables as factors
df_clean$BiRace <- as.factor(df_clean$BiRace)
df_clean$Gender <- as.factor(df_clean$Gender)

df_clean$BiRace <- as.factor(df_clean$BiRace)
df_clean$Gender <- as.factor(df_clean$Gender)

#center and scaling continuous variables
center_scale <- function(x) {
    scale(x, scale = TRUE, center = TRUE)
}
df_clean[, c(1:2, 10:14)] <- center_scale(df_clean[, c(1:2, 10:14)])

#transforming non-normally distributed continuous variables
df_tran <- df_clean
plot_histogram(df_tran) #ecog vars, hamd, dssi
preProc <- preProcess(df_tran[, c(1:2, 10, 13)], method = "YeoJohnson")
df_tran[, c(1:2, 10, 13)] <- predict(preProc, df_tran[, c(1:2, 10, 13)])
plot_histogram(df_tran)

#participant ECog modeling----------------------------------------------------------------------------------

####UNIVARIATE ANALYSES
##race
mod1 <- lm(b_ECOG_Total_Avg ~ BiRace, data = df_tran)
summary(mod1) #not sig

##gender
mod2 <- lm(b_ECOG_Total_Avg ~ Gender, data = df_tran)
summary(mod2) #not sig 

##education
mod3 <- lm(b_ECOG_Total_Avg ~ EducationYears, data = df_tran)
summary(mod3) #not sig

##age
mod4 <- lm(b_ECOG_Total_Avg ~ b_Age, data = df_tran)
summary(mod4) #not sig

##HAMD
mod5 <- lm(b_ECOG_Total_Avg ~ b_HAMD17Total, data = df_tran)
summary(mod5) #significant

##GSIS
mod6 <- lm(b_ECOG_Total_Avg ~ b_GSIS_Total, data = df_tran)
summary(mod6) #not significant

##GAD
mod7 <- lm(b_ECOG_Total_Avg ~ b_GAD7_Total, data = df_tran)
summary(mod7) #significant

##DSSI
mod8 <- lm(b_ECOG_Total_Avg ~ b_DSSI_Total, data = df_tran)
summary(mod8) #not significant

##PSS
mod9 <- lm(b_ECOG_Total_Avg ~ b_PSS_Total, data = df_tran)
summary(mod9) #significant

##CogScore
mod10 <- lm(b_ECOG_Total_Avg ~ CogScore, data = df_tran)
summary(mod10) #not significant


####STEPWISE REGRESSION
set.seed(101)
models <- regsubsets(b_ECOG_Total_Avg ~ b_GAD7_Total + b_PSS_Total + 
                         b_GSIS_Total + b_DSSI_Total +b_HAMD17Total + BiRace +
                         EducationYears + b_Age + Gender, data = df_tran, nvmax = 6,
                     method = "seqrep")
summary(models)
# gad --> gad, hamd --> gad hamd edu --> gad pss gsis dssi --> gad pss hamd race edu --> gad pss hamd race edu gender


####MULTIPLE REGRESSION
#w/ sig variables from univariate analysis
mr1 <- lm(b_ECOG_Total_Avg ~ b_GAD7_Total + b_PSS_Total + b_HAMD17Total, data = df_tran)
summary(mr1)
plot(mr1)

#w/ gad, hamd (BEST)
mr2 <- lm(b_ECOG_Total_Avg ~ b_GAD7_Total + b_HAMD17Total, data = df_tran)
summary(mr2)
plot(mr2)

#w/ gad hamd edu
mr3 <- lm(b_ECOG_Total_Avg ~ b_GAD7_Total + b_HAMD17Total + EducationYears, data = df_tran)
summary(mr3)
plot(mr3)

#w/ gad pss gsis dssi
mr4 <- lm(b_ECOG_Total_Avg ~ b_GAD7_Total + b_HAMD17Total + 
              b_GSIS_Total + b_DSSI_Total, data = df_tran)
summary(mr4)
plot(mr4) 

#w/ gad pss hamd race edu
mr5 <- lm(b_ECOG_Total_Avg ~ b_GAD7_Total + b_PSS_Total + BiRace +
              b_HAMD17Total + EducationYears, data = df_tran)
summary(mr5)
plot(mr5)

#w/ gad pss hamd race edu gender
mr6 <- lm(b_ECOG_Total_Avg ~ b_GAD7_Total + b_PSS_Total + BiRace +
              b_HAMD17Total + EducationYears + Gender, data = df_tran)
summary(mr6)
plot(mr6)


