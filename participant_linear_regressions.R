library(plyr)
library(dplyr)
library(caret)
library(caTools)
library(stats)
library(MASS)
library(pROC)
library(brglm2)
library(leaps)
source("~/Documents/Data_Science/projects/adni/sc_linear/linear_regression_functions.R")

#loading data-----------------------------------------------------------------------------------------
df_full <- read.csv("ADNID_Baseline_MasterFile_clean.csv", stringsAsFactors = FALSE)
df_clean <- df_full %>% dplyr::select(b_ECOG_Total_Avg, b_Informant_ECOG_Total_Avg, b_Age, BiRace, Gender,
                                     EducationYears, ECOG_Imp, INFORMECOG_Imp, CogScore, b_HAMD17Total, 
                                     b_GAD7_Total, b_PSS_Total, b_DSSI_Total, b_GSIS_Total)

#data transformation----------------------------------------------------------------------------------

#transforming variables in clean df
df_clean_tran <- transform_clean_df(df_clean)
plot_histogram(df_clean_tran)

#transforming variables in full df (for ecog and cognition subdomain analyses)
df_full_tran <- transform_full_df(df_cfull)
plot_histogram(df_full_tran)


#univariate analyses----------------------------------------------------------------------------------

##race
mod1 <- lm(b_ECOG_Total_Avg ~ BiRace, data = df_clean_tran)
summary(mod1) #not sig

##gender
mod2 <- lm(b_ECOG_Total_Avg ~ Gender, data = df_clean_tran)
summary(mod2) #not sig 

##education
mod3 <- lm(b_ECOG_Total_Avg ~ EducationYears, data = df_clean_tran)
summary(mod3) #not sig

##age
mod4 <- lm(b_ECOG_Total_Avg ~ b_Age, data = df_clean_tran)
summary(mod4) #not sig

##HAMD
mod5 <- lm(b_ECOG_Total_Avg ~ b_HAMD17Total, data = df_clean_tran)
summary(mod5) #significant

##GSIS
mod6 <- lm(b_ECOG_Total_Avg ~ b_GSIS_Total, data = df_clean_tran)
summary(mod6) #not significant

##GAD
mod7 <- lm(b_ECOG_Total_Avg ~ b_GAD7_Total, data = df_clean_tran)
summary(mod7) #significant

##DSSI
mod8 <- lm(b_ECOG_Total_Avg ~ b_DSSI_Total, data = df_clean_tran)
summary(mod8) #not significant

##PSS
mod9 <- lm(b_ECOG_Total_Avg ~ b_PSS_Total, data = df_clean_tran)
summary(mod9) #significant

##CogScore
mod10 <- lm(b_ECOG_Total_Avg ~ CogScore, data = df_clean_tran)
summary(mod10) #not significant


#stepwise regression----------------------------------------------------------------------------------
set.seed(101)
models <- regsubsets(b_ECOG_Total_Avg ~ b_GAD7_Total + b_PSS_Total + 
                         b_GSIS_Total + b_DSSI_Total +b_HAMD17Total + BiRace +
                         EducationYears + b_Age + Gender, data = df_clean_tran, nvmax = 6,
                     method = "seqrep")
summary(models)
# gad --> gad, hamd --> gad hamd edu --> gad pss gsis dssi --> gad pss hamd race edu --> gad pss hamd race edu gender


#multiple regression----------------------------------------------------------------------------------

#w/ sig variables from univariate analysis
mr1 <- lm(b_ECOG_Total_Avg ~ b_GAD7_Total + b_PSS_Total + b_HAMD17Total, data = df_clean_tran)
summary(mr1)
plot(mr1)

#w/ gad, hamd (BEST)
mr2 <- lm(b_ECOG_Total_Avg ~ b_GAD7_Total + b_HAMD17Total, data = df_clean_tran)
summary(mr2)
plot(mr2)

#w/ gad hamd edu
mr3 <- lm(b_ECOG_Total_Avg ~ b_GAD7_Total + b_HAMD17Total + EducationYears, data = df_clean_tran)
summary(mr3)
plot(mr3)

#w/ gad pss gsis dssi
mr4 <- lm(b_ECOG_Total_Avg ~ b_GAD7_Total + b_HAMD17Total + 
              b_GSIS_Total + b_DSSI_Total, data = df_clean_tran)
summary(mr4)
plot(mr4) 

#w/ gad pss hamd race edu
mr5 <- lm(b_ECOG_Total_Avg ~ b_GAD7_Total + b_PSS_Total + BiRace +
              b_HAMD17Total + EducationYears, data = df_clean_tran)
summary(mr5)
plot(mr5)

#w/ gad pss hamd race edu gender
mr6 <- lm(b_ECOG_Total_Avg ~ b_GAD7_Total + b_PSS_Total + BiRace +
              b_HAMD17Total + EducationYears + Gender, data = df_clean_tran)
summary(mr6)
plot(mr6)


#selected regression model with ecog subdomains-------------------------------------------------------
#use df_full data frame

#memory
sd1 <- lm(b_ECOG_Memory_Avg ~ b_GAD7_Total + b_HAMD17Total, data = df_full_tran)
summary(sd1)
plot(sd1)

#language
sd2 <- lm(b_ECOG_Language_Avg ~ b_GAD7_Total + b_HAMD17Total, data = df_full_tran)
summary(sd2)
plot(sd2)

#visuospatial
sd3 <- lm(b_ECOG_Visuospatial_Avg ~ b_GAD7_Total + b_HAMD17Total, data = df_full_tran)
summary(sd3)
plot(sd3)

#executive planning
sd4 <- lm(b_ECOG_ExecPlanning_Avg ~ b_GAD7_Total + b_HAMD17Total, data = df_full_tran)
summary(sd4)
plot(sd4)

#executive organization
sd5 <- lm(b_ECOG_ExecOrganization_Avg ~ b_GAD7_Total + b_HAMD17Total, data = df_full_tran)
summary(sd5)
plot(sd5)

#exectutive attention
sd6 <- lm(b_ECOG_ExecAttention_Avg ~ b_GAD7_Total + b_HAMD17Total, data = df_full_tran)
summary(sd6)
plot(sd6)

#executive total
sd7 <- lm(b_ECOG_ExecTotal_Avg ~ b_GAD7_Total + b_HAMD17Total, data = df_full_tran)
summary(sd7)
plot(sd7)


#ecog score predicted by cognitive subdomain----------------------------------------------------------
#use df_full data frame

#executive functioning
cs1 <- lm(b_ECOG_Total_Avg ~ b_Heaton_FAS_SS_imp2 + b_Stroop_CW_SSImp2 + b_Heaton_TMTB_SS_imp2, data = df_full_tran)
summary(cs1) #not sig

#visual perception functioning
cs2 <- lm(b_ECOG_Total_Avg ~ b_BentonJLO_SSImp2, data = df_full_tran)
summary(cs2) #not sig

#visual learning & memory
cs3 <- lm(b_ECOG_Total_Avg ~ b_BVMT_TotalRecall_SSImp2 + b_BVMT_DelayedRecall_SSImp2, data = df_full_tran)
summary(cs3) #not sig

#verbal learning & memory
cs4 <- lm(b_ECOG_Total_Avg ~ b_LM_Immediate_SSImp2 + b_LM_Delayed_SSImp2 + 
              b_HVLT_TotalRecall_SSImp2 + b_HVLT_DelayedRecall_SSImp2, data = df_full_tran)
summary(cs4) #not sig

#working memory (model fails as digit span has no impaired scores)
cs5 <- lm(b_ECOG_Total_Avg ~ b_DigitSpan_Total_SSImp2, data = df_full_tran)
summary(cs5) #not sig

#language ability
cs6 <- lm(b_ECOG_Total_Avg ~ b_Heaton_BNT_SS_imp2, data = df_full_tran)
summary(cs6) #not sig

#info processing speed
cs7 <- lm(b_ECOG_Total_Avg ~ b_DigitSymbol_SSImp2 + b_Heaton_TMTA_SS_imp2, data = df_full_tran)
summary(cs7) #not sig
