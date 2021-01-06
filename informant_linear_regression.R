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
mod1b <- lm(b_Informant_ECOG_Total_Avg ~ BiRace, data = df_clean_tran)
summary(mod1b) # sig

##gender
mod2b <- lm(b_Informant_ECOG_Total_Avg ~ Gender, data = df_clean_tran)
summary(mod2b) #not sig 

##education
mod3b <- lm(b_Informant_ECOG_Total_Avg ~ EducationYears, data = df_clean_tran)
summary(mod3b) #not sig

##age
mod4b <- lm(b_Informant_ECOG_Total_Avg ~ b_Age, data = df_clean_tran)
summary(mod4b) #not sig

##HAMD
mod5b <- lm(b_Informant_ECOG_Total_Avg ~ b_HAMD17Total, data = df_clean_tran)
summary(mod5b) #not sig

##GSIS
mod6b <- lm(b_Informant_ECOG_Total_Avg ~ b_GSIS_Total, data = df_clean_tran)
summary(mod6b) #not significant

##GAD
mod7b <- lm(b_Informant_ECOG_Total_Avg ~ b_GAD7_Total, data = df_clean_tran)
summary(mod7b) #not sig

##DSSI
mod8b <- lm(b_Informant_ECOG_Total_Avg ~ b_DSSI_Total, data = df_clean_tran)
summary(mod8b) #not sig

##PSS
mod9b <- lm(b_Informant_ECOG_Total_Avg ~ b_PSS_Total, data = df_clean_tran)
summary(mod9b) #significant

##CogScore
mod10b <- lm(b_Informant_ECOG_Total_Avg ~ CogScore, data = df_clean_tran)
summary(mod10b) #not significant


#stepwise regression----------------------------------------------------------------------------------
set.seed(101)
models_inf <- regsubsets(b_Informant_ECOG_Total_Avg ~ b_GAD7_Total + b_PSS_Total + 
                             b_GSIS_Total + b_DSSI_Total +b_HAMD17Total + BiRace +
                             EducationYears + b_Age + Gender, data = df_clean_tran, nvmax = 6,
                         method = "seqrep")
summary(models_inf)
# pss --> pss, race --> pss dssi race --> race pss gender dssi --> dssi pss hamd race gender --> 
#pss gsis dssi race edu gender


#multiple regression----------------------------------------------------------------------------------

#w/ pss race
mr1b <- lm(b_Informant_ECOG_Total_Avg ~ b_PSS_Total + BiRace, data = df_clean_tran)
summary(mr1b)
plot(mr1b)

#w/ pss dssi race 
mr2b <- lm(b_Informant_ECOG_Total_Avg ~ b_PSS_Total + b_DSSI_Total + BiRace, data = df_clean_tran)
summary(mr2b)
plot(mr2b)

#w/ race pss gender dssi (BEST)
mr3b <- lm(b_Informant_ECOG_Total_Avg ~ BiRace + b_DSSI_Total + 
               Gender+ b_PSS_Total, data = df_clean_tran)
summary(mr3b)
plot(mr3b)

#w/ dssi pss hamd race gender
mr4b <- lm(b_Informant_ECOG_Total_Avg ~ b_GAD7_Total + b_HAMD17Total + 
               b_GSIS_Total + b_DSSI_Total, data = df_clean_tran)
summary(mr4b)
plot(mr4b) 

#w/ pss gsis dssi race edu gender
mr5b <- lm(b_Informant_ECOG_Total_Avg ~ b_GAD7_Total + b_PSS_Total + BiRace +
               b_HAMD17Total + EducationYears, data = df_clean_tran)
summary(mr5b)
plot(mr5b)


#selected regression model with ecog subdomains-------------------------------------------------------
#use df_full data frame

#memory
sd1b <- lm(b_Informant_ECOG_Memory_Avg ~ b_PSS_Total + b_DSSI_Total + BiRace, data = df_full_tran)
summary(sd1b) #sig
plot(sd1b)

#language
sd2b <- lm(b_Informant_ECOG_Language_Avg ~ b_PSS_Total + b_DSSI_Total + BiRace, data = df_full_tran)
summary(sd2b) #sig
plot(sd2b)

#visuospatial
sd3b <- lm(b_Informant_ECOG_Visuospatial_Avg ~ b_PSS_Total + b_DSSI_Total + BiRace, data = df_full_tran)
summary(sd3b) #not sig
plot(sd3b)

#executive planning
sd4b <- lm(b_Informant_ECOG_ExecPlanning_Avg ~ b_PSS_Total + b_DSSI_Total + BiRace, data = df_full_tran)
summary(sd4b)
plot(sd4b)

#executive organization
sd5b <- lm(b_Informant_ECOG_ExecOrganization_Avg ~ b_PSS_Total + b_DSSI_Total + BiRace, data = df_full_tran)
summary(sd5b) #not sig
plot(sd5b)

#exectutive attention
sd6b <- lm(b_Informant_ECOG_ExecAttention_Avg ~ b_PSS_Total + b_DSSI_Total + BiRace, data = df_full_tran)
summary(sd6b)
plot(sd6b)

#executive total
sd7b <- lm(b_Informant_ECOG_ExecTotal_Avg ~ b_PSS_Total + b_DSSI_Total + BiRace, data = df_full_tran)
summary(sd7b)
plot(sd7b)


#ecog score predicted by cognitive subdomain----------------------------------------------------------
#use df_full data frame

#executive functioning
cs1b <- lm(b_Informant_ECOG_ExecTotal_Avg ~ b_Heaton_FAS_SS_imp2 + b_Stroop_CW_SSImp2 + b_Heaton_TMTB_SS_imp2, data = df_full_tran)
summary(cs1b) #not sig

#visual perception functioning
cs2b <- lm(b_Informant_ECOG_ExecTotal_Avg ~ b_BentonJLO_SSImp2, data = df_full_tran)
summary(cs2b) #not sig

#visual learning & memory
cs3b <- lm(b_Informant_ECOG_ExecTotal_Avg ~ b_BVMT_TotalRecall_SSImp2 + b_BVMT_DelayedRecall_SSImp2, data = df_full_tran)
summary(cs3b) #not sig

#verbal learning & memory
cs4b <- lm(b_Informant_ECOG_ExecTotal_Avg ~ b_LM_Immediate_SSImp2 + b_LM_Delayed_SSImp2 + 
               b_HVLT_TotalRecall_SSImp2 + b_HVLT_DelayedRecall_SSImp2, data = df_full_tran)
summary(cs4b) #not sig

#working memory (model fails as digit span has no impaired scores)
cs5b <- lm(b_Informant_ECOG_ExecTotal_Avg ~ b_DigitSpan_Total_SSImp2, data = df_full_tran)
summary(cs5b) #not sig

#language ability
cs6b <- lm(b_Informant_ECOG_ExecTotal_Avg ~ b_Heaton_BNT_SS_imp2, data = df_full_tran)
summary(cs6b) #not sig

#info processing speed
cs7b <- lm(b_Informant_ECOG_ExecTotal_Avg ~ b_DigitSymbol_SSImp2 + b_Heaton_TMTA_SS_imp2, data = df_full_tran)
summary(cs7b) #not sig
