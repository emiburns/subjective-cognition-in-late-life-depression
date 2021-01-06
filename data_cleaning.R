#load libraries
library(dplyr)
library(VIM)
library(finalfit)
library(caret)

#load baseline data
df_raw <- read.csv(file = 'ADNID_Baseline_MasterFile_EHB_20200713.csv', stringsAsFactors=FALSE)

#filter by enrollment status & study partner and select variables of interest
df_clean <- df_raw  %>% filter(Status == "Enrolled" & Study_Partner == "Yes") %>% 
    dplyr::select(b_HAMD17Total, b_GAD7_Total, b_PSS_Total, b_DSSI_Total, b_GSIS_Total, b_Heaton_FAS_SS_imp2,
                  b_BVMT_TotalRecall_SSImp2, b_BVMT_DelayedRecall_SSImp2, b_DigitSpan_Total_SSImp2, b_LM_Immediate_SSImp2,
                  b_HVLT_TotalRecall_SSImp2, b_HVLT_DelayedRecall_SSImp2, b_Stroop_CW_SSImp2, b_DigitSymbol_SSImp2, 
                  b_BentonJLO_SSImp2, b_LM_Delayed_SSImp2, b_Heaton_TMTA_SS_imp2, b_Heaton_BNT_SS_imp2, b_Heaton_TMTB_SS_imp2,
                  b_ECOG_Total_Avg, b_Informant_ECOG_Total_Avg, b_ECOG_Memory_Avg, b_ECOG_Language_Avg, b_ECOG_Visuospatial_Avg, 
                  b_ECOG_ExecPlanning_Avg, b_ECOG_ExecOrganization_Avg, b_ECOG_ExecAttention_Avg, b_ECOG_ExecTotal_Avg,
                  b_Informant_ECOG_Memory_Avg, b_Informant_ECOG_Language_Avg, b_Informant_ECOG_Visuospatial_Avg, 
                  b_Informant_ECOG_ExecPlanning_Avg, b_Informant_ECOG_ExecOrganization_Avg, b_Informant_ECOG_ExecAttention_Avg, 
                  b_Informant_ECOG_ExecTotal_Avg, b_Age, Race, Gender, EducationYears)

head(df_clean, 5)
tail(df_clean, 5)
str(df_clean)

#clean dummy coded variables
df_clean$Gender[df_clean$Gender == 'Male'] <- 1
df_clean$Gender[df_clean$Gender == 'Female'] <- 2

df_clean$Race[df_clean$Race == 'White'] <- 5
df_clean$Race[df_clean$Race == 'Asian'] <- 2
df_clean$Race[df_clean$Race == 'More than one race'] <- 6 
df_clean$Race[df_clean$Race == 'Black or African American'] <- 4

df_clean[df_clean == "Normal"] <- 0
df_clean[df_clean == "Impaired <7"] <- 1 

# converting all variables to integer
df_clean$b_Heaton_FAS_SS_imp2 <- as.integer(df_clean$b_Heaton_FAS_SS_imp2)
df_clean$b_ECOG_Total_Avg <- as.numeric(df_clean$b_ECOG_Total_Avg)
df_clean$b_Informant_ECOG_Total_Avg <- as.numeric(df_clean$b_Informant_ECOG_Total_Avg)
df_clean$b_BVMT_TotalRecall_SSImp2 <- as.integer(df_clean$b_BVMT_TotalRecall_SSImp2)
df_clean$b_BVMT_DelayedRecall_SSImp2 <- as.integer(df_clean$b_BVMT_DelayedRecall_SSImp2)
df_clean$b_DigitSpan_Total_SSImp2 <- as.integer(df_clean$b_DigitSpan_Total_SSImp2)
df_clean$b_LM_Immediate_SSImp2 <- as.integer(df_clean$b_LM_Immediate_SSImp2)
df_clean$b_HVLT_TotalRecall_SSImp2 <- as.integer(df_clean$b_HVLT_TotalRecall_SSImp2)
df_clean$b_HVLT_DelayedRecall_SSImp2 <- as.integer(df_clean$b_HVLT_DelayedRecall_SSImp2)
df_clean$b_Stroop_CW_SSImp2 <- as.integer(df_clean$b_Stroop_CW_SSImp2)
df_clean$b_DigitSymbol_SSImp2 <- as.integer(df_clean$b_DigitSymbol_SSImp2)
df_clean$b_BentonJLO_SSImp2 <- as.integer(df_clean$b_BentonJLO_SSImp2)
df_clean$b_LM_Delayed_SSImp2 <- as.integer(df_clean$b_LM_Delayed_SSImp2)
df_clean$b_Heaton_TMTA_SS_imp2 <- as.integer(df_clean$b_Heaton_TMTA_SS_imp2)
df_clean$b_Heaton_BNT_SS_imp2 <- as.integer(df_clean$b_Heaton_BNT_SS_imp2)
df_clean$b_Heaton_TMTB_SS_imp2 <- as.integer(df_clean$b_Heaton_TMTB_SS_imp2)
df_clean$Race <- as.integer(df_clean$Race)
df_clean$Gender <- as.integer(df_clean$Gender)

# initial look at NA values per case 
sum(!complete.cases(df_clean))
missing_pattern(df_clean)

# computing percent of values missing per variable
df_nas <- function(x){sum(is.na(x))/length(x)*100}
apply(df_clean, 2, df_nas)

sum(is.na(df_clean$b_Informant_ECOG_Total_Avg)) #17 (48 total)
sum(is.na(df_clean$b_ECOG_Total_Avg)) #1

#getting rid of participant w/ NA values for both ECog versions (total sample n=64)
df_clean <- df_clean[-45, ]

# creating composite cognition score per participant
df_clean$CogScore <- rowSums(df_clean[, 6:19], na.rm = T)

# feature engineering race variable
df_clean$BiRace <- with(df_clean, ifelse(df_clean$Race == 5, 0, 1))

# adding classification variables based on ecog impaired cutoff score 1.81
df_clean$ECOG_Imp <- ifelse(df_clean$b_ECOG_Total_Avg >=1.81, "1", "0")
df_clean$INFORMECOG_Imp <- ifelse(df_clean$b_Informant_ECOG_Total_Avg >=1.81, "1", "0")

#checking for near-zero-variance variables
nzv <- nearZeroVar(df_clean, saveMetrics= TRUE)
nzv[nzv$nzv,] #no zero-variance predictors returned

# writing final data frame to csv
write.csv(df_clean, file = './data/ADNID_Baseline_MasterFile_clean.csv')
