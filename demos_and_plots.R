library(dplyr)
library(RColorBrewer)
library(gridExtra)
library(psych)
library(ggplot2)

#loading data of interest------------------------------------------------------------------
df_clean <- read.csv("ADNID_Baseline_MasterFile_clean.csv", stringsAsFactors = FALSE)
df_full <- df_clean %>% dplyr::select(b_ECOG_Total_Avg, b_Informant_ECOG_Total_Avg, b_Age, BiRace, Gender,
                                     EducationYears, ECOG_Imp, INFORMECOG_Imp, CogScore, b_HAMD17Total, 
                                     b_GAD7_Total, b_PSS_Total, b_DSSI_Total)
#participant data
str(df_epa)

#study partner data
df_sp <- df_full[complete.cases(df_full[2]),]
str(df_sp)


#plots for poster------------------------------------------------------------------
#depression severity and participant subjective cognition
ggplot(df_epa, aes(x=b_HAMD17Total, y=b_ECOG_Total_Avg)) +
  geom_point() +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  xlab("Depression Severity (HDRS-17)") +
  ylab("Subjective Cognition (Self-ECog)")

#anxiety and participant subjective cognition
ggplot(df_epa, aes(x=b_GAD7_Total, y=b_ECOG_Total_Avg)) +
  geom_point() +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  xlab("Anxiety (GAD-7)") +
  ylab("Subjective Cognition (Self-ECog)")

#perceived stress and study partner rated subjective cognition
ggplot(df_epa, aes(x=b_PSS_Total, y=b_Informant_ECOG_Total_Avg)) +
  geom_point() +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  xlab("Perceived Stress (PSS)") +
  ylab("Study Partner Rated Cognition (SP-ECog)")

#perceived social support and study partner rated subjective cognition
ggplot(df_epa, aes(x=b_DSSI_Total, y=b_Informant_ECOG_Total_Avg)) +
  geom_point() +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  xlab("Social Support (DSSI)") +
  ylab("Study Partner Rated Cognition (SP-ECog)")


#demographic and clinical characteristics-----------------------------------------------------
##impaired ECog (n=26)
df_imp <- df_full[df_full$ECOG_Imp == 1, ]
str(df_imp)

#age
describe(df_imp$b_Age, na.rm=T) #sd 5.74, mean = 70.19

#female
table(df_imp$Gender) #15 women 

#education
describe(df_imp$EducationYears, na.rm=T) #4.67, mean = 18.85

#white
table(df_imp$BiRace) #23 white 

#hdrs
describe(df_imp$b_HAMD17Total, na.rm=T) #4.34, mean = 21.12

#gad
describe(df_imp$b_GAD7_Total, na.rm=T) #4.59, mean = 12.58

#pss
describe(df_imp$b_PSS_Total, na.rm=T) #3.56, mean = 24.77

#dssi
describe(df_imp$b_DSSI_Total, na.rm=T) #11.6, mean = 36.65


##nonimpaired ECog (n=38)
df_nonimp <- df_full[df_full$ECOG_Imp == 0, ]
str(df_nonimp)

#age
describe(df_nonimp$b_Age, na.rm=T) #sd 4.41, mean = 70.21

#female
table(df_nonimp$Gender) #26 women 

#education
describe(df_nonimp$EducationYears, na.rm=T) #4.73, mean = 18.68

#white
table(df_nonimp$BiRace) #30 white 

#hdrs
describe(df_nonimp$b_HAMD17Total, na.rm=T) #4.12, mean = 19.34

#gad
describe(df_nonimp$b_GAD7_Total, na.rm=T) #3.93, mean = 8.92

#pss
describe(df_nonimp$b_PSS_Total, na.rm=T) #5.59, mean = 21.42

#dssi
describe(df_nonimp$b_DSSI_Total, na.rm=T) #12.61, mean = 37.16