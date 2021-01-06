library(dplyr)
library(MASS)

#load data--------------------------------------------------------------------------------------------------------
df_clean <- read.csv("ADNID_Baseline_MasterFile_clean.csv", stringsAsFactors = FALSE)

#pulling predictor and outcome variables
df_epa <- df_clean %>% dplyr::select(b_ECOG_Total_Avg, b_Informant_ECOG_Total_Avg, b_Age, BiRace, Gender, EducationYears,
                                     ECOG_Imp, INFORMECOG_Imp, CogScore, b_HAMD17Total, b_GAD7_Total, b_PSS_Total, 
                                     b_DSSI_Total, b_GSIS_Total, ECOG_Imp, INFORMECOG_Imp)


head(df_epa)
tail(df_epa)
str(df_epa)


#chi-squared tests------------------------------------------------------------------------------------------------

#between ecog and objective cognition
df_chi <- df_epa %>% dplyr::select(ECOG_Imp,INFORMECOG_Imp, CogScore)
df_chi$OC <- ifelse(df_chi$CogScore == 0, 0, 1)

chisq.test(table(df_chi$ECOG_Imp, df_chi$OC)) #not significant (do not reject the null hypothesis that OC is independent of SC)
table(df_chi$ECOG_Imp, df_chi$OC) #closer look at confusion type matrix- high rates of misclassification

chisq.test(table(df_chi$INFORMECOG_Imp, df_chi$OC)) #not significant
table(df_chi$INFORMECOG_Imp, df_chi$OC) #closer look at confusion type matrix- higher rates of misclassification


#ANOVAs------------------------------------------------------------------------------------------------

df_anova <- df_clean %>% dplyr::select(ECOG_Imp, INFORMECOG_Imp, b_Age, EducationYears, CogScore, Race, 
                                       b_HAMD17Total, b_GAD7_Total, b_PSS_Total, b_DSSI_Total, b_GSIS_Total)

df_anova$INFORMECOG_Imp <- as.factor(df_anova$INFORMECOG_Imp)
df_anova$INFORMECOG_Imp <- revalue(df_anova$INFORMECOG_Imp, c("0" = "Normal", "1" = "Impaired"))
df_anova$ECOG_Imp <- revalue(df_anova$ECOG_Imp, c("0" = "Normal", "1" = "Impaired"))

#comparing profiles of participants self-rating ECog as impaired vs non-impaired  
part_aov <- manova(cbind(b_HAMD17Total, b_GAD7_Total, b_PSS_Total, b_DSSI_Total, CogScore, 
                         b_GSIS_Total) ~ ECOG_Imp, data = df_anova)
summary.aov(part_aov) #only gad and pss significant different according to group

# comparing means of impaired vs normal
aggregate(. ~ ECOG_Imp, df_anova[, -2], mean) #impaired group has sig higher gad and pss


#comparing participant profiles of informants impaired vs non-impaired  
inf_aov <- manova(cbind(b_HAMD17Total, b_GAD7_Total, b_PSS_Total, b_DSSI_Total, CogScore, 
                        b_GSIS_Total) ~ INFORMECOG_Imp, data = df_anova)
summary.aov(inf_aov) #non significant differences according to group

# comparing means of impaired vs normal
aggregate(. ~ INFORMECOG_Imp, df_anova[, -1], mean) 
#nearly equivalent cogscore & gad. Impaired have lower hamd and higher pss, dssi and gsis
#potentially speaks to high variability / small sample size
