library(ggplot2)
library(reshape2)
library(MASS)
library(dplyr)
library(DataExplorer)
library(RColorBrewer)
library(gridExtra)

#count plot of ecog impaired vs normal distribution
count_plot <- function(data){
    plot <- ggplot(data, aes(as.factor(ECOG_Imp), y=b_ECOG_Total_Avg, fill = as.factor(INFORMECOG_Imp))) + 
        geom_bar(stat="identity") +
        theme_bw() +
        scale_fill_brewer(palette="Set3") +
        xlab("Patient Self-Rating of Cognitive Impairment (0 = No, 1 = Yes)")
    
    return(plot)
}

#frequency of outcome variables
outcome_var_freq <- function(data){
    ecog_values <- data %>% dplyr::select(ECOG_Imp, INFORMECOG_Imp)
    ecog_table <- table(ecog_values)
    return(ecog_table)
}

#barplot of impaired objective cognition distribution
barplot_oc_imp <- function(data){
   plot2 <- ggplot(data, aes(as.factor(CogScore))) + 
        geom_bar(aes(fill = as.factor(ECOG_Imp))) +
        theme_bw() +
        scale_fill_brewer(palette="Set3")
   
   return(plot2)
}

#heatmap upper triangle base function
get_upper_tri <- function(cormat){
    cormat[lower.tri(cormat)]<- NA
    return(cormat)
}

#correlation heat map of all variables
heatmap_all_vars <- function(data){
    cormat <- round(cor(data[, -c(7:8)], use = "complete.obs"), 2)
    
    upper_tri <- get_upper_tri(cormat)
    melted_cormat <- melt(upper_tri, na.rm = TRUE)
    
    ggheatmap <- ggplot(data = melted_cormat, aes(Var2, Var1, fill = value)) +
        geom_tile(color = "white")+
        scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                             midpoint = 0, limit = c(-1,1), space = "Lab", 
                             name="Pearson\nCorrelation") +
        theme_minimal()+ 
        theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                         size = 12, hjust = 1)) +
        coord_fixed()
    
    plot3 <- ggheatmap + 
        geom_text(aes(Var2, Var1, label = value), color = "black", size = 2) +
        theme(
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            panel.grid.major = element_blank(),
            panel.background = element_blank(),
            axis.ticks = element_blank(),
            legend.justification = c(1, 0),
            legend.position = c(0.6, 0.7),
            legend.direction = "horizontal") +
        guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                                     title.position = "top", title.hjust = 0.5))
    
    return(plot3)
}

#checking for multicollinearity
collinear_check <- function(data){
    descrCor <-  cor(data[, -c(7:8)], use = "complete.obs")
    summary(descrCor[upper.tri(descrCor)])
    highCorr <- sum(abs(descrCor[upper.tri(descrCor)]) > .75)
    return(highCorr)
}

#plotting variable distrubtions
variable_dists <- function(data, y_var){
    p1 <- ggplot(df_epa, aes(b_ECOG_Total_Avg, y=y_var)) + 
        geom_point() +
        geom_smooth(method=lm)
    p2 <- ggplot(df_epa, aes(x=b_Informant_ECOG_Total_Avg, y=y_var)) + 
        geom_point() +
        geom_smooth(method=lm)
    
    return(gridExtra::grid.arrange(p1, p2, ncol=2))
}