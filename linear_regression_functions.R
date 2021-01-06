library(dplyr)
library(caret)

#center and scaling continuous variables
center_scale <- function(x) {
    scale(x, scale = TRUE, center = TRUE)
}

#transforming clean df variables
transform_clean_df <- function(dataframe){
    dataframe$BiRace <- as.factor(dataframe$BiRace)
    dataframe$Gender <- as.factor(dataframe$Gender)
    
    dataframe[, c(1:2, 10:14)] <- center_scale(dataframe[, c(1:2, 10:14)])
    
    preProc <- preProcess(dataframe[, c(1:2, 10, 13)], method = "YeoJohnson")
    dataframe[, c(1:2, 10, 13)] <- predict(preProc, dataframe[, c(1:2, 10, 13)])
    return(dataframe)

}

#transforming full df variables
transform_full_df <- function(dataframe){
    dataframe$BiRace <- as.factor(dataframe$BiRace)
    dataframe$Gender <- as.factor(dataframe$Gender)
    
    dataframe[, 1:5] <- center_scale(dataframe[, 1:5])
    preProc <- preProcess(dataframe[, c(1, 4, 20:35)], method = "YeoJohnson")
    dataframe[, c(1, 4, 20:35)] <- predict(preProc, dataframe[, c(1, 4, 20:35)])
    
    return(dataframe)
}
