
if (!require(reshape)) (install.packages(reshape))
if (!require(MASS)) (install.packages(MASS))
if (!require(dplyr)) (install.packages(dplyr))
if (!require(gvlma)) (install.packages(gvlma))

train <- read.csv('Data/moneyball-training-data.csv', header=T)
train$INDEX <- NULL
test <- read.csv('Data/moneyball-evaluation-data.csv', header=T)
test$INDEX <- NULL



cleanNames <- function(df) {
    name_list <- names(df)
    name_list <- gsub("TEAM_", "", name_list)
    names(df) <- name_list
    df
}

train <- cleanNames(train)
test <- cleanNames(test)

# PT1 - DataExploration
# PREP EDA PLOTS
trainplot <- melt(train, "TARGET_WINS")


subByType <- function(var) {
    df <- subset(trainplot, grepl(var, variable))
    df$variable <- gsub(paste0(var, "_"), "", df$variable)
    df
}

#split into batting, pitching, fielding, plot each
batplot <- subByType("BATTING")
pitchplot <- subByType("PITCHING")
fieldplot <- subByType("FIELDING")


# PT2 - Data Transformation
# Function to transform

#train$TARGET_WINS <- sqrt(train$TARGET_WINS)

#review list of outliers
#train <- train[-c(1342, 1584, 2136),]

Transform <- function(df, imputeMethod, scale=F) {
    
    #NA Managment
    #impute <- function(data, type) {
    #    # helper function to impute
    #    # http://www.listendata.com/2015/05/r-function-imputing-missing-values.html
    #    for (i in which(sapply(data, is.numeric))) {
    #        data[is.na(data[, i]), i] <- round(type(data[, i],  na.rm = TRUE),0)
    #    }
    #    return(data)
    #}
    #df <- impute(df, imputeMethod)
    
    #Feature Creation
    df <- df %>%
        mutate(BATTING_1B = BATTING_H - BATTING_HR - BATTING_3B -
                   BATTING_2B,
               BATTING_StealPer = BASERUN_SB / (BASERUN_SB + BASERUN_CS)) %>%
        dplyr::select(-BATTING_H, -BATTING_HBP)
    
    # Transformation
    if (scale) {
        ### needs work
        df <- data.frame(scale(df))
    }

    return(df)
}

train <- Transform(train, median)
test <- Transform(test, mean)

# Specialized cleaning
#train <- train[!train$TARGET_WINS < quantile(train$TARGET_WINS, .02), ] #weird values



# Modeling
m1 <- lm(TARGET_WINS ~ ., train)
m1.summary <- summary(m1)
###Check 
# linearity
# independance
# homoscedacity
# normality of error distribution
gvmodel <- gvlma(m1)
#summary(m1)


#step_for <- stepAIC(m1, direction="forward")
#summary_step_for <- summary(step_for)

#step_back <- stepAIC(m1, direction="backward")
#summary_step_back <- summary(step_back)

#m2 <- summary_step_for$call
#m3 <- summary_step_back$call
# Prediction

#m1_prediction <- predict(m1, test)

# transform predictions back from Log
#predictions <- exp(m1_prediction)
