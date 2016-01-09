# PRESENTATION LINK : http://rpubs.com/adena/wine

library(caret)
# Load in data
redwine <- read.csv2("~/Documents/Current Courses/PSYCH 486/Wine/winequality-red.csv")
whitewine <- read.csv2("~/Documents/Current Courses/PSYCH 486/Wine/winequality-white.csv")

# Create bar chart to see correlations between quality and other variables,
# separately for red and white wine, just in case type affects quality.
# Note: 'dev.off()' useful to reset graphics device.

##################################
# RED WINE / Preprocessing
##################################

# Change factor values to numbers
redwine$fixed.acidity <- as.numeric(as.character(redwine$fixed.acidity))
redwine$volatile.acidity <- as.numeric(as.character(redwine$volatile.acidity))
redwine$citric.acid <- as.numeric(as.character(redwine$citric.acid))
redwine$residual.sugar <- as.numeric(as.character(redwine$residual.sugar))
redwine$chlorides <- as.numeric(as.character(redwine$chlorides))
redwine$free.sulfur.dioxide <- as.numeric(as.character(redwine$free.sulfur.dioxide))
redwine$total.sulfur.dioxide <- as.numeric(as.character(redwine$total.sulfur.dioxide))
redwine$density <- as.numeric(as.character(redwine$density))
redwine$pH <- as.numeric(as.character(redwine$pH))
redwine$sulphates <- as.numeric(as.character(redwine$sulphates))
redwine$alcohol <- as.numeric(as.character(redwine$alcohol))

# Center, scale, and transform red wine data
preprocess_redwine <- preProcess(redwine[,1:11], method = c("BoxCox", "center", "scale"))
preprocess_redwine
new_redwine <- data.frame(predict(preprocess_redwine, redwine))
summary(new_redwine$trans.quality)
summary(redwine$quality)

# Check for normal distributions
hist(redwine$fixed.acidity)
hist(new_redwine$trans.fixed.acidity)

hist(redwine$volatile.acidity)
hist(new_redwine$trans.volatile.acidity)

hist(redwine$citric.acid)
hist(new_redwine$trans.citric.acid)

hist(redwine$residual.sugar)
hist(new_redwine$trans.residual.sugar)

hist(redwine$chlorides)
hist(new_redwine$trans.chlorides)

hist(redwine$free.sulfur.dioxide)
hist(new_redwine$trans.free.sulfur.dioxide)

hist(redwine$total.sulfur.dioxide)
hist(new_redwine$trans.total.sulfur.dioxide)

hist(redwine$density)
hist(new_redwine$trans.density)

hist(redwine$pH)
hist(new_redwine$trans.pH)

hist(redwine$sulphates)
hist(new_redwine$trans.sulphates)

hist(redwine$alcohol)
hist(new_redwine$trans.alcohol)

# Use boxplots to check for outliers
boxplot(new_redwine$trans.fixed.acidity) 
boxplot(new_redwine$trans.volatile.acidity)
boxplot(new_redwine$trans.citric.acid)
boxplot(new_redwine$trans.residual.sugar)
boxplot(new_redwine$trans.chlorides)
boxplot(new_redwine$trans.free.sulfur.dioxide)
boxplot(new_redwine$trans.total.sulfur.dioxide) # no outliers
boxplot(new_redwine$trans.density)
boxplot(new_redwine$trans.pH)
boxplot(new_redwine$trans.sulphates)
boxplot(new_redwine$trans.alcohol) # no outliers

# Remove outliers by removing entire row of data
new_redwine <- new_redwine[!abs(new_redwine$trans.fixed.acidity) > 3,]
new_redwine <- new_redwine[!abs(new_redwine$trans.volatile.acidity) > 3,]
new_redwine <- new_redwine[!abs(new_redwine$trans.citric.acid) > 3,] 
new_redwine <- new_redwine[!abs(new_redwine$trans.residual.sugar) > 3,] 
new_redwine <- new_redwine[!abs(new_redwine$trans.chlorides) > 3,]        
new_redwine <- new_redwine[!abs(new_redwine$trans.free.sulfur.dioxide) > 3,]                 
new_redwine <- new_redwine[!abs(new_redwine$trans.density) > 3,]       
new_redwine <- new_redwine[!abs(new_redwine$trans.pH) > 3,]       
new_redwine <- new_redwine[!abs(new_redwine$trans.sulphates) > 3,]

# Find correlations between predictors and quality
library(corrplot)
corrplot(cor(new_redwine), type = "lower", method = "number")

# (Top 5) Predictors with highest correlations: 
# volatile acidity, citric acid, total sulfur dioxide, sulphates, alcohol

##################################
# WHITE WINE / Preprocessing
##################################

# Change factor values to numbers
whitewine$fixed.acidity <- as.numeric(as.character(whitewine$fixed.acidity))
whitewine$volatile.acidity <- as.numeric(as.character(whitewine$volatile.acidity))
whitewine$citric.acid <- as.numeric(as.character(whitewine$citric.acid))
whitewine$residual.sugar <- as.numeric(as.character(whitewine$residual.sugar))
whitewine$chlorides <- as.numeric(as.character(whitewine$chlorides))
whitewine$free.sulfur.dioxide <- as.numeric(as.character(whitewine$free.sulfur.dioxide))
whitewine$total.sulfur.dioxide <- as.numeric(as.character(whitewine$total.sulfur.dioxide))
whitewine$density <- as.numeric(as.character(whitewine$density))
whitewine$pH <- as.numeric(as.character(whitewine$pH))
whitewine$sulphates <- as.numeric(as.character(whitewine$sulphates))
whitewine$alcohol <- as.numeric(as.character(whitewine$alcohol))

# Center, scale, and transform red wine data
preprocess_whitewine <- preProcess(whitewine[,1:11], c("BoxCox", "center", "scale"))
preprocess_whitewine
new_whitewine <- data.frame(trans = predict(preprocess_whitewine, whitewine))

# Check for normal distributions
hist(whitewine$fixed.acidity)
hist(new_whitewine$trans.fixed.acidity)

hist(whitewine$volatile.acidity)
hist(new_whitewine$trans.volatile.acidity)

hist(whitewine$citric.acid)
hist(new_whitewine$trans.citric.acid)

hist(whitewine$residual.sugar)
hist(new_whitewine$trans.residual.sugar)

hist(whitewine$chlorides)
hist(new_whitewine$trans.chlorides)

hist(whitewine$free.sulfur.dioxide)
hist(new_whitewine$trans.free.sulfur.dioxide)

hist(whitewine$total.sulfur.dioxide)
hist(new_whitewine$trans.total.sulfur.dioxide)

hist(whitewine$density)
hist(new_whitewine$trans.density)

hist(whitewine$pH)
hist(new_whitewine$trans.pH)

hist(whitewine$sulphates)
hist(new_whitewine$trans.sulphates)

hist(whitewine$alcohol)
hist(new_whitewine$trans.alcohol)

# Use boxplots to check for outliers
boxplot(new_whitewine$trans.fixed.acidity) 
boxplot(new_whitewine$trans.volatile.acidity)
boxplot(new_whitewine$trans.citric.acid)
boxplot(new_whitewine$trans.residual.sugar) # no outliers
boxplot(new_whitewine$trans.chlorides)
boxplot(new_whitewine$trans.free.sulfur.dioxide)
boxplot(new_whitewine$trans.total.sulfur.dioxide)
boxplot(new_whitewine$trans.density)
boxplot(new_whitewine$trans.pH)
boxplot(new_whitewine$trans.sulphates)
boxplot(new_whitewine$trans.alcohol) # no outliers

# Remove outliers by removing entire row of data
new_whitewine <- new_whitewine[!abs(new_whitewine$trans.fixed.acidity) > 3,]
new_whitewine <- new_whitewine[!abs(new_whitewine$trans.volatile.acidity) > 3,]
new_whitewine <- new_whitewine[!abs(new_whitewine$trans.citric.acid) > 3,] 
new_whitewine <- new_whitewine[!abs(new_whitewine$trans.chlorides) > 3,]        
new_whitewine <- new_whitewine[!abs(new_whitewine$trans.free.sulfur.dioxide) > 3,]  
new_whitewine <- new_whitewine[!abs(new_whitewine$trans.total.sulfur.dioxide) > 3,] 
new_whitewine <- new_whitewine[!abs(new_whitewine$trans.density) > 3,]       
new_whitewine <- new_whitewine[!abs(new_whitewine$trans.pH) > 3,]       
new_whitewine <- new_whitewine[!abs(new_whitewine$trans.sulphates) > 3,]

# Find correlations between predictors and quality
corrplot(cor(new_whitewine), type = "lower", method = "number")

# (Top 5) Predictors with highest correlations: 
# volatile.acidity, density, total.sulfur.dioxide, chlorides, alcohol

##################################
# Correlation Summaries 
# Took top 5 correlations with quality 
# Red wine: volatile acidity, citric acid, total sulfur dioxide, sulphates, alcohol
# White wine: volatile.acidity, density, total.sulfur.dioxide, chlorides, alcohol
# These will be my 'variables of interest'
##################################

# Test collinearity with Variance Inflation Factor on variables of interest only
# (Sees how much variance of an est. regression coefficient is increased because of collinearity.
# VIF indicates the factor by which variance of the co-efficient of a variable would 
# increase if it was not highly correlated with other variables.)

library(car)
redCollinear <- lm(quality ~ ., data = redwine)
vif(redCollinear)
corrplot(cor(redwine))
# No VIF > 10 for red wine, all ok

whiteCollinear <- lm(quality ~., data = whitewine)
vif(whiteCollinear)
# For variables of interest, alcohol and density have very high VIFs
corrplot(cor(whitewine), method = "number")
plot(whitewine$alcohol, whitewine$density)
cor(whitewine$alcohol, whitewine$density) 
# Alcohol (compared to Density) seems to have higher average correlations to the
# rest of the variables. Could consider removing alcohol as a predictor for white wine

# REVISED predictors for WHITE WINE: volatile.acidity, density, total.sulfur.dioxide, chlorides

##################################
# DATA SPLITTING
##################################

# Create new data frame with variables of interest for red wine and white wine
redwData <- data.frame(new_redwine$trans.volatile.acidity, 
                   new_redwine$trans.citric.acid,
                   new_redwine$trans.chlorides,
                   new_redwine$trans.total.sulfur.dioxide,
                   new_redwine$trans.alcohol,
                   new_redwine$trans.quality)

whitewData <- data.frame(new_whitewine$trans.volatile.acidity,
                         new_whitewine$trans.chlorides,
                         new_whitewine$trans.total.sulfur.dioxide,
                         new_whitewine$trans.density,
                         new_whitewine$trans.quality)

# Change/condense column names
colnames(redwData) <- c("volatile.acidity", "citric.acid", "chlorides", "total.SO2", "alcohol", "quality")
colnames(whitewData) <- c("volatile.acidity", "chlorides", "total.SO2", "density", "quality")

# 75/25 train/test split

# RED WINE
set.seed(1)
redSplit <- createDataPartition(redwData$quality,
                                p = 0.75,
                                list = FALSE)
redTrain <- redwData[redSplit,]
redTest  <- redwData[-redSplit,]

# WHITE WINE
whiteSplit <- createDataPartition(whitewData$quality, 
                                  p = 0.75,
                                  list = FALSE)
whiteTrain <- whitewData[whiteSplit,]
whiteTest  <- whitewData[-whiteSplit,]

##################################
# KNN / Red Wine
##################################

# Note: All predictors should be centered and scaled prior to performing KNN
# Background: Decided to tune using values of k from 1-30. For now, I will train/test
# red and white wine separately, and thus build separate models. All predictors will
# be centered, scaled, and transformed for building ALL models.

# Set resampling parameters
fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 10)

# Train red wine KNN model
set.seed(2)
redKnn1 <- train(quality ~.,
                data = redTrain,
                method = "knn",
                tuneGrid = data.frame(.k = 1:30),
                trControl = fitControl)
redKnn1$finalModel # k = 16
redKnn1

# Plot k vs. RMSE graph for red wine
plot(x = redKnn1$results$k, 
     y = redKnn1$results$RMSE, 
     ylab = "RMSE (Cross-validation)", 
     xlab = "Number of Neighbors (k)",
     main = "Red Wine")

# Test KNN model on red wine test data
library(class)
redw_knnPredictions1 <- predict(redKnn1, newdata = redTest)
r2_redw_knn1 <- R2(redw_knnPredictions1, redTest$quality)
rmse_redw_knn1 <- RMSE(redw_knnPredictions1, redTest$quality)

##################################
# KNN / White Wine
##################################

# Train white wine KNN model
set.seed(3)
whiteKnn1 <- train(quality ~.,
                   data = whiteTrain,
                method = "knn",
                tuneGrid = data.frame(.k = 1:30),
                trControl = fitControl)
whiteKnn1$finalModel # k = 21

# Plot k vs. RMSE graph for white wine
plot(x = whiteKnn1$results$k, 
     y = whiteKnn1$results$RMSE, 
     ylab = "RMSE (Cross-validation)", 
     xlab = "Number of Neighbors (k)", 
     main = "White Wine")

# Test KNN model on white wine test data
whitew_knnPredictions1 <- predict(whiteKnn1, newdata = whiteTest)
r2_whitew_knn1 <- R2(whitew_knnPredictions1, whiteTest$quality)
rmse_whitew_knn1 <- RMSE(whitew_knnPredictions1, whiteTest$quality)

##################################
# NEURAL NETWORKS / Red Wine
##################################

# Background: Will tune using number of hidden units from 1-5, and decay 
# values of 0, 0.001, 0.01, and 0.1. 

# Train red wine neural networks model
set.seed(4)
redNN_1 <- train(quality ~.,
                 data = redTrain,
                          method = "nnet",
                          linout = TRUE,
                          maxit = 100,
                          tuneGrid = expand.grid(.size=c(1:5),.decay=c(0,0.001,0.01,0.1)),
                          trControl = fitControl)
redNN_1$bestTune
redNN_1$results

# Plot RMSE ~ size & decay for red wine
plot(x = redNN_1$results$size, 
     y = redNN_1$results$RMSE, 
     xlab = "# Hidden Units", 
     ylab = "RMSE")
plot(x = as.factor(redNN_1$results$decay), 
     y=redNN_1$results$RMSE, 
     xlab = "Decay", 
     ylab = "RMSE")
# Trend: Increasing '# Hidden Units' increases variance of RMSE, while increasing decay decreases variance of RMSE

# Test model
redw_NNPredictions1 <- predict(redNN_1, newdata = redTest)
r2_redw_nn1 <- R2(redw_NNPredictions1, redTest$quality)
rmse_redw_nn1 <- RMSE(redw_NNPredictions1, redTest$quality)

##################################
# NEURAL NETWORKS / White Wine
##################################

set.seed(5)
whiteNN_1 <- train(quality ~.,
                            data = whiteTrain,
                          method = "nnet",
                          linout = TRUE,
                          maxit = 100,
                          tuneGrid = expand.grid(.size=c(1:5),.decay=c(0,0.001,0.01,0.1)),
                          trControl = fitControl)
whiteNN_1$bestTune

# Plot RMSE ~ size & decay for red wine
plot(x = whiteNN_1$results$size, 
     y = whiteNN_1$results$RMSE, 
     xlab = "# Hidden Units", 
     ylab = "RMSE")
plot(x = as.factor(whiteNN_1$results$decay), 
     y = whiteNN_1$results$RMSE, 
     xlab = "Decay", 
     ylab = "RMSE")
# Trend: Increasing '# Hidden Units' decreases variance/value of RMSE, 
# while decay doesn't seem to affect RMSE - steady at RMSE ~ 0.78

# Test model
whitew_NNPredictions1 <- predict(whiteNN_1, newdata = whiteTest)
r2_whitew_nn1 <- R2(whitew_NNPredictions1, whiteTest$quality)
rmse_whitew_nn1 <- RMSE(whitew_NNPredictions1, whiteTest$quality)

##################################
# SVM / Red Wine
##################################

# Background: Will tune using default grid search of 14 cost values.
# Sigma(σ) will be estimated analytically by default.

# Train
set.seed(7)
redSVM_1 <- train(quality ~.,
                  data = redTrain,
                method = "svmRadial", 
                tuneLength = 5,
                trControl = fitControl)
redSVM_1$finalModel
redSVM_1$results

# Plot cost values vs. RMSE
plot(x = redSVM_1$results$C, 
     y = redSVM_1$results$RMSE, 
     xlab = "Cost", 
     ylab = "RMSE")
# Trend: RMSE increases with increasing cost

# Test
redw_SVMPredictions1 <- predict(redSVM_1, newdata = redTest)
r2_redw_svm1 <- R2(redw_SVMPredictions1, redTest$quality)
rmse_redw_svm1 <- RMSE(redw_SVMPredictions1, redTest$quality)

##################################
# SVM / White Wine
##################################

# Train
set.seed(8)
whiteSVM_1 <- train(quality ~.,
                    data = whiteTrain,
                method = "svmRadial", 
                tuneLength = 5,
                trControl = fitControl)
whiteSVM_1$finalModel

# Plot cost values vs. RMSE
plot(x = whiteSVM_1$results$C, 
     y = whiteSVM_1$results$RMSE, 
     xlab = "Cost", 
     ylab = "RMSE")
# Trend: RMSE starts increases with increasing cost, but also is very
# high at very low costs. 

# Test
whitew_SVMPredictions1 <- predict(whiteSVM_1, newdata = whiteTest)
r2_whitew_svm1 <- R2(whitew_SVMPredictions1, whiteTest$quality)
rmse_whitew_svm1 <- RMSE(whitew_SVMPredictions1, whiteTest$quality)

##################################
# Summary:
# Models have performed ok so far, based on predictors that I have chosen.
# Want to test if models built with feature selection would perform better.
# NEXT, will build all 3 models again using feature selection to choose predictors.
##################################

##################################
# FEATURE SELECTION
##################################

# Background: Will use K-CV using linear regression
# Will choose features based on RMSE metric (other 3 metrics give same result)

# RED WINE
ctrl <- rfeControl(method = "repeatedcv",
                   repeats = 5,
                   verbose = TRUE,
                   functions = lmFuncs)
set.seed(9)
redwineRFE <- rfe(x = new_redwine[,1:11],
                  y = new_redwine[,12],
                  sizes = c(1:11),
                  metric = "RMSE",
                  rfeControl = ctrl)
print(redwineRFE)
# The top 5 variables (out of 11):
# alcohol, volatile.acidity, sulphates, total.sulfur.dioxide, density

# See variable performance visually
plot(redwineRFE, type=c("g", "o"))

# WHITE WINE
set.seed(10)
whitewineRFE <- rfe(x = new_whitewine[,1:11],
                  y = new_whitewine[,12],
                  sizes = c(1:11),
                  metric = "RMSE",
                  rfeControl = ctrl)
print(whitewineRFE)
# The top 5 variables (out of 11):
# volatile.acidity, residual.sugar, density, alcohol, free.sulfur.dioxide

# See variable performance visually
plot(whitewineRFE, type=c("g", "o"))

# Create new dataframes for red and white wine
redw_fs <- data.frame(new_redwine$trans.volatile.acidity,
                       new_redwine$trans.total.sulfur.dioxide,
                      new_redwine$trans.density,
                      new_redwine$trans.sulphates,
                       new_redwine$trans.alcohol,
                       new_redwine$trans.quality)

whitew_fs <- data.frame(new_whitewine$trans.volatile.acidity,
                        new_whitewine$trans.residual.sugar,
                        new_whitewine$trans.free.sulfur.dioxide,
                         new_whitewine$trans.density,
                        new_whitewine$trans.alcohol,
                         new_whitewine$trans.quality)

# Change/condense column names
colnames(redw_fs) <- c("volatile.acidity", "total.SO2", "density", "sulphates", "alcohol", "quality")
colnames(whitew_fs) <- c("volatile.acidity", "residual.sugar", "free.SO2", "density", "alcohol", "quality")

# 75/25 train/test split

# RED WINE
set.seed(172)
redSplit <- createDataPartition(redw_fs$quality,
                                p = 0.75,
                                list = FALSE)
redTrain <- redw_fs[redSplit,]
redTest  <- redw_fs[-redSplit,]

# WHITE WINE
whiteSplit <- createDataPartition(whitew_fs$quality, 
                                  p = 0.75,
                                  list = FALSE)
whiteTrain <- whitew_fs[whiteSplit,]
whiteTest  <- whitew_fs[-whiteSplit,]

##################################
# NEURAL NETWORKS v2 / Red Wine
##################################

# Train model using revised predictors, all other parameters consistent
set.seed(11)
new_redwine_nn <- train(quality ~.,
                        data = redTrain,
                        method = "nnet",
                        linout = TRUE,
                        maxit = 100,
                        tuneGrid = expand.grid(.size=c(1:5),.decay=c(0,0.001,0.01,0.1)),
                        trControl = fitControl)

# Plot RMSE ~ size & decay for red wine
plot(x = new_redwine_nn$results$size, 
     y = new_redwine_nn$results$RMSE, 
     xlab = "# Hidden Units", 
     ylab = "RMSE")
plot(x = as.factor(new_redwine_nn$results$decay), 
     y = new_redwine_nn$results$RMSE, 
     xlab = "Decay", 
     ylab = "RMSE")
# Trend: Lower decay -> higher RMSE (& variance)
# Higher hidden units related to higher RMSE

# Test
redwine_nn_pred <- predict(new_redwine_nn, newdata = redTest)
r2_redw_nn2 <- R2(redwine_nn_pred, redTest$quality)
rmse_redw_nn2 <- RMSE(redwine_nn_pred, redTest$quality)

##################################
# NEURAL NETWORKS v2 / White Wine
##################################

# Train
set.seed(12)
new_whitewine_nn <- train(quality ~.,
                          data = whiteTrain,
                        method = "nnet",
                        linout = TRUE,
                        maxit = 100,
                        tuneGrid = expand.grid(.size=c(1:5),.decay=c(0,0.001,0.01,0.1)),
                        trControl = fitControl)

# Plot RMSE ~ size & decay for red wine
plot(x = new_whitewine_nn$results$size, 
     y = new_whitewine_nn$results$RMSE, 
     xlab = "# Hidden Units", 
     ylab = "RMSE")
plot(x = as.factor(new_whitewine_nn$results$decay), 
     y = new_whitewine_nn$results$RMSE, 
     xlab = "Decay", 
     ylab = "RMSE")
# Trend: Higher hidden units related to lower RMSE
# Decay doesn't seem to have an effect on RMSE (constant ~ 0.745)

# Test
wwine_nn_pred <- predict(new_whitewine_nn, newdata = whiteTest)
r2_whitew_nn2 <- R2(wwine_nn_pred, whiteTest$quality)
rmse_whitew_nn2 <- RMSE(wwine_nn_pred, whiteTest$quality)

##################################
# KNN v2 / Red Wine
##################################

# Train model
set.seed(15)
redKnn_2 <- train(quality ~.,
                  data = redTrain,
                method = "knn",
                tuneGrid = data.frame(.k = 1:30),
                trControl = fitControl)
redKnn_2$finalModel # k = 17

# Plot k vs. RMSE graph for red wine
plot(x = redKnn_2$results$k, 
     y = redKnn_2$results$RMSE, 
     xlab = "Number of Neighbors (k)", 
     ylab = "RMSE (Cross-validation)",
     main = "Red Wine")
# Trend: RMSE seems to decrease with increasing k

# Test KNN model on red wine test data
redw_knnPredictions2 <- predict(redKnn_2, newdata = redTest)
r2_redw_knn2 <- R2(redw_knnPredictions2, redTest$quality)
rmse_redw_knn2 <- RMSE(redw_knnPredictions2, redTest$quality)

##################################
# KNN v2 / White Wine
##################################

# Train white wine KNN model
set.seed(16)
whiteKnn_2 <- train(quality ~.,
                    data = whiteTest,
                  method = "knn",
                  tuneGrid = data.frame(.k = 1:30),
                  trControl = fitControl)
whiteKnn_2$finalModel # k = 30

# Plot k vs. RMSE graph for white wine
plot(x = whiteKnn_2$results$k, 
     y = whiteKnn_2$results$RMSE, 
     xlab = "Number of Neighbors (k)", 
     ylab = "RMSE (Cross-validation)", 
     main = "White Wine")
# Trend: RMSE seems to decrease with increasing k, but then starts to increase

# Test KNN model on white wine test data
whitew_knnPredictions2 <- predict(whiteKnn_2, newdata = whiteTest)
r2_whitew_knn2 <- R2(whitew_knnPredictions2, whiteTest$quality)
rmse_whitew_knn2 <- RMSE(whitew_knnPredictions2, whiteTest$quality)

##################################
# SVM v2 / Red Wine
##################################

# Background: Will tune using default grid search of 14 cost values.
# Sigma(σ) will be estimated analytically by default.

# Train
set.seed(17)
redSVM_2 <- train(quality ~.,
                  data = redTrain,
                method = "svmRadial",
                tuneLength = 5,
                trControl = fitControl)
redSVM_2$finalModel
redSVM_2$results

# Plot cost values vs. RMSE
plot(x = redSVM_2$results$C, 
     y = redSVM_2$results$RMSE, 
     xlab = "Cost", 
     ylab = "RMSE")
# Trend: RMSE increases with increasing cost

# Test
redw_SVMPredictions2 <- predict(redSVM_2, newdata = redTest)
r2_redw_svm2 <- R2(redw_SVMPredictions2, redTest$quality)
rmse_redw_svm2 <- RMSE(redw_SVMPredictions2, redTest$quality)

##################################
# SVM v2 / White Wine
##################################

# Train 
set.seed(58)
whiteSVM_2 <- train(quality ~.,
                  data = whiteTrain,
                  method = "svmRadial",
                  tuneLength = 5,
                  trControl = fitControl)
whiteSVM_2$finalModel

# Plot cost values vs. RMSE
plot(x = whiteSVM_2$results$C, 
     y = whiteSVM_2$results$RMSE, 
     xlab = "Cost", 
     ylab = "RMSE")
# Trend: RMSE increases with increasing (beyond 3) and decreasing cost

# Test
whitew_SVMPredictions2 <- predict(whiteSVM_2, newdata = whiteTest)
r2_whitew_svm2 <- R2(whitew_SVMPredictions2, whiteTest$quality)
rmse_whitew_svm2 <- RMSE(whitew_SVMPredictions2, whiteTest$quality)

##################################
# GRAPHICAL PERFORMANCE SUMMARY
##################################

R2graph_redwine_1 <- c(r2_redw_knn1, r2_redw_nn1, r2_redw_svm1)
R2graph_redwine_2 <- c(r2_redw_knn2, r2_redw_nn2, r2_redw_svm2)
R2graph_whitewine_1 <- c(r2_whitew_knn1, r2_whitew_nn1, r2_whitew_svm1)
R2graph_whitewine_2 <- c(r2_whitew_knn2, r2_whitew_nn2, r2_whitew_svm2)

RMSEgraph_redwine_1 <- c(rmse_redw_knn1, rmse_redw_nn1, rmse_redw_svm1)
RMSEgraph_redwine_2 <- c(rmse_redw_knn2, rmse_redw_nn2, rmse_redw_svm2)
RMSEgraph_whitewine_1 <- c(rmse_whitew_knn1, rmse_whitew_nn1, rmse_whitew_svm1)
RMSEgraph_whitewine_2 <- c(rmse_whitew_knn2, rmse_whitew_nn2, rmse_whitew_svm2)

plot(R2graph_redwine_1, type="o", col= 1, xlim=c(0,3.5),ylim=c(0,0.8), ylab="R^2", xaxt = "n", xlab="Model Type", main = "Accuracy Measurement - R Squared")
axis(1, at = 1:3, labels=c("KNN","NN","SVM"))
lines(R2graph_redwine_2,type="o", col=2)
lines(R2graph_whitewine_1, type="o", col=3)
lines(R2graph_whitewine_2, type="o", col=4)
legend('topright', c("Red Wine - Manual","Red Wine - Automatic","White Wine - Manual", "White Wine - Automatic"), col = 1:4, lty=1)

plot(RMSEgraph_redwine_1, type="o", col= 1, xlim=c(0,3.5),ylim=c(0,0.8), ylab="RMSE", xaxt = "n", xlab="Model Type", main = "Accuracy Measurement - RMSE")
axis(1, at = 1:3, labels=c("KNN","NN","SVM"))
lines(RMSEgraph_redwine_2,type="o", col=2)
lines(RMSEgraph_whitewine_1, type="o", col=3)
lines(RMSEgraph_whitewine_2, type="o", col=4)
legend('bottomright', c("Red Wine - Manual","Red Wine - Automatic","White Wine - Manual", "White Wine - Automatic"), col = 1:4, lty=1)