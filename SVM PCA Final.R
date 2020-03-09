# SVM PCA analysis-- Alexei

# Set Directory
setwd("C:/Users/Clifton/Desktop/1. 2016 Assignments/A_OR568/Project")

# Libraries
library(e1071)
# Import CSV
Dat <- read.csv('SVM pcafinal.csv')
# Structure
str(Dat)

# Random NA in Test set.
# From continent 
Dat[is.na(Dat[,"Continent"]),"Continent"] <- "Africa"

# For now, keep weak foot and skill moves as ordinal, numeric type.
# Split Data into training and test set. 60/40 split
# Randomly select 60% for trainning
TrainPercentage <- .6 # Makes the partitioning dynamic
SampleSize <- floor(TrainPercentage*nrow(Dat))
set.seed(12345)
# Train index
TrainIndex <- sample(seq_len(nrow(Dat)), size = SampleSize)
# Split based on index
Train <- Dat[TrainIndex,]
Test <- Dat[-TrainIndex,]


# The samples to get an understanding for which paramaters are best for the final training.
# Small rows, 1000
SmallSize <- sample(seq_len(nrow(Dat)), size = 1000)
Small <- Dat[1:1000,]
Small <- Dat[SmallSize,]

# Second Sample 
SmallSize1 <- sample(seq_len(nrow(Dat)), size = 1000)
Small1 <- Dat[SmallSize1,]

# Third Sample
SmallSize2 <- sample(seq_len(nrow(Dat)), size = 1000)
Small2 <- Dat[SmallSize2,]



# On first tuned run, best parameters were cost = 100 and radial kernel. 
# Will resample from Dat and try again.
Tune <- tune(svm, Wage ~ ., data = Small, type = 'eps-regression', cross = 10, ranges = list(cost=c(0.001, 0.01, 0.1, 1,5,10,100), kernel = c('linear', 'radial', 'polynomial')))
# Second Attempt
Tune2 <- tune(svm, Wage ~ ., data = Small1, type = 'eps-regression', cross = 10, ranges = list(cost=c(0.001, 0.01, 0.1, 1,5,10,100), kernel = c('linear', 'radial', 'polynomial')))
# Third Attempt
Tune1 <- tune(svm, Wage ~ ., data = Small2, type = 'eps-regression', cross = 10, ranges = list(cost=c(0.001, 0.01, 0.1, 1,5,10,100), kernel = c('linear', 'radial', 'polynomial')))

# Tuning sampling three times has created two cost = 100 and two kernel = radial. Therefore use Radial for bigger model.
Support <- svm(Wage ~ ., data = Train, kernel = "radial", cost = 100, scale = FALSE, cross = 10) 

# Full Tuning of Training Data
TuneFull1 <- tune(svm, Wage ~ ., data = Train, type = 'eps-regression', cross = 10, ranges = list(cost = c(.001, .01, .1, 1, 5, 10, 100), kernel = c('linear', 'radial', 'polynomial')))

# Predict Values
PSupport <- predict(Support, Test)
PSupportfull <- predict(TuneFull$best.model, Test)

summary(Tune)

TSupport = Tune$best.model

summary(TSupport)
# Predict the Wage
PSupport <- predict(Support, Test)

# Caluculate MSE manually wiithin Test Set
y <- Test$Wage
yhat <- PSupportfull
MSE <- mean((y - yhat)^2)
RMSE <- sqrt(MSE)
Normrmse <- RMSE/mean(Test$Wage)
RangeNmrse <- RMSE/(420-1)

