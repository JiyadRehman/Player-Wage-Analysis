#----FIFA- updated file---
#libraries
library(caret)
library(dplyr)
library(corrplot)
#Reading in data
data = read.csv("~/Downloads/fifa_nation.txt")
dim(data) #[1] 15926 row,    73 columns
#View(data)
data$Release.Clause=NULL
summary(data)
dim(data)
colnames(data)

##---- Pairwise correlation matrix and plot of numeric variables ----####
##  Selecting only numeric variables 
numeric_data= select(data, Wage, Age, Overall, Potential,International.Reputation, Weak.Foot, 
                      Skill.Moves, Height, Weight,LS, ST, RS, LW, LF,
                      CF, RF, RW, LAM, CAM, RAM, LM, LCM, CM, RCM, RM, LWB, LDM, CDM, RDM, RWB, 
                      LB, LCB, CB, RCB, RB, Crossing, Finishing, 
                      HeadingAccuracy, ShortPassing,Volleys, Dribbling, Curve,
                      FKAccuracy, LongPassing, BallControl, Acceleration, SprintSpeed,
                      Agility, Reactions, Balance, ShotPower, Jumping, Stamina, Strength,
                      LongShots, Aggression, Interceptions, Positioning, Vision,
                      Penalties, Composure, Marking, StandingTackle, SlidingTackle, Duration)

# Checking correlation of numeric variables only

corcheck= cor(numeric_data)

corcheck #correlation coefficients

# Plotting Correlation Plot

par(mfrow=c(1,1))
corrplot(corcheck)

# Using findCorrelation() to get a vector of column numbers in data.frame should be removed, because they are highly correlated

highCorr= findCorrelation(corcheck, cutoff = 0.8)

highCorr 
# Returns following vector:
# 22 23 24 18 19 20 21 25 13 17 14 15 16 10 11 12 45 39  3 58 37 26 30 51 27 28 29 31 35 32 33 34 63 57 62 46

colnames(numeric_data) #to identify the column names that are to be removed
#delete following: LCM, CM, RCM, LAM, CAM, RAM, LM, RM, LW, CF, RF, LS, ST, RS, BallControl, ShortPassing, Overall, Positioning, Finishing, LWB, RWB, ShotPower, LDM, CDM, RDM, LB, RB, LCB, CB, RCB, StandingTackle, Interceptions, Marking, Acceleration



####---Creating new txt file with selected categorical & numerical variables---####
new_df= subset(data, select = -c(LCM, CM, RCM, LAM, CAM, RAM, LM, RM, LW, CF, RF, LS, ST,
                               RS,BallControl, ShortPassing, Overall, Positioning, Finishing, LWB, RWB,
                               ShotPower, LDM, CDM, RDM, LB, RB, LCB, CB, RCB, StandingTackle,
                               Interceptions, Marking, Acceleration, Preferred.Foot, Nationality, Club))

dim(new_df)

###--Exporting data to Excel File
# install.packages("xlsx")
# library(xlsx)
# # Writing to text file
# write.table(new_df, "fifa5.txt",sep = ",")



####--- EDA & Transformation---####

#Histogram, boxplot, density plots, nearZeroValues, Skewness & transformation 

library(caret) #get BoxCox
library(AppliedPredictiveModeling)
library(e1071)
rm(list = ls())

data= read.csv("~/Desktop/fifa5.txt") #reading in updated dataset 36 variables (including Wage) & 15926 rows
#column# 1:6, 10:33 & 35 are numeric. 
#columns 7:9, 34 are categorical
#release.clause has nulls so dropping it
dim(data)
data$Release.Clause=NULL
#Histograms for all numeric predictors

par(mfrow=c(2,4))
for (i in c(2:6, 10:33,34)) {
  hist(data[,i], xlab = names(data[i]), main= paste(names(data[i]), "Histogram"),
       col="pink")
}

# #For presentation slides and report
# par(mfrow=c(1,1))
# par(mfrow=c(3,1))
# for(i in c (2,28,31)){
#   hist(data[,i], xlab = names(data[i]), main= paste(names(data[i]), "Histogram"),
#                                      col="pink")
#   }

# Boxplots of all numeric predictors

par(mfrow=c(3,2))
for (i in c(2:6, 10:33, 34)) { 
  boxplot(data[,i], ylab= names(data[i]),
          horizontal = T, main= paste(names(data[i]),"outliers with boxplot"),
          col="light blue")
  }

#nearZeroVariance ALL predictors

nearZeroVar(data, names=TRUE, saveMetrics = T) #none of the predictors are sparse. i.e. all nzv are FALSE

#Density plots for numeric predictors
par(mfrow= c (2,4))
for (i in c(2:6, 10:33, 34)){
  d= density(data[,i], na.rm = TRUE)
  plot(d, main=paste(names(data[i]), "Density-Distribution"))
  polygon(d, col = "grey")
}

#Skewness before transformation
for (i  in c(2:6, 10:33, 34)) {
  print(paste(names(data[i]), "Skewness is", skewness(data[,i])))
}

#Centering, Scaling & Transforming
#head(data)
trans= preProcess(data[c(2:6, 10:33, 35)], method = c("center", "scale", "BoxCox"))
transformed= predict(trans,data)
par(mfrow=c(2,4))
for (i in c(2:6, 10:33, 34)) {
  hist(transformed[,i], xlab = names(transformed[i]), main= paste(names(transformed[i]), "Histogram"),
       col="yellow")
}

# Post normalization & transformation density plots
par(mfrow= c (2,3))
for (i  in c(2:6, 10:33, 34)) {
  d2 = density(transformed[,i], na.rm = TRUE)
  plot(d2, main=paste(names(transformed[i]), "Density Post-BoxCox"))
  polygon(d2,col = "coral")
}
# #for presentation & report
# par(mfrow=c(1,1))
# par(mfrow=c(2,3))
# for(i in c (2,3,28, 31:32)){
#   d2 = density(transformed[,i], na.rm = TRUE)
#   plot(d2, main=paste(names(transformed[i]), "Density Post-BoxCox"))
#   polygon(d2,col = "coral")
#   
# }

#Post Transformation Skewness
for (i  in c(2:6, 10:33, 34)) {
  print(paste(names(transformed[i]), "Skewness is", skewness(transformed[,i])))
}


# #for presentation & report 
# par(mfrow=c(3,1))
# for(i in c (2,28,31)){
#   d3 = density(transformed[,i], na.rm = TRUE)
#   plot(d3, main=paste(names(transformed[i]), "Density Post-BoxCox"))
#   polygon(d3,col = "coral")
# }

###----RandomForest (non-PCA)---####
#splitting data (70-30%)
#removing Release clause, it has nulls

dim(transformed) #15926    35
rowCount = nrow(transformed)
set.seed(123)
trainIndex= sample(rowCount, 0.7*rowCount, replace=FALSE)
treeTrain= transformed[trainIndex,]
treeTest = transformed[-trainIndex,]

dim(treeTrain) # 11148    35
dim(treeTest) # 4778   35

library(randomForest)
#train rf model
rfmodel= randomForest( Wage ~., data=treeTrain, ntree=500 ) # takes 3 mins

par(mfrow=c(1,1))
varImpPlot(rfmodel)
rfmodel
# colnames(data)
plot(rfmodel)

rf_yHat = predict(rfmodel,newdata=treeTest[-1])

library(ModelMetrics)
checkRMSE= treeTest$Wage
checkRMSE= as.data.frame(checkRMSE)
checkRMSE$test = rf_yHat

a= rmse(checkRMSE$checkRMSE, checkRMSE$test) #9.60
b= max(checkRMSE$checkRMSE) - min(checkRMSE$test) 

a/b # 0.02377045<==== normalized RMSE on test dataset/model because Wage has a large range (can vary in train and test ds)

rmse(rfmodel$predicted, treeTrain$Wage) #11.94085 transformed<=== training model
rmse(rf_yHat,treeTest$Wage) #9.603233 transformed <=== predicted model


####---- Random Forest with Cross-validation----####
####--Cross Validation RF without PCA non wage transformed --------####

#rm(list = ls())
transformedk <- transformed 
transformedk$Release.Clause= NULL
transformedk <- na.omit(transformedk) #15926    35
k = 5
fold = 1:k

transformedk$kfold <- sample(1:k, nrow(transformedk), replace = TRUE)  #adds another column with kfold

length(which(transformedk$kfold == 5))

prediction <- data.frame()
test_sets <- data.frame()

for(n in 1:k){
  ###Grab all the rows with the id 'n', aggregate them into a test set
  test_set = transformedk[which(transformedk$kfold %in% n), -ncol(transformedk)]
  
  ###All the other rows (the other 9 parts) go in the training set 
  train_set = transformedk[which(transformedk$kfold %in% fold[-c(n)]), -ncol(transformedk)]
  
  forest = randomForest(Wage ~., data = train_set, importance = TRUE,   ntree = 200)
  
  ###Run the model on the test set, save the prediction in a dataframe, the test set in another. Then you can compare them to get your performance measure.	
  n_predict = data.frame(predict(forest, test_set))
  prediction = rbind(prediction, n_predict)
  test_sets = rbind(test_sets, as.data.frame(test_set))
} 

a <- rmse(prediction$predict.forest..test_set., test_sets$Wage) # = 11.313349
b <- max(test_sets$Wage) -min(test_sets$Wage)
a/b #0.02005937
plot(forest)
rm(a,b)
varImpPlot(forest)


####--- Exploring and transforming Wage----####
#plotting wage
par(mfrow=c(1,1))
#hist
hist(data$Wage, main = "histogram of Wage")
#boxplot
boxplot(data$Wage, data=data, col="red")
#nzv
nearZeroVar(data$Wage, names=TRUE, saveMetrics = T) #nvz FALSE
#skewness
skewness(data$Wage) #7.801676
#density
dwage= density(data$Wage, na.rm = TRUE)
plot(dwage, main="Density-Distributionof Wage")
polygon(dwage, col = "purple")


#Centering, Scaling & Transforming Wage column
trans_W= preProcess(transformed[1], method = c("center", "scale", "BoxCox"))
transformed_W= predict(trans_W,transformed)
#hist
hist(transformed_W[,1], xlab = names(transformed_W[1]), main=  "Histogram of Trans_Wage",col="yellow")
#boxplot
boxplot(transformed_W$Wage, data=data, col="red")
#skewness
skewness(transformed_W$Wage, na.rm=TRUE) #0.09720052
#density
density_w = density(transformed_W$Wage, na.rm = TRUE)
plot(density_w, main=paste(names(transformed_W$Wage), "Density of Wage Post-BoxCox"))
polygon(density_w,col = "coral")




####---Gradient Boosting Method (Boosted tree)----####
rm(list = ls())
library(caret)
library(gbm)
library(dplyr)
data= read.csv("~/Desktop/fifa5.txt")
trans= preProcess(data[c(2:6, 10:33, 35)], method = c("center", "scale", "BoxCox"))
transformed= predict(trans,data)
transformed$Release.Clause= NULL

dim(transformed) #15926    35


rowCount = nrow(transformed)
set.seed=123

trainIndex= sample(rowCount, 0.7*rowCount, replace=FALSE)
treeTrain= transformed[trainIndex,]
treeTest = transformed[-trainIndex,]
#train model
gbmModel1= gbm.fit(x= treeTrain[,-1], y= treeTrain[,1],
                   distribution="gaussian", n.trees =100, 
                   interaction.depth=1, shrinkage=0.1)

#predict gbm on test 
gbm_yHat1 = predict(gbmModel1,n.trees = 100, newdata = data.frame(x=treeTest[,-1])) 

#Evaluation
gbm_trainPR1= postResample(pred= gbmModel1$fit, obs= treeTrain$Wage)
gbm_trainPR1 

gbmPR1 = postResample(pred=gbm_yHat1, obs=treeTest$Wage)
gbmPR1

par(mfrow=c(1,1))
summary(gbmModel1, plot = TRUE)
# train rmse= 11.4057563, test rmse= 11.8666908
#normalized RMSE
11.8666908/(max(treeTest$Wage)-min(treeTest$Wage))

#Tuned GBM 
#train
gbmModel2 = gbm.fit(x= treeTrain[,-1], y= treeTrain[,1],
               distribution="gaussian", n.trees =300, 
               interaction.depth=2, shrinkage=0.05) 

#test
gbm_yHat2 = predict(gbmModel2,n.trees = 300, newdata = data.frame(x=treeTest[,-1])) #test ntrees=100 at default

#defaults normalized rmse on gbm ==> 0.02832146 = 11.8666908/max(treeTest$Wage)-min(treeTest$Wage)


# ## performance evaluation
#library(caret)
gbm_trainPR2= postResample(pred= gbmModel2$fit, obs= treeTrain$Wage)
gbm_trainPR2 # RMSE training 9.7049918 OVERFIT

gbmPR2 = postResample(pred=gbm_yHat2, obs=treeTest$Wage)
gbmPR2 #RMSE test 11.2602592

#Normalized test RMSE 
11.2602592/(max(treeTest$Wage)-min(treeTest$Wage))
par(mfrow=c(1,1))
summary(gbmModel2, plot = TRUE)












########################---- Changes to Ruichen's code---#####
# # Wage as is
# # read in files
# data1 = read.csv("~/Desktop/fifa5.txt")
# data1$Release.Clause =NULL
# sum(is.na(data1))       #make sure no NULL values in the data set
# train_size = floor(0.7*nrow(data1))
# data1 = data1[!(data1$Body.Type=="C. Ronaldo" ),]
# data1 = data1[!(data1$Body.Type=="Akinfenwa" ),]
# data1 = data1[!(data1$Body.Type=="Shaqiri" ),]
# data1 = data1[!(data1$Body.Type=="PLAYER_BODY_TYPE_25" ),]
# data1 = data1[!(data1$Body.Type=="Neymar" ),]
# data1 = data1[!(data1$Body.Type=="Messi" ),]
# trans= preProcess(data1[c(2:6, 10:33, 35)], method = c("center", "scale", "BoxCox"))
# transformed= predict(trans,data1)
# # summary(data1$Body.Type)
# # summary(data1$Wage)
# # data1$Wage = log(data1$Wage)
# # summary(data1$Wage)
# #setting seed and split the data
# set.seed(123)
# trainindex <- sample(nrow(transformed), size=train_size, replace=FALSE)
# training <- transformed[trainindex, ]
# validation <- transformed[-trainindex, ]
# validation <- transformed[-trainindex, ]
# validation_x = subset(validation,select = -Wage)
# library(ModelMetrics)
# lm = lm(Wage~.,data = training)
# summary(lm)
# sp = coef(summary(lm))
# # ss_sig <- sp[sp[,"Pr(>|t|)"]<0.05,]
# # ss_sig
# result = predict(lm,validation_x, se.fit=TRUE)
# rmse(validation$Wage,result$fit)/(max(validation$Wage)-min(validation$Wage))#0.037
# rmse(validation$Wage,result$fit)
# rmse(training$Wage,lm$fit)/(max(training$Wage)-min(training$Wage))#0.031
# rmse(training$Wage,lm$fit)
# 
# 
# par(mfrow=c(2,2))
# plot(lm)
# 
# #### WAGE TRANSFORMED TOO
# data1 = read.csv("~/Desktop/fifa5.txt")
# data1$Release.Clause =NULL
# sum(is.na(data1))       #make sure no NULL values in the data set
# train_size = floor(0.7*nrow(data1))
# data1 = data1[!(data1$Body.Type=="C. Ronaldo" ),]
# data1 = data1[!(data1$Body.Type=="Akinfenwa" ),]
# data1 = data1[!(data1$Body.Type=="Shaqiri" ),]
# data1 = data1[!(data1$Body.Type=="PLAYER_BODY_TYPE_25" ),]
# data1 = data1[!(data1$Body.Type=="Neymar" ),]
# data1 = data1[!(data1$Body.Type=="Messi" ),]
# 
# trans= preProcess(data1[c(1:6, 10:33, 35)], method = c("center", "scale", "BoxCox"))
# transformed_W= predict(trans,data1)
# 
# #setting seed and split the data
# set.seed(123)
# trainindex <- sample(nrow(transformed_W), size=train_size, replace=FALSE)
# training <- transformed_W[trainindex, ]
# validation <- transformed_W[-trainindex, ]
# validation <- transformed_W[-trainindex, ]
# validation_x = subset(validation,select = -Wage)
# library(ModelMetrics)
# lm = lm(Wage~.,data = training)
# summary(lm) #r-squared=  0.6293 
# sp = coef(summary(lm))
# # ss_sig <- sp[sp[,"Pr(>|t|)"]<0.05,]
# # ss_sig
# result = predict(lm,validation_x, se.fit=TRUE)
# rmse(validation$Wage,result$fit)/(max(validation$Wage)-min(validation$Wage))#normalized test rmse=0.1677749
# rmse(validation$Wage,result$fit) #test RMSE 0.6143307
# rmse(training$Wage,lm$fit)/(max(training$Wage)-min(training$Wage))# training normalized RMSE 0.165779
# rmse(training$Wage,lm$fit) #train RMSE 0.6098339
# 
# par(mfrow=c(2,2))
# plot(lm)
# 
# #### PCA with Wage transformed
# # With PCA data
# data_pca = read.csv("~/Downloads/pcafinal.txt")
# data_pca = data_pca[!(data_pca$Body.Type=="C. Ronaldo" ),]     #remove rows with special body type
# data_pca = data_pca[!(data_pca$Body.Type=="Akinfenwa" ),]
# data_pca = data_pca[!(data_pca$Body.Type=="Shaqiri" ),]
# data_pca = data_pca[!(data_pca$Body.Type=="PLAYER_BODY_TYPE_25" ),]
# data_pca = data_pca[!(data_pca$Body.Type=="Neymar" ),]
# data_pca = data_pca[!(data_pca$Body.Type=="Messi" ),]
# 
# #transform the wage clomn
# trans_W= preProcess(data_pca[1], method = c("center", "scale", "BoxCox"))
# data_pca= predict(trans_W,data_pca)
# 
# train_size = floor(0.7*nrow(data_pca))
# set.seed(123)
# trainindex <- sample(nrow(data_pca), size=train_size, replace=FALSE)
# training_pca <- data_pca[trainindex, ]
# validation_pca <- data_pca[-trainindex, ]
# #summary(data_pca)
# lm_pca = lm(Wage~.,data = training_pca)
# summary(lm_pca)
# result_pca = predict(lm_pca,validation_pca, se.fit=TRUE)
# rmse(validation_pca$Wage,result_pca$fit)/(max(validation_pca$Wage)-min(validation_pca$Wage))#0.037
# rmse(validation_pca$Wage,result_pca$fit)
# rmse(training_pca$Wage,lm_pca$fit)/(max(training_pca$Wage)-min(training_pca$Wage))#0.031
# rmse(training_pca$Wage,lm_pca$fit)
# par(mfrow=c(2,2))
# plot(lm_pca)
# 
# 

