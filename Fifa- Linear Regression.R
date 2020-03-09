##Changes to LR code-- Ruichen (few edits by Ritika in non-PCA)
#### PCA with Wage transformed
# With PCA data
data_pca = read.csv("~/Downloads/pcafinal.txt")

data_pca = data_pca[!(data_pca$Body.Type=="C. Ronaldo" ),]     #remove rows with special body type
data_pca = data_pca[!(data_pca$Body.Type=="Akinfenwa" ),]
data_pca = data_pca[!(data_pca$Body.Type=="Shaqiri" ),]
data_pca = data_pca[!(data_pca$Body.Type=="PLAYER_BODY_TYPE_25" ),]
data_pca = data_pca[!(data_pca$Body.Type=="Neymar" ),]
data_pca = data_pca[!(data_pca$Body.Type=="Messi" ),]

#transform the wage clomn
trans_W= preProcess(data_pca[1], method = c("center", "scale", "BoxCox"))
data_pca= predict(trans_W,data_pca)

train_size = floor(0.7*nrow(data_pca))
set.seed(123)
trainindex <- sample(nrow(data_pca), size=train_size, replace=FALSE)
training_pca <- data_pca[trainindex, ]
validation_pca <- data_pca[-trainindex, ]
training_pca <- na.omit(training_pca)
validation_pca <- na.omit(validation_pca)
#summary(data_pca)
lm_pca = lm(Wage~.,data = training_pca)
summary(lm_pca) #R-squared is 0.6365 meaning predictors can explain 63.65% of the variance in Wage
result_pca = predict(lm_pca,validation_pca, se.fit=TRUE)
rmse(validation_pca$Wage,result_pca$fit)/(max(validation_pca$Wage)-min(validation_pca$Wage))# normalized test RMSE 0.1656875
rmse(validation_pca$Wage,result_pca$fit) # test 0.6066876
rmse(training_pca$Wage,lm_pca$fit)/(max(training_pca$Wage)-min(training_pca$Wage)) #normalized train 0.1641842
rmse(training_pca$Wage,lm_pca$fit) # train 0.603967
par(mfrow=c(2,2))
plot(lm_pca)


rm(list = ls())

## LR with non-PCA data & wage transformed
data1 = read.csv("~/Desktop/fifa5.txt")
data1$Release.Clause =NULL
sum(is.na(data1))       #make sure no NULL values in the data set
train_size = floor(0.7*nrow(data1))
data1 = data1[!(data1$Body.Type=="C. Ronaldo" ),]
data1 = data1[!(data1$Body.Type=="Akinfenwa" ),]
data1 = data1[!(data1$Body.Type=="Shaqiri" ),]
data1 = data1[!(data1$Body.Type=="PLAYER_BODY_TYPE_25" ),]
data1 = data1[!(data1$Body.Type=="Neymar" ),]
data1 = data1[!(data1$Body.Type=="Messi" ),]

trans= preProcess(data1[c(1:6, 10:33, 35)], method = c("center", "scale", "BoxCox"))
transformed_W= predict(trans,data1)

#setting seed and split the data
set.seed(123)
trainindex <- sample(nrow(transformed_W), size=train_size, replace=FALSE)
training <- transformed_W[trainindex, ]
validation <- transformed_W[-trainindex, ]
validation <- transformed_W[-trainindex, ]
validation_x = subset(validation,select = -Wage)
training <- na.omit(training)
validation <- na.omit(validation)
library(ModelMetrics)
lm = lm(Wage~.,data = training)
summary(lm) #r-squared=  0.6293 
sp = coef(summary(lm))
# ss_sig <- sp[sp[,"Pr(>|t|)"]<0.05,]
# ss_sig
result = predict(lm,validation_x, se.fit=TRUE)
rmse(validation$Wage,result$fit)/(max(validation$Wage)-min(validation$Wage))#normalized test rmse=0.1677749
rmse(validation$Wage,result$fit) #test RMSE 0.6143307
rmse(training$Wage,lm$fit)/(max(training$Wage)-min(training$Wage))# training normalized RMSE 0.165779
rmse(training$Wage,lm$fit) #train RMSE 0.6098339

par(mfrow=c(2,2))
plot(lm)

rm(list = ls())
