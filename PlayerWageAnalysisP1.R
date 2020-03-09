#INSTALL PACKAGES

install.packages("tidyr")

# Data loading

library(readxl)
data2 <- read_excel("D:/MASON/Spring 2019/OR 568/Project/fifa19/Updated/data2.xls")
View(data2)

fifa <- data2
data <- fifa
fifa <- data
rm(data2)

# ---------------------------------------------------------------

which(is.na(fifa$GKDiving))

fifa$Wage == 0

fifa <- fifa[which(fifa$Wage != 0),] # Removing where wage is 0

fifa <- as.data.frame(fifa)

fifa <- (fifa[(which(!is.na(fifa$GKDiving))),]) # Removing Null values


fifa$`Loaned From` <- NULL #Removed a column with alot of null values

# GOAL KEEPER RELATED STUFF REMOVED

fifa <- fifa[which(fifa$Position != 'GK'),] # Removing Goal Keeper

# REMOVING SYMBOL FROM RELEASE CLAUSE

fifa$`Release Clause` <- sapply(strsplit(as.character(fifa$`Release Clause`),"â,¬"),tail,1)


#Converting Release clause into proper values

#temp <- as.character(fifa$`Release Clause`)
#s <- strsplit(temp, split="")
temp <- data.frame(x = fifa$Wage)


'for(i in 1:length(s)){
 temp[i,] <-  tail(s[[i]], n=1)
  
}

rm(s)'

temp$x <- fifa$`Release Clause`

temp$y <- gsub(".$","",temp$x)

temp$z <- gsub("[0-9,.]", "", temp$x) 

temp$a <- temp$z

x <- which(is.na(temp$a))

temp[x,4] <- 'R'
rm(x)

for(i in 1:nrow(temp)){
  if(temp[i,4] == "M"){
    temp[i,4] <- 1000000
  }else if(temp[i,4] == "K"){
    temp[i,4] <- 1000
  }else
    temp[i,4] <- 0
}

temp$y <- as.numeric(temp$y)
temp$a <- as.numeric(temp$a)

temp$final <- temp$y*temp$a

fifa$`Release Clause` <- temp$final

rm(temp)

#REMOVING EXTRA COLUMNS

fifa$GKDiving <- NULL
fifa$GKHandling <- NULL
fifa$GKKicking <- NULL
fifa$GKPositioning <- NULL
fifa$GKReflexes <- NULL
fifa$Special <- NULL

#fifa$`Release Clause` <- NULL

fifa$`Real Face` <- NULL  # Removing this boolean column

#fifa$Club <- NULL
#fifa$Nationality <- NULL
fifa$Name <- NULL


#Converting Height into CM

library(tidyr)
Height <- fifa$Height

test <- as.data.frame(Height)

test <- test %>% separate(Height, c("A", "B"))

test$A <- as.numeric(test$A)
test$B <- as.numeric(test$B)

test$FM <- test$A*30.48 

test$IM <- test$B*2.54

test$Height <- test$FM+test$IM

#fifa$Height <- NULL

fifa$Height <- test$Height



# removing values after + 
for(i in 17:44)
{
  test <- as.character(fifa[,i])
  fifa[i] <- gsub("\\+.","",test)
}


# Removing lbs from weights
test <- (fifa$Weight)

fifa$Weight <- gsub("\\l.*","",test)

rm(i,test,Height)


# Working on JOINED

test <- (fifa$Joined)
fifa$Joined <- gsub(".* ","",test)

test <- (fifa$`Contract Valid Until`)
fifa$`Contract Valid Until` <- gsub(".* ","",test)

fifa$Duration <- as.numeric(fifa$`Contract Valid Until`) - as.numeric(fifa$Joined)

fifa$Joined <- NULL
fifa$`Contract Valid Until` <- NULL

rm(test)


# COVERTING COUNTRIES INTO CONTI....

library(countrycode)
fifa$Continent <- countrycode(sourcevar = fifa[,'Nationality'], origin = 'country.name', destination = 'continent')
# Many NAs, must use individual lists to convert names
subset(fifa[,c('Nationality', 'Continent')], is.na(fifa$Continent))

# First convert UK Countries
UK <- c('England', 'Northern Ireland', 'Scotland', 'Wales')
fifa$Continent[fifa$Nationality %in% UK] <- 'UK'
# Random Kosovo to Europ and Central Africa 
fifa$Continent[fifa$Nationality == 'Kosovo'] <- 'Europe'
fifa$Continent[fifa$Nationality %in% c('Central African Rep.',  'SÃ£o TomÃ© & PrÃ¬ncipe')] <- 'Africa'
# North America
North_America <- c('United States', 'Canada', 'Mexico', 'El Salvador', 'Antigua & Barbuda', 'Nicaragua', 'Bahamas',
                   'Barbados', 'Belize', 'Grenada', 'Guatemala', 'Panama', 'Canada', 'Costa Rica', 'Cuba', 'Haiti',
                   'Honduras', 'Jamaica', 'Trinidad & Tobago', 'Dominica', 'Dominican Republic')
fifa$Continent[fifa$Nationality %in% North_America] <- 'North America'

# Send rest of America to South America 
fifa$Continent[fifa$Continent == 'Americas'] <- 'South America'


# ADDING median values for NULL in duration

x <- which(is.na(fifa$Duration))

median(fifa$Duration, na.rm = TRUE)

fifa[x,72] <- median(fifa$Duration, na.rm = TRUE)

fifa[x,]

rm(x,North_America,UK)


#-------------------------------WORKING ON PCA -------------------------------------------------------------

pca <- fifa
#NULL CATEGORICAL VARIABLES

pca$Nationality <- NULL
pca$Club <- NULL
pca$`Release Clause` <- NULL # REMOVED BECAUSE IT HAS NULL VALUES
pca$`Body Type` <- NULL
pca$`Work Rate` <- NULL



pca$`Preferred Foot` <- NULL
pca$`International Reputation` <- NULL
pca$`Weak Foot` <- NULL
pca$`Skill Moves` <- NULL
pca$Position <- NULL
pca$Continent <- NULL

for(i in 1:ncol(pca)){
  
  pca[,i] <- as.numeric(pca[,i]) 
  
}

#pca$Duration <- NULL


prComp<-prcomp(pca[,-1],scale. = TRUE)

std_dev <- prComp$sdev
pr_var <- std_dev^2
prop_varex <- pr_var/sum(pr_var)
sum(prop_varex[1:30])

plot(cumsum(prop_varex), xlab = "Principal Component",ylab = "Cumulative Proportion of Variance Explained",type = "b")
abline(h=0.975,col='red',v=22)

pca.data<-data.frame(Wage = pca$Wage, prComp$x)

pca.data <- data.frame(pca.data[,1:23])


# ADD BACK CATEGORICAL VARIABLES

pca.data$Nationality <- as.factor(fifa$Nationality)
pca.data$Club <- as.factor(fifa$Club)
pca.data$`Body Type` <- as.factor(fifa$`Body Type`)
pca.data$`Work Rate` <- as.factor(fifa$`Work Rate`)



pca.data$`Preferred Foot` <- as.factor(fifa$`Preferred Foot`)
pca.data$`International Reputation` <- as.factor(fifa$`International Reputation`)
pca.data$`Weak Foot` <- as.factor(fifa$`Weak Foot`)
pca.data$`Skill Moves` <- as.factor(fifa$`Skill Moves`)
pca.data$Position <- as.factor(fifa$Position)
pca.data$Continent <- as.factor(fifa$Continent)

pcadata <- pca.data

length(which(is.na(fifa$Duration)))

mean(fifa$Duration, na.rm = TRUE)


#-----------------------------------------------

# Exporting data to Excel File
install.packages("xlsx")
library(xlsx)

# Writing to text file

write.table(fifa, "D:/MASON/Spring 2019/OR 568/Project/fifa19/Updated/fifa4.txt",sep = ",")

write.table(pcadata, "D:/MASON/Spring 2019/OR 568/Project/fifa19/Updated/pcadata.txt",sep = ",")

#-------------------------------------------------------

#----------- Loading the new dataset -----------------------

rm(list = ls())

fifa4 <- read.csv("D:/MASON/Spring 2019/OR 568/Project/fifa19/Updated/fifa4.txt")

pca <- read.csv("D:/MASON/Spring 2019/OR 568/Project/fifa19/Updated/pcadata.txt")


#----------- Loading the new dataset -----------------------


# Preparing the data of PCA --------------------------------

pca$Nationality <- NULL # as it has more than 53 categories

pca$Club <- NULL # as it has more than 53 categories

pca$Preferred.Foot <- NULL # not significant



pca$International.Reputation <- NULL # has more 15000 values of 1


# Preparing the data of PCA --------------------------------


# Producing final PCA file -----------------------------------------------------------

write.table(pca, "D:/MASON/Spring 2019/OR 568/Project/fifa19/Updated/pcafinal.txt",sep = ",")


# Producing final PCA file -----------------------------------------------------------

# Loading Final PCA Data -------------------------------------------------------------

pca <- read.csv("D:/MASON/Spring 2019/OR 568/Project/fifa19/Updated/pcafinal.txt")

# Loading Final PCA Data -------------------------------------------------------------


# Dividing the dataset into train and test -----------------

set.seed(123)
num <- sample(1:nrow(pca), round(nrow(pca)*.70), replace = FALSE)

train <- pca[num,]
test <- pca[-num,]

train <- na.omit(train)
test <- na.omit(test)

rm(num)
# Dividing the dataset into train and test -----------------


# Modelling with Random Forest -------------------------------------------------------

library(randomForest)

'
length(which(is.na(pcadata$`Preferred Foot`)))


rf <- randomForest(pcadata[2:27], pcadata[[1]])

test <- predict(rf,pcadata[1:10,2:27])

pcadata[1:10,1]

test$wage <- pcadata[1:10,1]
test <- as.data.frame(test)
'

library(ModelMetrics)
'
rmse(test$wage,test$test)

max(test$wage) -min(test$wage)
'

# After train test


rf <- randomForest(train[-1], train[[1]])

chk <- predict(rf,test[-1])

chkrmse <- test$Wage
chkrmse <- as.data.frame(chkrmse)
chkrmse$test <- chk

library(ModelMetrics)

a <- rmse(chkrmse$chkrmse,chkrmse$test) # = 10.8143

rmse(train$Wage,rf$predicted)  # = 13.2308

b <- max(chkrmse$chkrmse) -min(chkrmse$test)

a/b # = 0.02677031

plot(rf, main = 'Error with number of trees')

rm(a,b)

library(MLmetrics)

MSE(chkrmse$test,chkrmse$chkrmse)


# Modelling with Random Forest -------------------------------------------------------


# Cross Validation -------------------------------------------------------------------

rm(list = ls())

pca <- read.csv("D:/MASON/Spring 2019/OR 568/Project/fifa19/Updated/pcafinal.txt")


pcak <- pca 

pcak <- na.omit(pcak)

k = 5

fold = 1:k

set.seed(123)
pcak$kfold <- sample(1:k, nrow(pcak), replace = TRUE)

length(which(pcak$kfold == 4))

prediction <- data.frame()
test_sets <- data.frame()

trainorg <- data.frame()
trainpred <- data.frame()

for(n in 1:k){
  ###Grab all the rows with the id 'n', aggregate them into a test set
  test_set = pcak[which(pcak$kfold %in% n), -ncol(pcak)]
  
  ###All the other rows go in the training set 
  train_set = pcak[which(pcak$kfold %in% fold[-c(n)]), -ncol(pcak)]
  
  forest = randomForest(Wage ~., data = train_set, importance = TRUE,   ntree = 200)
  
  ###Run the model on the test set, save the prediction in a dataframe, the test set in another. Then you can compare them to get your performance measure.	
  n_predict = data.frame(predict(forest, test_set))
  prediction = rbind(prediction, n_predict)
  test_sets = rbind(test_sets, as.data.frame(test_set))
  
  #Train rmse
  trainpred <-  rbind(trainpred, as.data.frame(forest$predicted))  
  trainorg <- rbind(trainorg, as.data.frame(train_set))
} 

a <- rmse(prediction$predict.forest..test_set., test_sets$Wage) # = 12.58887

b <- max(test_sets$Wage) -min(test_sets$Wage)

a/b

plot(forest)

rm(a,b)

a <-  rmse(trainpred$`forest$predicted`,trainorg$Wage) # 12.61084
b <- max(trainorg$Wage) - min(trainorg$Wage) # 564

a/b # 0.2235965

rm(a,b)

trainorg$Wage
trainpred$`forest$predicted`
# Cross Validation -------------------------------------------------------------------


# Removing values above 45 -----------------------------------------------------------

rm(list = ls())

pca <- read.csv("D:/MASON/Spring 2019/OR 568/Project/fifa19/Updated/pcafinal.txt")

length(which(pca$Wage < 45))

clean <- pca[which(pca$Wage < 45),]

org <- pca
pca <- clean

set.seed(123)
num <- sample(1:nrow(pca), round(nrow(pca)*.70), replace = FALSE)

train <- pca[num,]
test <- pca[-num,]

train <- na.omit(train)
test <- na.omit(test)

rm(num)


rf <- randomForest(train[-1], train[[1]], ntree = 200)

chk <- predict(rf,test[-1])

chkrmse <- test$Wage
chkrmse <- as.data.frame(chkrmse)
chkrmse$test <- chk

rmse(chkrmse$chkrmse,chkrmse$test)

a <- rmse(chkrmse$chkrmse,chkrmse$test) # = 5.337619


rmse(train$Wage,rf$predicted)  # = 5.447483

b <- max(chkrmse$chkrmse) -min(chkrmse$test)

a/b # = 0.1241884

plot(rf, main = 'Error with number of trees')

rm(a,b)

# Removing values above 45 -----------------------------------------------------------




