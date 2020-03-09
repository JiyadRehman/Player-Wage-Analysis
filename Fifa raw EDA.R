### EDA-- Zulay's code
# Sections
## Missing data removal and quick looks at the fifa data

# 0. Setup

library(ISLR)
library(leaps)
library(lattice)
library(grid)
library(hexbin)
library(ggplot2)
library(GGally)
library(AppliedPredictiveModeling)
library(caret)
library(e1071) # misc library including skewness function
library(mlbench)

library(Sleuth3)
library(MASS)
library(car)
library(forecast)
library(rcompanion)




# 1. Missing data remove and quick looks at the fifa data
fifa<- read.csv("fifa3.txt")
View(fifa)

head(fifa)
names(fifa) # same as colnames
dim(fifa)  # rows and columns
#[1] 15926    75

# Check for missing data
sum(is.na(fifa$Wage))
sum(is.na(fifa$Nationality))

# Chekcing which columns have missing values
colnames(fifa)[colSums(is.na(fifa)) > 0]
# [1] "Joined"         "Release.Clause" "Duration"   

# Data preparation:
#   omit rows with missing data
fifa = na.omit(fifa)

# Quick checks on removal
dim(fifa)
# [1] 14743    75

sum(is.na(fifa))

# Scatterplot binning and smoothing functions are not designed for categorical variables
# so we keep just the numeric variables. A data.frame is also a list structure.
# The sapply function arguments are a list and a function to apply to
# each element. We ask each column if it is numeric.

keepVar <- sapply(fifa, is.numeric)
fifaN <- fifa[, keepVar]

View(fifaN)
dim(fifaN)


# To determinate any outliers presented in the dataset, here we present some graphs (boxplot, density etc)
# can be used to assess the shape of the distribution.

max(fifaN$Wage)/min(fifaN$Wage)
skewness(fifaN$Wage)


#Creating multiple plots with a loop

# -- Visualization 1: Boxplot 

# 1-10
a <- fifaN[,1:10]
par(mfrow = c(4, 4))
for (i in 1:ncol(a)) {
  boxplot(a[ ,i], xlab = names(a[i]), horizontal=T,main = paste(names(a[i]), "BoxP"), col="steelblue")
}

# 11-20
b <- fifaN[,11:20]
par(mfrow = c(4, 4))
for (i in 1:ncol(b)) {
  boxplot(b[ ,i], xlab = names(b[i]), horizontal=T,main = paste(names(b[i]), "BoxP"), col="steelblue")
}

# 21-30
c <- fifaN[,21:30]
par(mfrow = c(4, 4))
for (i in 1:ncol(c)) {
  boxplot(c[ ,i], xlab = names(c[i]), horizontal=T,main = paste(names(c[i]), "BoxP"), col="steelblue")
}

# 31-40
d <- fifaN[,31:40]
par(mfrow = c(4, 4))
for (i in 1:ncol(d)) {
  boxplot(d[ ,i], xlab = names(d[i]), horizontal=T,main = paste(names(d[i]), "BoxP"), col="steelblue")
}

# 41-50
e <- fifaN[,41:50]
par(mfrow = c(4, 4))
for (i in 1:ncol(e)) {
  boxplot(e[ ,i], xlab = names(e[i]), horizontal=T,main = paste(names(e[i]), "BoxP"), col="steelblue")
}

# 51-60
f <- fifaN[,51:60]
par(mfrow = c(4, 4))
for (i in 1:ncol(f)) {
  boxplot(f[ ,i], xlab = names(f[i]), horizontal=T,main = paste(names(f[i]), "BoxP"), col="steelblue")
}

# 61-68
g <- fifaN[,61:68]
par(mfrow = c(3, 3))
for (i in 1:ncol(g)) {
  boxplot(g[ ,i], xlab = names(g[i]), horizontal=T,main = paste(names(g[i]), "BoxP"), col="steelblue")
}


# -- Visualization 2: Kernel Density Plot

# 1-10
par(mfrow = c(4, 4))
for (i in 1:ncol(a)) {
  da<- density(a[,i], na.rm = TRUE)
  plot(da, main = paste(names(a[i]), "Density"))
  polygon(da, col="steelblue")};


# 11-20
par(mfrow = c(4, 4))
for (i in 1:ncol(b)) {
  db<- density(b[,i], na.rm = TRUE)
  plot(db, main = paste(names(b[i]), "Density"))
  polygon(db, col="steelblue")};

# 21-30
par(mfrow = c(4, 4))
for (i in 1:ncol(c)) {
  dc<- density(c[,i], na.rm = TRUE)
  plot(dc, main = paste(names(c[i]), "Density"))
  polygon(dc, col="steelblue")};

# 31-40
par(mfrow = c(4, 4))
for (i in 1:ncol(d)) {
  dd<- density(d[,i], na.rm = TRUE)
  plot(dd, main = paste(names(d[i]), "Density"))
  polygon(dd, col="steelblue")};

# 41-50
par(mfrow = c(4, 4))
for (i in 1:ncol(e)) {
  de<- density(e[,i], na.rm = TRUE)
  plot(de, main = paste(names(e[i]), "Density"))
  polygon(de, col="steelblue")};

# 51-60
par(mfrow = c(4, 4))
for (i in 1:ncol(f)) {
  df<- density(f[,i], na.rm = TRUE)
  plot(df, main = paste(names(f[i]), "Density"))
  polygon(df, col="steelblue")};

# 61-68
par(mfrow = c(3, 3))
for (i in 1:ncol(g)) {
  dg<- density(g[,i], na.rm = TRUE)
  plot(dg, main = paste(names(g[i]), "Density"))
  polygon(dg, col="steelblue")};

## Use caret's preProcess function to transform for skewness
# preProcess estimates the transformation (centering, scaling etc.) 
# function from the training data and can be applied to any data set with the same variables.

# Using powerpowerTransform() to determinate Lambda.
summary(powerTransform(fifaN[,1:68], family="yjPower"))$result[,1:2] #check this out: Lambda no given
# the dataset is too big

# Appliying transformat;ions with preProcces
ffN.Transformation= preProcess(fifaN[,1:68],method=c("BoxCox","center","scale"))
fifaN.Transformation=predict(ffN.Transformation,fifaN[,1:68])


