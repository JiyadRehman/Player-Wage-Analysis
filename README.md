# Player-Wage-Analysis
Developing a machine learning model that would help in predicting the wages of soccer players.

This is a group project for a graduate course. The name of the members is shared in the final report.

The data was taken from Kaggle and the path of it is shared in the project report.

The objective of this project is to develop a machine learning model that would precisely predict the wage of soccer players.
This can help the soccer clubs and managers to bid accordingly.

The project details are shared in the report and the following are the steps to run the codes:

Steps of Running “PlayersWageAnalysisP1.R”:
This file should prior to all the other R files shared:
Also set the working directory and all files should be in the same directory, you may need to change the loading path in the R-codes.
Run code from
1.	Line 1 till line 269 for basic data preprocessing and PCA
2.	Line 263 to line 261 for creating a .txt file of the data
3.	Line 277 to line 284 clears the environment and loads the data from newly created .txt file
4.	Line 287 to line 307 deletes columns that were not required for PCA and then creates another .txt file
5.	Line 313 loads the updated data for the PCA file
6.	Line 319 to line 329 divides the data into train and test set
7.	Line 334 to lone 387 runs simple random forest and calculates RMSE and normalized RMSE
8.	Line 392 to line 454 initially clears the environment and then loads the data for random forest with cross validation. Then calculates RMSE and normalized RMSE
9.	Line 459 to line 505 clears the environment then loads the data and performs random forest to data that had wage less than 45 and check how the model performs
The other R scripts can be run in any order. 

Steps for running “Fifa-Linear Regression.R”:
This code contains the models for linear regression on PCA and non-PCA data
Run code from:
For linear regression on PCA data:
1.	Line 4 to 14 to load PCA data, remove categorical variables, transforming wage
2.	Lines 16 to 30 to split the data into training and test sets, building mode, and gathering results. 
For linear regression on non-PCA data:
1.	Lines 36 to 47 to clear environment, load non-PCA data file, and transforming wage for linear regression. 
2.	Lines 50 to 77 to set seed, split data, build run run model, and gather results and plots. Further clearing the environment again. 

Steps for running “SVM PCA Final.R”:
Run the code as follows: 
1.	Line 1 to 27, load PCA data, and split dataset into train and test
2.	Lines 30 to 42 are creating samples to get understanding of which parameters are best for final training. 
3.	Lines 46 to 58 contain 1st tune, 2nd tune, 3rd tune, and full tuning of training data
4.	Lines 60 to 78, prediction model on test data using best model, and accuracy results of the model (normalized RMSE calculated manually). 

Steps for running “Fifa correlation, RF, GBM.R”:
Run the code as follows:
1.	Lines 1 to 13 for loading data, and exploring the columns. 
2.	Lines 16 to 36 to select the numeric variables in dataset to form a data frame, checking for correlation coefficients and plotting them.  
3.	Lines 38 to 48 to use identify which columns to delete
4.	Lines 51 to 63, creating updated file with selective numeric variables (that were not dropped after findCorrelation) and categorical variables (that passed the ANOVA test). Creating new .txt file named “fifa5.txt” (however commented out to avoid creating many files of same name). 
5.	Lines 67 to 163 for loading non-PCA data file (“fifa5.txt”), plotting histograms, boxplots, density plots, skewness, and checking for near zero variance in pre-transformed data, transforming data using preprocess function (center, scale, and box-cox), and again plotting histograms, boxplots, and density plots after transformation and normalization. 
6.	Lines 165 to 204 to run random forest with default parameters on training and test sets. Also get the RMSE’s and normalized RMSE
7.	Lines 205 to 242 for Cross Validation Random Forest, and its respective results (RMSE scores)
8.	Lines 245 to 276 to explore and transform wage column (includes histogram, boxplot, density plots pre and post processing)
9.	Lines 279 to 342 for default and tuned Gradient Boosting model (with their RMSE’s, normalized RMSE’s)

Run “Fifa imp categorical vars.R” file on R to see the aov() test results

Run “Fifa raw EDA.R” file to see the exploratory data analysis on raw data. Lines 189-190 perform transformation.
