#===========================================#
# Filename: Heart_Disease_Script.R #
# Author: Akash Kumar                       #
# Data modified: June 17, 2019              #
# R version 3.6.0                           #
#===========================================#


#======================#
# Setup and Clean Data #
#======================#
# Install packages:
if(!require(tidyverse)) 
  install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) 
  install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(pROC)) 
  install.packages("pROC", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) 
  install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require(klaR)) 
  install.packages("klaR", repos = "http://cran.us.r-project.org")
if(!require(kernlab)) 
  install.packages("kernlab", repos = "http://cran.us.r-project.org")

# Load the necessary libraries:
library(tidyverse)
library(caret)
library(pROC)
library(randomForest)
library(klaR)
library(kernlab)

# Heart Disease UCI dataset:
# https://www.kaggle.com/ronitf/heart-disease-uci
# https://archive.ics.uci.edu/ml/datasets/Heart+Disease

# heart set:
heart <- read.csv("heart.csv")
colnames(heart)[c(1,3,4,6,7,8,9,10,11,12)] <- c("age", "chest_pain", "resting_bps",
                              "blood_sugar_120", "resting_ecg",
                              "max_bpm", "angina", "st_oldpeak", "st_slope",
                              "vessel_count")
heart <- heart[, c(1,2,4,8,5,6,7,3,9,10,11,12,13,14)] # Reorder some columns.
heart <- na.omit(heart) # Remove NAs.
head(heart)
glimpse(heart)
summary(heart)
dim(heart) # 303 rows, 14 columns.

# train set will be 90% of heart set
# test set will be 10% of heart set
set.seed(1, sample.kind = "Rounding")
test_index <- createDataPartition(y = heart$age, times = 1, p = 0.1, list = FALSE)
train <- heart[-test_index,]
test <- heart[test_index,]


#=================================#
# Exploratory Data Analysis (EDA) #
#=================================#
train <- train %>% arrange(age) # Arrange rows according to age.
# Take a glimpse at the train set.
head(train)
glimpse(train)
summary(train)
dim(train) # 271 rows, 14 columns.

# Total population:
patient_pop <- train %>% nrow()
patient_pop # 271 patients
# Total number of men and women: 1 = male, 0 = female
men <- sum(train$sex)
men   # 187 men
women <- patient_pop - men
women # 84 women
# Number of patients with heart disease:
heart_disease <- sum(train$target)
heart_disease # 146 heart disease patients
# Number of patients without heart disease:
healthy <- patient_pop - heart_disease
healthy       # 125 healthy patients
# Number of men and women with heart disease:
hd_men <- train %>% filter(sex == 1 & target == 1) %>% nrow()
hd_men   # 85 hd men
hd_women <- train %>% filter(sex == 0 & target == 1) %>% nrow()
hd_women # 61 hd women
# Proportion of men and women with heart disease:
hd_men_prop <- hd_men / men
hd_men_prop   # 45.45%
hd_women_prop <- hd_women / women
hd_women_prop # 72.62%
# Age:
mean_age <- mean(train$age)
mean_age     # 54 years old
# Mode function:
getmode <- function(x) {
  uniqv <- unique(x)
  uniqv[which.max(tabulate(match(x, uniqv)))]
}
mode_age <- getmode(train$age)
mode_age     # Most patients are 58 years old
# Youngest age:
youngest_age <- min(train$age)
youngest_age # 29 years old
# Oldest age:
oldest_age <- max(train$age)
oldest_age   # 77 years old

#==========================#
# Plots and Visualizations #
#==========================#
# Factors that might influence the onset of heart disease:
# 1. Age
# 2. Sex
# 3. Max BPM
# 4. Cholesterol
# 5. Blood sugar
# 6. Number of major vessels

# 1. Age
# The graph looks like a normal distribution,
# with a peak at age 58.
train %>%
  group_by(age) %>%
  summarize(count = n()) %>%
  ggplot(aes(x = age, y = count)) +
  geom_line(color = "orange") +
  ggtitle("Patient Age Distribution")

# Check for a correlation between age and heart disease:
train %>%
  filter(target == 1) %>%
  group_by(age) %>%
  summarize(count = n()) %>%
  ggplot(aes(x = age, y = count)) +
  geom_line(color = "red") +
  ggtitle("Heart Disease Age Distribution")
# Patients with ages between 40 - 60 have a higher
# risk for heart disease.

# 2. Sex
train %>%
  mutate(gender = ifelse(sex == 0, "female", "male")) %>%
  group_by(gender) %>%
  summarize(count = n(), 
            prop = sum(target)/count*100) %>%
  ggplot(aes(x = gender, y = prop)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(x = "gender", y = "proportion with heart disease (in %)") +
  ggtitle("Heart Disease Proportions by Gender")
# Women have a greater risk of heart disease than men.

# 3. Max BPM
train %>%
  filter(target == 1) %>%
  ggplot(aes(max_bpm)) +
  geom_histogram(bins = 10, fill = "purple", color = "black") +
  labs(x = "max BPM", y = "heart disease count") +
  ggtitle("Max BPM vs Heart Disease")

# 4. Cholesterol
train %>%
  filter(target == 1) %>%
  ggplot(aes(chol)) +
  geom_histogram(bins = 10, fill = "red", color = "black") +
  labs(x = "cholesterol", y = "heart disease count") +
  ggtitle("Cholesterol vs Heart Disease")

# 5. Blood sugar > 120
train %>%
  mutate(sugar = ifelse(blood_sugar_120 == 0, "no", "yes")) %>%
  group_by(sugar) %>%
  summarize(count = n(),
            prop = sum(target)/count*100) %>%
  ggplot(aes(x = sugar, y = prop)) +
  geom_bar(stat = "identity", fill = "brown") +
  labs(x = "is blood sugar greater than 120?", 
       y = "proportion with heart disease (in %)") +
  ggtitle("Blood Sugar and Heart Disease")

# 6. Number of major vessels
train %>%
  group_by(vessel_count) %>%
  summarize(count = n()) %>%
  ggplot(aes(x = vessel_count, y = count)) +
  geom_bar(stat = "identity", fill = "black") +
  labs(x = "number of vessels", 
       y = "frequency") +
  ggtitle("Vessel Count Frequency")

train %>%
  filter(target == 1) %>%
  group_by(vessel_count) %>%
  summarize(count = n()) %>%
  ggplot(aes(x = vessel_count, y = count)) +
  geom_bar(stat = "identity", fill = "gold") +
  labs(x = "number of vessels", 
       y = "heart disease count") +
  ggtitle("Vessel Count and Heart Disease")


#===================#
# Prediction Models #
#===================#
# For this project, 4 prediction models have been built:
# Model 1: Naive Bayes
# Model 2: Logistic Regression
# Model 3: Support Vector Machine (SVM)
# Model 4: Random Forest

AUC = list()      # List of AUCs
accuracy = list() # List of model accuracies
# Change some columns in train and test sets to factor:
fac_col <- c(2,3,6,7,9,11,12,13,14)
for(i in fac_col) {  
  train[,i] = as.factor(train[,i])
}
for(i in fac_col) {  
  test[,i] = as.factor(test[,i])
}
# Remove any troublesome columns: (zero variances between variables)
train[[3]] <- NULL
test[[3]] <- NULL
# Set target level:
levels(train$target) <- make.names(levels(factor(train$target)))
levels(test$target) <- make.names(levels(factor(train$target)))
# Train control:
train_control <- trainControl(method = "repeatedcv",
                              number = 10,
                              repeats = 10,
                              classProbs = TRUE, # Estimate class probabilities.
                              summaryFunction = twoClassSummary)


#======================#
# Model 1: Naive Bayes #
#======================#
set.seed(1, sample.kind = "Rounding")
naive_bayes <- train(target ~ ., data = train, 
                     method = "nb", family = "binomial") 
nb_prediction <- predict(naive_bayes, test)
nb_prediction_prob <- predict(naive_bayes, test, type = "prob")[2]
# Confusion matrix:
nb_conf_mat <- confusionMatrix(nb_prediction, as.factor(test[,"target"]))
nb_conf_mat
#ROC curve:
AUC$naive_bayes <- roc(as.numeric(test$target),
                       as.numeric(as.matrix((nb_prediction_prob))))$auc
AUC
# Accuracy:
accuracy$naive_bayes <- nb_conf_mat$overall["Accuracy"]
accuracy


#==============================#
# Model 2: Logistic Regression #
#==============================#
set.seed(1, sample.kind = "Rounding")
log_reg <- train(target ~ ., data = train,
                 method = "glm", family = "binomial") 
log_reg_prediction <- predict(log_reg, test)
log_reg_prediction_prob <- predict(log_reg, test, type = "prob")[2]
# Confusion matrix:
log_reg_conf_mat <- confusionMatrix(log_reg_prediction, as.factor(test[,"target"]))
log_reg_conf_mat
#ROC curve:
AUC$log_reg <- roc(as.numeric(test$target),
                   as.numeric(as.matrix((log_reg_prediction_prob))))$auc
AUC
# Accuracy:
accuracy$log_reg <- log_reg_conf_mat$overall["Accuracy"]
accuracy


#=======================================#
# Model 3: Support Vector Machine (SVM) #
#=======================================#
set.seed(1, sample.kind = "Rounding")
svm <- train(target ~ ., data = train,
             method = "svmLinear",
             preProcess = c("center", "scale"),
             trControl = train_control,
             family = "binomial",
             tuneLength = 8,
             metric = "ROC")
svm_prediction <- predict(svm, test)
svm_prediction_prob <- predict(svm, test, type="prob")[2]
# Confusion matrix:
svm_conf_mat <- confusionMatrix(svm_prediction, test[,"target"])
svm_conf_mat
#ROC curve:
AUC$svm <- roc(as.numeric(test$target),
               as.numeric(as.matrix((svm_prediction_prob))))$auc
AUC
# Accuracy:
accuracy$svm <- svm_conf_mat$overall["Accuracy"]
accuracy


#========================#
# Model 4: Random Forest #
#========================#
set.seed(1, sample.kind = "Rounding")
rf <- train(target ~ ., data = train,
            method = "rf",
            ntree = 500,
            preProcess = c("center", "scale"),
            trControl = train_control,
            family = "binomial",
            tuneLength = 8,
            metric = "ROC")
# Alternative method:
#rf <- randomForest(target ~ .,data = train, ntree = 500)
rf_prediction <- predict(rf, test)
rf_prediction_prob = predict(rf, test, type="prob")[2]
# Confusion matrix:
rf_conf_mat <- confusionMatrix(rf_prediction, test[,"target"])
rf_conf_mat
#ROC curve:
AUC$rf <- roc(as.numeric(test$target),
              as.numeric(as.matrix((rf_prediction_prob))))$auc
AUC
# Accuracy:
accuracy$rf <- rf_conf_mat$overall["Accuracy"]
accuracy