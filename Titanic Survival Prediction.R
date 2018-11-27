# Titanic Machine Learning problem
install.packages("ggplot2")
library("ggplot2")
install.packages("gdata")
library("gdata")
install.packages("plyr")
library("plyr")

# Set the working directory

setwd("D:/Sri_DataScience_Practice/Kaggle Titanic Problem")

# Read the data set
train_1 = read.csv("train.csv")
test_1 = read.csv("test.csv")

# Check the data
View(train_1)
str(train_1)
summary(train_1)

# Plot historgram to view the data
#hist(train_1$PassengerId, labels = TRUE, col = "blue")
hist(train_1$Survived, labels = TRUE, col = "blue", main = "Survived Distribution") # 2 categories
NROW(which(is.na(train_1$Survived)))

hist(train_1$Pclass, labels = TRUE, col = "green", main = "Passenger class Distribution") # 3 classes
NROW(which(is.na(train_1$Pclass))) # 0 missing values

barplot(summary(train_1$Sex), names.arg = c("Female","Male"), col = "darkred", main = "Gender wise Passenger count")
ggplot(data.frame(train_1$Sex), aes(x=train_1$Sex), colour = "dark red")+geom_bar(stat = "count") # No Missing values

hist(train_1$Age, labels = TRUE, col = "dark red", main = "Age distribution")
NROW(which(is.na(train_1$Age))) # 177 NA values in Age

hist(train_1$SibSp, labels = TRUE,
     col = "dark green", main = "Sibling Spouse Distribution")
NROW(which(is.na(train_1$SibSp))) # 0 missing values, Most of them dont have siblings
ggplot(data.frame(train_1$SibSp), aes(x=train_1$SibSp) )+geom_bar(show.legend = TRUE, stat = 'count')

hist(train_1$Parch, labels = TRUE, col = "dark green", main = "Parent & Children Distribution", xlab = "Parent/Children Count", ylab = 'Frequency')
NROW(which(is.na(train_1$Parch)))#0 missing rows
ggplot(data.frame(train_1$Parch), aes(x=train_1$Parch)) + geom_bar(show.legend = TRUE,na.rm = FALSE)


barplot(summary(train_1$Ticket))
NROW(which(trim(train_1$Ticket)=="")) # 0 rows
View((count(train_1, vars = "Ticket")))


hist(train_1$Fare, labels = TRUE, col = "dark blue", border = "red", main = 'Ticket Fare Distribution', xlab = "Fare")
NROW(which(is.na(train_1$Fare))) # 0 rows
NROW(which(trim(train_1$Fare)=="")) # 0 rows
barplot(table(train_1$Fare), legend.text = TRUE)
ggplot(data.frame(train_1$Fare), aes(x=train_1$Fare)) + geom_bar(show.legend = TRUE,stat = "count")

NROW(which(trim(train_1$Cabin)=="")) # 687 empty values. Empty Majorily
ggplot(data.frame(train_1$Cabin), aes(x=train_1$Cabin)) + geom_bar(show.legend = TRUE,na.rm = FALSE)+ geom_smooth()

NROW(which(trim(train_1$Embarked)=="")) # 2 missing values
ggplot(data.frame(train_1$Embarked), aes(x=train_1$Embarked)) + geom_bar(show.legend = TRUE, stat = "count")

View(train_1[which(trim(train_1$Embarked)==""),])
View(train_1[train_1$Ticket==113572,])
View(train_1[train_1$Fare<=80,])
View(subset(train_1, train_1$Fare<=80&train_1$Sex=="female"&substr(as.character(train_1$Cabin),1,1)=="B"&train_1$Pclass=="1", select = c("Age","Embarked","Fare","Cabin") ))

# Based on above observations, C seems to be closer value for missing value imputation
# Imputing missing value of Embarked with S
train_1$Embarked[which(trim(train_1$Embarked)=="")] = "C"
table((train_new$Age))


# Check the structure of data frame again
str(train_1)


# Create new data set excluding Name, Cabin(as it has mostly missging values)

train_new = train_1[,!colnames(train_1) %in% c("Name","Cabin")]
View(train_new)
str(train_new)

train_new$Sex = as.factor(train_new$Sex)
train_new$Ticket = as.factor(train_new$Ticket)
train_new$Embarked = as.factor(train_new$Embarked)


# Convert certain continuos variables to factors
train_new$Survived = as.factor(train_new$Survived)
train_new$Pclass = as.factor(train_new$Pclass)


#Impute missing values for age

View(train_new[which(is.na(train_new$Age)),])
str(train_new)
View(train_new)

#Convert Fare to a categorical variable
hist(train_new$Fare, labels = TRUE)

train_new$Fare = as.factor(ifelse(train_new$Fare<=50,1,ifelse(train_new$Fare<=100,2,ifelse(train_new$Fare<=150,3,ifelse(train_new$Fare<=200,4,ifelse(train_new$Fare<=250,5,ifelse(train_new$Fare<=300,6,7)))))))
ggplot(data.frame(train_new$Fare), aes(x=train_new$Fare))+geom_bar()

str(train_new)
#train_new$Sex = as.factor(ifelse(train_new$Sex=='male',1,0))
#train_new$Embarked = as.factor(ifelse(train_new$Embarked=="C",1,ifelse(train_new$Embarked=="Q",2,3)))

# write function for mode

# Create the function.
getmode <- function(v) {
  uniqv <- unique(v, incomparables = TRUE)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

train_new$Age_upd = train_new$Age

for(level1 in unique(train_new$Pclass)){
  cat(level1)
  for(level2 in unique(train_new$Sex)){
    cat(level2)
    for(level3 in unique(train_new$Embarked)){
      cat(level3)
            age_mod = getmode(as.numeric(train_new$Age[which(train_new$Pclass==level1&&train_new$Sex==level2&&train_new$Embarked==level3)]))
      train_new$Age[which(is.na(train_new$Age))]=ifelse(is.na(age_mod), getmode(train_new$Age[which(is.na(train_new$Age)==FALSE)]),age_mod)
      cat(age_mod)
          }
  }
  
}

train_new$Age[which(is.na(train_new$Age))] = getmode(train_new$Age[which(is.na(train_new$Age)==FALSE)])
str(train_new)
View(train_new$Age)
train_new$Age = as.integer(train_new$Age)


# Check for corelation 
mod_age = aov(train_new$Age~train_new$Survived) # p value & F stat value are less
summary(mod_age) # InSignificant

hist(train_new$Age, labels = TRUE, col = 'dark red')
summary(train_new$Age)
ggplot(data.frame(train_new$Age),aes(x=train_new$Age))+geom_bar(stat = "count")

# Converting AGE to categorical variable
train_new$Age = as.factor(ifelse(train_new$Age<=10,"Infant",ifelse(train_new$Age<=20,"Young",ifelse(train_new$Age<=30,"Young_Adult",ifelse(train_new$Age<=40,"Adult",ifelse(train_new$Age<=50,"Sr_Adult",ifelse(train_new$Age<=60,"Old_Adult",ifelse(train_new$Age<=70,"Old","Very_Old"))))))))
levels(train_new$Age)

# Perform ANOVA for continuos variables
str(train_new)

mod = aov(train_new$SibSp~train_new$Survived)
summary(mod) # F statistic is low & p value is high. Insignificant

mod = aov(train_new$Parch~train_new$Survived)
summary(mod) # F value is slightly high & p value is just 0.01. Slightly Significant

barplot(summary(train_new$Age))
barplot(summary(train_new$Embarked))

# Perform Chi square test on categorical variables
chisq.test(train_new$Pclass, train_new$Survived) # Significant
chisq.test(train_new$Sex, train_new$Survived) # Significant
chisq.test(train_new$Age, train_new$Survived, simulate.p.value = TRUE) # Significant
chisq.test(train_new$Embarked, train_new$Survived) # Significant
chisq.test(train_new$Fare, train_new$Survived, simulate.p.value = TRUE) # Significant
table(train_new$Fare)

# After Chi square & ANOVA, only Sibsp is insignificant





View(train_new)
train_new_upd = train_new[,colnames(train_new) %in% c("Pclass","Sex","Age","Parch","Fare","Embarked","Survived")]
View(train_new_upd)

# Perform Logistic Regression
# Generate dummy columns for each of the categorical variables 
# as logistic regression is numerical algorithm
install.packages("dummies")
library("dummies")
train_new_LR = train_new_upd
str(train_new_LR)
train_new_dummy_variables_LR = dummy.data.frame(train_new_LR, names = c("Pclass","Sex","Age","Fare","Embarked"))

View(train_new_dummy_variables_LR)
str(train_new_dummy_variables_LR)

titanic_glm = glm(train_new_dummy_variables_LR$Survived ~ train_new_dummy_variables_LR$Pclass1+train_new_dummy_variables_LR$Pclass2+train_new_dummy_variables_LR$AgeInfant+train_new_dummy_variables_LR$Sexfemale+train_new_dummy_variables_LR$EmbarkedC+train_new_dummy_variables_LR$Parch, data = train_new_dummy_variables_LR, family = "binomial", control = list(maxit=1000))
summary(titanic_glm)


train_new_LR$predict = predict(titanic_glm, train_new_LR, type = "response")
View(train_new_LR)

library(ROCR)
pred <- prediction(train_new_LR$predict, train_new_LR$Survived)
perf <- performance(pred, "tpr", "fpr")
plot(perf)
KS <- max(attr(perf, 'y.values')[[1]]-attr(perf, 'x.values')[[1]])
KS

# Prob scalling
train_new_LR$predict = ifelse(train_new_LR$predict<0.47,0,1)

#Get confusion matrix
library("caret")
confusionMatrix(train_new_LR$Survived,train_new_LR$predict) #80% accuracy

## Area Under Curve
auc <- performance(pred,"auc"); 
auc <- as.numeric(auc@y.values)
auc

#LIFT
lift <- performance(pred,"lift")
lift

## Gini Coefficient

install.packages("ineq")
library(ineq)
gini = ineq(train_new_LR$predict, type="Gini")
gini


# Run Random Forest with new data set & get important variables


install.packages("randomForest")
library("randomForest")

View(train_new_upd)
train_new_RF = train_new_upd

# Check for class balancing
table(train_new_RF$Survived)


RF_Titanic= randomForest(train_new_RF$Survived ~ ., data = train_new_RF[,-1],
                         ntree = 600, mtry=2,nodesize=5,importance = TRUE,replace=TRUE)
print(RF_Titanic)


plot(RF_Titanic, main="Random Forest Summary")
legend("topright", c("OOB", "0", "1"), text.col=1:6, lty=1:3, col=1:3)
title(main="Error Rates Random Forest RFDF.dev")

RF_Titanic$err.rate

## List the importance of the variables.
impVar <- round(randomForest::importance(RF_Titanic), 2)
impVar[order(impVar[,3], decreasing=TRUE),]
View(train_new_RF[,-c(1)])

## Tuning Random Forest
tRF <- tuneRF(x = train_new_RF[,-c(1)], 
              y=as.factor(train_new_RF$Survived),
              mtryStart = 2, 
              ntreeTry=100, 
              stepFactor = 1, 
              improve = 0.01, 
              trace=FALSE, 
              plot = TRUE,
              doBest = TRUE,
              nodesize = 50, 
              importance=FALSE
)

tRF$importance

train_new_RF$predict_class = predict(tRF, train_new_RF, type = "class")
train_new_RF$predict_prob = predict(tRF, train_new_RF, type = "prob")
View(train_new_RF)


## deciling
## deciling code
decile <- function(x){
  deciles <- vector(length=10)
  for (i in seq(0.1,1,.1)){
    deciles[i*10] <- quantile(x, i, na.rm=T)
  }
  return (
    ifelse(x<deciles[1], 1,
           ifelse(x<deciles[2], 2,
                  ifelse(x<deciles[3], 3,
                         ifelse(x<deciles[4], 4,
                                ifelse(x<deciles[5], 5,
                                       ifelse(x<deciles[6], 6,
                                              ifelse(x<deciles[7], 7,
                                                     ifelse(x<deciles[8], 8,
                                                            ifelse(x<deciles[9], 9, 10
                                                            ))))))))))
}


train_new_RF$deciles <- decile(train_new_RF$predict_prob[,2])
View(train_new_RF)


library(data.table)
tmp_DT = data.table(train_new_RF)
rank <- tmp_DT[, list(
  cnt = length(train_new_RF$Survived), 
  cnt_resp = sum(train_new_RF$Survived == 1), 
  cnt_non_resp = sum(train_new_RF$Survived == 0)) , 
  by=deciles][order(-deciles)]
rank$rrate <- round (rank$cnt_resp / rank$cnt,2);
rank$cum_resp <- cumsum(rank$cnt_resp)
rank$cum_non_resp <- cumsum(rank$cnt_non_resp)
rank$cum_rel_resp <- round(rank$cum_resp / sum(rank$cnt_resp),2);
rank$cum_rel_non_resp <- round(rank$cum_non_resp / sum(rank$cnt_non_resp),2);
rank$ks <- abs(rank$cum_rel_resp - rank$cum_rel_non_resp);
View(rank)


library(scales)
rank$rrate <- percent(rank$rrate)
rank$cum_rel_resp <- percent(rank$cum_rel_resp)
rank$cum_rel_non_resp <- percent(rank$cum_rel_non_resp)


library(ROCR)
pred <- prediction(train_new_upd$predict_prob[,2], train_new_upd$Survived)
perf <- performance(pred, "tpr", "fpr")
plot(perf)
KS <- max(attr(perf, 'y.values')[[1]]-attr(perf, 'x.values')[[1]])
KS



## Area Under Curve
auc <- performance(pred,"auc"); 
auc <- as.numeric(auc@y.values)
auc


## Gini Coefficient

install.packages("ineq")
library(ineq)
gini = ineq(train_new_upd$predict_prob[,2], type="Gini")
gini


# Implement CART model

## loading the library

library(rpart)
library(rpart.plot)
train_new_CART = train_new_upd
View(train_new_CART)
## setting the control paramter inputs for rpart
r.ctrl = rpart.control(minsplit=20, minbucket = 10, cp = 0, xval = 5)

## setting the control paramter inputs for rpart
m1 <- rpart(formula = train_new_CART$Survived ~ ., data = train_new_CART[,!colnames(train_new_upd) %in% c("Survived")], method = "class", control = r.ctrl)
m1
View(train_new_CART)


install.packages("rattle")
install.packages("RColorBrewer")

library("rattle")
library("rpart.plot")
library("RColorBrewer")
fancyRpartPlot(m1)


## to find how the tree performs
printcp(m1)
plotcp(m1)



ptree<- prune(m1, cp=m1$cptable[which.min(m1$cptable[,"xerror"]),"CP"] ,"CP")
printcp(ptree)
print(m1)
fancyRpartPlot(ptree, uniform=TRUE,  main="Pruned Classification Tree")

install.packages("caret")
library("caret")
View(train_new_CART)
confusionMatrix(train_new_CART$Survived, train_new_CART$predict_class)
#78.9 accuracy

# Support Vector Machines
library("e1071")
train_new_SVM = train_new_upd
View(train_new_SVM)
# Create dummy variables for SVM
train_new_SVM_dummy = dummies::dummy.data.frame(train_new_SVM, names = c("Pclass","Sex","Age","Parch","Fare","Embarked"))
View(train_new_SVM_dummy)

svm_titanic = svm(train_new_SVM_dummy$Survived ~ ., data = train_new_SVM_dummy)
print(svm_titanic)

train_new_SVM_dummy$svm_predict = predict(svm_titanic, train_new_SVM_dummy)
View(train_new_SVM_dummy)

confusionMatrix(train_new_SVM_dummy$svm_predict, train_new_SVM_dummy$Survived)
#83.61 Accuracy

##########################################
  
# XGBOOST 

##########################################
train_new_XGB = train_new_upd
View(train_new_upd)

# XGBOOST works only on numeric data
install.packages("xgboost")
library("xgboost")

# Convert data frame to numeric, by converting categorical variables to dummy variables
train_new_xgb_dummy = dummies::dummy.data.frame(train_new_XGB, names = c("Pclass","Sex","Parch","Age","Fare","Embarked"))
train_new_xgb_dummy = as.data.frame(train_new_xgb_dummy)
str(train_new_xgb_dummy)

param       = list("objective" = "binary:logistic", # multi class classification
                    "nthread" = 8,   			 # number of threads to be used 
                   "max_depth" = 6,    		 # maximum depth of tree 
                   "eta" = 0.05,    			 # step size shrinkage 
                   "gamma" = 0.01,    			 # minimum loss reduction 
                   "subsample" = 0.6,    		 # part of data instances to grow tree 
                   "colsample_bytree" = 0.2, 		 # subsample ratio of columns when constructing each tree 
                   "min_child_weight" = 10  		 # minimum sum of instance hessian weight needed in a child 
)

#Identify the Predictors and the dependent variable, aka label.

predictors = colnames(train_new_xgb_dummy[-1])
predictors

# xgboost works only if the numeric labels start from 0. Hence, subtract 1 from the label.

label = as.numeric(train_new_xgb_dummy[,ncol(train_new_xgb_dummy)])-1
label

print(table (label))
str(train_new_xgb_dummy)
View(train_new_xgb_dummy)

train_new_xgb_dummy[] <- lapply(train_new_xgb_dummy[,c(-1)], as.numeric)

xgbModel_Titanic = xgboost(
  param=param,
  data =as.matrix(train_new_xgb_dummy[,predictors]),
  label = as.numeric(train_new_xgb_dummy$Survived),
  nrounds=100)

summary(xgbModel_Titanic)
xgb.importance(feature_names = predictors, data = train_new_xgb_dummy, model = xgbModel_Titanic )

####################################################################################
#
# Run the models on Test data
#
####################################################################################

# Plot historgram to view the data
#hist(train_1$PassengerId, labels = TRUE, col = "blue")
View(test_1) # 418 records


hist(test_1$Pclass, labels = TRUE, col = "green") # 3 classes
NROW(which(is.na(test_1$Pclass))) # 0 missing values

barplot(summary(test_1$Sex), names.arg = c("Female","Male"), col = "darkred", main = "Gender wise Passenger count")
ggplot(data.frame(test_1$Sex), aes(x=test_1$Sex), colour = "dark red")+geom_bar(stat = "count") # No Missing values

hist(test_1$Age, labels = TRUE, col = "dark red", main = "Age distribution")
NROW(which(is.na(test_1$Age))) # 86 NA values in Age

hist(test_1$SibSp, labels = TRUE,
     col = "dark green")
NROW(which(is.na(test_1$SibSp))) # 0 missing values, Most of them dont have siblings
ggplot(data.frame(test_1$SibSp), aes(x=test_1$SibSp) )+geom_bar(show.legend = TRUE, stat = 'count')

hist(test_1$Parch, labels = TRUE, col = "dark green")
NROW(which(is.na(test_1$Parch)))
ggplot(data.frame(test_1$Parch), aes(x=test_1$Parch)) + geom_bar(show.legend = TRUE,na.rm = FALSE)


barplot(summary(test_1$Ticket))
NROW(which(trim(test_1$Ticket)=="")) # 0 rows


hist(test_1$Fare, labels = TRUE, col = "dark blue", border = "red")
NROW(which(is.na(test_1$Fare))) # 1 rows
NROW(which(trim(test_1$Fare)=="")) # 0 rows
barplot(table(test_1$Fare), legend.text = TRUE)
ggplot(data.frame(test_1$Fare), aes(x=test_1$Fare)) + geom_bar(show.legend = TRUE,stat = "count")

NROW(which(trim(test_1$Cabin)=="")) # 327 of 487 empty values. Empty Majorily
ggplot(data.frame(test_1$Cabin), aes(x=test_1$Cabin)) + geom_bar(show.legend = TRUE,na.rm = FALSE)+ geom_smooth()

NROW(which(trim(test_1$Embarked)=="")) # 0 missing values
ggplot(data.frame(test_1$Embarked), aes(x=test_1$Embarked)) + geom_bar(show.legend = TRUE, stat = "count")

# Process missing Fare value
View(test_1)
View(test_1[which(is.na(test_1$Fare)),])
View(subset(test_1, test_1$Sex=="male"&test_1$Pclass==3&test_1$Embarked=="S"&test_1$Parch==0&test_1$SibSp==0, select = c("Pclass","Embarked","Fare","Age","Cabin","Sex","Ticket") ))

# Based on above observations, $ 8 seems to be approximate value
# Ticket is high for some passengers of same age group, as overall ticket value is printed
# against each passenger. Such passengers aren't single & are a family.
# Family ticket cost is marked against each of the passenger irrespective of age.
# Imputing missing value of Embarked with S
test_1$Fare[which(is.na(test_1$Fare))] = 8


# Check the structure of data frame again
str(test_1)


# Create new data set excluding Name, Cabin (as it has mostly missging values)

test_new = test_1[,!colnames(test_1) %in% c("Name","Cabin")]
View(test_new)
str(test_new)

test_new$Sex = as.factor(test_new$Sex)
test_new$Ticket = as.factor(test_new$Ticket)
test_new$Embarked = as.factor(test_new$Embarked)


# Convert certain continuos variables to factors
test_new$Pclass = as.factor(test_new$Pclass)


#Impute missing values for age

View(test_new[which(is.na(test_new$Age)),])
str(test_new)
View(test_new)
hist(test_new$Fare, labels = TRUE)

test_new$Fare = as.factor(ifelse(test_new$Fare<=50,1,ifelse(test_new$Fare<=100,2,ifelse(test_new$Fare<=150,3,ifelse(test_new$Fare<=200,4,ifelse(test_new$Fare<=250,5,ifelse(test_new$Fare<=300,6,7)))))))
ggplot(data.frame(test_new$Fare), aes(x=test_new$Fare))+geom_bar()



# write function for mode


test_new$Age_upd = test_new$Age
str(test_new)

for(level1 in unique(test_new$Pclass)){
  cat(level1)
  for(level2 in unique(test_new$Sex)){
    cat(level2)
    for(level3 in unique(test_new$Embarked)){
      cat(level3)
      age_mod = getmode(as.numeric(test_new$Age[which(test_new$Pclass==level1&&test_new$Sex==level2&&test_new$Survived==level3)]))
      test_new$Age[which(is.na(test_new$Age))]=ifelse(is.na(age_mod), getmode(test_new$Age[which(is.na(test_new$Age)==FALSE)]),age_mod)
      cat(age_mod)
    }
  }
  
}

test_new$Age[which(is.na(test_new$Age))] = getmode(test_new$Age[which(is.na(test_new$Age)==FALSE)])
View(test_new)

test_new$Age = as.integer(test_new$Age)


hist(test_new$Age, labels = TRUE, col = 'dark red')
summary(test_new$Age)
ggplot(data.frame(test_new$Age),aes(x=test_new$Age))+geom_bar(stat = "count")

# Converting AGE to categorical variable
test_new$Age = as.factor(ifelse(test_new$Age<=10,"Infant",ifelse(test_new$Age<=20,"Young",ifelse(test_new$Age<=30,"Young_Adult",ifelse(test_new$Age<=40,"Adult",ifelse(test_new$Age<=50,"Sr_Adult",ifelse(test_new$Age<=60,"Old_Adult",ifelse(test_new$Age<=70,"Old","Very_Old"))))))))
str(test_new)


# Prepare final data set
test_new_upd = test_new[,colnames(test_new) %in% c("Pclass","Sex","Age","Parch","Fare","Embarked")]
View(test_new_upd)

#Predict using Logistic Regression
# Create numerical data set
test_new_upd_dummy = dummy.data.frame(test_new_upd, names = c("Pclass","Sex","Age","Fare","Embarked"))
test_new_upd_dummy_Imp_var = test_new_upd_dummy[,c("Pclass1","Pclass2","AgeInfant","Sexfemale","EmbarkedC","Parch")]
test_new_upd_dummy$predict = predict(titanic_glm, newdata = test_new_upd_dummy_Imp_var )
View(test_new_upd_dummy_Imp_var)


# Use SVM 
View(test_new_upd_dummy)
predictors
test_new_upd_dummy[] = lapply(test_new_upd_dummy, as.numeric)
test_new_upd_dummy$Survived = predict(xgbModel_Titanic,as.matrix(test_new_upd_dummy ))

test_new_upd_dummy$Survived = ifelse(test_new_upd_dummy$Survived<0.27,0,1)
test_new_upd$PassengerId = test_1$PassengerId
test_new_upd$Survived = test_new_upd_dummy$Survived
test_new_final = test_new_upd[,c("PassengerId","Survived")]
View(test_new_final)
# Write output to csv
write.csv(test_new_final,file = "test_final.csv", col.names = TRUE)
