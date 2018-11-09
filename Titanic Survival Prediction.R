# Titanic Machine Learning problem
install.packages("ggplot2")
library("ggplot2")
install.packages("gdata")
library("gdata")

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
hist(train_1$Survived, labels = TRUE, col = "blue") # 2 categories
NROW(which(is.na(train_1$Survived)))

hist(train_1$Pclass, labels = TRUE, col = "green") # 3 classes
NROW(which(is.na(train_1$Pclass))) # 0 missing values

barplot(summary(train_1$Sex), names.arg = c("Female","Male"), col = "darkred", main = "Gender wise Passenger count")
ggplot(data.frame(train_1$Sex), aes(x=train_1$Sex), colour = "dark red")+geom_bar(stat = "count") # No Missing values

hist(train_1$Age, labels = TRUE, col = "dark red", main = "Age distribution")
NROW(which(is.na(train_1$Age))) # 177 NA values in Age

hist(train_1$SibSp, labels = TRUE,
     col = "dark green")
NROW(which(is.na(train_1$SibSp))) # 0 missing values, Most of them dont have siblings
ggplot(data.frame(train_1$SibSp), aes(x=train_1$SibSp) )+geom_bar(show.legend = TRUE, stat = 'count')

hist(train_1$Parch, labels = TRUE, col = "dark green")
NROW(which(is.na(train_1$Parch)))
ggplot(data.frame(train_1$Parch), aes(x=train_1$Parch)) + geom_bar(show.legend = TRUE,na.rm = FALSE)


barplot(summary(train_1$Ticket))
NROW(which(trim(train_1$Ticket)=="")) # 0 rows


hist(train_1$Fare, labels = TRUE, col = "dark blue", border = "red")
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


# Check the structure of data frame again
str(train_1)


# Create new data set excluding Name, Cabin (as it has mostly missging values)

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
hist(train_new$Fare, labels = TRUE)

train_new$Fare = as.factor(ifelse(train_new$Fare<=50,1,ifelse(train_new$Fare<=100,2,ifelse(train_new$Fare<=150,3,ifelse(train_new$Fare<=200,4,ifelse(train_new$Fare<=250,5,ifelse(train_new$Fare<=300,6,7)))))))
ggplot(data.frame(train_new$Fare), aes(x=train_new$Fare))+geom_bar()

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
    for(level3 in unique(train_new$Survived)){
      cat(level3)
            age_mod = getmode(as.numeric(train_new$Age[which(train_new$Pclass==level1&&train_new$Sex==level2&&train_new$Survived==level3)]))
      train_new$Age[which(is.na(train_new$Age))]=ifelse(is.na(age_mod), getmode(train_new$Age[which(is.na(train_new$Age)==FALSE)]),age_mod)
      cat(age_mod)
          }
  }
  
}

train_new$Age[which(is.na(train_new$Age))] = getmode(train_new$Age[which(is.na(train_new$Age)==FALSE)])
View(train_new)

mod_age = aov(train_new$Age~train_new$Survived) # p value & F stat value are less
summary(mod_age) # InSignificant

hist(train_new$Age, labels = TRUE, col = 'dark red')
summary(train_new$Age)
ggplot(data.frame(train_new$Age),aes(x=train_new$Age))+geom_bar(stat = "count")

# Converting AGE to categorical variable
train_new$Age = as.factor(ifelse(train_new$Age<=10,"Infant",ifelse(train_new$Age<=20,"Young",ifelse(train_new$Age<=30,"Young Adult",ifelse(train_new$Age<=40,"Adult",ifelse(train_new$Age<=50,"Sr Adult",ifelse(train_new$Age<=60,"Old Adult",ifelse(train_new$Age<=70,"Old","Very Old"))))))))
str(train_new)

# Perform ANOVA for continuos variables
str(train_new)

mod = aov(train_new$SibSp~train_new$Survived)
summary(mod) # F statistic is low & p value is high. Insignificant

mod = aov(train_new$Parch~train_new$Survived)
summary(mod) # F value is slightly high & p value is just 0.01. Slightly Significant

# Perform Chi square test on categorical variables
chisq.test(train_new$Pclass, train_new$Survived) # Significant
chisq.test(train_new$Sex, train_new$Survived) # Significant
chisq.test(train_new$Age, train_new$Survived) # Significant
chisq.test(train_new$Embarked, train_new$Survived) # Significant
chisq.test(train_new$Fare, train_new$Survived) # Significant

# After Chi square & ANOVA, only Sibsp is insignificant

# Run Random Forest with new data set & get important variables

View(train_new)
train_new_upd = train_new[,colnames(train_new) %in% c("PassengerId","Pclass","Sex","Age","Parch","Fare","Embarked","Survived")]
View(train_new_upd)

install.packages("randomForest")
library("randomForest")


RF_Titanic= randomForest(train_new_upd$Survived ~ ., data = train_new_upd[,-2],
                         ntree = 600, mtry=2,nodesize=10,importance = TRUE,replace=TRUE)
print(RF_Titanic)


plot(RF_Titanic, main="")
legend("topright", c("OOB", "0", "1"), text.col=1:6, lty=1:3, col=1:3)
title(main="Error Rates Random Forest RFDF.dev")

RF_Titanic$err.rate

## List the importance of the variables.
impVar <- round(randomForest::importance(RF_Titanic), 2)
impVar[order(impVar[,3], decreasing=TRUE),]


## Tuning Random Forest
tRF <- tuneRF(x = train_new_upd[,-c(2)], 
              y=as.factor(train_new_upd$Survived),
              mtryStart = 2, 
              ntreeTry=100, 
              stepFactor = 2, 
              improve = 0.001, 
              trace=FALSE, 
              plot = FALSE,
              doBest = TRUE,
              nodesize = 150, 
              importance=FALSE
)

tRF$importance

train_new_upd$predict_class = predict(tRF, train_new_upd, type = "class")
train_new_upd$predict_prob = predict(tRF, train_new_upd, type = "prob")
View(train_new_upd)


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


train_new_upd$deciles <- decile(train_new_upd$predict_prob[,2])
View(train_new_upd)


library(data.table)
tmp_DT = data.table(train_new_upd)
rank <- tmp_DT[, list(
  cnt = length(train_new_upd$Survived), 
  cnt_resp = sum(train_new_upd$Survived == 1), 
  cnt_non_resp = sum(train_new_upd$Survived == 0)) , 
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


## setting the control paramter inputs for rpart
r.ctrl = rpart.control(minsplit=100, minbucket = 10, cp = 0, xval = 5)

## setting the control paramter inputs for rpart
m1 <- rpart(formula = train_new_upd$Survived ~ ., data = train_new_upd[,!colnames(train_new_upd) %in% c("Survived","predict_class","predict_prob","deciles","PassengerId")], method = "class", control = r.ctrl)
m1
View(train_new_upd)


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
confusionMatrix(train_new_upd$Survived, train_new_upd$predict_class)
#78.9 accuracy

# Support Vector Machines
library("e1071")

svm_titanic = svm(train_new_upd$Survived ~ train_new_upd$Pclass+train_new_upd$Sex+train_new_upd$Age+train_new_upd$Parch+train_new_upd$Fare+train_new_upd$Embarked, data = train_new_upd[,!colnames(train_new_upd) %in% c("Survived","predict_class","predict_prob","deciles","PassengerId")])
print(svm_titanic)

train_new_upd$svm_predict = predict(svm_titanic, train_new_upd)
View(train_new_upd)

confusionMatrix(train_new_upd$svm_predict, train_new_upd$Survived)
#79.24 Accuracy

# XGBOOST

train_new_upd_xgb = train_new_upd

# XGBOOST works only on numeric data
install.packages("xgboost")
library("xgboost")

# Convert data frame to numeric
train_new_upd_xgb = as.data.frame(train_new_upd_xgb[,!colnames(train_new_upd_xgb) %in% c("predict_class","predict_prob","deciles")])

param       = list("objective" = "binary:logistic", # multi class classification
                   "num_class"= 2 ,  		# Number of classes in the dependent variable.
                                     "nthread" = 8,   			 # number of threads to be used 
                   "max_depth" = 6,    		 # maximum depth of tree 
                   "eta" = 0.05,    			 # step size shrinkage 
                   "gamma" = 0.01,    			 # minimum loss reduction 
                   "subsample" = 0.8,    		 # part of data instances to grow tree 
                   "colsample_bytree" = 0.4, 		 # subsample ratio of columns when constructing each tree 
                   "min_child_weight" = 50  		 # minimum sum of instance hessian weight needed in a child 
)

#Identify the Predictors and the dependent variable, aka label.

predictors = colnames(train_new_upd_xgb[-ncol(train_new_upd_xgb)])
predictors
#xgboost works only if the labels are numeric. Hence, convert the labels (Response) to numeric.
unique(train_new_upd_xgb$Age)

train_new_upd_xgb$Age = as.factor(ifelse(train_new_upd_xgb$Age=="Infant",1,ifelse(train_new_upd_xgb$Age=="Young",2,ifelse(train_new_upd_xgb$Age=="Young Adult",3,ifelse(train_new_upd_xgb$Age=="Adult",4,ifelse(train_new_upd_xgb$Age=="Sr Adult",5,ifelse(train_new_upd_xgb$Age=="Old Adult",6,ifelse(train_new_upd_xgb$Age=="Old",7,8))))))))

label = as.numeric(train_new_upd_xgb[,ncol(train_new_upd_xgb)])-1
print(table(label))
train_new_upd_xgb$Pclass=as.numeric(train_new_upd_xgb$Pclass)
train_new_upd_xgb$Age=as.numeric(train_new_upd_xgb$Age)
train_new_upd_xgb$Sex=as.numeric(train_new_upd_xgb$Sex)
train_new_upd_xgb$Fare=as.numeric(train_new_upd_xgb$Fare)
train_new_upd_xgb$Embarked=as.numeric(train_new_upd_xgb$Embarked)
train_new_upd_xgb=train_new_upd_xgb[,!colnames(train_new_upd_xgb) %in% c("svm_predict")]
train_new_upd_xgb$Survived=as.numeric(train_new_upd_xgb$Survived)


xgbModel = xgboost(
  param=param,
  data =as.matrix(train_new_upd_xgb[,predictors]),
  label = train_new_upd_xgb$Survived,
  nrounds=100)

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

for(level1 in unique(test_new$Pclass)){
  cat(level1)
  for(level2 in unique(test_new$Sex)){
    cat(level2)
    for(level3 in unique(test_new$Survived)){
      cat(level3)
      age_mod = getmode(as.numeric(test_new$Age[which(test_new$Pclass==level1&&test_new$Sex==level2&&test_new$Survived==level3)]))
      test_new$Age[which(is.na(test_new$Age))]=ifelse(is.na(age_mod), getmode(test_new$Age[which(is.na(test_new$Age)==FALSE)]),age_mod)
      cat(age_mod)
    }
  }
  
}

test_new$Age[which(is.na(test_new$Age))] = getmode(test_new$Age[which(is.na(test_new$Age)==FALSE)])
View(test_new)


hist(test_new$Age, labels = TRUE, col = 'dark red')
summary(test_new$Age)
ggplot(data.frame(test_new$Age),aes(x=test_new$Age))+geom_bar(stat = "count")

# Converting AGE to categorical variable
test_new$Age = as.factor(ifelse(test_new$Age<=10,"Infant",ifelse(test_new$Age<=20,"Young",ifelse(test_new$Age<=30,"Young Adult",ifelse(test_new$Age<=40,"Adult",ifelse(test_new$Age<=50,"Sr Adult",ifelse(test_new$Age<=60,"Old Adult",ifelse(test_new$Age<=70,"Old","Very Old"))))))))
str(test_new)


# Prepare final data set
test_new_upd = test_new[,colnames(test_new) %in% c("PassengerId","Pclass","Sex","Age","Parch","Fare","Embarked")]
View(test_new_upd)

#Predict using SVM
svm_titanic = svm(train_new_upd$Survived ~ train_new_upd$Pclass+train_new_upd$Sex+train_new_upd$Age+train_new_upd$Parch+train_new_upd$Fare+train_new_upd$Embarked, data = train_new_upd)
test_new_upd$Survived<-NA
m<-predict(svm_titanic, newdata=test_new_upd)
View(test_new_upd[,-1])
View(train_new_upd)

confusionMatrix(train_new_upd$svm_predict, train_new_upd$Survived)
