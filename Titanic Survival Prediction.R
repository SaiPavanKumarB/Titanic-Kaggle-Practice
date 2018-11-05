# Titanic Machine Learning problem
install.packages("ggplot2")
library("ggplot2")

# Set the working directory

setwd("D:/Sri_DataScience_Practice/Titanic Machine Learning Kaggle")

# Read the data set
train = read.csv("train.csv")
test = read.csv("test.csv")

# Check the data
View(train)
str(train)
summary(train)

# Plot historgram to view the data
hist(train$PassengerId)
hist(train$Survived) # 2 categories
hist(train$Pclass) # 3 classes
NROW(which(is.na(train$Pclass))) # 0 missing values

barplot(summary(train$Sex))
ggplot(data.frame(train$Sex), aes(x=train$Sex))+geom_bar() # No Missing values

hist(train$Age)
NROW(which(is.na(train$Age))) # 177 NA values in Age

hist(train$SibSp)
NROW(which(is.na(train$SibSp))) # 0 missing values
ggplot(data.frame(train$SibSp), aes(x=train$SibSp))+geom_bar(show.legend = TRUE, na.rm = TRUE)

hist(train$Parch)
NROW(which(is.na(train$Parch)))
ggplot(data.frame(train$Parch), aes(x=train$Parch)) + geom_bar(show.legend = TRUE,na.rm = FALSE) 

hist(train$Ticket)
NROW(which(trimws(train$Ticket)=="")) # 0 rows


hist(train$Fare)
NROW(which(is.na(train$Fare))) # 0 rows
ggplot(data.frame(train$Fare), aes(x=train$Fare)) + geom_bar(show.legend = TRUE,na.rm = FALSE)

NROW(which(trimws(train$Cabin)=="")) # 687 empty values. Empty Majorily
ggplot(data.frame(train$Cabin), aes(x=train$Cabin)) + geom_bar(show.legend = TRUE,na.rm = FALSE)

NROW(which(trimws(train$Embarked)=="")) # 2 missing values
ggplot(data.frame(train$Embarked), aes(x=train$Embarked)) + geom_bar(show.legend = TRUE,na.rm = FALSE)

View(train[which(trimws(train$Embarked)==""),])
View(train[train$Ticket==113572,])

# Imputing missing value of Embarked with S
train$Embarked[which(trimws(train$Embarked)=="")] = "S"


# Check the structure of data frame again
str(train)

# Create new data set excluding Name, Cabin (as it has mostly missging values)

train_new = train[,!colnames(train) %in% c("Name","Cabin")]
View(train_new)
str(train_new)

train_new$Sex = as.factor(train_new$Sex)
train_new$Ticket = as.factor(train$Ticket)
train_new$Embarked = as.factor(train_new$Embarked)

# Create dummy columns for factor variables.
install.packages("fastDummies")
library("fastDummies")

str(train_new)
train_new_dummy = fastDummies::dummy_columns(train_new)
View(train_new_dummy)

train_new$Survived = as.factor(train_new$Survived)



# Perform ANOVA for continuos variables
str(train_new)
mod = aov(train_new$Fare~train_new$Survived)
summary(mod)

# F Value istrain_new high & p value is low. Significant
TukeyHSD(mod, conf.level = 0.95)

#Impute missing values for age

View(train_new[which(is.na(train_new$Age)),])
table(train_new$Pclass, train_new$Survived)

train_new$Embarked = as.factor(train_new$Embarked)
train_new$Pclass = as.factor(train_new$Pclass)
train_new$Fare = as.factor(ifelse(train_new$Fare<=100,1,ifelse(train_new$Fare<=200,2,ifelse(train_new$Fare<=300,3,ifelse(train_new$Fare<=400,4,ifelse(train_new$Fare<=500,5,6))))))
ggplot(data.frame(train_new$Fare), aes(x=train_new$Fare))+geom_bar()

train_new$Sex = as.factor(ifelse(train_new$Sex=='male',1,0))
train_new$Embarked = as.factor(ifelse(train_new$Embarked=="C",1,ifelse(train_new$Embarked=="Q",2,3)))

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

hist(train_new$Age)
summary(train_new$Age)
ggplot(data.frame(train_new$Age),aes(x=train_new$Age))+geom_bar()

# Converting AGE to categorical variable
train_new$Age = as.factor(ifelse(train_new$Age<=10,"Infant",ifelse(train_new$Age<=25,"Young",ifelse(train_new$Age<=40,"Adult",ifelse(train_new$Age<=60,"Elder","Old")))))
str(train_new)

# Perform Chi square test on categorical variables
chisq.test(train_new$Sex, train_new$Survived) # Significant
chisq.test(train_new$Age, train_new$Survived) # Significant
chisq.test(train_new$Pclass, train_new$Survived) # Significant
chisq.test(train_new$Fare, train_new$Survived) # Significant
chisq.test(train_new$Embarked, train_new$Survived) # Significant

# Run Random Forest with new data set & get important variables

View(train_new)
train_new_upd = train_new[,!colnames(train_new) %in% c("Age_upd","Ticket")]
View(train_new_upd)

install.packages("randomForest")
library("randomForest")

RF_Titanic= randomForest(train_new_upd$Survived ~ ., data = train_new_upd[,-2],
                         ntree = 500, mtry=3,nodesize=10,importance = TRUE)
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
              mtryStart = 3, 
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
m1 <- rpart(formula = train_new_upd$Survived ~ ., data = train_new_upd[,!colnames(train_new_upd) %in% c("Survived","predict_class","predict_prob","deciles")], method = "class", control = r.ctrl)
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
fancyRpartPlot(ptree, uniform=TRUE,  main="Pruned Classification Tree")

install.packages("caret")
library("caret")
confusionMatrix(train_new_upd$Survived, train_new_upd$predict_class)

# Support Vector Machines
library("e1071")

svm_titanic = svm(train_new_upd$Survived ~ train_new_upd$PassengerId+train_new_upd$Pclass+train_new_upd$Sex+train_new_upd$Age+train_new_upd$SibSp+train_new_upd$Parch+train_new_upd$Fare+train_new_upd$Embarked, data = train_new_upd[,!colnames(train_new_upd) %in% c("Survived","predict_class","predict_prob","deciles")])

train_new_upd$svm_predict = predict(svm_titanic, train_new_upd)
View(train_new_upd)

confusionMatrix(train_new_upd$svm_predict, train_new_upd$Survived)

# XGBOOST

train_new_upd_xgb = train_new_upd

# XGBOOST works only on numeric data
install.packages("xgboost")
library("xgboost")

# Convert data frame to numeric
train_new_upd_xgb = as.data.frame(train_new_upd_xgb[,!colnames(train_new_upd_xgb) %in% c("predict_class","predict_prob","deciles")])

param       = list("objective" = "binary:logistic", # multi class classification
                   "num_class"= 2 ,  		# Number of classes in the dependent variable.
                   "eval_metric" = "Logloss",  	 # evaluation metric 
                   "nthread" = 8,   			 # number of threads to be used 
                   "max_depth" = 6,    		 # maximum depth of tree 
                   "eta" = 0.05,    			 # step size shrinkage 
                   "gamma" = 0.01,    			 # minimum loss reduction 
                   "subsample" = 0.8,    		 # part of data instances to grow tree 
                   "colsample_bytree" = 0.5, 		 # subsample ratio of columns when constructing each tree 
                   "min_child_weight" = 50  		 # minimum sum of instance hessian weight needed in a child 
)

#Identify the Predictors and the dependent variable, aka label.

predictors = colnames(train_new_upd_xgb[-ncol(train_new_upd_xgb)])
predictors
#xgboost works only if the labels are numeric. Hence, convert the labels (Response) to numeric.
unique(train_new_upd_xgb$Age)

train_new_upd_xgb$Age = as.factor(ifelse(train_new_upd_xgb$Age=="Infant",1,ifelse(train_new_upd_xgb$Age=="Young",2,ifelse(train_new_upd_xgb$Age=="Adult",3,ifelse(train_new_upd_xgb$Age=="Elder",4,5)))))

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

