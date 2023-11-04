
train<-read.csv('Train.csv')
test<-read.csv('Test.csv')

attach(train)
attach(test)

dim(train)
summary(train)
View(train)


#install.packages('naniar')
library(naniar)

#-------------

library(tidyverse)
library(caTools)
#library(visdat)
library(ROCR)
library(e1071)
library(caret)


str(train)
str(test)


# replacing 0 with NA - TRAIN
train$Rating<-ifelse(train$Rating==0,NA,train$Rating)
train$Price[train$Price=='0']='null'
train$Season[train$Season=='0']='null'

# replacing 'null' with NA
train<- train %>% replace_with_na_all(condition = ~.x == 'null')
train<- train %>% replace_with_na_all(condition = ~.x == 'NULL')

# removing one row with majority values being NA
train<-train%>%
  filter(train$Dress_CODE!=100663)

# sum of null values
colSums(is.na(train))

#### TEST
View(test)

# replacing 0 with NA
test$Rating<-ifelse(test$Rating==0,NA,test$Rating)
test$Price[test$Price=='0']='null'
test$Season[test$Season=='0']='null'

# replacing 'null' with NA
test<- test %>% replace_with_na_all(condition = ~.x == 'null')
test<- test %>% replace_with_na_all(condition = ~.x == 'NULL')

# sum of null values
colSums(is.na(test))



#DATA PRE-PROCESSING 

#cleaning train and test style column data 
table(train$Style)
train$Style=as.character(train$Style)
train$Style[c(which(train$Style=="sexy"))]="Sexy"

#trainn$Style=as.character(trainn$Style)
train$Style=as.factor(train$Style)
levels(train$Style)


table(test$Style)
test$Style=as.character(test$Style)
test$Style[c(which(test$Style=="sexy"))]="Sexy"
#train$Style=as.character(train$Style)
test$Style=as.factor(test$Style)
levels(test$Style)

#cleaning train price column data
summary(train$Price)
table(train$Price)
train$Price=as.character(train$Price)
train$Price[c(which(train$Price=='low'))]='Low'
train$Price[c(which(train$Price=='high'))]='High'
train$Price=as.character(train$Price)
train$Price =as.factor(train$Price)
levels(train$Price)

#cleaning test price column data
test$Price =as.factor(test$Price)
levels(test$Price)
table(test$Price)
test$Price[c(which(test$Price=='low'))]='Low'
test$Price[c(which(test$Price=='high'))]='High'
test$Price=as.character(test$Price)
test$Price =as.factor(test$Price)
levels(test$Price)


# cleaning size train and testing 
train$Size =as.factor(train$Size)
levels(train$Size)

train$Size =as.character(train$Size)
train$Size[c(which(train$Size=='free'))]='F'
train$Size[c(which(train$Size=='small'))]='S'
train$Size[c(which(train$Size=='s'))]='S'

train$Size =as.factor(train$Size)
levels(train$Size)
table(train$Size)


table(test$Size)
summary(test$Size)
test$Size =as.character(test$Size)
test$Size[c(which(test$Size=='free'))]='F'
test$Size[c(which(test$Size=='small'))]='S'
test$Size[c(which(test$Size=='s'))]='S'
test$Size =as.factor(test$Size)
levels(test$Size)


# cleaning neckline training and test data
table(train$NeckLine)
train$NeckLine=as.factor(train$NeckLine)
levels(train$NeckLine)


train$NeckLine=as.character(train$NeckLine)
train$NeckLine[train$NeckLine == "sweetheart"] = "Sweetheart"
train$NeckLine=as.factor(train$NeckLine)
levels(train$NeckLine)
#plot(train$NeckLine)

test$NeckLine
summary(test$NeckLine)
table(test$NeckLine)
test$NeckLine=as.character(test$NeckLine)
test$NeckLine[test$NeckLine == "sweetheart"] = "Sweetheart"
test$NeckLine=as.factor(test$NeckLine)
levels(test$NeckLine)
#plot(test$NeckLine)


#checking waistline training and testing data
train$waiseline=as.factor(train$waiseline)
levels(train$waiseline)
table(train$waiseline)


test$waiseline=as.factor(test$waiseline)
levels(test$waiseline)
table(test$waiseline)
test$waiseline=as.character(test$waiseline)



# cleaning season training and testing data
train$Season=as.factor(train$Season)
levels(train$Season)
table(train$Season)

train$Season=as.character(train$Season)
train$Season[c(which(train$Season=='Automn'))]='Autumn'
train$Season[c(which(train$Season=='spring'))]='Spring'
train$Season[c(which(train$Season=='summer'))]='Summer'
train$Season[c(which(train$Season=='winter'))]='Winter'
train$Season=as.factor(train$Season)
levels(train$Season)
#plot(train$Season)
table(train$Season)


test$Season[c(which(test$Season=='Automn'))]='Autumn'
test$Season[c(which(test$Season=='spring'))]='Spring'
test$Season[c(which(test$Season=='summer'))]='Summer'
test$Season[c(which(test$Season=='winter'))]='Winter'

test$Season=as.factor(test$Season)
levels(test$Season)
#plot(test$Season)
table(test$Season)

# cleaning Sleevelength training and testing data
train$SleeveLength=as.factor(train$SleeveLength)
levels(train$SleeveLength)

table(train$SleeveLength)
train$SleeveLength=as.character(train$SleeveLength)

train$SleeveLength[train$SleeveLength %in% c('sleeevless')] = 'sleevless'
train$SleeveLength[train$SleeveLength %in% c('sleeveless')] = 'sleevless'
train$SleeveLength[train$SleeveLength %in% c('sleveless')] = 'sleevless'
train$SleeveLength[train$SleeveLength %in% c('cap-sleeves')] = 'capsleeves'
train$SleeveLength[train$SleeveLength %in% c('half')] = 'halfsleeve'
train$SleeveLength[train$SleeveLength %in% c('thressqatar')] = 'threequarter'
train$SleeveLength[train$SleeveLength %in% c('threequater')] = 'threequarter'
train$SleeveLength[train$SleeveLength == "urndowncollor"] <- "turndowncollor"
train$SleeveLength=as.factor(train$SleeveLength)
levels(train$SleeveLength)
#plot(train$SleeveLength)

test$SleeveLength=as.factor(test$SleeveLength)
levels(test$SleeveLength)

test$SleeveLength[test$SleeveLength %in% c('sleeveless')] = 'sleevless'
test$SleeveLength[test$SleeveLength %in% c('cap-sleeves')] = 'capsleeves'
test$SleeveLength[test$SleeveLength %in% c('thressqatar')] = 'threequarter'
test$SleeveLength=as.character(test$SleeveLength)
test$SleeveLength=as.factor(test$SleeveLength)
levels(test$SleeveLength)
#plot(test$SleeveLength)

# cleaning Material training and testing data
train$Material=as.factor(train$Material)
levels(train$Material)

table(train$Material)
train$Material=as.character(train$Material)
train$Material[c(which(train$Material=='modal'))]='model'
train$Material[c(which(train$Material=='sill'))]='silk'
train$Material[c(which(train$Material=='shiffon'))]='chiffonfabric'


train$Material=as.factor(train$Material)
levels(train$Material)

table(test$Material)
test$Material=as.factor(test$Material)
levels(test$Material)


# cleaning pattern type training and testing data
table(train$Pattern.Type)
train$Pattern.Type=as.character(train$Pattern.Type)


train$Pattern.Type=as.factor(train$Pattern.Type)
levels(train$Pattern.Type)

table(test$Pattern.Type)

test$Pattern.Type=as.character(test$Pattern.Type)
test$Pattern.Type=as.factor(test$Pattern.Type)
levels(test$Pattern.Type)

str(train)
str(test)

# Imputing null values using knn
library(VIM)
df_mod <- VIM::kNN(train, k = 20, numFun = laeken::weightedMean, weightDist = TRUE)
df_mod <- df_mod[,c(1:14)]
colSums(is.na(df_mod))
str(df_mod)

df_mod[-4] <- lapply(df_mod[-4], factor) 
str(df_mod)

# Imputing null values using knn
library(VIM)
df_mod_test <- VIM::kNN(test, k = 10, numFun = laeken::weightedMean, weightDist = TRUE)
df_mod_test <- df_mod_test[,c(1:13)]
colSums(is.na(df_mod_test))

df_mod_test[-4] <- lapply(df_mod_test[-4], factor) 
str(df_mod_test)
#--------------------------------------------------------------------------

# Installing package
# install.packages("caTools")       # For sampling the dataset
# install.packages("randomForest")  # For implementing random forest algorithm

# Splitting data in train and test data

df_mod <- df_mod %>% mutate(across(c(-Rating), factor))
View(df_mod)

#  SPLITTING INTO TRAIN AND TEST
library('caTools')
set.seed(123)
dt = sort(sample(nrow(df_mod), nrow(df_mod)*.7))
train<-df_mod[dt,]
test<-df_mod[-dt,]

# Fitting Random Forest to the train dataset
# Loading package
library(caTools)
library(randomForest)
set.seed(120)  # Setting seed
classifier_RF = randomForest(x = train[c(-1,-14,-2)],
                             y = train$Recommendation,
                             ntree = 490)

classifier_RF

# Predicting the Test set results
y_pred = predict(classifier_RF, newdata = test[-14])



# Confusion Matrix
confusion_mtx = table(test[,14], y_pred)
confusion_mtx
sum(diag(confusion_mtx))/sum(confusion_mtx)
# 0.6747967
# Plotting model
plot(classifier_RF)

# Importance plot
importance(classifier_RF)

# Variable importance plot
varImpPlot(classifier_RF)

#----------------------
# NAIVE BAYES
library(e1071)
classifier <- naiveBayes(train[c(-14,-10,-5, -1, -2, -12)], train$Recommendation, 
                              laplace = 1)
test_pred <- predict(classifier, test)
table <- table(test_pred, test$Recommendation)
sum(diag(table))/ sum(table)
str(train)
# 0.7

# Style is dependent on all these variables
chisq.test(Style, Price)
chisq.test(Style,NeckLine)
chisq.test(Style,SleeveLength)
chisq.test(Style,FabricType)
chisq.test(Style,Decoration)


#-------------------------------------------------
# DECISION TREE

#check the proportion of class variable
prop.table(table(train$Recommendation))
prop.table(table(test$Recommendation))

# training a model on the data
set.seed(1234)
#install.packages('C50')
library(C50)

model <- C5.0(train[c(-1,-14)],
                     train$Recommendation)

pred <- predict(model, test)
table(pred)

#step4 : evaluating model performance
library(gmodels)
CrossTable(test$Recommendation, pred,
           prop.chisq=FALSE, prop.c = FALSE, prop.r=FALSE,
           dnn=c('actual', 'predicted'))


# Boosting the accuracy of decision trees
# boosted decision tree with 10 trials
# Making some mistakes more costly than others

# build the matrix
cost_mat <- matrix(c(0, 1, 2, 0), nrow = 2)
rownames(cost_mat) <- colnames(cost_mat) <- c(0, 1)
cost_mat

#apply the cost matrix to the tree
model_cost <- C5.0(train[c(-14,-1,-10)], train$Recommendation,
                    costs=cost_mat, trials=10)

model_cost_pred <- predict(model_cost, test)

CrossTable(test$Recommendation, model_cost_pred)
82/120 # 0.6833333  
#---------------------------------------------------
# Bagging Decision Tree

library(dplyr)       #for data wrangling
library(e1071)       #for calculating variable importance
library(caret)       #for general model fitting
library(rpart)       #for fitting decision trees
library(ipred)       #for fitting bagged decision trees
#make this example reproducible
set.seed(1)
train1 <- as.data.frame(train[2:14])
str(train1)
#fit the bagged model
bag <- bagging(
  formula = train1$Recommendation ~ . ,
  data = train1,
  nbagg = 11,   
  coob = TRUE,
  control = rpart.control(minsplit = 1, cp = 0)
)

#display fitted bagged model
bag

#Bagging regression trees with 150 bootstrap replications 

predict <- predict(bag, newdata=test)
table <- table(predict, test$Recommendation)
sum(diag(table))/sum(table)
#-----------------------------------------------------
# XGBOOST
install.packages("xgboost")
library(xgboost)
library(caTools)
library(dplyr)
library(caret)

df_mod$Price <- as.numeric(df_mod$Price)
df_mod$Size <- as.numeric(df_mod$Size)
df_mod$Season <- as.numeric(df_mod$Season)
df_mod$NeckLine <- as.numeric(df_mod$NeckLine)
df_mod$SleeveLength <- as.numeric(df_mod$SleeveLength)
df_mod$waiseline <- as.numeric(df_mod$waiseline)
df_mod$Material <- as.numeric(df_mod$Material)
df_mod$FabricType <- as.numeric(df_mod$FabricType)
df_mod$Decoration <- as.numeric(df_mod$Decoration)
df_mod$Pattern.Type <- as.numeric(df_mod$Pattern.Type)
df_mod$Style <- as.numeric(df_mod$Style)

library(xgboost)
library(tidyverse)
library(caret)
library(readxl)
library(caret)
library(data.table)
install.packages("mlr")
library(mlr)

set.seed(123)
dt = sort(sample(nrow(df_mod), nrow(train)*.7))
train<-df_mod[dt,]
test<-df_mod[-dt,]
setDT(train) 
setDT(test)


labels <- train$Recommendation
ts_label <- test$Recommendation
new_tr <- model.matrix(~.+0,data = train[,-c("Recommendation"),with=F]) 
new_ts <- model.matrix(~.+0,data = test[,-c("Recommendation"),with=F])

#convert factor to numeric 
labels <- as.numeric(labels)-1
ts_label <- as.numeric(ts_label)-1
class(new_tr)

#preparing matrix 
dtrain <- xgb.DMatrix(data = new_tr,label = labels) 
dtest <- xgb.DMatrix(data = new_ts,label=ts_label)


bstSparse <- xgboost(data = dtrain, label = labels, max.depth = 2, eta = 1, nthread = 2, nrounds = 2, objective = "binary:logistic")
pred <- predict(bstSparse, dtest)
table(pred)


prediction <- as.numeric(pred > 0.55)
print(head(prediction))
table <- table(prediction,test$Recommendation)
sum(diag(table))/sum(table)

#---------------------------------------------------

# RANDOM FOREST
library(randomForest)
set.seed(1)
bag.model = randomForest(y = train$Recommendation, x = train[,c(-14,-1,-10 )],
                          mtry = 3, importance = TRUE,corr.bias=T, proximity = T, localImp = T) #default trees - 500
bag.model


#Obtain MSE on the test data set
yhat.bag = predict(bag.model, newdata = test)
table <- table(yhat.bag,test$Recommendation)
sum(diag(table))/sum(table)


#----------------------------------------------------
library(Matrix)  
sparse_matrix <- sparse.model.matrix(train$Recommendation ~ .-1, data = train)
library(xgboost)
library(readr)
library(stringr)
library(caret)
library(car)

set.seed(100)
# load data
df_train = train
df_test = test
# Loading labels of train data
labels = df_train[Recommendation]
df_train = df_train[-grep(Recommendation, colnames(df_train))]
# combine train and test data
df_all = rbind(df_train,df_test)

ohe_feats = c("Style", "Price", "Size","Season", "NeckLine","SleeveLength","waiseline","Material","FabricType","Decoration", data=train)
dummies <- dummyVars(~ Style + Price+ Size+Season+NeckLine+SleeveLength+waiseline+Material+FabricType+Decoration, data = df_all)
df_all_ohe <- as.data.frame(predict(dummies, newdata = df_all))
df_all_combined <- cbind(df_all[,-c(which(colnames(df_all) %in% ohe_feats))],df_all_ohe)df_all_combined$agena <- as.factor(ifelse(df_all_combined$age < 0,1,0))
