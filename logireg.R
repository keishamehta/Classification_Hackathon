### ------------MODEL 8 - LOGISTIC REGRESSION

# for this the data needed to be pre processed again - Not a representative sample
# Validation dataset included levels for variables which were not present in the train
# Hence these values were assigned as 'null' and then imputed using kNN as done previously


#DATA PRE-PROCESSING for LR
train<-read.csv('Train.csv')
test<-read.csv('Test.csv')

attach(train)
attach(test)

#install.packages('naniar')
library(naniar)
library('tidyverse')
library(tidyverse)
library(caTools)
#library(visdat)
library(ROCR)
library(e1071)
library(caret)

####
#DATA PRE-PROCESSING 

#cleaning train and test style column data 
table(train$Style)
train$Style=as.character(train$Style)
train$Style[c(which(train$Style=="sexy"))]="Sexy"
train$Style[c(which(train$Style=="fashion"))]="null"
train$Style[c(which(train$Style=="OL"))]="null"

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

train$NeckLine[train$NeckLine == "backless"] = "null"
train$NeckLine[train$NeckLine == "halter"] = "null"
train$NeckLine[train$NeckLine == "ruffled"] = "null"
train$NeckLine[train$NeckLine == "mandarin-collor"] = "null"


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
test$waiseline[test$waiseline=="princess"]="null"



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
train$Material[c(which(train$Material=='knitting'))]='null'
train$Material[c(which(train$Material=='sill'))]='silk'
train$Material[c(which(train$Material=='shiffon'))]='chiffonfabric'
train$Material[c(which(train$Material=='wool'))]='null'


train$Material=as.factor(train$Material)
levels(train$Material)

table(test$Material)
test$Material[c(which(test$Material=='viscos'))]='null'
test$Material=as.factor(test$Material)
levels(test$Material)


# cleaning Fabric type training and testing data
table(train$FabricType)
train$FabricType=as.character(train$FabricType)
train$FabricType[train$FabricType=='flannael']='flannel'
train$FabricType[train$FabricType=='sattin']='satin'
train$FabricType[train$FabricType=='shiffon']='chiffon'
train$FabricType[train$FabricType=='wollen']='woolen'
train$FabricType[train$FabricType=='other']='null'
train$FabricType[train$FabricType=='organza']='null'
train$FabricType[train$FabricType=='dobby']='null'


train$FabricType=as.factor(train$FabricType)
levels(train$FabricType)


table(test$FabricType)
test$FabricType[test$FabricType=='sattin']='satin'
test$FabricType[test$FabricType=='shiffon']='chiffon'
test$FabricType[test$FabricType=='knitting']='knitted'
test$FabricType[test$FabricType=='dobby']='null'


test$FabricType=as.factor(test$FabricType)
levels(test$FabricType)


# cleaning pattern type training and testing data
table(train$Pattern.Type)
train$Pattern.Type=as.character(train$Pattern.Type)
train$Pattern.Type[train$Pattern.Type=='leopard']='animal'
train$Pattern.Type[train$Pattern.Type=='floral']='null'
train$Pattern.Type[train$Pattern.Type=='none']='null'
train$Pattern.Type[train$Pattern.Type=='character']='null'


train$Pattern.Type=as.factor(train$Pattern.Type)
levels(train$Pattern.Type)

table(test$Pattern.Type)

test$Pattern.Type=as.character(test$Pattern.Type)
test$Pattern.Type[test$Pattern.Type=='leapord']='animal'
test$Pattern.Type[test$Pattern.Type=='splice']='null'
test$Pattern.Type[test$Pattern.Type=='floral']='null'

test$Pattern.Type=as.factor(test$Pattern.Type)
levels(test$Pattern.Type)

# checking train and test decoration data
table(train$Decoration)
train$Decoration=as.factor(train$Decoration)
train$Decoration[train$Decoration=='cascading']='null'
train$Decoration[train$Decoration=='draped']='null'
train$Decoration[train$Decoration=='tassel']='null'


levels(train$Decoration)

train$Decoration=as.factor(train$Decoration)
levels(train$Decoration)


#-------------------------- 
# replacing 0 with NA
train$Rating<-ifelse(train$Rating==0,NA,train$Rating)
train$Price[train$Price=='0']='null'
train$Season[train$Season=='0']='null'

# replacing 'null' with NA
train<- train %>% replace_with_na_all(condition = ~.x == 'null')
train<- train %>% replace_with_na_all(condition = ~.x == 'NULL')
str(train)


# removing one row with majority values being NA
train<-train%>%
  filter(train$Dress_CODE!=100663)
View(train)

# sum of null values
colSums(is.na(train))

# Imputing null values using knn
library(VIM)
df_mod <- VIM::kNN(train, k = 20, numFun = laeken::weightedMean, weightDist = TRUE)
df_mod <- df_mod[,c(2:14)]

str(df_mod)

df_mod$Recommendation=as.factor(df_mod$Recommendation)
df_mod$Season=as.factor(df_mod$Season)
df_mod$Price=as.factor(df_mod$Price)


str(df_mod)

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

# Imputing null values using knn
library(VIM)
df_mod_test <- VIM::kNN(test, k = 10, numFun = laeken::weightedMean, weightDist = TRUE)
df_mod_test <- df_mod_test[,c(2:13)]
colSums(is.na(df_mod_test))

str(df_mod_test)

df_mod_test$Season=as.factor(df_mod_test$Season)
df_mod_test$Price=as.factor(df_mod_test$Price)
df_mod_test$waiseline=as.factor(df_mod_test$waiseline)
df_mod_test$Decoration=as.factor(df_mod_test$Decoration)

str(df_mod_test)


#***********************************************************************************

#applying logistic regression model 
prop.table(table(df_mod$Recommendation)/length(df_mod$Recommendation)) # take thrreshold as 0.55

#install.packages('caTools')

set.seed(123)
dt = sort(sample(nrow(df_mod), nrow(df_mod)*.7))
dresstrain<-df_mod[dt,]
dressvalid<-df_mod[-dt,]

View(dresstrain)
#logistic regression model
modeltest = glm (Recommendation ~ ., data = dresstrain, family = binomial)
summary(modeltest)

#predicting for the dresstrain data 
predict_train = predict(modeltest, type = 'response')
predict_train
predict_train1=ifelse(predict_train>0.55,1,0)
predict_train1
table_mat01 = table(dresstrain$Recommendation, predict_train1)
table_mat01

#checking the accuracy for dresstrain 
accuracy_test = sum(diag(table_mat01)) / sum(table_mat01)
accuracy_test

#predicting for the dresstest data 
predict_train2 = predict(modeltest, newdata=dressvalid[,c(-13)],type = 'response')
predict_train2
predict_train3=ifelse(predict_train2>0.55,1,0)
predict_train3
table_mat = table(dressvalid$Recommendation, predict_train3)
table_mat

accuracy_test = sum(diag(table_mat)) / sum(table_mat)
accuracy_test



modeltest2 = glm (Recommendation ~., data = dresstrain[c(-9,-4,-1,-11)], family = binomial)
summary(modeltest2)
#predicting for the dresstrain data 
predict_train = predict(modeltest2, type = 'response')
predict_train
predict_train1=ifelse(predict_train>0.55,1,0)
predict_train1
table_mat01 = table(dresstrain$Recommendation, predict_train1)
table_mat01

#checking the accuracy for dresstrain 
accuracy_test = sum(diag(table_mat01)) / sum(table_mat01)
accuracy_test


#predicting for the dresstest data 
predict_train2 = predict(modeltest2, newdata=dressvalid[,c(-9,-4,-1,-11,-13)],type = 'response')
predict_train2
predict_train3=ifelse(predict_train2>0.55,1,0)
predict_train3
table_mat = table(dressvalid$Recommendation, predict_train3)
table_mat

accuracy_test = sum(diag(table_mat)) / sum(table_mat)
accuracy_test
#69.16667% accuracy