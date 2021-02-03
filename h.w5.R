###Remove Unwanted Columns###
CENTURY.d <- subset(CENTURY, select = -c(fontVariant, m_label, orientation, m_top, m_left, originalH, originalW, h, w))
EBRIMA.d <- subset(EBRIMA, select = -c(fontVariant, m_label, orientation, m_top, m_left, originalH, originalW, h, w))
GILL.d <- subset(GILL, select = -c(fontVariant, m_label, orientation, m_top, m_left, originalH, originalW, h, w))
#Three classes of images of "normal" characters
CL1 <- subset(CENTURY.d, strength == 0.4 & italic == 0)
CL2 <- subset(EBRIMA.d, strength == 0.4 & italic == 0)
CL3 <- subset(GILL.d, strength == 0.4 & italic == 0)
###Combine CL1,CL2,CL3 to get DATA###
DATA_full <- rbind(CL1,CL2,CL3)
fonts <- DATA_full$font
DATA <- DATA_full[,-c(1:3)]#omit font, strength, italic columns
#Standardize features in full data set
nor <-function(x) { (x -mean(x))/sd(x)   } #function to standardize
sdata <- as.data.frame(lapply(DATA, nor))
#PCA
res.pca <- prcomp(sdata, scale = FALSE)
#New Data with 103 principal components
newdata<- res.pca$x[,1:103]

#Change structure 
#install.packages("dplyr")
library(dplyr)
newdata <- as.data.frame(newdata)
newdata %>% mutate_if(is.character,as.numeric)
fonts <- factor(fonts)
newdata <- cbind(newdata,fonts)

#Three classes 
class1 <- subset(newdata, fonts == 'CENTURY')
class2 <- subset(newdata, fonts == 'EBRIMA')
class3 <- subset(newdata, fonts == 'GILL')

#Train Test Split for cl1
set.seed(101)
n = nrow(class1) - 350
trainIndex = sample(1:n, size = round(0.8*n), replace=FALSE)
traincl1 = class1[trainIndex ,]
testcl1 = class1[-trainIndex ,]

#Train Test Split for cl2
set.seed(102)
n = nrow(class2)
trainIndex1 = sample(1:n, size = round(0.8*n), replace=FALSE)
traincl2 = class2[trainIndex1 ,]
testcl2 = class2[-trainIndex1 ,]

#Train Test Split for cl3
set.seed(103)
n = nrow(class3)
trainIndex2 = sample(1:n, size = round(0.8*n), replace=FALSE)
traincl3 = class3[trainIndex2 ,]
testcl3 = class3[-trainIndex2 ,]


#Full training set and test set
trainset <- rbind(traincl1,traincl2,traincl3)
testset <- rbind(testcl1,testcl2,testcl3)

##extract font label column of training and test set
test_category <- testset[,104]
train_category <- trainset[,104]

trainset1 <- trainset[,1:103]
testset1 <- testset[,1:103]

#Random Forests
#install.packages("randomForest")
library(randomForest)

rf = randomForest(fonts  ~., data=trainset, ntree=300, mtry=10,importance=TRUE)

#Predictions for test set
pred <- predict(rf,newdata=testset1)
pred
#Confusion matrix for Test set 
conf.ma <- table(test_category,pred)
conf.ma

#Formula calculates global accuracy
globacc <- function(x) {(x)/rowSums(x)*100}
#Function to calculate accuracy
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}


#Random Forest for trainset (ntree = 100)
rf_train_100 = randomForest(train_category ~., data=trainset1, ntree=100, mtry=10,importance=TRUE)
rf_train_100

#Random Forest for testset (ntree = 100)
rf_test_100 = randomForest(test_category ~., data=testset1, ntree=100, mtry=10,importance=TRUE)
rf_test_100

##Further Exploration of Random Forest##
#Random Forest for testset (ntree = 10)
rf_test_10 = randomForest(test_category ~., data=testset1, ntree=10, mtry=10,importance=TRUE)
rf_test_10

#Random Forest for testset (ntree = 50)
rf_test_50 = randomForest(test_category ~., data=testset1, ntree=50, mtry=10,importance=TRUE)
rf_test_50

#Random Forest for testset (ntree = 200)
rf_test_200 = randomForest(test_category ~., data=testset1, ntree=200, mtry=10,importance=TRUE)
rf_test_200

#Random Forest for testset (ntree = 300)
rf_test_300 = randomForest(test_category ~., data=testset1, ntree=300, mtry=10,importance=TRUE)
rf_test_300

#Random Forest for testset (ntree = 400)
rf_test_400 = randomForest(test_category ~., data=testset1, ntree=400, mtry=10,importance=TRUE)
rf_test_400


#Confusion matrix in percents
round(globacc(conf.ma),0)
#Accuracy for predictions of test set
accuracy(conf.ma)

##Accuracy of RF ntree = 10, test set
rf_mat_10 = rf_test_10$confusion
testperf_10 = sum(rf_mat_10[1,1], rf_mat_10[2,2], rf_mat_10[3,3])/nrow(testset1)

##Accuracy of RF ntree = 50, test set
rf_mat_50 = rf_test_50$confusion
testperf_50 = sum(rf_mat_50[1,1], rf_mat_50[2,2], rf_mat_50[3,3])/nrow(testset1)

##Accuracy of RF ntree = 100, test set
rf_mat_100 = rf_test_100$confusion
testperf_100 = sum(rf_mat_100[1,1], rf_mat_100[2,2], rf_mat_100[3,3])/nrow(testset1)
testperf_100

##Accuracy of RF ntree = 200, test set
rf_mat_200 = rf_test_200$confusion
testperf_200 = sum(rf_mat_200[1,1], rf_mat_200[2,2], rf_mat_200[3,3])/nrow(testset1)
testperf_200

##Accuracy of RF ntree = 300, test set
rf_mat_300 = rf_test_300$confusion
testperf_300 = sum(rf_mat_300[1,1], rf_mat_300[2,2], rf_mat_300[3,3])/nrow(testset1)
testperf_300

##Accuracy of RF ntree = 400, test set
rf_mat_400 = rf_test_400$confusion
testperf_400 = sum(rf_mat_400[1,1], rf_mat_400[2,2], rf_mat_400[3,3])/nrow(testset1)
testperf_400

##Plot Accuracy v. ntrees
ntrees = c(10,50,100,200,300,400)
testperfs = c(testperf_10, testperf_50,testperf_100, testperf_200, testperf_300, testperf_400)
plot(ntrees, testperfs, xlim = c(1,415),ylim = c(.45,.75) ,type = "b", col = "blue", xlab = "N Trees", ylab = "Accuracy")

##Get Accuracy for each font 
##Century Accuracy of RF ntree = 10, test set
centuryperf_10 = rf_mat_10[1,1]/sum(rf_mat_10[1,1:3])

##Century Accuracy of RF ntree = 50, test set
centuryperf_50 = rf_mat_50[1,1]/sum(rf_mat_50[1,1:3])

##Century Accuracy of RF ntree = 100, test set
centuryperf_100 = rf_mat_100[1,1]/sum(rf_mat_100[1,1:3])

##Century Accuracy of RF ntree = 200, test set
centuryperf_200 = rf_mat_200[1,1]/sum(rf_mat_200[1,1:3])

##Century Accuracy of RF ntree = 300, test set
centuryperf_300 = rf_mat_300[1,1]/sum(rf_mat_300[1,1:3])

##Century Accuracy of RF ntree = 400, test set
centuryperf_400 = rf_mat_400[1,1]/sum(rf_mat_400[1,1:3])

##Plot Century Accuracy v. ntree
centuryperfs = c(centuryperf_10, centuryperf_50, centuryperf_100, centuryperf_200, centuryperf_300, centuryperf_400)
plot(ntrees, centuryperfs, xlim = c(1,415),ylim = c(.55,.8) ,type = "b", col = "green", xlab = "N Trees", ylab = "Accuracy", main = "Century Accuracy v. N Trees")

##ebrima Accuracy of RF ntree = 10, test set
ebrimaperf_10 = rf_mat_10[2,2]/sum(rf_mat_10[2,1:3])

##ebrima Accuracy of RF ntree = 50, test set
ebrimaperf_50 = rf_mat_50[2,2]/sum(rf_mat_50[2,1:3])

##ebrima Accuracy of RF ntree = 100, test set
ebrimaperf_100 = rf_mat_100[2,2]/sum(rf_mat_100[2,1:3])

##ebrima Accuracy of RF ntree = 200, test set
ebrimaperf_200 = rf_mat_200[2,2]/sum(rf_mat_200[2,1:3])

##ebrima Accuracy of RF ntree = 300, test set
ebrimaperf_300 = rf_mat_300[2,2]/sum(rf_mat_300[2,1:3])

##ebrima Accuracy of RF ntree = 400, test set
ebrimaperf_400 = rf_mat_400[2,2]/sum(rf_mat_400[2,1:3])

##Plot ebrima Accuracy v. ntree
ebrimaperfs = c(ebrimaperf_10, ebrimaperf_50, ebrimaperf_100, ebrimaperf_200, ebrimaperf_300, ebrimaperf_400)
plot(ntrees, ebrimaperfs, xlim = c(1,415),ylim = c(.35,.6) ,type = "b", col = "red", xlab = "N Trees", ylab = "Accuracy", main = "Ebrima Accuracy v. N Trees")

##gill Accuracy of RF ntree = 10, test set
gillperf_10 = rf_mat_10[3,3]/sum(rf_mat_10[3,1:3])

##gill Accuracy of RF ntree = 50, test set
gillperf_50 = rf_mat_50[3,3]/sum(rf_mat_50[3,1:3])

##gill Accuracy of RF ntree = 100, test set
gillperf_100 = rf_mat_100[3,3]/sum(rf_mat_100[3,1:3])

##gill Accuracy of RF ntree = 200, test set
gillperf_200 = rf_mat_200[3,3]/sum(rf_mat_200[3,1:3])

##gill Accuracy of RF ntree = 300, test set
gillperf_300 = rf_mat_300[3,3]/sum(rf_mat_300[3,1:3])

##gill Accuracy of RF ntree = 400, test set
gillperf_400 = rf_mat_400[3,3]/sum(rf_mat_400[3,1:3])

##Plot gill Accuracy v. ntree
gillperfs = c(gillperf_10, gillperf_50, gillperf_100, gillperf_200, gillperf_300, gillperf_400)
plot(ntrees, gillperfs, xlim = c(1,415),ylim = c(.45,.8) ,type = "b", col = "purple", xlab = "N Trees", ylab = "Accuracy", main = "Gill Accuracy v. N Trees")

testperf_100



#convert fonts to century=1 or not century=0
test_category1 <- as.character(test_category)
train_category1 <- as.character(train_category)
#C1 versus {C2+C3} train set
train_category2 <- c()
for (i in 1:length(train_category1)){
  if(train_category1[i] == "CENTURY") {
    train_category2 <- append(train_category2, 1)
  } 
  else {
    train_category2 <- append(train_category2, 0)
  }
}
#C1 versus {C2+C3} test set
test_category2 <- c()
for (i in 1:length(test_category1)){
  if(test_category1[i] == 'CENTURY') {
    test_category2 <- append(test_category2, 1)
  } 
  else {
    test_category2 <- append(test_category2, 0)
  }
}
#install.packages("ROSE")
library("ROSE")
#convert to factor for Random Forest
train_category2 <- as.factor(train_category2)
#drop fonts column from trainset
trainset <- trainset[ -c(104) ]
#add century not century column
trainset <- cbind(trainset,train_category2)
testset = cbind(testset,test_category2)
?randomForest

over1=ovun.sample(train_category2~.,data=trainset,method = "over",N=5090)$data
table(over1$train_category2)

rfover1=randomForest(train_category2~.,data=over1,ntree=200,mtry=10,importance=TRUE)


library("caret")
library("e1071")
confusionMatrix(predict(rfover1,testset),as.factor(testset$test_category2),positive = "1")
library(gbm)

over1gbm=gbm(formula=train_category2~.,distribution="bernoulli",data=over1,n.trees=200)

table(factor(over1),factor(testset$test_category2))
##########################
#C1 vc {C2&C3} with under sampling
table(trainset$train_category2)

under1=ovun.sample(train_category2~.,data=trainset,method = "under",N=2638)$data
table(under1$train_category2)


rfunder1=randomForest(train_category2~.,data=under1,ntree=200,mtry=10,importance=TRUE)
confusionMatrix(predict(rfunder1,testset),as.factor(testset$test_category2),positive = "1")

##########################
#C1 vc {C2&C3} with both over and under sampling

Both1=ovun.sample(train_category2~.,data=trainset,method = "both",seed=1,N=3864)$data
table(Both1$train_category2)


rfBoth1=randomForest(train_category2~.,data=Both1,ntree=200,mtry=10,importance=TRUE)
confusionMatrix(predict(rfBoth1,testset),as.factor(testset$test_category2),positive = "1")
##################################
#####################################
#############################
#convert fonts to ebrima=1 or not ebrima=0
test_category1 <- as.character(test_category)
train_category1 <- as.character(train_category)
#C2 versus {C1+C3} train set
train_category3 <- c()
for (i in 1:length(train_category1)){
  if(train_category1[i] == "EBRIMA") {
    train_category3 <- append(train_category3, 1)
  } 
  else {
    train_category3 <- append(train_category3, 0)
  }
}
#C2 versus {C1+C3} test set
test_category3 <- c()
for (i in 1:length(test_category1)){
  if(test_category1[i] == 'EBRIMA') {
    test_category3 <- append(test_category3, 1)
  } 
  else {
    test_category3 <- append(test_category3, 0)
  }
}
#convert to factor for Random Forest
train_category3 <- as.factor(train_category3)
#drop fonts column from trainset
trainset <- trainset[ -c(104) ]
#add Ebrima not ebrima column
trainset <- cbind(trainset,train_category3)
testset = cbind(testset,test_category3)
##########################

#Over sampling the data to C2 vs {C1 & C3}, C2=2486,  {C1 & C3}=2486
table(trainset$train_category3)

over2=ovun.sample(train_category3~.,data=trainset,method = "over",N=4972)$data#original=
table(over2$train_category3)

rfover2=randomForest(train_category3~.,data=over2,ntree=200,mtry=10,importance=TRUE)

#Confusion matrix with over sampling the data to C2 vs {C1 & C3}
confusionMatrix(predict(rfover2,testset),as.factor(testset$test_category3),positive = "1")

###########################
###########################

#C2 vs {C1&C3} with under sampling
table(trainset$train_category3)

under2=ovun.sample(train_category3~.,data=trainset,method = "under",N=2756)$data
table(under2$train_category3)

rfunder2=randomForest(train_category3~.,data=under2,ntree=200,mtry=10,importance=TRUE)
confusionMatrix(predict(rfunder2,testset),as.factor(testset$test_category3),positive = "1")



##########################
#C2 vs {C1&C3} with both over and under sampling

Both2=ovun.sample(train_category3~.,data=trainset,method = "both",seed=1,N=3864)$data
table(Both2$train_category3)


rfBoth2=randomForest(train_category3~.,data=Both2,ntree=200,mtry=10,importance=TRUE)
confusionMatrix(predict(rfBoth2,testset),as.factor(testset$test_category3),positive = "1")
######################################
########################################
########################################


#convert fonts to Gill=1 or not Gill=0
test_category1 <- as.character(test_category)
train_category1 <- as.character(train_category)
#C3 versus {C1+C2} train set
train_category4 <- c()
for (i in 1:length(train_category1)){
  if(train_category1[i] == "GILL") {
    train_category4 <- append(train_category4, 1)
  } 
  else {
    train_category4 <- append(train_category4, 0)
  }
}
#C3 versus {C1+C2} test set
test_category4 <- c()
for (i in 1:length(test_category1)){
  if(test_category1[i] == 'GILL') {
    test_category4 <- append(test_category4, 1)
  } 
  else {
    test_category4 <- append(test_category4, 0)
  }
}
#convert to factor for Random Forest
train_category4 <- as.factor(train_category4)
#drop fonts column from trainset
trainset <- trainset[ -c(104) ]
#add Ebrima not ebrima column
trainset <- cbind(trainset,train_category4)
testset = cbind(testset,test_category4)
##########################

#Over sampling the data to C3 vs {C1 & C2}, C3=2486,  {C1 & C3}=2486
table(trainset$train_category4)

over3=ovun.sample(train_category4~.,data=trainset,method = "over",N=5394)$data
table(over3$train_category4)

rfover3=randomForest(train_category4~.,data=over3,ntree=200,mtry=10,importance=TRUE)

#Confusion matrix with over sampling the data to C2 vs {C1 & C3}
confusionMatrix(predict(rfover3,testset),as.factor(testset$test_category4),positive = "1")

###########################
###########################

#C2 vs {C1&C3} with under sampling
table(trainset$train_category4)

under3=ovun.sample(train_category4~.,data=trainset,method = "under",N=2334)$data
table(under3$train_category4)

rfunder3=randomForest(train_category4~.,data=under3,ntree=200,mtry=10,importance=TRUE)
confusionMatrix(predict(rfunder3,testset),as.factor(testset$test_category4),positive = "1")



##########################
#C1 vc {C2&C3} with both over and under sampling

Both3=ovun.sample(train_category4~.,data=trainset,method = "both",seed=1,N=3864)$data
table(Both3$train_category4)


rfBoth3=randomForest(train_category4~.,data=Both3,ntree=200,mtry=10,importance=TRUE)
confusionMatrix(predict(rfBoth3,testset),as.factor(testset$test_category4),positive = "1")


#convert fonts to ebrima=1 or not ebrima=0
test_category1 <- as.character(test_category)
train_category1 <- as.character(train_category)
#C2 versus {C1+C3} train set
train_category3 <- c()
for (i in 1:length(train_category1)){
  if(train_category1[i] == "EBRIMA") {
    train_category3 <- append(train_category3, 1)
  } 
  else {
    train_category3 <- append(train_category3, 0)
  }
}
#C2 versus {C1+C3} test set
test_category3 <- c()
for (i in 1:length(test_category1)){
  if(test_category1[i] == 'EBRIMA') {
    test_category3 <- append(test_category3, 1)
  } 
  else {
    test_category3 <- append(test_category3, 0)
  }
}
#convert to factor for Random Forest
train_category3 <- as.factor(train_category3)
#drop fonts column from trainset
trainset <- trainset[ -c(104) ]
trainset <- trainset[ -c(104:105) ]
#add Ebrima not ebrima column
trainset <- cbind(trainset,train_category3)
testset = cbind(testset,test_category3)
##########################

#Over sampling the data to C2 vs {C1 & C3}, C2=2486,  {C1 & C3}=2486
table(trainset$train_category3)

over2=ovun.sample(train_category3~.,data=trainset,method = "over",seed=1,N=7000)$data#4972
table(over2$train_category3)

rfover2=randomForest(train_category3~.,data=over2,ntree=700,mtry=10)

#Confusion matrix with over sampling the data to C2 vs {C1 & C3}
confusionMatrix(predict(rfover2,testset),as.factor(testset$test_category3),positive = "1")

###########################
###########################

#C2 vs {C1&C3} with under sampling
table(trainset$train_category3)

under2=ovun.sample(train_category3~.,data=trainset,method = "under",seed=1,N=2756)$data
table(under2$train_category3)

rfunder2=randomForest(train_category3~.,data=under2,ntree=200,mtry=10,importance=TRUE)
confusionMatrix(predict(rfunder2,testset),as.factor(testset$test_category3),positive = "1")
install.packages("gbm")
library(gbm)

over2gbm=gbm(train_category3~.,data=over2,distribution="bernoulli",n.trees=5000)
?gbm
