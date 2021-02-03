###Remove Unwanted Columns###
CENTURY.d <- subset(CENTURY, select = -c(fontVariant, m_label, orientation, m_top, m_left, originalH, originalW, h, w))
EBRIMA.d <- subset(EBRIMA, select = -c(fontVariant, m_label, orientation, m_top, m_left, originalH, originalW, h, w))
GILL.d <- subset(GILL, select = -c(fontVariant, m_label, orientation, m_top, m_left, originalH, originalW, h, w))

###Define CL1###
CL1 = data.frame()
for(i in 1:nrow(CENTURY.d)){
  if(CENTURY.d[i,"strength"] == .4){
    if (CENTURY.d[i,"italic"]==0){
      CL1 <- rbind(CL1, CENTURY.d[i,])}
  }
}

###Define CL2###
CL2 = data.frame()
for(i in 1:nrow(EBRIMA.d)){
  if(EBRIMA.d[i,"strength"] == .4){
    if (EBRIMA.d[i,"italic"]==0){
      CL2 <- rbind(CL2, EBRIMA.d[i,])}
  }
}

###Define CL3###
CL3 = data.frame()
for(i in 1:nrow(GILL.d)){
  if(GILL.d[i,"strength"] == .4){
    if (GILL.d[i,"italic"]==0){
      CL3 <- rbind(CL3, GILL.d[i,])}
  }
}

###Randomize CL1 and remove 350 rows###
set.seed(26) 
rows <- sample(nrow(CL1))
CL1 <- CL1[rows,]
CL1 = CL1[-c(1:350),]

###get n1,n2,n3###
n1 = nrow(CL1)
n2 = nrow(CL2)
n3 = nrow(CL3)

###Combine CL1,CL2,CL3 to get DATA###
DATA_full <- rbind(CL1,CL2,CL3)
DATA <- DATA_full[,-c(1:3)]#omit font, strength, italic columns

###get N###
N = nrow(DATA)

###get means for each feature###
means = colMeans(DATA)

###get standard deviation of data###
sds = apply(DATA, 2, sd)

###Standardize Data###
SDATA = data.frame()
for(i in 1:nrow(DATA)){
  for(j in 1:ncol(DATA)){
    SDATA[i,j] = (DATA[i,j]-means[j])/sds[j]
  }
}

#get r for each class 
r1 = floor(.2*n1)
r2 = floor(.2*n2)
r3 = floor(.2*n3)

#get s for each class
s1 = n1 - r1
s2 = n2 - r2
s3 = n3 - r3

#training and testing sets for CL1 
random_v1 = sample(seq(1:n1))
train_rows_1 = random_v1[1:s1]
CL1_train = CL1[train_rows_1,]
test_rows_1 = random_v1[(s1+1):n1]
CL1_test = CL1[test_rows_1,]


#training and testing sets for CL2
random_v2 = sample(seq(1:n2))
train_rows_2 = random_v2[1:s2]
CL2_train = CL2[train_rows_2,]
test_rows_2 = random_v2[(s2+1):n2]
CL2_test = CL2[test_rows_2,]

#training and testing sets for CL3 
random_v3 = sample(seq(1:n3))
train_rows_3 = random_v3[1:s3]
CL3_train = CL3[train_rows_3,]
test_rows_3 = random_v3[(s3+1):n3]
CL3_test = CL3[test_rows_3,]

#get TRAINSET
TRAINSET = rbind(CL1_train,CL2_train,CL3_train)

#get TESTSET
TESTSET = rbind(CL1_test, CL2_test, CL3_test)

###TRAINSET target####
train_set_target = TRAINSET[,1]

####TESTSET target####
test_set_target = TESTSET[,1]

##remove font, strength, and italic for knn###
TRAINSET = TRAINSET[-c(1:3)]
TESTSET = TESTSET[-c(1:3)]


##################Run KNN like HW 2###################################
####################################
####k=1####
####################################
###knn results for test k = 1###
require(class)
m1_test_k1 =knn(train=TRAINSET, test=TESTSET, train_set_target, k=1 )

test_mat_k1 = table(test_set_target, m1_test_k1)

testperf_k1 = sum(test_mat_k1[1,1], test_mat_k1[2,2], test_mat_k1[3,3])/length(test_set_target)
###knn results for train k = 1###
m2_train_k1 =knn(train=TRAINSET, test=TRAINSET, train_set_target, k=1 )

train_mat_k1 = table(train_set_target,m2_train_k1)

trainperf_k1 = sum(train_mat_k1[1,1], train_mat_k1[2,2], train_mat_k1[3,3]) /length(train_set_target)


####################################
####k=5####
####################################
###knn results for test k = 5###
require(class)
m1_test_k5 =knn(train=TRAINSET, test=TESTSET, train_set_target, k=5 )

test_mat_k5 = table(test_set_target, m1_test_k5)

testperf_k5 = sum(test_mat_k5[1,1], test_mat_k5[2,2], test_mat_k5[3,3])/length(test_set_target)

###knn results for train k = 5###
m2_train_k5 =knn(train=TRAINSET, test=TRAINSET, train_set_target, k=5 )

train_mat_k5 = table(train_set_target,m2_train_k5)

trainperf_k5 = sum(train_mat_k5[1,1], train_mat_k5[2,2], train_mat_k5[3,3]) /length(train_set_target)

####################################
####k=10####
####################################
###knn test results for test k = 10###
require(class)
m1_test_k10 =knn(train=TRAINSET, test=TESTSET, train_set_target, k=10 )

test_mat_k10 = table(test_set_target, m1_test_k10)

testperf_k10 = sum(test_mat_k10[1,1], test_mat_k10[2,2], test_mat_k10[3,3])/length(test_set_target)


###knn train results for train k = 10###
m2_train_k10 =knn(train=TRAINSET, test=TRAINSET, train_set_target, k=10 )

train_mat_k10 = table(train_set_target,m2_train_k10)

trainperf_k10 = sum(train_mat_k10[1,1], train_mat_k10[2,2], train_mat_k10[3,3]) /length(train_set_target)

####################################
####k=15####
####################################
###knn test results for test k = 15###
m1_test_k15 =knn(train=TRAINSET, test=TESTSET, train_set_target, k=15 )

test_mat_k15 = table(test_set_target, m1_test_k15)

testperf_k15 = sum(test_mat_k15[1,1], test_mat_k15[2,2], test_mat_k15[3,3])/length(test_set_target)

###knn train results for train k = 15###
m2_train_k15 =knn(train=TRAINSET, test=TRAINSET, train_set_target, k=15 )

train_mat_k15 = table(train_set_target,m2_train_k15)

trainperf_k15 = sum(train_mat_k15[1,1], train_mat_k15[2,2], train_mat_k15[3,3]) /length(train_set_target)

####################################
####k=20####
####################################
###knn test results for test k = 20###
m1_test_k20 =knn(train=TRAINSET, test=TESTSET, train_set_target, k=20 )

test_mat_k20 = table(test_set_target, m1_test_k20)

testperf_k20 = sum(test_mat_k20[1,1], test_mat_k20[2,2], test_mat_k20[3,3])/length(test_set_target)

###knn train results for train k = 20###
m2_train_k20 =knn(train=TRAINSET, test=TRAINSET, train_set_target, k=20 )

train_mat_k20 = table(train_set_target,m2_train_k20)

trainperf_k20 = sum(train_mat_k20[1,1], train_mat_k20[2,2], train_mat_k20[3,3]) /length(train_set_target)

####################################
####k=30####
####################################
###knn test results for test k = 30###
m1_test_k30 =knn(train=TRAINSET, test=TESTSET, train_set_target, k=30 )

test_mat_k30 = table(test_set_target, m1_test_k30)

testperf_k30 = sum(test_mat_k30[1,1], test_mat_k30[2,2], test_mat_k30[3,3])/length(test_set_target)

###knn train results for train k = 30###
m2_train_k30 =knn(train=TRAINSET, test=TRAINSET, train_set_target, k=30 )

train_mat_k30 = table(train_set_target,m2_train_k30)

trainperf_k30 = sum(train_mat_k30[1,1], train_mat_k30[2,2], train_mat_k30[3,3]) /length(train_set_target)


####################################
####k=40####
####################################
###knn test results for test k = 40###
m1_test_k40 =knn(train=TRAINSET, test=TESTSET, train_set_target, k=40 )

test_mat_k40 = table(test_set_target, m1_test_k40)

testperf_k40 = sum(test_mat_k40[1,1], test_mat_k40[2,2], test_mat_k40[3,3])/length(test_set_target)

###knn train results for train k = 40###
m2_train_k40 =knn(train=TRAINSET, test=TRAINSET, train_set_target, k=40 )

train_mat_k40 = table(train_set_target,m2_train_k40)

trainperf_k40 = sum(train_mat_k40[1,1], train_mat_k40[2,2], train_mat_k40[3,3]) /length(train_set_target)

####################################
####k=50####
####################################
###knn test results for test k = 50###
m1_test_k50 =knn(train=TRAINSET, test=TESTSET, train_set_target, k=50 )

test_mat_k50 = table(test_set_target, m1_test_k50)

testperf_k50 = sum(test_mat_k50[1,1], test_mat_k50[2,2], test_mat_k50[3,3])/length(test_set_target)

###knn train results for train k = 50###
m2_train_k50 =knn(train=TRAINSET, test=TRAINSET, train_set_target, k=50 )

train_mat_k50 = table(train_set_target,m2_train_k50)

trainperf_k50 = sum(train_mat_k50[1,1], train_mat_k50[2,2], train_mat_k50[3,3]) /length(train_set_target)

####################################
####k=100####
####################################
###knn test results for test k = 100###
m1_test_k100 =knn(train=TRAINSET, test=TESTSET, train_set_target, k=100 )

test_mat_k100 = table(test_set_target, m1_test_k100)

testperf_k100 = sum(test_mat_k100[1,1], test_mat_k100[2,2], test_mat_k100[3,3])/length(test_set_target)


###knn train results for train k = 100###
m2_train_k100 =knn(train=TRAINSET, test=TRAINSET, train_set_target, k=100 )

train_mat_k100 = table(train_set_target,m2_train_k100)

trainperf_k100 = sum(train_mat_k100[1,1], train_mat_k100[2,2], train_mat_k100[3,3]) /length(train_set_target)

####################################
####k=2####
####################################
###knn test results for test k = 2###
m1_test_k2 =knn(train=TRAINSET, test=TESTSET, train_set_target, k=2 )

test_mat_k2 = table(test_set_target, m1_test_k2)

testperf_k2 = sum(test_mat_k2[1,1], test_mat_k2[2,2], test_mat_k2[3,3])/length(test_set_target)

###knn train results for train k = 2###
m2_train_k2 =knn(train=TRAINSET, test=TRAINSET, train_set_target, k=2 )

train_mat_k2 = table(train_set_target,m2_train_k2)

trainperf_k2 = sum(train_mat_k2[1,1], train_mat_k2[2,2], train_mat_k2[3,3]) /length(train_set_target)

####################################
####k=3####
####################################
###knn test results for test k = 3###
m1_test_k3 =knn(train=TRAINSET, test=TESTSET, train_set_target, k=3 )

test_mat_k3 = table(test_set_target, m1_test_k3)

testperf_k3 = sum(test_mat_k3[1,1], test_mat_k3[2,2], test_mat_k3[3,3])/length(test_set_target)

###knn train results for train k = 3###
m2_train_k3 =knn(train=TRAINSET, test=TRAINSET, train_set_target, k=3 )

train_mat_k3 = table(train_set_target,m2_train_k3)

trainperf_k3 = sum(train_mat_k3[1,1], train_mat_k3[2,2], train_mat_k3[3,3]) /length(train_set_target)

####################################
####k=4####
####################################
###knn test results for test k = 4###
m1_test_k4 =knn(train=TRAINSET, test=TESTSET, train_set_target, k=4 )

test_mat_k4 = table(test_set_target, m1_test_k4)

testperf_k4 = sum(test_mat_k4[1,1], test_mat_k4[2,2], test_mat_k4[3,3])/length(test_set_target)

###knn train results for train k = 4###
m2_train_k4 =knn(train=TRAINSET, test=TRAINSET, train_set_target, k=4 )

train_mat_k4 = table(train_set_target,m2_train_k4)

trainperf_k4 = sum(train_mat_k4[1,1], train_mat_k4[2,2], train_mat_k4[3,3]) /length(train_set_target)

trainperf = c(trainperf_k1,trainperf_k2,trainperf_k3,trainperf_k4,trainperf_k5,trainperf_k10,trainperf_k15,trainperf_k20,trainperf_k30,trainperf_k40,trainperf_k50, trainperf_k100)
k_values = c(1,2,3,4,5,10,15,20,30,40,50,100)
testperf = c(testperf_k1,testperf_k2,testperf_k3,testperf_k4,testperf_k5,testperf_k10,testperf_k15,testperf_k20,testperf_k30,testperf_k40,testperf_k50, testperf_k100)

plot(k_values, trainperf, xlim = c(1,100),ylim = c(.3,1) ,type = "b", col = "blue", xlab = "K Values", ylab = "Accuracy")
axis(1, at = seq(0,100,5))
par(new = TRUE)
legend("topright",
       c("trainperf","testperf"),
       fill=c("blue","red"))
par(new = TRUE)
plot(k_values, testperf, xlim = c(1,100),ylim = c(.3,1) ,type = "b", col = "red", xlab = "", ylab = "")

###train confidence interval for 95 (BEFORE PCA)%###
train_century_perf = train_mat_k1[1,1]/sum(train_mat_k1[1,])
train_ebrima_perf = train_mat_k1[2,2]/sum(train_mat_k1[2,])
train_gill_perf = train_mat_k1[3,3]/sum(train_mat_k1[3,])
train_century_sigma = sqrt((train_century_perf*(1-train_century_perf))/sum(train_mat_k1[1,]))
train_century_left_int = train_century_perf - (1.96*train_century_sigma)
train_century_left_int
train_century_right_int = train_century_perf + (1.96*train_century_sigma)
train_ebrima_sigma = sqrt((train_ebrima_perf*(1-train_ebrima_perf))/sum(train_mat_k1[2,]))
train_ebrima_left_int = train_ebrima_perf - (1.96*train_ebrima_sigma)

train_ebrima_right_int = train_ebrima_perf + (1.96*train_ebrima_sigma)

train_gill_sigma = sqrt((train_gill_perf*(1-train_gill_perf))/sum(train_mat_k1[3,]))
train_gill_left_int = train_gill_perf - (1.96*train_gill_sigma)

train_gill_right_int = train_gill_perf + (1.96*train_gill_sigma)

###test confidence interval for 95% (BEFORE PCA)###

test_century_perf = test_mat_k1[1,1]/sum(test_mat_k1[1,])
test_ebrima_perf = test_mat_k1[2,2]/sum(test_mat_k1[2,])
test_gill_perf = test_mat_k1[3,3]/sum(test_mat_k1[3,])
test_century_sigma = sqrt((test_century_perf*(1-test_century_perf))/sum(test_mat_k1[1,]))
test_century_left_int = test_century_perf - (1.96*test_century_sigma)

test_century_right_int = test_century_perf + (1.96*test_century_sigma)

test_ebrima_sigma = sqrt((test_ebrima_perf*(1-test_ebrima_perf))/sum(test_mat_k1[2,]))
test_ebrima_left_int = test_ebrima_perf - (1.96*test_ebrima_sigma)

test_ebrima_right_int = test_ebrima_perf + (1.96*test_ebrima_sigma)

test_gill_sigma = sqrt((test_gill_perf*(1-test_gill_perf))/sum(test_mat_k1[3,]))
test_gill_left_int = test_gill_perf - (1.96*test_gill_sigma)

test_gill_right_int = test_gill_perf + (1.96*test_gill_sigma)

dim(DATA)
install.packages("factoextra")
install.packages("FactoMineR")
library("factoextra")
library("FactoMineR")
nor <- function(x) { (x-mean(x))/sd(x) }
sdata <- as.data.frame(lapply(DATA, nor))
cdata <- cor(sdata)
install.packages("xlsx")
library("xlsx")
write.xlsx(cdata,"C:/Users/haroo/OneDrive/Documents/R/Azencott H.w/corelation.xlsx")

#10 pairs of features with the 20 highest correlations
v1v2c <- as.data.frame(as.table(cdata)) #create table from correlations variable 1,variable 2, and correlation

v1v2c <- v1v2c[order(-abs(v1v2c$Freq)),] #takes the absolute value of correlations and arranges largest to smallest
head(v1v2c)
v1v2c1 <- subset(v1v2c, v1v2c$Freq != 1) #new table without 1 correlations
head(v1v2c1,n=20L) #displays the top 10 highest correlation values with the pixel positions corresponding the pair


PCA
res.pca <- prcomp(DATA, scale = TRUE)
#eigen values
eig.val <- get_eigenvalue(res.pca)
#Plot of Eigenvalues Lr versus r
fviz_eig(res.pca, choice = "eigenvalue", addlabels=TRUE,ylim = c(0, 75),ncp=35)
#Percentage of Variance explained versus r
fviz_eig(res.pca, ncp = 23,addlabels=TRUE,ylim = c(0, 20))
#New Data with 103 principal components
newdata<- res.pca$x[,1:103]

write.xlsx(eig.val,"C:/Users/haroo/OneDrive/Documents/R/Azencott H.w/eigvals.xlsx")

ev=eigen(cdata)
eig.vec=ev$vectors
write.xlsx(eig.vec,"C:/Users/haroo/OneDrive/Documents/R/Azencott H.w/eigvecs.xlsx")
evec=res.pca$rotation
write.xlsx(evec,"C:/Users/haroo/OneDrive/Documents/R/Azencott H.w/eigvecs.xlsx")
