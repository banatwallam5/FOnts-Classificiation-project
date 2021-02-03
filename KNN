
CAMBRIA.d <- subset(CAMBRIA, select = -c(fontVariant, m_label, orientation, m_top, m_left, originalH, originalW, h, w))
head(CAMBRIA.d)
TIMES.d <- subset(TIMES, select = -c(fontVariant, m_label, orientation, m_top, m_left, originalH, originalW, h, w))
BASKERVILLE.d <- subset(BASKERVILLE, select = -c(fontVariant, m_label, orientation, m_top, m_left, originalH, originalW, h, w))

###Define CL1###
CL1 = data.frame()
for(i in 1:nrow(CAMBRIA.d)){
  if(CAMBRIA.d[i,"strength"] == .4){
    if (CAMBRIA.d[i,"italic"]==0){
      CL1 <- rbind(CL1, CAMBRIA.d[i,])}
  }
}

###Define CL2###
CL2 = data.frame()
for(i in 1:nrow(TIMES.d)){
  if(TIMES.d[i,"strength"] == .4){
    if (TIMES.d[i,"italic"]==0){
      CL2 <- rbind(CL2, TIMES.d[i,])}
  }
}

###Define CL3###
CL3 = data.frame()
for(i in 1:nrow(BASKERVILLE.d)){
  if(BASKERVILLE.d[i,"strength"] == .4){
    if (BASKERVILLE.d[i,"italic"]==0){
      CL3 <- rbind(CL3, BASKERVILLE.d[i,])}
  }
}



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
means
###get standard deviation for each feature###
sds = apply(DATA, 2, sd)

###Standardize Data###
SDATA = data.frame()
for(i in 1:nrow(DATA)){
  for(j in 1:ncol(DATA)){
    SDATA[i,j] = (DATA[i,j]-means[j])/sds[j]
  }
}

CORR <- cor(SDATA)
CORR
###Get 10 highest Correlation Values###
pos_CORR = abs(CORR)
diag(pos_CORR) <- NA

W = as.vector(pos_CORR)
U = sort(W, decreasing = TRUE)
top_20 = U[1:20] #gets the top 10 values
unique_top_20 = unique(top_20) #gets rid of duplicates 
indices = data.frame() #indices gives the position of those values 
for(i in 1:length(unique_top_20)){
  indices = rbind(indices, which(CORR == unique_top_20[i], TRUE))
}

#this part gets the name of the pixel positions of the top 10 correlation 
col_names = colnames(DATA)
col_indices = indices$col
top_10_pixels = c()
for(i in 1:length(col_indices)){
  top_10_pixels = c(top_10_pixels, col_names[col_indices[i]])
}
top_10_pixels.m = matrix(top_10_pixels, ncol = 2, byrow = TRUE) #each row corresponds to the two correlated pixels
#part 1

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

class_ord_mat=rbind(CL1_train,CL2_train,CL3_train,CL1_test,CL2_test,CL3_test)
standardize=function(x){
  return((x-mean(x))/sd(x))
}
standardize(c(10,20,30,40,50))
class_ord_mat_s=as.data.frame(lapply(class_ord_mat[,4:403],standardize))

###gets train and test target values###
train_set=class_ord_mat_s[1:6603,]
test_set=class_ord_mat_s[6604:8252,]
train_set_target=class_ord_mat[1:6603,1]
test_set_target=class_ord_mat[6604:8252,1]

require(class)

cl=train_set_target[,1,drop=TRUE]
###knn results for test
m1=knn(train=train_set, test=test_set, cl, k=12 )
m1
###knn results for train###
m2=knn(train=train_set, test=train_set, cl, k=12 )

test_mat_k12=table(test_set_target[,1,drop=TRUE],m1)                                                                                                      
test_mat_k12
###get the testperf###
testperf_k12 = sum(test_mat_k12[1,1], test_mat_k12[2,2], test_mat_k12[3,3])/length(test_set_target[,1,drop=TRUE])
testperf_k12

###Train confusion matrix###
train_mat_k12=table(cl,m2)
train_mat_k12

###get the trainperf###
trainperf_k12 = sum(train_mat_k12[1,1], train_mat_k12[2,2], train_mat_k12[3,3])/length(cl)
trainperf_k12


####################################
####k=5####
####################################

###knn results for test
m1_k5=knn(train=train_set, test=test_set, cl, k=5 )
m1_k5
###knn results for train###
m2_k5=knn(train=train_set, test=train_set, cl, k=5 )
m2_k5
test_mat_k5=table(test_set_target[,1,drop=TRUE],m1_k5)                                                                                                      
test_mat_k5
###get the testperf###
testperf_k5 = sum(test_mat_k5[1,1], test_mat_k5[2,2], test_mat_k5[3,3])/length(test_set_target[,1,drop=TRUE])
testperf_k5

###Train confusion matrix###
train_mat_k5=table(cl,m2_k5)
train_mat_k5

###get the trainperf###
trainperf_k5 = sum(train_mat_k5[1,1], train_mat_k5[2,2], train_mat_k5[3,3])/length(cl)
trainperf_k5
####################################
####k=10####
####################################

###knn results for test
m1_k10=knn(train=train_set, test=test_set, cl, k=10 )
m1_k10
###knn results for train###
m2_k10=knn(train=train_set, test=train_set, cl, k=10 )
m2_k10
test_mat_k10=table(test_set_target[,1,drop=TRUE],m1_k10)                                                                                                      
test_mat_k10
###get the testperf###
testperf_k10 = sum(test_mat_k10[1,1], test_mat_k10[2,2], test_mat_k10[3,3])/length(test_set_target[,1,drop=TRUE])
testperf_k10

###Train confusion matrix###
train_mat_k10=table(cl,m2_k10)
train_mat_k10

###get the trainperf###
trainperf_k10 = sum(train_mat_k10[1,1], train_mat_k10[2,2], train_mat_k10[3,3])/length(cl)
trainperf_k10
####################################
####k=15####
####################################

###knn results for test
m1_k15=knn(train=train_set, test=test_set, cl, k=15 )
m1_k15
###knn results for train###
m2_k15=knn(train=train_set, test=train_set, cl, k=15 )
m2_k15
test_mat_k15=table(test_set_target[,1,drop=TRUE],m1_k15)                                                                                                      
test_mat_k15
###get the testperf###
testperf_k15 = sum(test_mat_k15[1,1], test_mat_k15[2,2], test_mat_k15[3,3])/length(test_set_target[,1,drop=TRUE])
testperf_k15

###Train confusion matrix###
train_mat_k15=table(cl,m2_k15)
train_mat_k15

###get the trainperf###
trainperf_k15 = sum(train_mat_k15[1,1], train_mat_k15[2,2], train_mat_k15[3,3])/length(cl)
trainperf_k15

####################################
####k=20####
####################################

###knn results for test
m1_k20=knn(train=train_set, test=test_set, cl, k=20 )
m1_k20
###knn results for train###
m2_k20=knn(train=train_set, test=train_set, cl, k=20 )
m2_k20
test_mat_k20=table(test_set_target[,1,drop=TRUE],m1_k20)                                                                                                      
test_mat_k20
###get the testperf###
testperf_k20 = sum(test_mat_k20[1,1], test_mat_k20[2,2], test_mat_k20[3,3])/length(test_set_target[,1,drop=TRUE])
testperf_k20

###Train confusion matrix###
train_mat_k20=table(cl,m2_k20)
train_mat_k20

###get the trainperf###
trainperf_k20 = sum(train_mat_k20[1,1], train_mat_k20[2,2], train_mat_k20[3,3])/length(cl)
trainperf_k20
####################################
####k=30####
####################################

###knn results for test
m1_k30=knn(train=train_set, test=test_set, cl, k=30 )
m1_k30
###knn results for train###
m2_k30=knn(train=train_set, test=train_set, cl, k=30 )
m2_k30
test_mat_k30=table(test_set_target[,1,drop=TRUE],m1_k30)                                                                                                      
test_mat_k30
###get the testperf###
testperf_k30 = sum(test_mat_k30[1,1], test_mat_k30[2,2], test_mat_k30[3,3])/length(test_set_target[,1,drop=TRUE])
testperf_k30

###Train confusion matrix###
train_mat_k30=table(cl,m2_k30)
train_mat_k30

###get the trainperf###
trainperf_k30 = sum(train_mat_k30[1,1], train_mat_k30[2,2], train_mat_k30[3,3])/length(cl)
trainperf_k30
####################################
####k=40####
####################################

###knn results for test
m1_k40=knn(train=train_set, test=test_set, cl, k=40 )
m1_k40
###knn results for train###
m2_k40=knn(train=train_set, test=train_set, cl, k=40 )
m2_k40
test_mat_k40=table(test_set_target[,1,drop=TRUE],m1_k40)                                                                                                      
test_mat_k40
###get the testperf###
testperf_k40 = sum(test_mat_k40[1,1], test_mat_k40[2,2], test_mat_k40[3,3])/length(test_set_target[,1,drop=TRUE])
testperf_k40

###Train confusion matrix###
train_mat_k40=table(cl,m2_k40)
train_mat_k40

###get the trainperf###
trainperf_k40 = sum(train_mat_k40[1,1], train_mat_k40[2,2], train_mat_k40[3,3])/length(cl)
trainperf_k40
####################################
####k=50####
####################################

###knn results for test
m1_k50=knn(train=train_set, test=test_set, cl, k=50 )
m1_k50
###knn results for train###
m2_k50=knn(train=train_set, test=train_set, cl, k=50 )
m2_k50
test_mat_k50=table(test_set_target[,1,drop=TRUE],m1_k50)                                                                                                      
test_mat_k50
###get the testperf###
testperf_k50 = sum(test_mat_k50[1,1], test_mat_k50[2,2], test_mat_k50[3,3])/length(test_set_target[,1,drop=TRUE])
testperf_k50

###Train confusion matrix###
train_mat_k50=table(cl,m2_k50)
train_mat_k50

###get the trainperf###
trainperf_k50 = sum(train_mat_k50[1,1], train_mat_k50[2,2], train_mat_k50[3,3])/length(cl)
trainperf_k50
####################################
####k=100####
####################################

###knn results for test
m1_k100=knn(train=train_set, test=test_set, cl, k=100 )
m1_k100
###knn results for train###
m2_k100=knn(train=train_set, test=train_set, cl, k=100 )
m2_k100
test_mat_k100=table(test_set_target[,1,drop=TRUE],m1_k100)                                                                                                      
test_mat_k100
###get the testperf###
testperf_k100 = sum(test_mat_k100[1,1], test_mat_k100[2,2], test_mat_k100[3,3])/length(test_set_target[,1,drop=TRUE])
testperf_k100

###Train confusion matrix###
train_mat_k100=table(cl,m2_k100)
train_mat_k100

###get the trainperf###
trainperf_k100 = sum(train_mat_k100[1,1], train_mat_k100[2,2], train_mat_k100[3,3])/length(cl)
trainperf_k100
####################################
####k=1####
####################################

###knn results for test
m1_k1=knn(train=train_set, test=test_set, cl, k=1 )
m1_k1
###knn results for train###
m2_k1=knn(train=train_set, test=train_set, cl, k=1 )
m2_k1
test_mat_k1=table(test_set_target[,1,drop=TRUE],m1_k1)                                                                                                      
test_mat_k1
###get the testperf###
testperf_k1 = sum(test_mat_k1[1,1], test_mat_k1[2,2], test_mat_k1[3,3])/length(test_set_target[,1,drop=TRUE])
testperf_k1

###Train confusion matrix###
train_mat_k1=table(cl,m2_k1)
train_mat_k1

###get the trainperf###
trainperf_k1 = sum(train_mat_k1[1,1], train_mat_k1[2,2], train_mat_k1[3,3])/length(cl)
trainperf_k1

#######
# Storing all performance of knns in a vector
testperfs=c(testperf_k1,testperf_k5,testperf_k10,testperf_k12,testperf_k15,testperf_k20,testperf_k30,testperf_k40,testperf_k50,testperf_k100)
testperfs.round=round(testperfs,2)

# storing all knn values in another vector as strings
kvals=c("k=1","k=5","k=10","k=12","k=15","k=20","k=30","k=40","k=50","k=100")


#merging them together to create a 11 x2 data frame where each val k corresponds to the performance of knn at that value
kperfs=as.data.frame(cbind(kvals.character,testperfs.round))
library(ggplot2)
level_order=(c("k=1","k=5","k=10","k=12","k=15","k=20","k=30","k=40","k=50","k=100"))
ggplot(kperfs, aes(x=factor(kvals, levels = level_order),y=testperfs.round,color=kvals)) + geom_point() + xlab("KNN Values")+ylab("Test Set Accuracy")+ggtitle("Values of k Vs Accuracy")+theme(plot.title = element_text(hjust = 0.5))

####################################
####k=2####
####################################

###knn results for test
m1_k2=knn(train=train_set, test=test_set, cl, k=2 )
m1_k2
###knn results for train###
m2_k2=knn(train=train_set, test=train_set, cl, k=2 )
m2_k2
test_mat_k2=table(test_set_target[,1,drop=TRUE],m1_k2)                                                                                                      
test_mat_k2
###get the testperf###
testperf_k2 = sum(test_mat_k2[1,1], test_mat_k2[2,2], test_mat_k2[3,3])/length(test_set_target[,1,drop=TRUE])
testperf_k2

###Train confusion matrix###
train_mat_k2=table(cl,m2_k2)
train_mat_k2

###get the trainperf###
trainperf_k2 = sum(train_mat_k2[1,1], train_mat_k2[2,2], train_mat_k2[3,3])/length(cl)
trainperf_k2
####################################
####k=3####
####################################

###knn results for test
m1_k3=knn(train=train_set, test=test_set, cl, k=3 )
m1_k3
###knn results for train###
m2_k3=knn(train=train_set, test=train_set, cl, k=3 )
m2_k3
test_mat_k3=table(test_set_target[,1,drop=TRUE],m1_k3)                                                                                                      
test_mat_k3
###get the testperf###
testperf_k3 = sum(test_mat_k3[1,1], test_mat_k3[2,2], test_mat_k3[3,3])/length(test_set_target[,1,drop=TRUE])
testperf_k3

###Train confusion matrix###
train_mat_k3=table(cl,m2_k3)
train_mat_k3

###get the trainperf###
trainperf_k3 = sum(train_mat_k3[1,1], train_mat_k3[2,2], train_mat_k3[3,3])/length(cl)
trainperf_k3

####################################
####k=4####
####################################

###knn results for test
m1_k4=knn(train=train_set, test=test_set, cl, k=4 )
m1_k4
###knn results for train###
m2_k4=knn(train=train_set, test=train_set, cl, k=4 )
m2_k4
test_mat_k4=table(test_set_target[,1,drop=TRUE],m1_k4)                                                                                                      
test_mat_k4
###get the testperf###
testperf_k4 = sum(test_mat_k4[1,1], test_mat_k4[2,2], test_mat_k4[3,3])/length(test_set_target[,1,drop=TRUE])
testperf_k4

###Train confusion matrix###
train_mat_k4=table(cl,m2_k4)
train_mat_k4

###get the trainperf###
trainperf_k4 = sum(train_mat_k4[1,1], train_mat_k4[2,2], train_mat_k4[3,3])/length(cl)
trainperf_k4

# we use the max function to find the max of knns 1 through 5
max_testperf=max(testperf_k1,testperf_k2,testperf_k3,testperf_k4,testperf_k5)
max_testperf

#Since our range is small we look through our values visually, we can tell max_testperf is at k =1, we check thorugh r just to be safe
max_testperf== testperf_k1

train_mat_k1=table(cl,m2_k1)
train_mat_k1

test_mat_k1=table(test_set_target[,1,drop=TRUE],m1_k1)                                                                                                      
test_mat_k1


#######################################
###1.6
#######################################
#takes out pack1 rlcm(l=[0:9],m=[0:9])
class_ord_mat_s_pack1=class_ord_mat_s[,c((1:10),(21:30),(41:50),(61:70),(81:90),(101:110),(121:130),(141:150),(161:170),(181:190))]
class_ord_mat_s_pack1

#takes out pack2 rlcm(l=[0:9],m=[10:19])
class_ord_mat_s_pack2=class_ord_mat_s[,c((11:20),(31:40),(51:60),(71:80),(91:100),(111:120),(131:140),(151:160),(171:180),(191:200))]
class_ord_mat_s_pack2

#takes out pack4 rlcm(l=[10:19],m=[0:9])
class_ord_mat_s_pack4=class_ord_mat_s[,c((201:210),(221:230),(241:250),(261:270),(281:290),(301:310),(321:330),(341:350),(361:370),(381:390))]
class_ord_mat_s_pack4


#takes out pack4 r4cm(l=[10:19],m=[10:19])
class_ord_mat_s_pack4=class_ord_mat_s[,c((211:220),(231:240),(251:260),(271:280),(291:300),(311:320),(331:340),(351:360),(371:380),(391:400))]
class_ord_mat_s_pack4
##############################
#gets our class ordered matrix with test set at the bottom and only the columns needed for each pack

class_ord_mat_pack1=class_ord_mat[,c((1:13),(24:33),(44:53),(64:73),(84:93),(104:113),(124:133),(144:153),(164:173),(184:193))]
class_ord_mat_pack1

class_ord_mat_pack2=class_ord_mat[,c((1:3),(14:23),(34:43),(54:63),(74:83),(94:103),(114:123),(134:143),(154:163),(174:183),(194:203))]
class_ord_mat_pack2

class_ord_mat_pack3=class_ord_mat[,c((1:3),(204:213),(224:233),(244:253),(264:273),(284:293),(304:313),(324:333),(344:353),(364:373),(384:393))]
class_ord_mat_pack3

class_ord_mat_pack4=class_ord_mat[,c((1:3),(214:223),(234:243),(254:263),(274:283),(294:303),(314:323),(334:343),(354:363),(374:383),(394:403))]
class_ord_mat_pack2
##############################

###gets train and test target values for pack 1###
train_set_pack1=class_ord_mat_s_pack1[1:6603,]
test_set_pack1=class_ord_mat_s_pack1[6604:8252,]
train_set_target_pack1=class_ord_mat_pack1[1:6603,1]
test_set_target_pack1=class_ord_mat_pack1[6604:8252,1]
#############################################

###knn results for test of pack 1 k=1###
require(class)
train_set_target_pack1=train_set_target_pack1[,1,drop=TRUE]
m1_pack1=knn(train=train_set_pack1, test=test_set_pack1, train_set_target_pack1, k=1 )

###knn results for train###

m2_pack1=knn(train=train_set, test=train_set, train_set_target_pack1, k=1 )
####test matric for pack1####
test_mat_pack1=table(test_set_target_pack1[,1,drop=TRUE],m1_pack1)
test_mat_pack1

testperf_pack1 = sum(test_mat_pack1[1,1], test_mat_pack1[2,2], test_mat_pack1[3,3])/length(test_set_target_pack1[,1,drop=TRUE])
testperf_pack1

###gets train and test target values for pack 2###
train_set_pack2=class_ord_mat_s_pack2[1:6603,]
test_set_pack2=class_ord_mat_s_pack2[6604:8252,]
train_set_target_pack2=class_ord_mat_pack2[1:6603,1]
test_set_target_pack2=class_ord_mat_pack2[6604:8252,1]
#############################################

###knn results for test of pack 1 k=1###
require(class)
train_set_target_pack2=train_set_target_pack2[,1,drop=TRUE]
m1_pack2=knn(train=train_set_pack2, test=test_set_pack2, train_set_target_pack2, k=1 )

###knn results for train###

m2_pack2=knn(train=train_set, test=train_set, train_set_target_pack2, k=1 )
####test matric for pack2####
test_mat_pack2=table(test_set_target_pack2[,1,drop=TRUE],m1_pack2)
test_mat_pack2

testperf_pack2 = sum(test_mat_pack2[1,1], test_mat_pack2[2,2], test_mat_pack2[3,3])/length(test_set_target_pack2[,1,drop=TRUE])
testperf_pack2
######################
#####################
#########################
###gets train and test target values for pack 3###
train_set_pack3=class_ord_mat_s_pack3[1:6603,]
test_set_pack3=class_ord_mat_s_pack3[6604:8252,]
train_set_target_pack3=class_ord_mat_pack3[1:6603,1]
test_set_target_pack3=class_ord_mat_pack3[6604:8252,1]
#############################################

###knn results for test of pack 1 k=1###
require(class)
train_set_target_pack3=train_set_target_pack3[,1,drop=TRUE]
m1_pack3=knn(train=train_set_pack3, test=test_set_pack3, train_set_target_pack3, k=1 )

###knn results for train###

m2_pack3=knn(train=train_set, test=train_set, train_set_target_pack3, k=1 )
####test matric for pack3####
test_mat_pack3=table(test_set_target_pack3[,1,drop=TRUE],m1_pack3)
test_mat_pack3

testperf_pack3 = sum(test_mat_pack3[1,1], test_mat_pack3[2,2], test_mat_pack3[3,3])/length(test_set_target_pack3[,1,drop=TRUE])
testperf_pack3
########################
########################
########################

###gets train and test target values for pack 3###
train_set_pack4=class_ord_mat_s_pack4[1:6603,]
test_set_pack4=class_ord_mat_s_pack4[6604:8252,]
train_set_target_pack4=class_ord_mat_pack4[1:6603,1]
test_set_target_pack4=class_ord_mat_pack4[6604:8252,1]
#############################################

###knn results for test of pack 1 k=1###
require(class)
train_set_target_pack4=train_set_target_pack4[,1,drop=TRUE]
m1_pack4=knn(train=train_set_pack4, test=test_set_pack4, train_set_target_pack4, k=1 )

###knn results for train###

m2_pack4=knn(train=train_set, test=train_set, train_set_target_pack4, k=1 )
####test matric for pack4####
test_mat_pack4=table(test_set_target_pack4[,1,drop=TRUE],m1_pack4)
test_mat_pack4

testperf_pack4 = sum(test_mat_pack4[1,1], test_mat_pack4[2,2], test_mat_pack4[3,3])/length(test_set_target_pack4[,1,drop=TRUE])
testperf_pack4

######################################
###1.8
######################################


#first we apply the weight to our training set in each pack

weighted_pack1_train=testperf_pack1*train_set_pack1

weighted_pack2_train=testperf_pack2*train_set_pack2

weighted_pack3_train=testperf_pack3*train_set_pack3

weighted_pack4_train=testperf_pack4*train_set_pack4

### we combine our weighted training set to get all values of our training set
# Note that the order of columns will change but it does not matter in our computation
weighted_packs_train=cbind(weighted_pack1_train,weighted_pack2_train,weighted_pack3_train,weighted_pack4_train)

#Now we apply the weight to our test set in each pack
weighted_pack1_test=testperf_pack1*test_set_pack1

weighted_pack2_test=testperf_pack2*test_set_pack2

weighted_pack3_test=testperf_pack3*test_set_pack3

weighted_pack4_test=testperf_pack4*test_set_pack4

# we combine them, the order of the columns will be the same as in our weighted_packs_train matrix

weighted_packs_test=cbind(weighted_pack1_test,weighted_pack2_test,weighted_pack3_test,weighted_pack4_test)

#We put our test and training sets together to from a new matrix of all values, since the order of the columns was the same the integrity of the data will be maintained. Note also that all the test cases will be together at the bottom

weighted_packs_all=rbind(weighted_packs_train,weighted_packs_test)




# finally we merge all of our data including the first three columns
essen_cols=class_ord_mat[,1:3]

weighted_data=cbind(essen_cols,weighted_packs_all)
weighted_data

## now we will normalize the 400 vectors of our data

normalize= function(x){
  return((x-min(x))/(max(x)-min(x)))
}
normalize(c(1,2,3,4,5))  

Weighted_data_n=as.data.frame(lapply(weighted_data[,4:403], normalize))

### finally we apply knn to test the performance of our new normalized and weighted data
#we get our training and testing sets and the targets for the knn function
weighted_train_set=Weighted_data_n[1:6603,]
Weighted_test_set=Weighted_data_n[6604:8252,]
weighted_train_set_target=weighted_data[1:6603,1]
Weighted_test_set_target=weighted_data[6604:8252,1]

###knn results for test###
require(class)
m1_weighted=knn(train=weighted_train_set, test=Weighted_test_set, weighted_train_set_target, k=1 )
m1_weighted

test_mat_weighted=table(Weighted_test_set_target,m1_weighted)
test_mat_weighted

testperf_weighted=sum(test_mat_weighted[1,1], test_mat_weighted[2,2], test_mat_weighted[3,3])/length(test_set_target[,1,drop=TRUE])
testperf_weighted
#################Train Perfomance###################
m2_weighted=knn(train=weighted_train_set, test=weighted_train_set, weighted_train_set_target, k=1 )
m2_weighted

train_mat_weighted=table(weighted_train_set_target,m2_weighted)
train_mat_weighted

trainperf_weighted=sum(train_mat_weighted[1,1], train_mat_weighted[2,2], train_mat_weighted[3,3])/length(train_set_target[,1,drop=TRUE])
trainperf_weighted
