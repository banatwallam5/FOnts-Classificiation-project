
#question 1
#data
CENTURY <- read.csv("CENTURY.csv", header = TRUE)
EBRIMA <- read.csv("EBRIMA.csv", header = TRUE)
GILL <- read.csv("GILL.csv", header = TRUE)
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

#k means clustering with k=1 to 10
sm= c()
s1=c()
for (i in 1:10){
  km.out=kmeans(sdata,i, nstart=50)
  sm = c(sm,sum(km.out$tot.withinss)) #Total within-cluster sum of squares
  s1 = c(s1,km.out$totss) #the total variance in the data
  qm = sm/s1
  perfk1 = 1-qm
}
qm = sm/s1
perfk1 = 1-qm
par(mar = c(5, 4, 4, 2) + 0.1)
plot(1:10,perfk1,xlab="K",ylab="Reduction of Variance Perf(k)",main="K Means Clustering: k Performance",type='o',pch=19,col='red')



#Question 2
#PCA
res.pca <- prcomp(sdata, scale = FALSE)
#Dataframe with PC1 PC2 and PC3
newdata = res.pca$x[,1:3]
#Eigenvector w1,w2,w3
eigenvec3 <- res.pca$rotation[,1:3]

#k means with 3 clusters
km.out=kmeans(sdata,3, nstart=50)
clusters = km.out$cluster
#Scalar product of w1,w2,w3 and the centers
centers = km.out$centers
ck = centers%*%eigenvec3

#plot of centers
install.packages("scatterplot3d")
library("scatterplot3d")
colors <- c("red", "yellow", "green")
colors <- colors[as.numeric(ck)]
scatterplot3d(ck,color=colors,main="Cluster Centers",pch = 16)
legend("topright",legend=c("Cluster 1","Cluster 2","Cluster 3"), col=c("red", "yellow", "green"),
       pch=c(16,16,16),lty=c(1,2), ncol=1)

#pc scores with cluster assignment
newdata2 <- cbind(newdata,clusters)
colors <- c("red", "yellow", "green")
colors <- colors[as.numeric(newdata2[, 'clusters'])]
scatterplot3d(newdata2,color=colors,pch = 16,main="K Means Clustering")
legend("topright",legend=c("Cluster 1","Cluster 2","Cluster 3"), col=c("red", "yellow", "green"),
       pch=c(16,16,16),lty=c(1,2), ncol=1)

#sizes of 3 clusters
km.out$size #cluster 1 is the largest
#add fonts to principal component data
newdata2 <- cbind(newdata2,fonts)
#Data frame with only cluster 1 
bigCLU <- subset(newdata2, clusters == 1)
#Plot with largest cluster colored by class
colors <- c("#999999", "#E69F00", "#56B4E9")
colors <- colors[as.factor(bigCLU[, 'fonts'])]
scatterplot3d(bigCLU[,1:3],color=colors,pch = 16,main="K Means Clustering: Largest Cluster by Class")
legend("topright",legend=c("Century","Ebrima","Gill"), col=c("#999999", "#E69F00", "#56B4E9"),
       pch=c(16,16,16),lty=c(1,2), ncol=1)

#regular data with standardized data, clusters, and fonts
DATA <- cbind(sdata,clusters,fonts)

#cluster1
clus1 <- subset(DATA, clusters == 1)
#cluster2
clus2 <- subset(DATA, clusters == 2)
#cluster3
clus3 <- subset(DATA, clusters == 3)
install.packages('plyr')
library(plyr)
#get a count of each font in cluster 1
count1 <- count(clus1$fonts)
#get a count of each font in cluster 2
count2 <- count(clus2$fonts)
#get a count of each font in cluster 3
count3 <-count(clus3$fonts)
#put counts together
counts <- cbind(count1,count2$freq,count3$freq)
names(counts) <- c("Fonts","Cluster 1", "Cluster 2","Cluster 3")

#Formula calculates Frequency
freq <- function(x) {(x)/colSums(x)}
#data frame with frequencies of fonts in each cluster
FREQF <- cbind(counts$Fonts,freq(counts[,2:4]))
names(FREQF) <- c("Fonts","Cluster 1", "Cluster 2","Cluster 3")
FREQF

DATA <- cbind(SDATA,clusters, DATA_full$font)

#cluster1
clus1 <- subset(DATA, clusters == 1)
#cluster2
clus2 <- subset(DATA, clusters == 2)
#cluster3
clus3 <- subset(DATA, clusters == 3)

##Get counts of each font
counts = c()
count1 = table(clus1$`DATA_full$font`)
count2 = table(clus2$`DATA_full$font`)
count3 = table(clus3$`DATA_full$font`)
##Combine counts
counts <- cbind(count1,count2,count3)

freq <- function(x) {(x)/colSums(x)}

Fonts = c("CENTURY", "EBRIMA", "GILL")
FREQF <- cbind(Fonts,freq(counts[,1:3]))
names(FREQF) <- c("Fonts","Cluster 1", "Cluster 2","Cluster 3")
FREQF

#Predictions
prediction <- c()
for (i in 1:length(clusters)){
  if(clusters[i] == 1) {
    prediction <- append(prediction,'EBRIMA')
  } 
  else if(clusters[i] == 2) {
    prediction <- append(prediction, 'GILL')
  }
  else if(clusters[i] == 3) {
    prediction <- append(prediction, 'CENTURY')
  }
}
CONF <- table(DATA_full$font,prediction)






#Formula calculates global accuracy
globacc <- function(x) {(x)/rowSums(x)*100}
round(globacc(CONF),0)
