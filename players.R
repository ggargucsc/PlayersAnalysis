######################### Players dataset #########################

#load the functions file
source('~/Desktop/functions.r')

#read the data
data <- read.csv("~/Desktop/DataScienceCandidateAssignment/DataSet1.csv")

#check zero variance/ near zero variance predictors
library(caret)
drops <- c("purchase_success_pct_2w", "purchase_success_pct_4w","spin_mixed_cnt_3d","spin_mixed_cnt_2w","spin_mixed_cnt_3w",
           "spin_mixed_cnt_4w", "spin_mixed_amt_3d","spin_mixed_amt_2w","spin_mixed_amt_3w","spin_mixed_amt_4w","spin_paid_cnt_2w",
           "spin_paid_amt_1d","spin_paid_cnt_1d","message_sent_shortfeed_cnt","message_sent_notif_cnt","purchase_success_pct_3d","purchase_success_pct_3w", "spin_paid_amt_3d"," spin_paid_cnt_1d","spin_paid_cnt_3d","spin_paid_cnt_3w", "spin_paid_cnt_4w", "spin_paid_amt_2w", "spin_paid_amt_3w", "spin_paid_amt_4w")

data <- data[,!(names(data) %in% drops)]

#visualize your data
library(GGally)
ggpairs(data[,c(10:15)], colour='IsPayer', alpha=0.4)

#correlation
library(corrplot)

#feature selection using correlation
correlationMatrix <- cor(data)

# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.7)

# print indexes of highly correlated attributes
print(highlyCorrelated)
subset <- correlationMatrix[,c(highlyCorrelated)]

d <- apply(subset, 2, function(x) {
  
  return (x[x>=0.7 & x<1])
  
})

#normalize data
data<- as.data.frame(lapply(data, normalize))

############# subset data for first run on penalized logistic regression #########
attach(data)
n <- nrow(data)
prb <- ifelse(IsPayer==0,.02,.98)
smpl <- filteredData[sample(nrow(filteredData), 8000),]  
table(smpl[1])
train.index <- sample(nrow(smpl), 5000)
smpl.train <- smpl[train.index,]
smpl.test <- smpl[-train.index,]


#using logistic regression with L1 penalty(Lasso)
# 10-fold cross-validation
library(glmnet)

nfolds <- 10
case.folds <- rep(1:nfolds,length.out=nrow(data))
case.folds <- sample(case.folds)
data <- na.omit(data)
valid <- sample(nrow(data), 10000)

#valid set
Validation <- as.matrix(na.omit(data[valid,]))
data.cv <- na.omit(data[-valid,])

pred <- matrix(NA, nrow=10000, ncol=nfolds)

for(fold in 1:nfolds)
{ 
  train <- as.matrix(na.omit(data.cv[case.folds!=fold,]))
  test <- as.matrix(data.cv[case.folds==fold, ])
  
  #penalized logistic regression
  model <- glmnet(x=train[,-1], y=train[,1], alpha=1, family="binomial") #lambda=0.001
  
  #tune lambda parameter on validation set
  cv.model <- cv.glmnet(x=Validation[,-1], y= Validation[,1], alpha=1)
  
  best_lambda <- cv.model$lambda.min
  
  temp <- predict(model, newx = test[,-1], type = "response",  best_lambda)
  pred[,fold] <- temp 
}

#avg of predicted values
pred.Avg <- apply(pred, 1, mean)


#prediction metrics
test <- as.data.frame(test)
test$pred <- pred.Avg
test$pred <- temp
test <- na.omit(test)


cutoff <- seq(from=0.4, to=0.6, by=0.05)

#classification metrics
pTemp <- ifelse(test$pred > 0.5, 1, 0)

#confusion matrix
confusion_matrix <- ftable(test$IsPayer,  pTemp)

#accuracy
accuracy <- 100* (sum(diag(confusion_matrix)) / length(pTemp))
sensitivity <-  confusion_matrix[1]/(confusion_matrix[1]+confusion_matrix[3])
specificity <-  confusion_matrix[4]/(confusion_matrix[2]+confusion_matrix[4])
fpr <- 1-specificity 
tpr<- sensitivity

#most important predictors
varImp <- predict(model, type="coefficients")

############# Classification and Regression Trees #####################

#recursive partitioning decision tree method
library(rpart)

fit <- rpart(IsPayer~., method="class",data=smpl.train)
# summarize the fit
printcp(fit) # display the results
plotcp(fit) # visualize cross-validation results
summary(fit)
plot(fit, uniform=TRUE, main="Classification Tree for Chemicals")
text(fit, use.n=TRUE, all=TRUE, cex=.8)
# make predictions
predictions <- predict(fit, smpl.test[,2:194], type="class")
# summarize accuracy
table(predictions, smpl.test[,1])


library(tree)
tr = tree(IsPayer~., method="class",data=smpl.train)
summary(tr)
plot(tr); text(tr)


#randomforest
library(randomForest)
# fit model
fit <- randomForest(IsPayer~., data=data, mtry = 2, importance = TRUE,do.trace = TRUE)
# summarize the fit
summary(fit)
# plot tree
importance(fit)

varImpPlot(fit)
# make predictions
predictions <- predict(fit, smpl.test[2:194], type="class")
# summarize accuracy
table(predictions, smpl.test[,1])


########### k-means clustering #############################
#using pca to reduce dimensions
compactData <- pcaData(data, 0.70)

# Determine number of clusters(using SSE)
#this suggests around 2-3 clusters
wss <- (nrow(data)-1)*sum(apply(data,2,var))
for (i in 2:15) wss[i] <- round(sum(kmeans(data,centers=i)$withinss), digits=0)

d <- data.frame(1:15,wss)
names(d) <- c("loc", "wss")
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

#k-means clustering
km1 = kmeans(compactData, 2, nstart=45) #iter.max=1500
summary(silhouette(km1))

# Cluster sizes
sort(table(km1$clust))
clust <- names(sort(table(km1$clust)))

# First cluster
cluster1 <- row.names(data[km1$clust==clust[1],])
subset1 <- data[cluster1,]
# Second Cluster
cluster2 <- row.names(data[km1$clust==clust[2],])
subset2 <- data[cluster2,]
# Third Cluster
cluster3 <- row.names(data[km1$clust==clust[3],])
subset3 <- data[cluster3,]


#visulaize clusters
library(fpc)
plotcluster(data[,c(reduceddim)], km1$cluster)
pairs(compactData[,c(1:3)], col=km1$cluster)

############################### spherical k-means 
library(skmeans)
library(cluster)
clust_sk <- skmeans(compactData, 3, method='pclust', control=list(verbose=TRUE))
summary(silhouette(clust_sk))


############################ association rules mining
library(arules)
data_trans <- as(as(t(subset1), 'ngCMatrix'), 'transactions')
rules <- apriori(data_trans, parameter = 
                   list(supp = 0.01, conf = 0.01, target = "rules"))
summary(rules)


########################## affinity propagation rules
library(apcluster)
d1 <- as.matrix(data) 
sim <- crossprod(d1)
sim <- sim / sqrt(sim)
clust_ap <- apcluster(sim)



##################decision tree on clusters 
library("rpart")
tree <- rpart(jackpot_choose_bet_cnt_4w ~.,data=subset2[m]) 
plot(tree)
text(tree, pretty=0)

plot(data$recency, data$IsPayer, col=km1$cluster )

############### hierarchical clustering

d <- dist(compactData)   # find distance matrix 
hc <- hclust(d)                # apply hirarchical clustering 
democut<-cutree(hc,k=3)
plot(democut)


############# sparse k-means clustering
library(sparcl)

m <- c(1:20, 26, 32, 38, 44, 50, 56, 62,68, 74, 80, 82, 84,90,96, 98, 104, 110, 116, 122, 128, 134, 140, 146, 148, 150, 158, 164, 170, 176, 182, 188, 194)
dat <- smpl[,m]
km <- KMeansSparseCluster(k, K=2, wbounds = NULL, nstart = 10, silent =
                      FALSE, maxiter=3, centers=NULL)

plot(k[, c(1:4)], col=km$cluster)

####################### Thank you ######################