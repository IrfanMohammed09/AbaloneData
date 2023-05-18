library('pacman')
p_load('readr','sna','psych','corrplot','factoextra','class','gmodels','ggplot2','plotly')
columnNames=c('Sex','Length','Diameter','Height','Whole weight','Shucked weight','Viscera weight','Shell weight','Rings')
#reading data 

abaloneData=read.table('Documents/sem 2/Bigdata/Project2/abalone.data',stringsAsFactors = FALSE,sep = ',',col.names = columnNames)
head(abaloneData)
str(abaloneData)
abaloneData1=abaloneData
plot(abaloneData)

ggplot(data = abaloneData, aes(x = Length, y = Rings)) + 
  geom_point() +
  facet_grid(Sex ~ .) +
  labs(title = "Scatterplot matrix of abaloneData length vs rings")

ggplot(data = abaloneData, aes(x = Diameter, y = Rings)) + 
  geom_point() +
  facet_grid(Sex ~ .) +
  labs(title = "Scatterplot matrix of abaloneData Diameter vs rings")

ggplot(data = abaloneData, aes(x = Height, y = Rings)) + 
  geom_point() +
  facet_grid(Sex ~ .) +
  labs(title = "Scatterplot matrix of abaloneData Height vs rings")

ggplot(data = abaloneData, aes(x = Whole.weight, y = Rings)) + 
  geom_point() +
  facet_grid(Sex ~ .) +
  labs(title = "Scatterplot matrix of abaloneData Whole weight vs rings")

ggplot(data = abaloneData, aes(x = Shucked.weight, y = Rings)) + 
  geom_point() +
  facet_grid(Sex ~ .) +
  labs(title = "Scatterplot matrix of abaloneData Shucked weight vs rings")

ggplot(data = abaloneData, aes(x = Viscera.weight, y = Rings)) + 
  geom_point() +
  facet_grid(Sex ~ .) +
  labs(title = "Scatterplot matrix of abaloneData Viscera weight vs rings")

ggplot(data = abaloneData, aes(x = Shell.weight, y = Rings)) + 
  geom_point() +
  facet_grid(Sex ~ .) +
  labs(title = "Scatterplot matrix of abaloneData Shell weight vs rings")


ggplot(data = abaloneData, aes(x = Sex, y = Rings)) + 
  geom_boxplot() +
  labs(title = "Boxplot of Rings by Sex")

plot_ly(abaloneData, x = ~Length, y = ~Diameter, z = ~Rings, color = ~Sex) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = "Length"),
                      yaxis = list(title = "Diameter"),
                      zaxis = list(title = "Rings")),
         title = "3D Scatterplot of abaloneData")

plot_ly(abaloneData, x = ~Shucked.weight, y = ~Viscera.weight, z = ~Shell.weight, color = ~Sex) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = "Shucked.weight"),
                      yaxis = list(title = "Viscera.weight"),
                      zaxis = list(title = "Shell.weight")),
         title = "3D Scatterplot of abaloneData")


#converting categorical value into numerical value
abaloneData1$Sex <- replace(abaloneData1$Sex, abaloneData1$Sex == "M", 0)
abaloneData1$Sex <- replace(abaloneData1$Sex, abaloneData1$Sex == "F", 1)
abaloneData1$Sex <- replace(abaloneData1$Sex, abaloneData1$Sex == "I", 2)


abaloneData1$Sex=as.numeric(abaloneData1$Sex)
head(abaloneData1)

#preparing data

summary(abaloneData1)
describe(abaloneData1)
plot(abaloneData1)
#plotting Correlation
correlation_table=cor(abaloneData1)
corrplot(correlation_table,method = 'circle')
#removing sex as it is not correlated with rings
abaloneData1=abaloneData1[2:9]
#correlation plot after removing'Sex' column
correlation_table=cor(abaloneData1)
corrplot(correlation_table,method = 'circle')
#min-max normalization
normalize <- function(x) {((x-min(x))/(max(x)-min(x)))}
abaloneData1.norm=as.data.frame(lapply(abaloneData1,normalize))
#Z-score normalization
abaloneData1.znorm=as.data.frame(lapply(abaloneData1, scale))
head(abaloneData1.norm)

#k-means

abaloneKmeans2=kmeans(x = abaloneData1.norm,centers = 2,nstart = 2,algorithm = c("Hartigan-Wong"),trace = FALSE)
abaloneKmeans2
factoextra::fviz_cluster(abaloneKmeans2,abaloneData1.norm)

abaloneKmeans3=kmeans(x = abaloneData1.norm,centers = 3,nstart = 3,algorithm = c("Hartigan-Wong"),trace = FALSE)
abaloneKmeans3
factoextra::fviz_cluster(abaloneKmeans3,abaloneData1.norm)

abaloneKmeans4=kmeans(x = abaloneData1.norm,centers = 4,nstart = 4,algorithm = c("Hartigan-Wong"),trace = FALSE)
abaloneKmeans4
factoextra::fviz_cluster(abaloneKmeans4,abaloneData1.norm)

abaloneKmeans5=kmeans(x = abaloneData1.norm,centers = 5,nstart = 5,algorithm = c("Hartigan-Wong"),trace = FALSE)
abaloneKmeans5
factoextra::fviz_cluster(abaloneKmeans5,abaloneData1.norm)

abaloneKmeans6=kmeans(x = abaloneData1.norm,centers = 6,nstart = 6,algorithm = c("Hartigan-Wong"),trace = FALSE)
abaloneKmeans6
factoextra::fviz_cluster(abaloneKmeans6,abaloneData1.norm)

abaloneKmeans7=kmeans(x = abaloneData1.norm,centers = 7,nstart = 7,algorithm = c("Hartigan-Wong"),trace = FALSE)
abaloneKmeans7
factoextra::fviz_cluster(abaloneKmeans7,abaloneData1.norm)

abaloneKmeans8=kmeans(x = abaloneData1.norm,centers = 8,nstart = 8,algorithm = c("Hartigan-Wong"),trace = FALSE)
abaloneKmeans8
factoextra::fviz_cluster(abaloneKmeans8,abaloneData1.norm)

abaloneKmeans9=kmeans(x = abaloneData1.norm,centers = 9,nstart = 9,algorithm = c("Hartigan-Wong"),trace = FALSE)
abaloneKmeans9
factoextra::fviz_cluster(abaloneKmeans9,abaloneData1.norm)

factoextra::fviz_nbclust(x = abaloneData1.norm,FUNcluster = kmeans,method = "wss",k.max = 14,verbose = TRUE)

#train test split using 70-30

abaloneData1.norm.rows=nrow(abaloneData1.norm)
trainDataPercent=0.7
trainDataRows=trainDataPercent*abaloneData1.norm.rows
abaloneData1.train=sample(abaloneData1.norm.rows,trainDataRows)
length(abaloneData1.train)
abaloneData1.trainData=abaloneData1.norm[abaloneData1.train,]
head(abaloneData1.trainData)
abaloneData1.testData=abaloneData1.norm[-abaloneData1.train,]
head(abaloneData1.testData)


#abaloneData1.trainData.labels=abaloneData1.trainData[9]
#head(abaloneData1.trainData.labels)
#abaloneData1.trainData.inputs=abaloneData1.trainData[1:8]
#head(abaloneData1.trainData.inputs)
#abaloneData1.testData.labels=abaloneData1.testData[9]
#head(abaloneData1.trainData.labels)
#abaloneData1.testData.inputs=abaloneData1.testData[1:8]
#head(abaloneData1.trainData.inputs)
#cl=abaloneData1.trainData.labels[,1]
#abaloneData1.testData.k7=knn(train = abaloneData1.trainData.inputs,test = abaloneData1.testData.inputs,cl,prob = FALSE,k = 7,)

#using Cluster of size 9
#k-means on train data
abaloneData1.trainData.kmeans9=kmeans(abaloneData1.trainData,centers = 9,nstart = 9,algorithm = c("Hartigan-Wong"),trace = FALSE)
abaloneData1.trainData.kmeans9

#knn 
abaloneData1.testData.k9=knn(abaloneData1.trainData,abaloneData1.testData,abaloneData1.trainData.kmeans9$cluster,prob = FALSE,k = 9)
head(abaloneData1.testData.k9)

#k-means on test data
abaloneData1.testData.kmeans9=kmeans(abaloneData1.testData,centers = 9)
abaloneData1.testData.kmeans9
abaloneData1.testData.k9.labels=abaloneData1.testData.kmeans9$cluster
head(abaloneData1.testData.k9.labels)

#glm computing a glm using train data
abaloneData1.trainData.glm<-glm(formula = Rings~Length+Diameter+Height+Whole.weight+Shucked.weight+Viscera.weight+Shell.weight,family = gaussian, data=abaloneData1.trainData)
summary(abaloneData1.trainData.glm)
abaloneData1.trainData.glm.anova=anova(abaloneData1.trainData.glm, test = "Chisq")
abaloneData1.trainData.glm.anova
plot(abaloneData1.trainData.glm)

#predicting rings from test data
newdata=as.data.frame(abaloneData1.testData)
abaloneData1.testData.pred=predict(object = abaloneData1.trainData.glm, newdata = newdata)
head(abaloneData1.testData.pred)
length(abaloneData1.testData.pred)
summary(abaloneData1.testData.pred)
confint(abaloneData1.trainData.glm)

#applying k-means on predicted values 
abaloneData1.testData.pred.kmeans9=kmeans(abaloneData1.testData.pred,centers = 9)
abaloneData1.testData.pred.kmeans9

#caluclate cross table
abaloneData1.test.comp.k9=CrossTable(abaloneData1.testData.pred.kmeans9$cluster,abaloneData1.testData.kmeans9$cluster,prop.chisq=TRUE) 

#compute accuracy

# Compute confusion matrix
conf_mat <- abaloneData1.test.comp.k9$t

# Compute classification metrics
n_classes <- nrow(conf_mat)
accuracy <- sum(diag(conf_mat)) / sum(conf_mat)
precision <- diag(conf_mat) / colSums(conf_mat)
recall <- diag(conf_mat) / rowSums(conf_mat)
specificity <- (sum(diag(conf_mat)) - diag(conf_mat)) / (sum(conf_mat) - rowSums(conf_mat))
error <- 1 - accuracy
f_measure <- 2 * precision * recall / (precision + recall)


plot(abaloneData1.testData$Rings,abaloneData1.testData.pred)

# cluster of size of 5

#k-means on train data
abaloneData1.trainData.kmeans5=kmeans(abaloneData1.trainData,centers = 5,nstart = 5,algorithm = c("Hartigan-Wong"),trace = FALSE)
abaloneData1.trainData.kmeans5

#knn 
abaloneData1.testData.k5=knn(abaloneData1.trainData,abaloneData1.testData,abaloneData1.trainData.kmeans5$cluster,prob = FALSE,k = 5)
head(abaloneData1.testData.k5)

#k-means on test data
abaloneData1.testData.kmeans5=kmeans(abaloneData1.testData,centers = 5)
abaloneData1.testData.kmeans5
abaloneData1.testData.k5.labels=abaloneData1.testData.kmeans5$cluster
head(abaloneData1.testData.k5.labels)


#applying k-means on predicted values 
abaloneData1.testData.pred.kmeans5=kmeans(abaloneData1.testData.pred,centers = 5)
abaloneData1.testData.pred.kmeans5

#caluclate cross table
abaloneData1.test.comp.k5=CrossTable(abaloneData1.testData.pred.kmeans5$cluster,abaloneData1.testData.kmeans5$cluster,prop.chisq=TRUE) 

#compute accuracy

# Compute confusion matrix
conf_mat <- abaloneData1.test.comp.k5$t

# Compute classification metrics
n_classes <- nrow(conf_mat)
accuracy <- sum(diag(conf_mat)) / sum(conf_mat)
precision <- diag(conf_mat) / colSums(conf_mat)
recall <- diag(conf_mat) / rowSums(conf_mat)
specificity <- (sum(diag(conf_mat)) - diag(conf_mat)) / (sum(conf_mat) - rowSums(conf_mat))
error <- 1 - accuracy
f_measure <- 2 * precision * recall / (precision + recall)


plot(abaloneData1.testData$Rings,abaloneData1.testData.pred)

#cluster of size 7
#k-means on train data
abaloneData1.trainData.kmeans7=kmeans(abaloneData1.trainData,centers = 7,nstart = 7,algorithm = c("Hartigan-Wong"),trace = FALSE)
abaloneData1.trainData.kmeans7

#knn 
abaloneData1.testData.k7=knn(abaloneData1.trainData,abaloneData1.testData,abaloneData1.trainData.kmeans7$cluster,prob = FALSE,k = 7)
head(abaloneData1.testData.k7)

#k-means on test data
abaloneData1.testData.kmeans7=kmeans(abaloneData1.testData,centers = 7)
abaloneData1.testData.kmeans7
abaloneData1.testData.k7.labels=abaloneData1.testData.kmeans7$cluster
head(abaloneData1.testData.k7.labels)


#applying k-means on predicted values 
abaloneData1.testData.pred.kmeans7=kmeans(abaloneData1.testData.pred,centers = 7)
abaloneData1.testData.pred.kmeans7

#caluclate cross table
abaloneData1.test.comp.k7=CrossTable(abaloneData1.testData.pred.kmeans7$cluster,abaloneData1.testData.kmeans7$cluster,prop.chisq=TRUE) 

#compute accuracy

# Compute confusion matrix
conf_mat <- abaloneData1.test.comp.k7$t

# Compute classification metrics
n_classes <- nrow(conf_mat)
accuracy <- sum(diag(conf_mat)) / sum(conf_mat)
precision <- diag(conf_mat) / colSums(conf_mat)
recall <- diag(conf_mat) / rowSums(conf_mat)
specificity <- (sum(diag(conf_mat)) - diag(conf_mat)) / (sum(conf_mat) - rowSums(conf_mat))
error <- 1 - accuracy
f_measure <- 2 * precision * recall / (precision + recall)


plot(abaloneData1.testData$Rings,abaloneData1.testData.pred)






#train test split using 60-40

abaloneData1.norm.rows=nrow(abaloneData1.norm)
trainDataPercent=0.6
trainDataRows=trainDataPercent*abaloneData1.norm.rows
abaloneData1.train=sample(abaloneData1.norm.rows,trainDataRows)
length(abaloneData1.train)
abaloneData1.trainData=abaloneData1.norm[abaloneData1.train,]
head(abaloneData1.trainData)
abaloneData1.testData=abaloneData1.norm[-abaloneData1.train,]
dim(abaloneData1.testData)
head(abaloneData1.testData)


#using Cluster of size 9
#k-means on train data
abaloneData1.trainData.kmeans9=kmeans(abaloneData1.trainData,centers = 9,nstart = 9,algorithm = c("Hartigan-Wong"),trace = FALSE)
abaloneData1.trainData.kmeans9

#knn 
abaloneData1.testData.k9=knn(abaloneData1.trainData,abaloneData1.testData,abaloneData1.trainData.kmeans9$cluster,prob = FALSE,k = 9)
head(abaloneData1.testData.k9)

#k-means on test data
abaloneData1.testData.kmeans9=kmeans(abaloneData1.testData,centers = 9)
abaloneData1.testData.kmeans9
abaloneData1.testData.k9.labels=abaloneData1.testData.kmeans9$cluster
head(abaloneData1.testData.k9.labels)

#glm computing a glm using train data
abaloneData1.trainData.glm<-glm(formula = Rings~Length+Diameter+Height+Whole.weight+Shucked.weight+Viscera.weight+Shell.weight,family = gaussian, data=abaloneData1.trainData)
summary(abaloneData1.trainData.glm)
abaloneData1.trainData.glm.anova=anova(abaloneData1.trainData.glm, test = "Chisq")
abaloneData1.trainData.glm.anova
plot(abaloneData1.trainData.glm)

#predicting rings from test data
newdata=as.data.frame(abaloneData1.testData)
abaloneData1.testData.pred=predict(object = abaloneData1.trainData.glm, newdata = newdata)
head(abaloneData1.testData.pred)
length(abaloneData1.testData.pred)
summary(abaloneData1.testData.pred)
confint(abaloneData1.trainData.glm)

#applying k-means on predicted values 
abaloneData1.testData.pred.kmeans9=kmeans(abaloneData1.testData.pred,centers = 9)
abaloneData1.testData.pred.kmeans9

#caluclate cross table
abaloneData1.test.comp.k9=CrossTable(abaloneData1.testData.pred.kmeans9$cluster,abaloneData1.testData.kmeans9$cluster,prop.chisq=TRUE) 

#compute accuracy

# Compute confusion matrix
conf_mat <- abaloneData1.test.comp.k9$t

# Compute classification metrics
n_classes <- nrow(conf_mat)
n_classes
accuracy <- sum(diag(conf_mat)) / sum(conf_mat)
accuracy
precision <- diag(conf_mat) / colSums(conf_mat)
recall <- diag(conf_mat) / rowSums(conf_mat)
specificity <- (sum(diag(conf_mat)) - diag(conf_mat)) / (sum(conf_mat) - rowSums(conf_mat))
error <- 1 - accuracy
f_measure <- 2 * precision * recall / (precision + recall)


plot(abaloneData1.testData$Rings,abaloneData1.testData.pred)

# cluster of size of 5

#k-means on train data
abaloneData1.trainData.kmeans5=kmeans(abaloneData1.trainData,centers = 5,nstart = 5,algorithm = c("Hartigan-Wong"),trace = FALSE)
abaloneData1.trainData.kmeans5

#knn 
abaloneData1.testData.k5=knn(abaloneData1.trainData,abaloneData1.testData,abaloneData1.trainData.kmeans5$cluster,prob = FALSE,k = 5)
head(abaloneData1.testData.k5)

#k-means on test data
abaloneData1.testData.kmeans5=kmeans(abaloneData1.testData,centers = 5)
abaloneData1.testData.kmeans5
abaloneData1.testData.k5.labels=abaloneData1.testData.kmeans5$cluster
head(abaloneData1.testData.k5.labels)


#applying k-means on predicted values 
abaloneData1.testData.pred.kmeans5=kmeans(abaloneData1.testData.pred,centers = 5)
abaloneData1.testData.pred.kmeans5

#caluclate cross table
abaloneData1.test.comp.k5=CrossTable(abaloneData1.testData.pred.kmeans5$cluster,abaloneData1.testData.kmeans5$cluster,prop.chisq=TRUE) 

#compute accuracy

# Compute confusion matrix
conf_mat <- abaloneData1.test.comp.k5$t

# Compute classification metrics
n_classes <- nrow(conf_mat)
accuracy <- sum(diag(conf_mat)) / sum(conf_mat)
precision <- diag(conf_mat) / colSums(conf_mat)
recall <- diag(conf_mat) / rowSums(conf_mat)
specificity <- (sum(diag(conf_mat)) - diag(conf_mat)) / (sum(conf_mat) - rowSums(conf_mat))
error <- 1 - accuracy
f_measure <- 2 * precision * recall / (precision + recall)


plot(abaloneData1.testData$Rings,abaloneData1.testData.pred)

#cluster of size 7
#k-means on train data
abaloneData1.trainData.kmeans7=kmeans(abaloneData1.trainData,centers = 7,nstart = 7,algorithm = c("Hartigan-Wong"),trace = FALSE)
abaloneData1.trainData.kmeans7

#knn 
abaloneData1.testData.k7=knn(abaloneData1.trainData,abaloneData1.testData,abaloneData1.trainData.kmeans7$cluster,prob = FALSE,k = 7)
head(abaloneData1.testData.k7)

#k-means on test data
abaloneData1.testData.kmeans7=kmeans(abaloneData1.testData,centers = 7)
abaloneData1.testData.kmeans7
abaloneData1.testData.k7.labels=abaloneData1.testData.kmeans7$cluster
head(abaloneData1.testData.k7.labels)


#applying k-means on predicted values 
abaloneData1.testData.pred.kmeans7=kmeans(abaloneData1.testData.pred,centers = 7)
abaloneData1.testData.pred.kmeans7

#caluclate cross table
abaloneData1.test.comp.k7=CrossTable(abaloneData1.testData.pred.kmeans7$cluster,abaloneData1.testData.kmeans7$cluster,prop.chisq=TRUE) 

#compute accuracy

# Compute confusion matrix
conf_mat <- abaloneData1.test.comp.k7$t

# Compute classification metrics
n_classes <- nrow(conf_mat)
accuracy <- sum(diag(conf_mat)) / sum(conf_mat)
precision <- diag(conf_mat) / colSums(conf_mat)
recall <- diag(conf_mat) / rowSums(conf_mat)
specificity <- (sum(diag(conf_mat)) - diag(conf_mat)) / (sum(conf_mat) - rowSums(conf_mat))
error <- 1 - accuracy
f_measure <- 2 * precision * recall / (precision + recall)


plot(abaloneData1.testData$Rings,abaloneData1.testData.pred)








# test-train data split of 50-50

abaloneData1.norm.rows=nrow(abaloneData1.norm)
trainDataPercent=0.5
trainDataRows=trainDataPercent*abaloneData1.norm.rows
abaloneData1.train=sample(abaloneData1.norm.rows,trainDataRows)
length(abaloneData1.train)
abaloneData1.trainData=abaloneData1.norm[abaloneData1.train,]
head(abaloneData1.trainData)
abaloneData1.testData=abaloneData1.norm[-abaloneData1.train,]
dim(abaloneData1.testData)
abaloneData1.testData= abaloneData1.testData[1:2088,]
dim(abaloneData1.testData)
head(abaloneData1.testData)

#using Cluster of size 9
#k-means on train data
abaloneData1.trainData.kmeans9=kmeans(abaloneData1.trainData,centers = 9,nstart = 9,algorithm = c("Hartigan-Wong"),trace = FALSE)
abaloneData1.trainData.kmeans9

#knn 
abaloneData1.testData.k9=knn(abaloneData1.trainData,abaloneData1.testData,abaloneData1.trainData.kmeans9$cluster,prob = FALSE,k = 9)
head(abaloneData1.testData.k9)

#k-means on test data
abaloneData1.testData.kmeans9=kmeans(abaloneData1.testData,centers = 9)
abaloneData1.testData.kmeans9
abaloneData1.testData.k9.labels=abaloneData1.testData.kmeans9$cluster
head(abaloneData1.testData.k9.labels)

#glm computing a glm using train data
abaloneData1.trainData.glm<-glm(formula = Rings~Length+Diameter+Height+Whole.weight+Shucked.weight+Viscera.weight+Shell.weight,family = gaussian, data=abaloneData1.trainData)
summary(abaloneData1.trainData.glm)
abaloneData1.trainData.glm.anova=anova(abaloneData1.trainData.glm, test = "Chisq")
abaloneData1.trainData.glm.anova
plot(abaloneData1.trainData.glm)

#predicting rings from test data
newdata=as.data.frame(abaloneData1.testData)
abaloneData1.testData.pred=predict(object = abaloneData1.trainData.glm, newdata = newdata)
head(abaloneData1.testData.pred)
length(abaloneData1.testData.pred)
summary(abaloneData1.testData.pred)
confint(abaloneData1.trainData.glm)

#applying k-means on predicted values 
abaloneData1.testData.pred.kmeans9=kmeans(abaloneData1.testData.pred,centers = 9)
abaloneData1.testData.pred.kmeans9

#caluclate cross table
length(abaloneData1.testData.pred.kmeans9$cluster)
length(abaloneData1.testData.kmeans9$cluster)
abaloneData1.test.comp.k9=CrossTable(abaloneData1.testData.pred.kmeans9$cluster,abaloneData1.testData.kmeans9$cluster,prop.chisq=TRUE) 

#compute accuracy

# Compute confusion matrix
conf_mat <- abaloneData1.test.comp.k9$t

# Compute classification metrics
n_classes <- nrow(conf_mat)
accuracy <- sum(diag(conf_mat)) / sum(conf_mat)
precision <- diag(conf_mat) / colSums(conf_mat)
recall <- diag(conf_mat) / rowSums(conf_mat)
specificity <- (sum(diag(conf_mat)) - diag(conf_mat)) / (sum(conf_mat) - rowSums(conf_mat))
error <- 1 - accuracy
f_measure <- 2 * precision * recall / (precision + recall)


plot(abaloneData1.testData$Rings,abaloneData1.testData.pred)

# cluster of size of 5

#k-means on train data
abaloneData1.trainData.kmeans5=kmeans(abaloneData1.trainData,centers = 5,nstart = 5,algorithm = c("Hartigan-Wong"),trace = FALSE)
abaloneData1.trainData.kmeans5

#knn 
abaloneData1.testData.k5=knn(abaloneData1.trainData,abaloneData1.testData,abaloneData1.trainData.kmeans5$cluster,prob = FALSE,k = 5)
head(abaloneData1.testData.k5)

#k-means on test data
abaloneData1.testData.kmeans5=kmeans(abaloneData1.testData,centers = 5)
abaloneData1.testData.kmeans5
abaloneData1.testData.k5.labels=abaloneData1.testData.kmeans5$cluster
head(abaloneData1.testData.k5.labels)


#applying k-means on predicted values 
abaloneData1.testData.pred.kmeans5=kmeans(abaloneData1.testData.pred,centers = 5)
abaloneData1.testData.pred.kmeans5

#caluclate cross table
abaloneData1.test.comp.k5=CrossTable(abaloneData1.testData.pred.kmeans5$cluster,abaloneData1.testData.kmeans5$cluster,prop.chisq=TRUE) 

#compute accuracy

# Compute confusion matrix
conf_mat <- abaloneData1.test.comp.k5$t

# Compute classification metrics
n_classes <- nrow(conf_mat)
accuracy <- sum(diag(conf_mat)) / sum(conf_mat)
precision <- diag(conf_mat) / colSums(conf_mat)
recall <- diag(conf_mat) / rowSums(conf_mat)
specificity <- (sum(diag(conf_mat)) - diag(conf_mat)) / (sum(conf_mat) - rowSums(conf_mat))
error <- 1 - accuracy
f_measure <- 2 * precision * recall / (precision + recall)


plot(abaloneData1.testData$Rings,abaloneData1.testData.pred)

#cluster of size 7
#k-means on train data
abaloneData1.trainData.kmeans7=kmeans(abaloneData1.trainData,centers = 7,nstart = 7,algorithm = c("Hartigan-Wong"),trace = FALSE)
abaloneData1.trainData.kmeans7

#knn 
abaloneData1.testData.k7=knn(abaloneData1.trainData,abaloneData1.testData,abaloneData1.trainData.kmeans7$cluster,prob = FALSE,k = 7)
head(abaloneData1.testData.k7)

#k-means on test data
abaloneData1.testData.kmeans7=kmeans(abaloneData1.testData,centers = 7)
abaloneData1.testData.kmeans7
abaloneData1.testData.k7.labels=abaloneData1.testData.kmeans7$cluster
head(abaloneData1.testData.k7.labels)


#applying k-means on predicted values 
abaloneData1.testData.pred.kmeans7=kmeans(abaloneData1.testData.pred,centers = 7)
abaloneData1.testData.pred.kmeans7

#caluclate cross table
abaloneData1.test.comp.k7=CrossTable(abaloneData1.testData.pred.kmeans7$cluster,abaloneData1.testData.kmeans7$cluster,prop.chisq=TRUE) 

#compute accuracy

# Compute confusion matrix
conf_mat <- abaloneData1.test.comp.k7$t

# Compute classification metrics
n_classes <- nrow(conf_mat)
accuracy <- sum(diag(conf_mat)) / sum(conf_mat)
precision <- diag(conf_mat) / colSums(conf_mat)
recall <- diag(conf_mat) / rowSums(conf_mat)
specificity <- (sum(diag(conf_mat)) - diag(conf_mat)) / (sum(conf_mat) - rowSums(conf_mat))
error <- 1 - accuracy
f_measure <- 2 * precision * recall / (precision + recall)


plot(abaloneData1.testData$Rings,abaloneData1.testData.pred)
