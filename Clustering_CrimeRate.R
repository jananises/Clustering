######R Code on Clustering Assignment#######################
# Assignment1 - Crime Rate
##Data Load
CrimeAll <- read.csv("C:\\Data Science\\Assignments\\Clustering\\crime_data.csv") 
Crime <- CrimeAll[,-1]    # Exclude Country Names

#barplot(Crime, main="Crime Indicators", xlab="No of occurences", ylab="Crime Types", horiz=TRUE)

# Normalizing columns to bring them under same scale
Crime.Scaled <- scale(CrimeAll[,2:5])   #excluding the X column before normalizing
View(Crime.Scaled)

#Computing the distance Matrix"
d <- dist(Crime.Scaled, method = "euclidean") 

#Buiding algorithm
fit.average <- hclust(d, method="average") 

#display dendogram
plot(fit.average, labels=CrimeAll$X, hang=-1, cex=.8, main="Average Linkage Clustering")

#Selecting the best number of clusters
library(NbClust)

#Selecting the optimum/best number of clusters
nc <- NbClust(Crime.Scaled, distance="euclidean",min.nc=2, max.nc=15, method="average")
table(nc$Best.n[1,])

#Going with 5 as the best number of clusters
barplot(table(nc$Best.n[1,]),
        xlab="Numer of Clusters", ylab="Number of Criteria",
        main="Number of Clusters Chosen by 26 Criteria")


#draw dendogram with red borders around 5 clusters
#Buiding algorithm
plot(fit.average, labels=CrimeAll$X, hang=-1)  #display dendogram
#draw dendogram with red borders around 5 clusters
rect.hclust(fit.average, k=5, border = "red")

#cut tree into 5 clusters
clusters <- cutree(fit.average, k=5)
table(clusters)

membership<-as.matrix(clusters) # groups or cluster numbers

aggregate(Crime, by=list(cluster=clusters), median)

aggregate(as.data.frame(Crime.Scaled), by=list(cluster=clusters),median)

final <- data.frame('Country'=CrimeAll[1], membership)
View(final)
plot(final)

plot(fit.average, hang=-1, cex=.8, labels=CrimeAll$X, main="Average Linkage Clustering\n5 Cluster Solution")
rect.hclust(fit.average, k=5, border = "red")


##########################################################################################
####Assignment using K-Means

######R Code on K Means Clustering #######################

# Normalizing columns to bring them under same scale
Crime.Scaled <- scale(CrimeAll[,2:5])   #excluding the X column before normalizing
View(Crime.Scaled)

#Function to Determine the number of Clusters

wssplot <- function(data, nc=15, seed=1234)
  { 
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
    for (i in 2:nc){
        set.seed(seed)
        wss[i] <- sum(kmeans(data, centers=i)$withinss)}
        plot(1:nc, wss, type="b", xlab="Number of Clusters",
        ylab="Within groups sum of squares")
   }

#Plot the elbow Curve, determine the number of clusters
wssplot(Crime.Scaled)

#Find the best K-Value
library(NbClust)
set.seed(1234)
nc <- NbClust(Crime.Scaled, min.nc=2, max.nc=15, method="kmeans")
table(nc$Best.n[1,])

barplot(table(nc$Best.n[1,]),
        xlab="Number of Clusters", ylab="Number of Criteria",
        main="Number of Clusters Chosen by 26 Criteria")

set.seed(1234)
fit.km <- kmeans(Crime, 6, nstart=25)

fit.km$size

fit.km$centers

# Aggregate the clusters
aggregate(Crime, by=list(cluster=fit.km$cluster), mean)


##Animation
install.packages("animation")
library("animation")

windows()
km<- kmeans.ani(Crime,6)
