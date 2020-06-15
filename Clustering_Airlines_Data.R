######R Code on Clustering Assignment#######################
# Assignment2 - Airlines using Hierarchical Clustering
##Data Load
library(readxl)
library(cluster)

#Load from Excel
AirlinesAll <- read_excel("C:\\Data Science\\Assignments\\Clustering\\EastWestAirlines.xlsx", sheet = "data") 
Airlines <- AirlinesAll[,-1]    # # Removing the first column which is an ID column
head(Airlines,10)   #Return 10 rows

#Check for Null, Missing values
CheckforNull <- sum(is.na(Airlines))
CheckforNull #If 0, then no Null Values

dim(Airlines)
colnames(Airlines)
str(Airlines)

#Conversion for cc1_miles, cc2_miles, cc3_miles, converted the cc1_miles, cc2_miles, cc3_miles to numeric by taking the average of the respective range
Airlines$cc1_miles = ifelse(Airlines$cc1_miles==1,2500,
                              ifelse(Airlines$cc1_miles==2,7500,
                                     ifelse(Airlines$cc1_miles==3,17500,
                                            ifelse(Airlines$cc1_miles==4,32500,
                                                   ifelse(Airlines$cc1_miles==5,50000,0)))))

Airlines$cc2_miles = ifelse(Airlines$cc2_miles==1,2500,
                              ifelse(Airlines$cc2_miles==2,7500,
                                     ifelse(Airlines$cc2_miles==3,17500,
                                            ifelse(Airlines$cc2_miles==4,32500,
                                                   ifelse(Airlines$cc2_miles==5,50000,0)))))

Airlines$cc3_miles = ifelse(Airlines$cc3_miles==1,2500,
                              ifelse(Airlines$cc3_miles==2,7500,
                                     ifelse(Airlines$cc3_miles==3,17500,
                                            ifelse(Airlines$cc3_miles==4,32500,
                                                   ifelse(Airlines$cc3_miles==5,50000,0)))))

summary(Airlines)

# Normalizing columns to bring them under same scale
Airlines.Scaled <- scale(AirlinesAll[,c(-1)])   #excluding the ID column before normalizing
View(Airlines.Scaled)

#barplot(as.matrix(Airlines.Scaled), main="Airlines Parameters", xlab="Airline Parameters", ylab="No of Occurences", horiz=FALSE)


#Computing the distance Matrix"
d <- dist(Airlines.Scaled, method = "euclidean") 

#Buiding algorithm, Ward's minimum variance method - dissimilarities are squared before clustering
fit.ward <- hclust(d, method="ward.D2") 

#display dendogram
plot(fit.ward, hang=-1, cex=0.5, main="Ward Clustering")

#Selecting the best number of clusters
library(NbClust)

#Selecting the optimum/best number of clusters
nc <- NbClust(Airlines.Scaled, distance="euclidean",min.nc=2, max.nc=8, method="ward.D2")
table(nc$Best.n[1,])

#Going with 5 as the best number of clusters
barplot(table(nc$Best.n[1,]),
        xlab="Number of Clusters", ylab="Number of Criteria",
        main="Number of Clusters Chosen by 26 Criteria")



#draw dendogram 
plot(fit.ward, hang=-1, cex=0.8)  #display dendogram
#draw dendogram with red borders around 5 clusters
rect.hclust(fit.ward, k=5, border = "red")

#cut tree into 5 clusters
clusters <- cutree(fit.ward, k=5)
table(clusters)


#Aggregate data based on Clusters
g1 <- aggregate(Airlines, by=list(cluster=clusters), median)

plot1 <- data.frame(Cluster=g1[,1],Freq=as.vector(table(clusters)),g1[,-1])

######R Code on Clustering Assignment using K-Means#######################
######R Code on K Means Clustering #######################

# Normalizing columns to bring them under same scale
Airlines.Scaled <- scale(AirlinesAll[,c(-1)])   #excluding the ID column before normalizing
View(Airlines.Scaled)

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
wssplot(Airlines.Scaled)

set.seed(1234)
fit.km <- kmeans(Airlines.Scaled, 5, nstart=25)

summary(fit.km)
fit.km$size
fit.km$centers   

groups <- fit.km$cluster
#Use this function for printing out customers in each cluster
print_clusters <- function(labels, k) {
  for(i in 1:k) {
    print(paste("cluster", i))
    print(Airlines[labels==i,c("Balance","Qual_miles","cc1_miles","cc2_miles",
                               "cc3_miles","Bonus_miles","Bonus_trans","Flight_miles_12mo",
                               "Flight_trans_12","Days_since_enroll","Award?")])
  }
}
print_clusters(groups, 5)

# Aggregate the clusters
aggregate(Airlines, by=list(cluster=fit.km$cluster), mean)
#b<- fit.km$size


##Animation
install.packages("animation")
library("animation")

windows()
km<- kmeans.ani(Airlines,5)

