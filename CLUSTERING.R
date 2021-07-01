mall_customer<-read.csv('/Users/saineymanga/Desktop/DSE/datasets_7721_10938_Mall_Customers.csv')
head(mall_customer)
attach(mall_customer)
any(is.na(mall_customer))
install.packages("factoextra")
library(factoextra)
install.packages("NbClust")
library(dplyr)
library(stats)
library(NbClust)
library(ggplot2)
library(cluster)
### SHOW THE DISTRIBUTION OF SPENDING SCORE AND ANNUAL INCOME
hist(mall_customer$Annual.Income..k..,
     main="Histogram for Annual Income",
     col="#660033",    
     xlab="Annual Income Class",
     ylab="Frequency",
     labels=TRUE)

hist(mall_customer$Spending.Score..1.100.,
     main="Histogram for spending score",
     col="#660033",    
     xlab="Annual Income Class",
     ylab="Frequency",
     labels=TRUE)



## DROP THE GENDER COLUMN TO REMOVE NON NUMERIC INSTANCES
my_data<- select(mall_customer,c(1,3,4,5))
## SCALING THE DATA
df<-scale(my_data)

## FINDING THE NUMBER OF OPTIMAL CLUSTERS USING THE ELBOW METHOD
fviz_nbclust(df, kmeans, method = "wss") +
  geom_vline(xintercept = 5, linetype = 2) + # add line for better visualisation
  labs(subtitle = "Elbow method") # add subtitle



### PLOTING THE CLUSTERS
set.seed(1)
k5<-kmeans(my_data[3:4],5,iter.max=100,nstart=50,algorithm="Lloyd")
k5
ggplot(my_data, aes(x =Annual.Income..k.., y = Spending.Score..1.100.)) + 
  geom_point(stat = "identity", aes(color = as.factor(k5$cluster))) +
  scale_color_discrete(name=" ",
                       breaks=c("1", "2", "3", "4", "5"),
                       labels=c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5")) +
  ggtitle("Segments of Mall Customers", subtitle = "Using K-means Clustering")

###### HIERARCHICAL CLUSTERING
## FINDING THE OPTIMALS NUMBER OF CLUSTERS USING THE DENDOGRAM

dataset<-my_data[3:4]
dendrogram = hclust(d = dist(dataset, method = 'euclidean'), method = 'ward.D')
plot(dendrogram,
     main = paste('Dendrogram'),
     xlab = 'Customers',
     ylab = 'Euclidean distances')

hc = hclust(d = dist(dataset, method = 'euclidean'), method = 'ward.D')
y_hc = cutree(hc, 6)
clusplot(dataset,
         y_hc,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels= 2,
         plotchar = FALSE,
         span = TRUE,
         main = paste('Clusters of customers'),
         xlab = 'Annual Income',
         ylab = 'Spending Score')

