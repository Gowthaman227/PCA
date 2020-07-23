Wine_data <- read.csv(file.choose())
View(Wine_data)
Wine <- Wine_data[-1]# First column has Type
View(Wine)
attach(Wine)
cor(Wine)
### Principal Component Analysis
Wine_PCA <- princomp(Wine,cor=TRUE,scores=TRUE,covmat=NULL)
summary(Wine_PCA)
loadings(Wine_PCA)
plot(Wine_PCA)
Wine_PCA$scores## Check the scores of principal components
Wine_PCA$scores[,1:3]## Top 3 PCA Scores which represents the whole data
Wine_data <- cbind(Wine_data,Wine_PCA$scores[,1:3])
View(Wine_data)

##Hierarchial Clustering
Clust_Wine <- Wine_data[,c(15:17)]
Clust_Wine

## Normalization
norm_Wine <- scale(Clust_Wine)
View(norm_Wine)
distance <- dist(norm_Wine,method="euclidean")
Wine_model <- hclust(distance,method="complete")
summary(Wine_model)
## Dendogram
plot(Wine_model)
plot(Wine_model,hang=-1)

Wine_group <- cutree(Wine_model,5)
rect.hclust(Wine_model,k=5,border="red")

memership <- as.matrix(Wine_group)
View(memership)
Wine_data_Final <- cbind(Wine_data,memership)
View(Wine_data_Final)
View(aggregate(Wine_data_Final[,-c(15:18)],by=list(memership),FUN=mean))

### K Means Clustering
library(kselection)
library(animation)
library(NbClust)
library(factoextra)
Wine_Km <- kmeans(norm_Wine,5)
km <- kmeans.ani(norm_Wine,5)
str(Wine_Km)
Wine_KM_Model <- data.frame(Wine_data,Wine_Km$cluster)
View(Wine_KM_Model)
View(aggregate(Wine_KM_Model,by=list(Wine_KM_Model$Wine_Km.cluster),FUN=mean))

## Elbow Method
k_value <- fviz_nbclust(norm_Wine, kmeans, method = "wss",k.max=7) +
  geom_vline(xintercept = 5, linetype = 2)+
  labs(subtitle = "Elbow method")
plot(k_value)     

### Clustering With Original Data
## Hierarchial Clustering ##
View(Wine)
## Normalization
normal_Wine <- scale(Wine)
View(normal_Wine)

d <- dist(normal_Wine,method="euclidean")
Wine_hmodel <- hclust(d,method="complete")
plot(Wine_hmodel)
plot(Wine_hmodel,hang=-1)
Wine_tree <- cutree(Wine_hmodel,5)
rect.hclust(Wine_hmodel,k=5,border="blue")
Member <- as.matrix(Wine_tree)
Member
Model <- cbind(Wine,Member)
View(Model)
View
## K Means Clustering ##
Wine_kmeans <- kmeans(normal_Wine,5)
Wine_ani <- kmeans.ani(normal_Wine,5)
str(Wine_kmeans)
membershp <- as.matrix(Wine_kmeans$cluster)
membershp
Wine_Kmeans_Model <- cbind(Wine,membershp)
View(Wine_Kmeans_Model)

## Elbow Method
k_val <- fviz_nbclust(normal_Wine, kmeans, method = "wss",k.max=7) +
  geom_vline(xintercept = 5, linetype = 2)+
  labs(subtitle = "Elbow method")
plot(k_val) 
