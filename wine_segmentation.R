library(rvest)
library(xml2)
library(dplyr)
library(ggplot2)

url <- "https://vincentarelbundock.github.io/Rdatasets/datasets.html"

r_datasets <- read_html(url) %>% # read url
  html_nodes("table") %>% # extract all the tables
  .[[2]] %>% # it's the second table we want
  html_table() # convert it to a usable format (data.frame)

r_datasets %>% filter(grepl("cat", Item)) 

r_datasets %>% filter(Cols == 3 & Rows >= 100)

install.packages("Ecdat")
library(Ecdat)

??prussian 
??CPSch3 

library(MASS)
?cats
str(cats)
head(cats)

install.packages('HistData')
library(HistData)     
?Macdonell
str(Macdonell)

Macdonell %>% 
  ggplot(aes(height, Hwt, color = Sex)) +
  geom_point()



cats %>% 
  ggplot(aes(Bwt, Hwt, color = Sex)) +
  geom_point()

scaled_cats <- scale(cats)

library(gclus)
data(wine)

str(wine)
table(wine$Class)

scaled_wine <- scale(wine) %>% as.data.frame()
str(scaled_wine)
head(scaled_wine)

scaled_wine2 <- scaled_wine %>% dplyr::select(-Class)
str(scaled_wine2)
head(scaled_wine2)

#### OPTIMAL NUMBER OF CLUSTERS ####

#### ELBOW METHOD = 3 ####

#### elbow in the sum of squared error (SSE) scree plot

### scaled_wine
wss <- (nrow(scaled_wine)-1)*sum(apply(scaled_wine,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(scaled_wine,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")




### scaled_wine2
wss <- (nrow(scaled_wine2)-1)*sum(apply(scaled_wine2,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(scaled_wine2,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")



##### partitioning around medoids USING PAMK FUNCTION  = 3 ####

library(fpc)
pamk.best <- pamk(scaled_wine)
cat("number of clusters estimated by optimum average silhouette width:", pamk.best$nc, "\n")
plot(pam(scaled_wine, pamk.best$nc))



pamk.best2 <- pamk(scaled_wine2)
cat("number of clusters estimated by optimum average silhouette width:", pamk.best2$nc, "\n")
plot(pam(scaled_wine2, pamk.best2$nc))



####  Calinsky criterion: 3 ####
install.packages("vegan")
library(vegan)
cal_fit <- cascadeKM(scaled_wine, 1, 10, iter = 1000)
plot(cal_fit, sortg = TRUE, grpmts.plot = TRUE)
calinski.best <- as.numeric(which.max(cal_fit$results[2,]))
cat("Calinski criterion optimal number of clusters:", calinski.best, "\n")




cal_fit2 <- cascadeKM(scaled_wine2, 1, 10, iter = 1000)
plot(cal_fit2, sortg = TRUE, grpmts.plot = TRUE)
calinski.best2 <- as.numeric(which.max(cal_fit2$results[2,]))
cat("Calinski criterion optimal number of clusters:", calinski.best2, "\n")


#### Bayesian Information Criterion for expectation-maximization 3 ####
#### initialized by hierarchical clustering for parameterized Gaussian mixture models 

library(mclust)
# Run the function to see how many clusters
# it finds to be optimal, set it to search for
# at least 1 model and up 20.
d_clust <- Mclust(as.matrix(scaled_wine), G=1:20)
m.best <- dim(d_clust$z)[2]
cat("model-based optimal number of clusters:", m.best, "\n")
# 4 clusters
plot(d_clust)



d_clust2 <- Mclust(as.matrix(scaled_wine2), G=1:20)
m.best2 <- dim(d_clust2$z)[2]
cat("model-based optimal number of clusters:", m.best2, "\n")
# 4 clusters
plot(d_clust2)



### Affinity propagation (AP) clustering 14 ####

install.packages("apcluster")
library(apcluster)
d.apclus <- apcluster(negDistMat(r=2), scaled_wine)
cat("affinity propogation optimal number of clusters:", length(d.apclus@clusters), "\n")
# 14
heatmap(d.apclus)
plot(d.apclus, scaled_wine)


## 15 optimal clusters

d.apclus2 <- apcluster(negDistMat(r=2), scaled_wine2)
cat("affinity propogation optimal number of clusters:", length(d.apclus2@clusters), "\n")
# 14
heatmap(d.apclus2)
plot(d.apclus2, scaled_wine2)


#### Gap Statistic 3 ####

library(cluster)
clusGap(scaled_wine, kmeans, 10, B = 100, verbose = interactive())

clusGap(scaled_wine2, kmeans, 10, B = 100, verbose = interactive())


#### hierarchical clustering 2 or 3 ####

wine_dist <- dist(as.matrix(scaled_wine))   # find distance matrix 
plot(hclust(wine_dist)) 


wine_dist2 <- dist(as.matrix(scaled_wine2))   # find distance matrix 
plot(hclust(wine_dist2)) 



#### a Bayesian clustering method - not working ####

install.packages("bclust")
library(bclust)
x <- as.matrix(scaled_wine)
d.bclus <- bclust(x, transformed.par = c(0, -50, log(16), 0, 0, 0))
viplot(imp(d.bclus)$var); plot(d.bclus); ditplot(d.bclus)
dptplot(d.bclus, scale = 20, horizbar.plot = TRUE,varimp = imp(d.bclus)$var, horizbar.distance = 0, dendrogram.lwd = 2)


x2 <- as.matrix(scaled_wine2)
d.bclus2 <- bclust(x2, transformed.par = c(0, -50, log(16), 0, 0, 0))
viplot(imp(d.bclus2)$var); plot(d.bclus2); ditplot(d.bclus2)
dptplot(d.bclus2, scale = 20, horizbar.plot = TRUE,varimp = imp(d.bclus2)$var, horizbar.distance = 0, dendrogram.lwd = 2)



####  p-values for hierarchical clustering via multiscale bootstrap resampling.2 or 3 ####

install.packages("pvclust")
library(pvclust)
library(MASS)

wine.pv <- pvclust(scaled_wine)
plot(wine.pv)


wine.pv2 <- pvclust(scaled_wine2)
plot(wine.pv2)
