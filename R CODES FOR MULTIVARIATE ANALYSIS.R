setwd('D:\\SCMA 2024\\Data')
df = read.csv('Wine.csv',header=FALSE)
head(df)

names(df)= c('class','Alcohol','Malic_acid','Ash','Alcalinity_ash','Magnesium','Total_phenols','Flavanoids','Nonflavanoid_phenols','Proanthocyanins','Color_intensity','Hue','diluted_wines','Proline')
dput(names(df))
dim(df)
summary(df)

heatmap(cor(df))


#TO PERFORM FACTOR ANALYSIS

factanal(df, factors = 5) # varimax is the default
factanal(df, factors = 5, rotation = "promax")
# The following shows the g factor as PC1
prcomp(m1) # signs may depend on platform

## formula interface
scores = factanal(~., data=df, factors = 5,scores = "Bartlett")$scores
names(df)
dim(scores)
scores = data.frame(scores)

str(scores)
dim(df)
pooled = cbind(df,scores)      
names(pooled)
scores = factanal(~., data=df, factors = 5,scores = "Bartlett")$scores


pooled$composite = 3.313*pooled$Factor1 + 2.320*pooled$Factor2+ 1.776*pooled$Factor3+ 1.414*pooled$Factor4 +1.234*pooled$Factor5
head(pooled)


#PERFORM CLUSTER ANALYSIS

df1 = scale(df)
head(df1)
library(psych)
test.data <- df1
ic.out <- iclust(test.data,title="ICLUST of the Wine data")
summary(ic.out)

cl <- kmeans(df1, 5, nstart = 25)
## IGNORE_RDIFF_END
plot(df1, col = cl$cluster)
points(cl$centers, col = 1:5, pch = 8)


# Decide how many clusters to look at
n_clusters <- 10

# Initialize total within sum of squares error: wss

library(tidyverse)
# Load the necessary package
library(tibble)

n_clusters <- 10
wss <- numeric(n_clusters)

set.seed(123)

# Look over 1 to n_clusters possible clusters
for (i in 1:n_clusters) {
  km.out <- kmeans(df1, centers = i, nstart = 20)
  # Save the within cluster sum of squares
  wss[i] <- km.out$tot.withinss
}

# Create a tibble with the results
wss_df <- tibble(clusters = 1:n_clusters, wss = wss)

scree_plot <- ggplot(wss_df, aes(x = clusters, y = wss, group = 1)) +
    geom_point(size = 4)+
    geom_line() +
    scale_x_continuous(breaks = c(2, 4, 6, 8, 10)) +
    xlab('Number of clusters')
scree_plot

set.seed(123)
km.out <- kmeans(df1, centers = 3, nstart = 20)
km.out
plot(km.out,centers = 3, nstart = 20)

==========
library(factoextra)
library(cluster)
#perform hierarchical clustering using Ward's minimum variance
clust <- agnes(df, method = "ward")

#produce dendrogram
pltree(clust, cex = 0.6, hang = -1, main = "Dendrogram") 

#calculate gap statistic for each number of clusters (up to 10 clusters)
gap_stat <- clusGap(df1, FUN = hcut, nstart = 25, K.max = 10, B = 50)

#produce plot of clusters vs. gap statistic
fviz_gap_stat(gap_stat)

#compute distance matrix
d <- dist(df1, method = "euclidean")

#perform hierarchical clustering using Ward's method
final_clust <- hclust(d, method = "ward.D2" )

#cut the dendrogram into 3 clusters
groups <- cutree(final_clust, k=3)
plot(groups)
#find number of observations in each cluster
table(groups)

#append cluster labels to original data
final_data <- cbind(df, cluster = groups)

#display first six rows of final data
head(final_data)

#find mean values for each cluster
aggregate(final_data, by=list(cluster=final_data$cluster), FUN=mean)

