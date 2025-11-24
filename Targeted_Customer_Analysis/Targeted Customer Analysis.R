# ----------------------------------------
# LOAD DATA
# ----------------------------------------

# Use file chooser (100% path-proof)
customer_data <- read.csv(file.choose())

# Check structure
str(customer_data)
names(customer_data)
head(customer_data)


# ----------------------------------------
# BASIC ANALYSIS
# ----------------------------------------

summary(customer_data$Age)
sd(customer_data$Age)

summary(customer_data$Annual.Income..k..)
sd(customer_data$Annual.Income..k..)

summary(customer_data$Spending.Score..1.100.)
sd(customer_data$Spending.Score..1.100.)


# ----------------------------------------
# CUSTOMER GENDER VISUALIZATION
# ----------------------------------------

a <- table(customer_data$Gender)

barplot(a,
        main = "Using BarPlot to display Gender Comparison",
        ylab = "Count",
        xlab = "Gender",
        col = rainbow(3),
        legend = rownames(a))

pct <- round(a / sum(a) * 100)
lbs <- paste(c("Female", "Male"), " ", pct, "%", sep = " ")

library(plotrix)
pie3D(a, labels = lbs,
      main = "Pie Chart Depicting Ratio of Female and Male")


# ----------------------------------------
# AGE DISTRIBUTION
# ----------------------------------------

hist(customer_data$Age,
     col = "green",
     main = "Histogram of Age",
     xlab = "Age",
     ylab = "Frequency",
     labels = TRUE)

boxplot(customer_data$Age,
        col = "#746AB0",
        main = "Boxplot of Age")


# ----------------------------------------
# ANNUAL INCOME ANALYSIS
# ----------------------------------------

hist(customer_data$Annual.Income..k..,
     col = "blue",
     main = "Histogram of Annual Income",
     xlab = "Annual Income (k$)",
     ylab = "Frequency",
     labels = TRUE)

plot(density(customer_data$Annual.Income..k..),
     col = "yellow",
     main = "Density Plot of Annual Income",
     xlab = "Annual Income",
     ylab = "Density")

polygon(density(customer_data$Annual.Income..k..),
        col = "#288ba8")

boxplot(customer_data$Spending.Score..1.100.,
        horizontal = TRUE,
        col = "red",
        main = "BoxPlot of Spending Score")

hist(customer_data$Spending.Score..1.100.,
     main = "Histogram of Spending Score",
     xlab = "Spending Score",
     ylab = "Frequency",
     col = "violet",
     labels = TRUE)


# ----------------------------------------
# K-MEANS CLUSTERING
# ----------------------------------------

library(purrr)
set.seed(123)

iss <- function(k) {
  kmeans(customer_data[, 3:5], k, iter.max = 100, nstart = 100)$tot.withinss
}

k.values <- 1:10
iss_values <- map_dbl(k.values, iss)

plot(k.values, iss_values,
     type = "b", pch = 19, 
     xlab = "Number of clusters K",
     ylab = "Total Within-Cluster SS")


# ----------------------------------------
# SILHOUETTE METHOD
# ----------------------------------------

library(cluster)
library(gridExtra)
library(grid)

for (k in 2:10) {
  km <- kmeans(customer_data[, 3:5], k, iter.max = 100, nstart = 50)
  sil <- silhouette(km$cluster, dist(customer_data[, 3:5], "euclidean"))
  plot(sil, main = paste("Silhouette Plot for k =", k))
}


# ----------------------------------------
# BEST K USING NBCLUST & GAP STAT
# ----------------------------------------

library(NbClust)
library(factoextra)

fviz_nbclust(customer_data[, 3:5], kmeans, method = "silhouette")

set.seed(125)
stat_gap <- clusGap(customer_data[, 3:5], FUN = kmeans,
                    nstart = 25, K.max = 10, B = 50)

fviz_gap_stat(stat_gap)


# ----------------------------------------
# FINAL MODEL (k = 6)
# ----------------------------------------

k6 <- kmeans(customer_data[, 3:5], 6, iter.max = 100, nstart = 50)
k6


# ----------------------------------------
# PCA VISUALIZATION
# ----------------------------------------

pcclust <- prcomp(customer_data[, 3:5], scale = FALSE)
summary(pcclust)
pcclust$rotation[, 1:2]


# ----------------------------------------
# PLOTS OF CLUSTERS
# ----------------------------------------

library(ggplot2)

# Income vs Spending Score
ggplot(customer_data, aes(x = Annual.Income..k.., 
                          y = Spending.Score..1.100.,
                          color = as.factor(k6$cluster))) +
  geom_point() +
  ggtitle("Segments of Mall Customers",
          subtitle = "K-means Clustering Results")

# Spending Score vs Age
ggplot(customer_data, aes(x = Spending.Score..1.100.,
                          y = Age,
                          color = as.factor(k6$cluster))) +
  geom_point() +
  ggtitle("Segments of Mall Customers",
          subtitle = "K-means Clustering Results")


# PCA Cluster Plot
kCols <- function(vec){
  cols <- rainbow(length(unique(vec)))
  return(cols[as.numeric(as.factor(vec))])
}

digCluster <- k6$cluster
dignm <- as.character(digCluster)

plot(pcclust$x[,1:2],
     col = kCols(digCluster),
     pch = 19,
     xlab = "PC1",
     ylab = "PC2",
     main = "PCA - K-means Clusters")

legend("bottomleft", unique(dignm), fill = unique(kCols(digCluster)))

