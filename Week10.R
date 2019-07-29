#an example with simulated data to demonstrate how the K-means algorithm works
set.seed(1234)
x <- rnorm(12, mean = rep(1:3, each = 4), sd = 0.2)
y <- rnorm(12, mean = rep(c(1, 2, 1), each = 4), sd = 0.2)
plot(x, y, col = "blue", pch = 19, cex = 2)
text(x + 0.05, y + 0.05, labels = as.character(1:12))

#Using the kmeans() function
dataFrame <- data.frame(x, y)
kmeansObj <- kmeans(dataFrame, centers = 3)
names(kmeansObj)
#[1] "cluster"      "centers"      "totss"        "withinss"     "tot.withinss" "betweenss"    "size"         "iter"         "ifault" 

#we can see which cluster each data point will get assigned to by looking at the cluster element of the list returned by the kmeans() function
kmeansObj$cluster
# [1] 3 3 3 3 1 1 1 1 2 2 2 2

#Building heatmaps from K-means solutions
set.seed(1234)
dataMatrix <- as.matrix(dataFrame)[sample(1:12), ]
kmeansObj <- kmeans(dataMatrix, centers = 3)

#making an image plot using the K-means clusters
par(mfrow = c(1, 2))
image(t(dataMatrix)[, nrow(dataMatrix):1], yaxt = "n", main = "Original Data")
image(t(dataMatrix)[, order(kmeansObj$cluster)], yaxt = "n", main = "Clustered Data")

#=======================================================================================================================================================#

library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization

#we'll use the built-in R data set USArrests
df <- USArrests

#remove any missing value that might be present in the data
df <- na.omit(df)

#we start by scaling/standardizing the data using the R function scale
df <- scale(df)
head(df)
#Murder   Assault   UrbanPop         Rape
#Alabama    1.24256408 0.7828393 -0.5209066 -0.003416473
#Alaska     0.50786248 1.1068225 -1.2117642  2.484202941
#Arizona    0.07163341 1.4788032  0.9989801  1.042878388
#Arkansas   0.23234938 0.2308680 -1.0735927 -0.184916602
#California 0.27826823 1.2628144  1.7589234  2.067820292
#Colorado   0.02571456 0.3988593  0.8608085  1.864967207

#we will compute and visualize the distance matrix using the functions get_dist and fviz_dist from the factoextra R package
distance <- get_dist(df)
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))


#Computing k-means clustering in R
k2 <- kmeans(df, centers = 2, nstart = 25)
str(k2)
#List of 9
#$ cluster     : Named int [1:50] 1 1 1 2 1 1 2 2 1 1 ...
#..- attr(*, "names")= chr [1:50] "Alabama" "Alaska" "Arizona" "Arkansas" ...
#$ centers     : num [1:2, 1:4] 1.005 -0.67 1.014 -0.676 0.198 ...
#..- attr(*, "dimnames")=List of 2
#.. ..$ : chr [1:2] "1" "2"
#.. ..$ : chr [1:4] "Murder" "Assault" "UrbanPop" "Rape"
#$ totss       : num 196
#$ withinss    : num [1:2] 46.7 56.1
#$ tot.withinss: num 103
#$ betweenss   : num 93.1
#$ size        : int [1:2] 20 30
#$ iter        : int 1
#$ ifault      : int 0
#- attr(*, "class")= chr "kmeans"

#If we print the results we'll see that our groupings resulted in 2 cluster sizes of 30 and 20
k2
#K-means clustering with 2 clusters of sizes 20, 30
#
#Cluster means:
#  Murder    Assault   UrbanPop       Rape
#1  1.004934  1.0138274  0.1975853  0.8469650
#2 -0.669956 -0.6758849 -0.1317235 -0.5646433
#
#Clustering vector:
#  Alabama         Alaska        Arizona       Arkansas     California       Colorado    Connecticut       Delaware        Florida        Georgia         Hawaii 
#1              1              1              2              1              1              2              2              1              1              2 
#Idaho       Illinois        Indiana           Iowa         Kansas       Kentucky      Louisiana          Maine       Maryland  Massachusetts       Michigan 
#2              1              2              2              2              2              1              2              1              2              1 
#Minnesota    Mississippi       Missouri        Montana       Nebraska         Nevada  New Hampshire     New Jersey     New Mexico       New York North Carolina 
#2              1              1              2              2              1              2              2              1              1              1 
#North Dakota           Ohio       Oklahoma         Oregon   Pennsylvania   Rhode Island South Carolina   South Dakota      Tennessee          Texas           Utah 
#2              2              2              2              2              2              1              2              1              1              2 
#Vermont       Virginia     Washington  West Virginia      Wisconsin        Wyoming 
#2              2              2              2              2              2 
#
#Within cluster sum of squares by cluster:
#  [1] 46.74796 56.11445
#(between_SS / total_SS =  47.5 %)
#
#Available components:
#  
#  [1] "cluster"      "centers"      "totss"        "withinss"     "tot.withinss" "betweenss"    "size"         "iter"         "ifault"

#We can also view our results by using fviz_cluster
fviz_cluster(k2, data = df)

#using several different values of k and examine the differences in the results
k3 <- kmeans(df, centers = 3, nstart = 25)
k4 <- kmeans(df, centers = 4, nstart = 25)
k5 <- kmeans(df, centers = 5, nstart = 25)

# plots to compare
p1 <- fviz_cluster(k2, geom = "point", data = df) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point",  data = df) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point",  data = df) + ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom = "point",  data = df) + ggtitle("k = 5")

library(gridExtra)
grid.arrange(p1, p2, p3, p4, nrow = 2)

#Elbow method implementation in R
set.seed(123)

# function to compute total within-cluster sum of square 
wss <- function(k) {
  kmeans(df, k, nstart = 10 )$tot.withinss
}

# Compute and plot wss for k = 1 to k = 15
k.values <- 1:15

# extract wss for 2-15 clusters
wss_values <- map_dbl(k.values, wss)

plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

#"Elbow method" has been wrapped up in a single function (fviz_nbclust)
set.seed(123)
fviz_nbclust(df, kmeans, method = "wss")

# function to compute average silhouette for k clusters
avg_sil <- function(k) {
  km.res <- kmeans(df, centers = k, nstart = 25)
  ss <- silhouette(km.res$cluster, dist(df))
  mean(ss[, 3])
}

# Compute and plot wss for k = 2 to k = 15
k.values <- 2:15

# extract avg silhouette for 2-15 clusters
avg_sil_values <- map_dbl(k.values, avg_sil)


plot(k.values, avg_sil_values,
     type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of clusters K",
     ylab = "Average Silhouettes")

#To compute the gap statistic method we can use the clusGap function which provides the gap statistic and standard error for an output
# compute gap statistic
set.seed(123)
gap_stat <- clusGap(df, FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)
# Print the result
print(gap_stat, method = "firstmax")
#Clustering Gap statistic ["clusGap"] from call:
#  clusGap(x = df, FUNcluster = kmeans, K.max = 10, B = 50, nstart = 25)
#B=50 simulated reference sets, k = 1..10; spaceH0="scaledPCA"
#--> Number of clusters (method 'firstmax'): 4
#logW   E.logW       gap     SE.sim
#[1,] 3.458369 3.638250 0.1798804 0.03653200
#[2,] 3.135112 3.371452 0.2363409 0.03394132
#[3,] 2.977727 3.235385 0.2576588 0.03635372
#[4,] 2.826221 3.120441 0.2942199 0.03615597
#[5,] 2.738868 3.020288 0.2814197 0.03950085
#[6,] 2.669860 2.933533 0.2636730 0.03957994
#[7,] 2.598748 2.855759 0.2570109 0.03809451
#[8,] 2.531626 2.784000 0.2523744 0.03869283
#[9,] 2.468162 2.716498 0.2483355 0.03971815
#[10,] 2.394884 2.652241 0.2573567 0.04104674

#We can visualize the results with fviz_gap_stat which suggests four clusters as the optimal number of clusters
fviz_gap_stat(gap_stat)

#Extracting Results
# Compute k-means clustering with k = 4
set.seed(123)
final <- kmeans(df, 4, nstart = 25)
print(final)
#Clustering vector:
#  Alabama         Alaska        Arizona       Arkansas     California       Colorado    Connecticut       Delaware        Florida        Georgia         Hawaii 
#4              3              3              4              3              3              2              2              3              4              2 
#Idaho       Illinois        Indiana           Iowa         Kansas       Kentucky      Louisiana          Maine       Maryland  Massachusetts       Michigan 
#1              3              2              1              2              1              4              1              3              2              3 
#Minnesota    Mississippi       Missouri        Montana       Nebraska         Nevada  New Hampshire     New Jersey     New Mexico       New York North Carolina 
#1              4              3              1              1              3              1              2              3              3              4 
#North Dakota           Ohio       Oklahoma         Oregon   Pennsylvania   Rhode Island South Carolina   South Dakota      Tennessee          Texas           Utah 
#1              2              2              2              2              2              4              1              4              3              2 
#Vermont       Virginia     Washington  West Virginia      Wisconsin        Wyoming 
#1              2              2              1              1              2 
#
#Within cluster sum of squares by cluster:
#  [1] 11.952463 16.212213 19.922437  8.316061
#(between_SS / total_SS =  71.2 %)
#
#Available components:
#  
#  [1] "cluster"      "centers"      "totss"        "withinss"     "tot.withinss" "betweenss"    "size"         "iter"         "ifault"   

#==================================================================================================================================================================#

#Hierarchical Cluster Analysis
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering visualization
library(dendextend) # for comparing two dendrograms

#Agglomerative Hierarchical Clustering
# Dissimilarity matrix
d <- dist(df, method = "euclidean")

# Hierarchical clustering using Complete Linkage
hc1 <- hclust(d, method = "complete" )

# Plot the obtained dendrogram
plot(hc1, cex = 0.6, hang = -1)

#Alternatively, we can use the agnes function
# Compute with agnes
hc2 <- agnes(df, method = "complete")

# Agglomerative coefficient
hc2$ac
#[1] 0.8531583

#Ward's method identifies the strongest clustering structure of the four methods assessed
# methods to assess
m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")

# function to compute coefficient
ac <- function(x) {
  agnes(df, method = x)$ac
}

map_dbl(m, ac)
#average    single  complete      ward 
#0.7379371 0.6276128 0.8531583 0.9346210

# visualizing the dendrogram
hc3 <- agnes(df, method = "ward")
pltree(hc3, cex = 0.6, hang = -1, main = "Dendrogram of agnes") 

#Divisive Hierarchical Clustering
# compute divisive hierarchical clustering
hc4 <- diana(df)

# Divise coefficient; amount of clustering structure found
hc4$dc
#[1] 0.8514345

# plot dendrogram
pltree(hc4, cex = 0.6, hang = -1, main = "Dendrogram of diana")

# Ward's method
hc5 <- hclust(d, method = "ward.D2")

# Cut tree into 4 groups
sub_grp <- cutree(hc5, k = 4)

# Number of members in each cluster
table(sub_grp)
#sub_grp
#1  2  3  4 
#7 12 19 12

#using the cutree output to add the the cluster each observation belongs to to our original data
USArrests %>%
  mutate(cluster = sub_grp) %>%
  head
#Murder Assault UrbanPop Rape cluster
#1   13.2     236       58 21.2       1
#2   10.0     263       48 44.5       2
#3    8.1     294       80 31.0       2
#4    8.8     190       50 19.5       3
#5    9.0     276       91 40.6       2
#6    7.9     204       78 38.7       2

#drawing the dendrogram with a border around the 4 clusters
plot(hc5, cex = 0.6)
rect.hclust(hc5, k = 4, border = 2:5)

#To use cutree with agnes and diana we can perform the following
# Cut agnes() tree into 4 groups
hc_a <- agnes(df, method = "ward")
cutree(as.hclust(hc_a), k = 4)
#Alabama         Alaska        Arizona       Arkansas     California       Colorado    Connecticut       Delaware        Florida        Georgia         Hawaii 
#1              2              2              3              2              2              3              3              2              1              3 
#Idaho       Illinois        Indiana           Iowa         Kansas       Kentucky      Louisiana          Maine       Maryland  Massachusetts       Michigan 
#4              2              3              4              3              3              1              4              2              3              2 
#Minnesota    Mississippi       Missouri        Montana       Nebraska         Nevada  New Hampshire     New Jersey     New Mexico       New York North Carolina 
#4              1              3              4              4              2              4              3              2              2              1 
#North Dakota           Ohio       Oklahoma         Oregon   Pennsylvania   Rhode Island South Carolina   South Dakota      Tennessee          Texas           Utah 
#4              3              3              3              3              3              1              4              1              2              3 
#Vermont       Virginia     Washington  West Virginia      Wisconsin        Wyoming 
#4              3              3              4              4              3 

# Cut diana() tree into 4 groups
hc_d <- diana(df)
cutree(as.hclust(hc_d), k = 4)
#Alabama         Alaska        Arizona       Arkansas     California       Colorado    Connecticut       Delaware        Florida        Georgia         Hawaii 
#1              2              2              3              2              2              3              3              2              1              3 
#Idaho       Illinois        Indiana           Iowa         Kansas       Kentucky      Louisiana          Maine       Maryland  Massachusetts       Michigan 
#4              2              3              4              3              4              1              4              2              3              2 
#Minnesota    Mississippi       Missouri        Montana       Nebraska         Nevada  New Hampshire     New Jersey     New Mexico       New York North Carolina 
#4              1              2              4              4              2              4              3              2              2              1 
#North Dakota           Ohio       Oklahoma         Oregon   Pennsylvania   Rhode Island South Carolina   South Dakota      Tennessee          Texas           Utah 
#4              3              3              3              3              3              1              4              1              2              3 
#Vermont       Virginia     Washington  West Virginia      Wisconsin        Wyoming 
#4              3              3              4              4              3 

# we can also compare two dendrograms
# Compute distance matrix
res.dist <- dist(df, method = "euclidean")

# Compute 2 hierarchical clusterings
hc1 <- hclust(res.dist, method = "complete")
hc2 <- hclust(res.dist, method = "ward.D2")

# Create two dendrograms
dend1 <- as.dendrogram (hc1)
dend2 <- as.dendrogram (hc2)

tanglegram(dend1, dend2)

#The output of tanglegram can be customized using many other options as follow
dend_list <- dendlist(dend1, dend2)

tanglegram(dend1, dend2,
           highlight_distinct_edges = FALSE, # Turn-off dashed lines
           common_subtrees_color_lines = FALSE, # Turn-off line colors
           common_subtrees_color_branches = TRUE, # Color common branches 
           main = paste("entanglement =", round(entanglement(dend_list), 2))
)

#Average Silhouette Method
fviz_nbclust(df, FUN = hcut, method = "silhouette")