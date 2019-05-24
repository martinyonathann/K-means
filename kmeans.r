library(tidyverse)
library(cluster)
library(factoextra)

#Data Preprosesing
df<- USArrests
er<-na.omit(df)
#Standarisasi data
df<-scale(df)
head(df)

distance<-get_dist(df)
fviz_dist(distance,gradient = list(low="#00AFBB",mid="white",high="#FC4E07"))

k2<-kmeans(df,centers = 2,nstart = 25)
str(k2)

k2
fviz_cluster(k2,data = df,title="Latihan Cluster")

df %>%
  as_tibble()%>%
  mutate(cluster = k2$cluster,
         state = row.names(USArrests)) %>%
  ggplot(aes(UrbanPop, Murder, color = factor(cluster), label = state)) +
  geom_text()


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

#Elbow Method
set.seed(123)
library(purrr)
wss<-function(k){
  kmeans(df,k,nstart = 10)$tot.withinssll
}
k.values<-1:15
wss_values<-map_dbl(k.values,wss)

plot(k.values,wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares"
     )
set.seed(123)
fviz_nbclust(df,kmeans,method ="wss")

#Extracting Result
set.seed(123)
final<-kmeans(df, 4, nstart = 25)
print(final)
fviz_cluster(final, data = df)

USArrests%>%
  mutate(Cluster=final$cluster)%>%
  group_by(Cluster)%>%
  summarise_all("mean")
