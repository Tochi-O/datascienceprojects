

# Loading the packages that will be used
list.of.packages <- c("tm", "dbscan", "proxy", "colorspace")
install.packages("SnowballC")

library(SnowballC)
library(tidyverse)

# (downloading and) requiring packages
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) 
  install.packages(new.packages)
for (p in list.of.packages) 
  require(p, character.only = TRUE)

rm(list = ls()) # Cleaning environment
options(header = FALSE, stringsAsFactors = FALSE, fileEncoding = "latin1")

tweetsdata <- read.csv("cyberbullying_tweets.csv")
set.seed(2347723)                            # Set seed
#tweetsdata <- lapply(tweetsdata,sample)
tweetsdata <- tweetsdata[sample(1:nrow(tweetsdata)),]
tweetsdata <- head(tweetsdata,3000)
head(tweetsdata,5)
nrow(tweetsdata)
unique(tweetsdata$cyberbullying_type)
tweetsdata %>%
  group_by(cyberbullying_type) %>%
  summarize(distinct_points = n_distinct(tweet_text))

colnames(tweetsdata)

sentences <- sub("http://([[:alnum:]|[:punct:]])+", '', tweetsdata$tweet_text)
sentences <- gsub('@','',sentences)

head(sentences)

corpusvar = tm::Corpus(tm::VectorSource(sentences))

# Handling UTF-8 encoding problem from the dataset
corpusvar.cleaned <- tm::tm_map(corpusvar, function(x) iconv(x, to='UTF-8', sub='byte')) 
corpusvar.cleaned <- tm::tm_map(corpusvar.cleaned, tm::removeWords, tm::stopwords('english')) # Removing stop-words
corpusvar.cleaned <- tm::tm_map(corpusvar, tm::stemDocument, language = "english") # Stemming the words 
corpusvar.cleaned <- tm::tm_map(corpusvar.cleaned, tm::stripWhitespace) # Trimming excessive whitespaces
length(corpusvar.cleaned)

length(tdm)
tdm <- tm::DocumentTermMatrix(corpusvar.cleaned)
tdm.tfidf <- tm::weightTfIdf(tdm)


tdm.tfidf <- tm::removeSparseTerms(tdm.tfidf, 0.999)

# There is the memory-problem part
# - Native matrix isn't "sparse-compliant" in the memory
# - Sparse implementations aren't necessary compatible with clustering algorithms
tfidf.matrix <- as.matrix(tdm.tfidf)
# Cosine distance matrix (useful for specific clustering algorithms)
dist.matrix = proxy::dist(tfidf.matrix, method = "cosine")

truth.K <- 6




# k means clustering
clustering.kmeans <- kmeans(tfidf.matrix, truth.K)

# Hierarchical clustering}
clustering.hierarchical <- hclust(dist.matrix, method = "ward.D2")

#Density-based clustering}
clustering.dbscan <- dbscan::hdbscan(dist.matrix, minPts = 10)


#Stacking clustering initialization}
master.cluster <- clustering.kmeans$cluster
slave.hierarchical <- cutree(clustering.hierarchical, k = truth.K)
slave.dbscan <- clustering.dbscan$cluster

# Preparing the stacked clustering
stacked.clustering <- rep(NA, length(master.cluster)) 
names(stacked.clustering) <- 1:length(master.cluster)


#r Stacking clustering execution}
for (cluster in unique(master.cluster)) {
  indexes = which(master.cluster == cluster, arr.ind = TRUE)
  slave1.votes <- table(slave.hierarchical[indexes])
  slave1.maxcount <- names(slave1.votes)[which.max(slave1.votes)]
  
  slave1.indexes = which(slave.hierarchical == slave1.maxcount, arr.ind = TRUE)
  slave2.votes <- table(slave.dbscan[indexes])
  slave2.maxcount <- names(slave2.votes)[which.max(slave2.votes)]
  
  stacked.clustering[indexes] <- slave2.maxcount
}


previous.par <- par(mfrow=c(2,2), mar = rep(1.5, 4)) # partitionning the plot space
plot(points,
     main = 'K-Means clustering',
     col = as.factor(master.cluster),
     mai = c(0, 0, 0, 0),
     mar = c(0, 0, 0, 0),
     xaxt = 'n', yaxt = 'n',
     xlab = '', ylab = '')

plot(points,
     main = 'Hierarchical clustering',
     col = as.factor(slave.hierarchical),
     mai = c(0, 0, 0, 0),
     mar = c(0, 0, 0, 0),
     xaxt = 'n', yaxt = 'n',
     xlab = '', ylab = '')
plot(points,
     main = 'Density-based clustering',
     col = as.factor(slave.dbscan),
     mai = c(0, 0, 0, 0),
     mar = c(0, 0, 0, 0),
     xaxt = 'n', yaxt = 'n',
     xlab = '', ylab = '')
plot(points,
     main = 'Stacked clustering',
     col = as.factor(stacked.clustering),
     mai = c(0, 0, 0, 0),
     mar = c(0, 0, 0, 0),
     xaxt = 'n', yaxt = 'n',
     xlab = '', ylab = '')
par(previous.par) # recovering the original plot space parameters

kmeans_basic_df <- data.frame(Cluster = clustering.kmeans$cluster, tweetsdata)
head(kmeans_basic_df)

points <- cmdscale(dist.matrix,k=2) #pca
palette <- colorspace::diverge_hcl(truth.K) # Creating a color palette
master.cluster <- clustering.kmeans$cluster

ggplot(data = kmeans_basic_df, aes(y = Cluster)) +
  geom_bar(aes(fill = cyberbullying_type)) +
  ggtitle("Count of Clusters by Region") +
  theme(plot.title = element_text(hjust = 0.5))


library(cluster)
library("factoextra")

# Fancy kmeans
kmeans_fancy <- kmeans(tfidf.matrix, 6, nstart = 10)
# plot the clusters
fviz_cluster(kmeans_fancy, data = tfidf.matrix, geom = c("point"),ellipse.type = "euclid")

length(tfidf.matrix)
previous.par <- par(mfrow=c(2,2), mar = rep(1.5, 4)) # partitionning the plot space
plot(points,
     main = 'K-Means clustering',
     col = as.factor(master.cluster),
     mai = c(0, 0, 0, 0),
     mar = c(0, 0, 0, 0),
     xaxt = 'n', yaxt = 'n',
     xlab = '', ylab = '')
# Plotting the graph
plot(points, 
     xlab ="x", ylab ="y",
     col =as.factor(master.cluster))
plot(points)
par(previous.par) # recovering the original plot space parameters

