set.seed(3)

#read in data
data <- fread('./project/volume/data/interim/data.csv')
submit <- fread('./project/volume/data/interim/submit.csv')

# do a pca
pca <- prcomp(data)

# look at the percent variance explained by each pca
screeplot(pca)

# look at the rotation of the variables on the PCs
pca

# see the values of the scree plot in a table 
summary(pca)

# see a biplot of the first 2 PCs
biplot(pca)

# use the unclass() function to get the data in PCA space
pca_dt <- data.table(unclass(pca)$x)

#use t-sne
tsne <- Rtsne(pca_dt, pca = F, perplexity = 100, check_duplicates = F)

# grab out the coordinates
tsne_dt <- data.table(tsne$Y)

# use a gaussian mixture model to find optimal k and then get probability of membership for each row to each group

# this fits a gmm to the data for all k=1 to k= max_clusters, we then look for a major change in likelihood between k values
k_bic <- Optimal_Clusters_GMM(tsne_dt[,.(V1,V2)], max_clusters = 10, criterion = "BIC")

# now we will look at the change in model fit between successive k values
delta_k <- c(NA,k_bic[-1] - k_bic[-length(k_bic)])

# I'm going to make a plot so you can see the values, this part isnt necessary
del_k_tab <- data.table(delta_k = delta_k, k = 1:length(delta_k))

# plot 
ggplot(del_k_tab,aes(x = k,y = -delta_k)) + geom_point() + geom_line() +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))+
  geom_text(aes(label = k),hjust = 0, vjust = -1)


opt_k <- 4

# now we run the model with our chosen k value
gmm_data <- GMM(tsne_dt[,.(V1, V2)], opt_k)

# the model gives a log-likelihood for each datapoint's membership to each cluster, me need to convert this 
# log-likelihood into a probability
l_clust <- gmm_data$Log_likelihood^5

l_clust <- data.table(l_clust)

net_lh <- apply(l_clust, 1, FUN = function(x){sum(1/x)})

cluster_prob <- 1/l_clust/net_lh

# we can now plot to see what cluster 1 looks like
tsne_dt$Cluster_1_prob <- cluster_prob$V1
tsne_dt$Cluster_2_prob <- cluster_prob$V2
tsne_dt$Cluster_3_prob <- cluster_prob$V3
tsne_dt$Cluster_4_prob <- cluster_prob$V4

tsne_dt$breed_3 <- tsne_dt$Cluster_4_prob
tsne_dt$breed_2 <- tsne_dt$Cluster_2_prob
tsne_dt$breed_4 <- tsne_dt$Cluster_3_prob
tsne_dt$breed_1 <- tsne_dt$Cluster_1_prob

ggplot(tsne_dt, aes(x = V1, y = V2, col = breed_1)) + geom_point()

submit$breed_1 <- tsne_dt$breed_1
submit$breed_2 <- tsne_dt$breed_2
submit$breed_3 <- tsne_dt$breed_3
submit$breed_4 <- tsne_dt$breed_4

fwrite(submit, './project/volume/data/processed/submitFINAL.csv')
#confirm R1 = 3, R5 = 2, R6 = 4