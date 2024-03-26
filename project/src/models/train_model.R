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



# Laurens van der Maaten page on t-SNE author of original t-SNE paper
#https://lvdmaaten.github.io/tsne/

# distill.pub guide on using t-SNE effectivly
#https://distill.pub/2016/misread-tsne/

# response to distill.pub article with discussion of practical application of t-SNE
#https://towardsdatascience.com/how-to-tune-hyperparameters-of-tsne-7c0596a18868


# run t-sne on the PCAs, note that if you already have PCAs you need to set pca=F or it will run a pca again. 
# pca is built into Rtsne, ive run it seperatly for you to see the internal steps

tsne <- Rtsne(pca_dt,pca = F,perplexity=5,check_duplicates = F)

# grab out the coordinates
tsne_dt <- data.table(tsne$Y)


# plot, note that in this case I have access to party so I can see that it seems to have worked, You do not have access
# to species so you will just be plotting in black to see if there are groups. 
ggplot(tsne_dt,aes(x = V1,y = V2)) + geom_point()



# use a gaussian mixture model to find optimal k and then get probability of membership for each row to each group

# this fits a gmm to the data for all k=1 to k= max_clusters, we then look for a major change in likelihood between k values
k_bic <- Optimal_Clusters_GMM(tsne_dt[,.(V1,V2)],max_clusters = 4,criterion = "BIC")

# now we will look at the change in model fit between successive k values
delta_k <- c(NA,k_bic[-1] - k_bic[-length(k_bic)])

# I'm going to make a plot so you can see the values, this part isnt necessary
del_k_tab <- data.table(delta_k=delta_k,k=1:length(delta_k))

# plot 
ggplot(del_k_tab,aes(x = k,y = -delta_k)) + geom_point() + geom_line() +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))+
  geom_text(aes(label = k),hjust = 0, vjust = -1)



opt_k <- 3

# now we run the model with our chosen k value
gmm_data <- GMM(tsne_dt[,.(V1,V2)], opt_k)

# the model gives a log-likelihood for each datapoint's membership to each cluster, me need to convert this 
# log-likelihood into a probability

l_clust <- gmm_data$Log_likelihood^10

l_clust <- data.table(l_clust)

net_lh <- apply(l_clust,1,FUN = function(x){sum(1/x)})

cluster_prob <- 1/l_clust/net_lh

# we can now plot to see what cluster 1 looks like

tsne_dt$Cluster_1_prob <- cluster_prob$V1

ggplot(tsne_dt,aes(x = V1,y = V2,col = Cluster_1_prob)) + geom_point()
View(tsne_dt)

View(submit)

fwrite(submit, './project/volume/data/processed/submit.csv')
#confirm R1 = 3, R5 = 2, R6 = 4