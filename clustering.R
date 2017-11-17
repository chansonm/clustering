# TODO: Adapt distance measure
.clustering.distanceMethod = "euclidean" # originally: "euclidean", then: "jaccard"
.clustering.Method = "ward" #originally: ward

# stopping-rule for cuttind a dendogram ------------------------------------------------
.cutDendroByMojenaTest <- function(hc, criticalValue=1.25) {

  # establish the relevant height for cutting by mean(h) + 1.25*sd(h)
  selected_h <- min(hc$height[hc$height > mean(hc$height) + criticalValue*sd(hc$height)]) # Mojena (1977)
  selected_k <- 1 + length(hc$height[hc$height > mean(hc$height) + criticalValue*sd(hc$height)]) 
  print(paste("The following shows the plotted heights from the Mojena test, with threshold =", selected_h))
  plot(sort(hc$height, decreasing=T), main=paste("Mojena test distribution, threshold", selected_h), xlab="Number of clusters", ylab="Height")
  
  x <- c()
  ks <- c()
  hs <- c()
  for(cv in seq(1.25, 5.0, by=0.25))
  {
    h <- min(hc$height[hc$height > mean(hc$height) + cv*sd(hc$height)]) # Mojena (1977)  
    k <- 1 + length(hc$height[hc$height > mean(hc$height) + cv*sd(hc$height)]) 
    print(paste('cv =', cv, ", threshold =", h, "resulting clusters = ", k))
    x<-c(x, cv)
    ks<-c(ks, k)
    hs<-c(hs, h)
  }
  
  print("The following shows how different critical values (in 0.25 steps) affect the threshold value h and therefore the number of clusters k")
  plot(x=c(x, x), y=c(ks,hs), main="Effect of critical value in Mojena test to number of clusters",  xlab="Mojena test critical value ", ylab="")
  lines(x, ks, col='blue')
  lines(x, hs, col='green')
  abline(v=criticalValue, col='red', lwd=3, lty="dashed")
  mtext("Threshold value h", side=2, line=3, cex.lab=1,las=0, col="green")
  mtext("Number of clusters k", side=2, line=2, cex.lab=1,las=0, col="blue")
  
  return(list(k = selected_k, h = selected_h, group = cutree(hc, k = k)))
}


.clustering.withPCA <- function(pr.out, numPCs = 6, criticalValue = 2.75){
  
  ##  Hierarchical clustering of the first 6 PC of scores
  dataForClustering <- pr.out$x[,1:numPCs]
  #plot(dataForClustering, pch=16)

  # MC: This is the clustering Algorithm
  d <- dist(dataForClustering, method = .clustering.distanceMethod) # distance matrix
  fit <- hclust(d, method= .clustering.Method)

  # MC: This calculates number of cluster with Mojena. Alternatively, you could define number of clusters directly in the next line
  numClusters <- .cutDendroByMojenaTest(fit, criticalValue)
  numClusters
  
  print(paste("Mojena test with critical value of", criticalValue, "returned the following number of clusters:", numClusters$k))
  
  plot(fit, lwd=2) # display dendogram
  # draw dendogram with red borders around the  clusters 
  #numClusters$k = 7 # manually overwrite number of clusters
  groups <- cutree(fit, k=numClusters$k) # cut tree into k clusters
  print("The following diagram shows a dendogram which is cut into k clusters through red lines")
  rect.hclust(fit, k=numClusters$k, border="red")
  
  .clustering.showNumberOfRecordsPerCluster(fit, numClusters$k)
  return(groups)
}

.clustering.withRawData <- function(dataForClustering, criticalValue = 2.75){
  #d <- dist(dataForClustering, method = .clustering.distanceMethod) # distance matrix
  d<-vegdist(dataForClustering,method="jaccard")
  
  fit <- hclust(d, method= .clustering.Method)
  
  numClusters <- .cutDendroByMojenaTest(fit, criticalValue)
  numClusters
  
  print(paste("Mojena test with critical value of", criticalValue, "returned the following number of clusters:", numClusters$k))
  
  plot(fit, lwd=2) # display dendogram
  # draw dendogram with red borders around the  clusters 
  #numClusters$k = 7 # manually overwrite number of clusters
  groups <- cutree(fit, k=numClusters$k) # cut tree into k clusters
  print("The following diagram shows a dendogram which is cut into k clusters through red lines")
  rect.hclust(fit, k=numClusters$k, border="red")
  
  .clustering.showNumberOfRecordsPerCluster(fit, numClusters$k)
  return(groups)
}

.clustering.showNumberOfRecordsPerCluster <- function(fit, maxClusters){
  # Show number of companies per cluster for different number of clusters
  n.cluster<-sapply(2:maxClusters, function(n.cluster)table(cutree(fit,n.cluster)))
  print("Number of companies per clusters in a k-cluster solution")
  print(n.cluster)
}

.clustering.bindResults <- function(data, groups)
{
  if("cluster" %in% colnames(data))
  {
    data$cluster <- NULL  
  }
  data <- cbind(data, cluster=groups)
  return(data)
}

.clustering.displayClusters <- function(data, groups){
  print("The following IDs/Names would be in each cluster")
  ids <- sapply(unique(groups), function(g)data[which(data$cluster == g),]$id)
  names <- sapply(unique(groups), function(g)data[which(data$cluster == g),]$name)
  print(ids)
  print(names)
  return(ids)
}

.clustering.overviewClusterStats <- function(dataForClustering, groups){  
  #Calculate some statistics to describe the clusters
  groups.median<-aggregate(dataForClustering,list(groups),median)
  groups.min<-aggregate(dataForClustering,list(groups),min)
  groups.max<-aggregate(dataForClustering,list(groups),max)
  
  print(groups.median)
  print(groups.min)
  print(groups.max)
}

.clustering.detailedAttributeClusterStats <- function(dataForClustering, groups, attribute){ 
  #Summary statistics for one variable
  groups.summary<-aggregate(dataForClustering[, get(attribute)],list(groups),summary)
  print(groups.summary)
  return(groups.summary)
}

.clustering.screePlot <- function(dataForClustering, maxNumberOfClusters = 15){
  #Method III : Scree plot to determine the number of clusters
  print("Using kmeans for drawing the elbow chart. Is this correct?")
  wss <- (nrow(dataForClustering)-1)*sum(apply(dataForClustering,2,var))
  for (i in 2:maxNumberOfClusters) {
    wss[i] <- sum(kmeans(dataForClustering,centers=i)$withinss)
  } 
  plot(1:15, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")
}
