.runPCA <- function(data){
  pr.out=prcomp(data, scale=TRUE)  
  return(pr.out)
}

.pca.biPlot <- function(pr.out){
  pr.out$rotation=-pr.out$rotation
  pr.out$x=-pr.out$x
  print("The following plot shows the biplot of the first two Principal Components")
  biplot(pr.out, scale=0)
}

.pca.eigenValues <- function(pr.out){
  
  pr.out$sdev
  pr.var=pr.out$sdev^2
  # Eigenvalues of PCs
  print("The following shows the eigen value of each principal component; For selecting the number of PCs, roughly make the cut-off where there is a steep decrease ('elbow chart'), but above 1")
  pr.var
}

.pca.selectNumberComponents <- function(pr.out){
#   Selection
#   1- eigenvalue one criterion: based on this criterion you choose the first components with eigenvalues higher than 1.
#   2- Amount of explained variance: based on this, the chosen factors should explain 70 to 80% of your variance at least.
#   3- Scree plot: this is a graphical method in which you choose the factors until a break in the graph. 
  pr.var=pr.out$sdev^2
  pve=pr.var/sum(pr.var)
  print("The following explains how much variance is explained. Sum up numbers until ~70% is reached, and take the number of clusters")
  print(pve)
  
  plot(pve, xlab="Principal Component", ylab="Proportion of Variance Explained ", ylim=c(0,1),type="b")
  print("The following shows the cumulative variance explained (choose number of PCs that exceed ~70%)")
  print(cumsum(pve)) # 8 PCs selected
  print("The following is a visual representation of the cumulative variance explained ('elbow graph')")
  plot(cumsum(pve), xlab="Principal Component ", ylab=" Cumulative Proportion of Variance Explained ", ylim=c(0,1), type="b")

  #loadings(pr.out)
}