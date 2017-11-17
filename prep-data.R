.data.read <- function(filename){
  csv <- fread(filename)
  data <- copy(csv)
  # REMOVE EXCLUDED COMPANIES
  # Todo: We do not need to exclude anything so far
  # data <- data[which(data$excluded == 0),]
  return(data)

}
.data.prepAndTest <- function(data, excludedColumns){
  rownames(data) <- data$name
  colnames(data)
  
  
  #dataForClustering <- data[which(data$excluded == 0),]
  dataForClustering <- data[, !excludedColumns, with=FALSE]
  dataForClustering
  apply(dataForClustering, 2, mean)
  apply(dataForClustering, 2, var)  
  return(dataForClustering)
}