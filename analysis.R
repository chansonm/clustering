dim_valuechain <- c('frontoffice', 'backoffice', 'infrastructure')
dim_financialservicearea <- c('investment_wealthmgmt', 'savings', 'payments', 'lending', 'capital_raising', 'market_provisioning', 'insurance')
dim_customer <- c('b2c', 'b2b')
cell_attributes <- c('txc_search', 'txc_verification', 'txc_monitoring', 'txc_enforcement', 'txc_automation', 'info_asymmetries', 'at_maturity', 'at_denomination', 'at_liquidity', 'at_risk')

.analysis.runSignificanceTests <- function(data){  
  
  clusters <- unique(data$cluster)
  
  variables <- colnames(data)
  variables <- variables[variables != "cluster"]

  stats <- c('chisq_global_stat', 'chisq_global_p_value')
  for(i in 1:length(clusters)) {
    for(j in 1:length(clusters)) {
      if(i < j) { 
        stats <- c(stats, paste("t_test_cluster_", i, '_', j, "_t", sep=''))  
        stats <- c(stats, paste("t_test_cluster_", i, '_', j, "_p_value", sep=''))  
      }
    }
  }
  
  results <- matrix(, nrow=length(variables), ncol=length(stats))
  colnames(results) <- stats
  rownames(results) <- variables

  for(i in 1:length(variables))
  {
    print("=================================")
    print(paste("Testing for", variables[i]))
    results[i,] <- test.all(variables[i], data)
  }

  print(results)
  return(results)
}

test.betweenclusters <- function(variableName, clusterA, clusterB){
  result <- tryCatch({
    t.test(clusterA[,get(variableName)], clusterB[,get(variableName)])
  },error=function(e){
    print(paste(variableName, 'error:', e))
    return(list(statistic = NaN, p.value = NaN))
  }
  )
  return(result)
}

test.all <- function(variableName, data){

  # Chi Squared
  global <- chisq.test(data[,get(variableName)], data$cluster)
  results <- c(global$statistic, global$p.value)
  
  # Pairwise t-tests
  clusters <- unique(data$cluster)
  for(i in 1:length(clusters)) {
    for(j in 1:length(clusters)) {
      if(i < j) { 
        clusterA <- data[ which(data$cluster == i),]
        clusterB <- data[ which(data$cluster == j),]
        
        ttest <- test.betweenclusters(variableName, clusterA, clusterB)
        results <- c(results, ttest$statistic, ttest$p.value)
      }
    }
  }

  return(results)
}


# Correlation analysis
.analysis.correlationMatrixValuechainAreaCustomersegment <- function(data){
  results = list()
  results$valuechain <- matrix(, nrow=length(cell_attributes), ncol=length(dim_valuechain))
  results$financialservicearea <- matrix(, nrow=length(cell_attributes), ncol=length(dim_financialservicearea))
  results$customer <- matrix(, nrow=length(cell_attributes), ncol=length(dim_customer))
  colnames(results$valuechain) <- dim_valuechain
  rownames(results$valuechain) <- cell_attributes
  colnames(results$financialservicearea) <- dim_financialservicearea
  rownames(results$financialservicearea) <- cell_attributes
  colnames(results$customer) <- dim_customer
  rownames(results$customer) <- cell_attributes
  
  
  for(i in 1:length(cell_attributes))
  {
    variableName = cell_attributes[i]
    
    for(j in 1:length(dim_valuechain))
    {
      dimName = dim_valuechain[j]
      testResult <- cor.test(data[,get(variableName)], data[,get(dimName)])
      if(testResult$p.value <= 0.05 && testResult$estimate > 0)
      {
        results$valuechain[i, j] <- testResult$estimate
      }
    }
    
    for(j in 1:length(dim_financialservicearea))
    {
      dimName = dim_financialservicearea[j]
      testResult <- cor.test(data[,get(variableName)], data[,get(dimName)])
      if(testResult$p.value <= 0.05 && testResult$estimate > 0)
      {
        results$financialservicearea[i, j] <- testResult$estimate
      }
    }
    
    for(j in 1:length(dim_customer))
    {
      dimName = dim_customer[j]
      testResult <- cor.test(data[,get(variableName)], data[,get(dimName)])
      if(testResult$p.value <= 0.05 && testResult$estimate > 0)
      {
        results$customer[i, j] <- testResult$estimate  
      } 
    }
  }
  
  print(results)
  return(results)
}
