# clustering

require(data.table)
require(vegan)
source("Documents/Master thesis/R_scripts/prep-data.R")
source("Documents/Master thesis/R_scripts/pca.R")
source("Documents/Master thesis/R_scripts/clustering.R")
source("Documents/Master thesis/R_scripts/analysis.R")

# Read data and test that they're OK
filename <- "Documents/Master thesis/simple_data.csv"
#excludedColumns <- c("name",	"traditional_funding_before",	"traditional_funding_after",	"%",	"profit	blockchain",	"single",	"token_utility_product",	"only_payment",	"transaction",	"generation",	"other_usage",	"main_value",	"voting_shareholder",	"voting_product",	"voting_other",	"profit_sharing",	"info", "funding", 	"coin_offered",	"cap_money",	"same_price",	"expensive",	"start_date",	"ICO_days",	"cap_time",	"bitcoin",	"Ethereum",	"other_bc",	"bs",	"other_p",	"transaction_fee")
excludedColumns <- c("name")
data <- .data.read(filename)
dataForClustering <- .data.prepAndTest(data, excludedColumns)

# Principal Component Analysis
#pr.out <- .runPCA(dataForClustering)
#.pca.biPlot(pr.out)
#.pca.eigenValues(pr.out)
#.pca.selectNumberComponents(pr.out)

# Clustering
.clustering.screePlot(dataForClustering)
mojenaTestCriticalValue <- 3.0 # suggested: 2.75 - 3.50; higher values lead to fewer clusters; 1.25 also seems to be a popular choice

# CHOSE EITHER PCA clustering OR RAW DATA CLUSTERING
# clusteringOutput <- .clustering.withPCA(pr.out, 6, mojenaTestCriticalValue)
clusteringOutput <- .clustering.withRawData(dataForClustering, mojenaTestCriticalValue)

data <- .clustering.bindResults(data, clusteringOutput)
dataForClustering <- .clustering.bindResults(dataForClustering, clusteringOutput)
clusteredCompanyIds <- .clustering.displayClusters(data, clusteringOutput)
#write.csv(clusteredCompanyIds, file="~/Downloads/clustering-results.csv") # THIS DOES NOT WORK YET

#.clustering.overviewClusterStats(dataForClustering, clusteringOutput)
#.clustering.detailedAttributeClusterStats(dataForClustering, clusteringOutput, "savings")
# Analysis

results <- .analysis.runSignificanceTests(dataForClustering)
write.csv(results, file="~/Downloads/analytics-results.csv")
correlation.results <- .analysis.correlationMatrixValuechainAreaCustomersegment(dataForClustering)