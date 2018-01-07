require(data.table)
require(vegan)
source("./prep-data.R")
source("./pca.R")
source("./clustering.R")
source("./analysis.R")
# Read data and test that they're OK
# TODO: Replace this path with the filename you want to read
filename <- "./for_R_master.csv"
excludedColumns <- c("name",
                     "traditional_funding_after",
                     "%",
                     "opensource_solution",
                     "nonprofit_cryptocurrency",
                     "existence_m",
                     "fte",
                     "token_utility_product",
                     "only_payment",
                     "transaction",
                     "generation",
                     "other_usage",
                     "info",
                     "funding",
                     "coin_offered",
                     "cheaper",
                     "price_ratio",
                     "start_date",
                     "ICO_days",
                     "bs",
                     "other_p",
                     "minimum_pledge",
                     "high_transaction_fee",
                     "platform_used",
                     "In_2017",
                     "funding_standardized")
#excludedColumns <- c("name")
data <- .data.read(filename)
dataForClustering <- .data.prepAndTest(data, excludedColumns)

# Principal Component Analysis
#pr.out <- .runPCA(dataForClustering)
#.pca.biPlot(pr.out)
#.pca.eigenValues(pr.out)
#.pca.selectNumberComponents(pr.out)

# Clustering
.clustering.screePlot(dataForClustering)
mojenaTestCriticalValue <- 2.5 # suggested: 2.75 - 3.50; higher values lead to fewer clusters; 1.25 also seems to be a popular choice

# CHOSE EITHER PCA clustering OR RAW DATA CLUSTERING
# clusteringOutput <- .clustering.withPCA(pr.out, 6, mojenaTestCriticalValue)
clusteringOutput <- .clustering.withRawData(dataForClustering, mojenaTestCriticalValue)

data <- .clustering.bindResults(data, clusteringOutput)
dataForClustering <- .clustering.bindResults(dataForClustering, clusteringOutput)
clusteredCompanyIds <- .clustering.displayClusters(data, clusteringOutput)
#write.csv(clusteredCompanyIds, file="~/Downloads/clustering-results.csv") # THIS DOES NOT WORK YET
write.csv(data, file="./clustering-results.csv")

#.clustering.overviewClusterStats(dataForClustering, clusteringOutput)
#.clustering.detailedAttributeClusterStats(dataForClustering, clusteringOutput, "savings")
# Analysis

# results <- .analysis.runSignificanceTests(dataForClustering)
# write.csv(results, file="~/Downloads/analytics-results.csv")
# correlation.results <- .analysis.correlationMatrixValuechainAreaCustomersegment(dataForClustering)
