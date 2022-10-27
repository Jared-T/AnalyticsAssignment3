# Scopus uses four broad subject areas: Physical sciences, Health sciences, Social Sciences and Life Sciences. 
# These are further divided into 27 major subject areas and over 300 minor subject areas.
# To view a list of the current classification scheme, open the 
# 'Scopus Source List' on the bottom of this page. In the Excel-file go to the sheet ASJC_code_list.
setwd("~/Library/Group Containers/UBF8T346G9.OneDriveStandaloneSuite/OneDrive.noindex/OneDrive/Documents/University stuff/Honours Year/Analytics/Assignment 3")

# Installing Packages
#install.packages("arules")
#install.packages("arulesViz")

# Loading package
library(arules)
library(arulesViz)
library(readr)
library(datasets)
library(grid)
library(tidyverse)
library(readxl)
library(knitr)
library(ggplot2)
library(lubridate)
library(plyr)
library(dplyr)
library(corrplot)
library(RColorBrewer)

options(scipen = 999)

# Set the seed for reproducible results
set.seed(123)

load("Scopus2020.RData")
data = CiteScore2020

# Create the new variables to store the true labels
# These are NOT part of the clustering process, but
# used to check our results. 
# There are two levels for which our algorithms need to be checked:
# 1) The 17 main groupings and 2) The 4 supergroups
trueSubjectAreas = read_delim("SubjectCodes.csv", 
                              delim = ";", escape_double = FALSE, trim_ws = TRUE)
trueSubjectAreas$code = round(trueSubjectAreas$code / 100)

# It is good to get an idea for what our clustering "should" produce. This is done by adding
# the group and supergroup labels to the data and doing some EDA. These are then used as a benchmark
# for the actual analysis, which will not have these variables available to it.

# A function to assign assign the correct group and 
# supergroup labels.
assign.label = function(code, type = 1) {
  code = round(code/ 100)
  
  return(trueSubjectAreas[which(code == trueSubjectAreas$code), type + 1])
}

# Setup the labels for the EDA (note that we probably would not have these in a "true" unsupervised learning problem, so treat it as 
# merely diagnostic - focus on the relationships between the other variables)
data$GroupLabel = unlist(apply(as.matrix(dataEDA$`Scopus ASJC Code (Sub-subject Area)`), 1, FUN = assign.label, type = 1))
data$SuperGroupLabel = unlist(apply(as.matrix(dataEDA$`Scopus ASJC Code (Sub-subject Area)`), 1, FUN = assign.label, type = 2))

## Clean and prep the data

# Check the structure of the data
str(data)

# There are some variables which need to be treated as factors.
# The numeric variables also need to be on the same scale.
data$Title = as.factor(data$Title)
data$`Scopus Sub-Subject Area` = as.factor(data$`Scopus Sub-Subject Area`)
data$Publisher = as.factor(data$Publisher)
data$Type = as.factor(data$Type)
data$`Open Access` = as.factor(data$`Open Access`)
data$`Top 10% (CiteScore Percentile)` = as.factor(data$`Top 10% (CiteScore Percentile)`)
data$GroupLabel = as.factor(data$GroupLabel)
data$SuperGroupLabel = as.factor(data$SuperGroupLabel)
data$Quartile = as.factor(data$Quartile)
data$Percentile = as.factor(data$Percentile)

# The "rank" and "rank out of" variables can be combined
data$RANK = data$RANK / data$`Rank Out Of`

data = as.data.frame(data)

# Remove the variables that are not needed
data = data[, -which(names(data) %in% c("Scopus Source ID",
                                        "URL Scopus Source ID",
                                        "Rank Out Of", 
                                        "Type", 
                                        "Print ISSN",
                                        "E-ISSN",
                                        "Scopus ASJC Code (Sub-subject Area)"
                                        ))]

numeric.vars = sapply(data, is.numeric)
numeric.data = data[numeric.vars]
numeric.data = as.data.frame(numeric.data)



# Univariate analysis
str(data)

# Look at the data summary
summary(data)
summary(numeric.data)

# Looking at the summary of the numeric data, they all seem to be showing the same pattern.
# Need to look at a correlation plot to make sure and see the effect of leaving most out.

# Title has too many levels to be useful - Replace with supergroup

# Plots

# Correlation plot of the numeric variables
M = cor(numeric.data)
M2 = cor(numeric.data[, -which(names(numeric.data) %in% c("Percent Cited", 
                             "Scholarly Output",
                             "SNIP",
                             "SJR"))])

corrplot(M, order="hclust",
         col=brewer.pal(n=8, name="RdYlBu"))
corrplot(M2, order="hclust",
         col=brewer.pal(n=8, name="RdYlBu"))

# Remove the variables that are highly correlated with each other
data.no.cor = data[, -which(names(data) %in% c("Percent Cited", 
                                               "Scholarly Output",
                                               "SNIP",
                                               "SJR"
                                               ))]

# Useful for the SOM
data.num.no.cor = numeric.data[, -which(names(numeric.data) %in% c("Percent Cited", 
                                               "Scholarly Output",
                                               "SNIP",
                                               "SJR"))]

# Scale the numeric datasets
numeric.data.scaled = scale(numeric.data)
data.num.no.cor.scaled = scale(data.num.no.cor)

# Distribution of all citations - make it look nicer
#plot(sort(data.no.cor$RANK, decreasing = T), type = 'l')
#points(sort(data.no.cor$RANK, decreasing = T))

boxplot(data.no.cor$RANK)

par(mfrow = c(1,2))
# Citation count
plot(sort(data.no.cor$`Citation Count`, decreasing = T), type = 'l', xlab = "Observation", ylab = "Citation count")
points(sort(data.no.cor$`Citation Count`, decreasing = T))

# Citescore 2020
plot(sort(data.no.cor$`CiteScore 2020`, decreasing = T), type = 'l',  xlab = "Observation", ylab = "Citation count")
points(sort(data.no.cor$`CiteScore 2020`, decreasing = T))


# Once again, very similiar, seems to follow Pareto distribution






##################### SOM #######################






library(kohonen)
library(caret)

set.seed(123)
sample.i = 1:nrow(numeric.data) #sample(1:nrow(numeric.data.scaled), round(nrow(numeric.data.scaled) / 3))

# Split into two SOMS - one for Q1 and one for the rest
# This is because values in Q1 are considered outliers
Q1.i = which(data$Quartile == 1)
data.som1=as.matrix(numeric.data)[-Q1.i, ]
data.som2=as.matrix(numeric.data)[Q1.i, ]

#nrow(data.som1)
#nrow(data.som2)

# Log transform some of the variables
data.som1[, c(1,2,4,5,6)] = log(data.som1[, c(1,2,4,5,6)])
data.som2[, c(1,2,4,5,6)] = log(data.som2[, c(1,2,4,5,6)])

# Handle -Inf
data.som1[data.som1 == -Inf] = -1
data.som2[data.som2 == -Inf] = -1

data.som1 = scale(data.som1)
data.som2 = scale(data.som2)

# Percentile needs to be scaled
Percentile = scale(as.numeric(data$Percentile)[sample.i])

# Quartile needs to be one-hot encoded
Quartile = as.data.frame(data$Quartile[sample.i])
dummy <- dummyVars(" ~ .", data=Quartile)
Quartile <- data.frame(predict(dummy, newdata=Quartile))
colnames(Quartile) = c("Q1", "Q2", "Q3", "Q4")

# Open access needs to be one-hot encoded
OA = as.data.frame(data$`Open Access`[sample.i])
dummy = dummyVars(" ~ .", data=OA)
OA = data.frame(predict(dummy, newdata=OA))
colnames(OA) = c("OA.No", "OA.Yes")

# Top 10 needs to be one-hot encoded
T10 = as.data.frame(data$`Top 10% (CiteScore Percentile)`[sample.i])
dummy = dummyVars(" ~ .", data=T10)
T10 = data.frame(predict(dummy, newdata=T10))
colnames(T10) = c("T10.NO", "T10.Yes")

# Detect and remove extreme outliers
detect.outlier = function(x) {
  
  # calculate first quantile
  Quantile1 = quantile(x, probs=.25)
  
  # calculate third quantile
  Quantile3 = quantile(x, probs=.75)
  
  # calculate inter quartile range
  IQR = Quantile3-Quantile1
  
  # return true or false
  #x > Quantile3 + (IQR*1.5) | x < Quantile1 - (IQR*1.5)
  x > Quantile3 + (IQR*3) | x < Quantile1 - (IQR*3)
}

# create remove outlier function
remove.outliers = function(dataframe,
                            columns=names(dataframe)) {
  
  # for loop to traverse in columns vector
  for (col in columns) {
    
    # remove observation if it satisfies outlier function
    dataframe = dataframe[!detect.outlier(dataframe[[col]]), ]
  }
  
  # return dataframe
  print("Removed outliers")
  #print(dataframe)
  return(dataframe)
}

before_outlier_removal1 = as.data.frame(cbind(data.som1, Percentile[-Q1.i], Quartile[-Q1.i, 2:4]))
before_outlier_removal2 = as.data.frame(cbind(data.som2, Percentile[Q1.i]))

# Remove outliers
data.som1 = remove.outliers(before_outlier_removal1)
data.som2 = remove.outliers(before_outlier_removal2)

data.som1 = as.matrix(data.som1)
data.som2 = as.matrix(data.som2)

# Check which rows were removed due to outliers
colsToUse <- intersect(colnames(before_outlier_removal1), colnames(data.som1))
removed.i = match(do.call("paste", as.data.frame(before_outlier_removal1[, colsToUse])), do.call("paste", as.data.frame(data.som1[, colsToUse])))
keep.i.1 = which(!is.na(removed.i))

colsToUse <- intersect(colnames(before_outlier_removal2), colnames(data.som2))
removed.i = match(do.call("paste", as.data.frame(before_outlier_removal2[, colsToUse])), do.call("paste", as.data.frame(data.som2[, colsToUse])))
keep.i.2 = which(!is.na(removed.i))

nrow(data.som1)
nrow(data.som2)

# Store the true labels
true.labs.1 = data$SuperGroupLabel[-Q1.i]
true.labs.1 = true.labs.1[keep.i.1]

true.labs.2 = data$SuperGroupLabel[Q1.i]
true.labs.2 = true.labs.1[keep.i.2]

# Create the grid - want about 10 observations per node
# Training on full data set
som.grid.1 = somgrid(xdim = 65, ydim = 65, topo="hexagonal")
som.grid.2 = somgrid(xdim = 35, ydim = 35, topo="hexagonal")

set.seed(123)

som.model1 <- som(data.som1, grid=som.grid.1, 
                 rlen=3000, 
                 alpha=c(0.05,0.01),
                 keep.data = TRUE
)


som.model2 <- som(data.som2, grid=som.grid.2, 
                  rlen=3000, 
                  alpha=c(0.05,0.01),
                  keep.data = TRUE
)


save(som.model1, file = 'som1.Rdata')
save(som.model2, file = 'som2.Rdata')

#load('som1.Rdata')
#load('som2.Rdata')

#names(som.model1)
#names(som.model2)

# Create the colour palette
coolBlueHotRed = function(n, alpha = 1) {rainbow(n, end=4/6, alpha=alpha)[n:1]}

par(mfrow=c(1,2))
# Training process
plot(som.model1, type="changes", main = "Model with Q2 - Q4")
plot(som.model2, type="changes", main = "Model with Q1 only")

# Node counts
plot(som.model1, type="counts", main = "", palette.name = coolBlueHotRed)
plot(som.model2, type="counts", main = "", palette.name = coolBlueHotRed)


# Neighbor distance
plot(som.model1, type="dist.neighbours", main = "", palette.name = coolBlueHotRed)
plot(som.model2, type="dist.neighbours", main = "", palette.name = coolBlueHotRed)

# Codes / weight vectors
#plot(som.model1, type="codes")
#plot(som.model2, type="codes")

# Quality
plot(som.model1, type="quality", main = "", palette.name = coolBlueHotRed)
plot(som.model2, type="quality", main = "", palette.name = coolBlueHotRed)

# Mapping
plot(som.model1, type="mapping", main = "", palette.name = coolBlueHotRed)
plot(som.model2, type="mapping", main = "", palette.name = coolBlueHotRed)

# Heatmaps
codes1=som.model1$codes[[1]]
codes2=som.model2$codes[[1]]

#summary(as.factor(T10[keep.i, 2]))

colnames(data.som1)[8] = "Percentile"
colnames(data.som2)[8] = "Percentile"

# Scaled heatmaps
par(mfrow=c(4,3))
for(i in 1:11){
  plot(som.model1, type = "property", property = codes1[,i], main=colnames(data.som1)[i], palette.name = coolBlueHotRed)
}

par(mfrow=c(4,2))
for(i in 1:8){
  plot(som.model2, type = "property", property = codes2[,i], main=colnames(data.som2)[i], palette.name = coolBlueHotRed)
}

#dev.off()

# Clustering
wss = (nrow(data.som1)-1)*sum(apply(data.som1, 2, var)) 
wss.2 = (nrow(data.som2)-1)*sum(apply(data.som2, 2, var)) 

for (i in 1:11) {
  wss[i] = sum(kmeans(data.som1, centers=i)$withinss)
}

for (i in 1:8) {
  wss.2[i] = sum(kmeans(data.som2, centers=i)$withinss)
}

par(mfrow = c(1,2))
plot(wss, type = "o")
plot(wss.2, type = "o", ylab = "wss")

# Use Hierarchical clustering
som_cluster = cutree(hclust(dist(som.model1$codes[[1]])), 4)
som_cluster2 = cutree(hclust(dist(som.model2$codes[[1]])), 4)

plot(som.model1, type="mapping", 
     main = "Clusters", cex = .5) 
add.cluster.boundaries(som.model1, som_cluster)

plot(som.model2, type="mapping", 
     main = "Clusters", cex = .5) 
add.cluster.boundaries(som.model2, som_cluster2)


# Hierarchical clustering plot
pretty_palette <- c("#1f77b4", '#ff7f0e', '#2ca02c', '#d62728', '#9467bd', '#8c564b', '#e377c2')
plot(som.model1, 
     type="mapping", 
     bgcol = pretty_palette[som_cluster], cex=.8, pchs="", main = "")
som.hc.1 <- cutree(hclust(dist(som.model1$codes[[1]])), 4)
add.cluster.boundaries(som.model1, som.hc.1)


plot(som.model2, 
     type="mapping", 
     bgcol = pretty_palette[som_cluster2], cex=.8, pchs="", main = "")
som.hc.2 <- cutree(hclust(dist(som.model2$codes[[1]])), 4)
add.cluster.boundaries(som.model2, som.hc.2)



### Mapping the clusters back to the original variables ###

finaldata1 = as.data.frame(data.som1)
finaldata2 = as.data.frame(data.som2)

# get vector with cluster value for each original data sample
cluster_assignment = som.hc.1[som.model1$unit.classif]
cluster_assignment2 = som.hc.2[som.model2$unit.classif]

# for each of analysis, add the assignment as a column in the original data:
finaldata1$cluster = cluster_assignment
finaldata1$truelabs = true.labs.1
finaldata1$whichnode = som.model1$unit.classif

finaldata2$cluster = cluster_assignment2
finaldata2$truelabs = true.labs.2
finaldata2$whichnode = som.model2$unit.classif


# Compare the true labels with the clusters
finaldata1 = arrange(finaldata1, cluster, truelabs)
finaldata2 = arrange(finaldata2, cluster, truelabs)


# Get the total number of observations for each of the true labels
summary(finaldata1$truelabs)
summary(as.factor(finaldata1$cluster))

summary(finaldata2$truelabs)
summary(as.factor(finaldata2$cluster))

finaldata1$cluster = as.factor(finaldata1$cluster)
finaldata2$cluster = as.factor(finaldata2$cluster)

par(mfrow = c(1,2))
plot(`Citation Count` ~ cluster, data = finaldata1)
plot(`Citation Count` ~ truelabs, data = finaldata1)

## Which clusters best represent the true labels? ###

# Which cluster matches Social Sciences best?
socsci = filter(finaldata2, truelabs == "Social Sciences")
nrow(filter(socsci, cluster == 1))
nrow(filter(socsci, cluster == 2)) 
nrow(filter(socsci, cluster == 3)) # Cluster 3 and 4 seems 
nrow(filter(socsci, cluster == 4))

# Which cluster matches Physical Sciences best?
physci = filter(finaldata2, truelabs == "Physical Sciences")
nrow(filter(physci, cluster == 1))
nrow(filter(physci, cluster == 2))
nrow(filter(physci, cluster == 3))
nrow(filter(physci, cluster == 4))

# Which cluster matches Health Sciences best?
healthsci = filter(finaldata2, truelabs == "Health Sciences")
nrow(filter(healthsci, cluster == 1))
nrow(filter(healthsci, cluster == 2))
nrow(filter(healthsci, cluster == 3))
nrow(filter(healthsci, cluster == 4))

# Which cluster matches Life Sciences best?
lifesci = filter(finaldata2, truelabs == "Life Sciences")
nrow(filter(lifesci, cluster == 1))
nrow(filter(lifesci, cluster == 2))
nrow(filter(lifesci, cluster == 3))
nrow(filter(lifesci, cluster == 4))


hist(cluster_assignment)
hist(as.numeric(data$SuperGroupLabel))


