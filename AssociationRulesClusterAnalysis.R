# Scopus uses four broad subject areas: Physical sciences, Health sciences, Social Sciences and Life Sciences. 
# These are further divided into 27 major subject areas and over 300 minor subject areas.
# To view a list of the current classification scheme, open the 
# 'Scopus Source List' on the bottom of this page. In the Excel-file go to the sheet ASJC_code_list.
#setwd("~/Library/Group Containers/UBF8T346G9.OneDriveStandaloneSuite/OneDrive.noindex/OneDrive/Documents/University stuff/Honours Year/Analytics/Assignment 3")

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
library(cluster)
library(dendextend)
library(NMF)
library(Biobase)
library(clustMixType)

#if (!requireNamespace("BiocManager", quietly = TRUE))
  #install.packages("BiocManager")

#BiocManager::install("Biobase")

options(scipen = 999)

# Set the seed for reproducible results
set.seed(123)

load("Scopus2020.RData")
data = CiteScore2020

# Create the new variables to store the true labels
# These are NOT part of the clustering process, but
# used to check our results. 
# Our algorithms need to be checked for the 4 supergroups
trueSubjectAreas = read_delim("SubjectCodes.csv", 
                              delim = ";", escape_double = FALSE, trim_ws = TRUE)
trueSubjectAreas$code = round(trueSubjectAreas$code / 100)

table(trueSubjectAreas$Supergroup)

# It is good to get an idea for what our clustering "should" produce. This is done by adding
# the group and supergroup labels to the data and doing some EDA. These are then used as a benchmark
# for the actual analysis, which will not have these variables available to it.

# A function to assign assign the correct group and 
# supergroup labels.
assign.label = function(code, type = 1) {
  code = round(code/ 100)
  
  return(trueSubjectAreas[which(code == trueSubjectAreas$code), type + 1])
}

dataNoLab = data

table(data$SuperGroupLabel)

# Setup the labels for the EDA (note that we probably would not have these in a "true" unsupervised learning problem, so treat it as 
# merely diagnostic - focus on the relationships between the other variables)
data$GroupLabel = unlist(apply(as.matrix(data$`Scopus ASJC Code (Sub-subject Area)`), 1, FUN = assign.label, type = 1))
data$SuperGroupLabel = unlist(apply(as.matrix(data$`Scopus ASJC Code (Sub-subject Area)`), 1, FUN = assign.label, type = 2))

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

# Distribution of all citations - make it look nicer
#plot(sort(data.no.cor$RANK, decreasing = T), type = 'l')
#points(sort(data.no.cor$RANK, decreasing = T))

# Citation count
plot(sort(data.no.cor$`Citation Count`, decreasing = T), type = 'l')
points(sort(data.no.cor$`Citation Count`, decreasing = T))

# Citescore 2020
plot(sort(data.no.cor$`CiteScore 2020`, decreasing = T), type = 'l')
points(sort(data.no.cor$`CiteScore 2020`, decreasing = T))


# Once again, very similiar, seems to follow Pareto distribution


dataEDA = data.no.cor

### Association rules

### Association rules for EDA

# Fitting model
# Training on the dataset
set.seed = 123 # Setting seed

# Mining frequent associations between observations by level-wise search
rulesdata = dataEDA[, -c(1, 4, 11)]
str(rulesdata)

# Support is the proportion of observations containing some feature.
# Frequent itemset when confidence exceeds min threshold

# Confidence is the predictive power of the identified association.
# It describes the probability of one feature given another.
# 0.1 support and 0.8 confidence are default mins
a.rules = apriori(data = rulesdata, 
                  parameter = list(support = 0.2, 
                                   confidence = 0.8))
rules = arules::sort(a.rules, by='confidence', decreasing = TRUE)
inspect(rules)

rules@

# Visualising the results for the top 10 rules
subrules = head(a.rules, n = 20, by = "confidence")
subdf = data.frame(
  lhs = labels(lhs(subrules)),
  rhs = labels(rhs(subrules)), 
  subrules@quality)
kable(subdf, format = "latex")
plot(subrules, method = "graph",  engine = "htmlwidget")

citation("cluster")

## Hierarchical Clustering - Method 1 ##

# Sample 10% of dataset for clustering
clusterdat = dataEDA[, -c(1, 4)]
set.seed(123)
clustersub = slice_sample(clusterdat, prop = 0.1)
# Remove labels
blankclustersub = clustersub[, -c(9, 10)]

# Singleton removed
clustersub2 = clustersub[-5310, ]
blankclustersub2 = blankclustersub[-5310, ]

# Finding distance matrix

# Original
distance_mat = daisy(blankclusterdat)
# Singleton removed
distance_mat2 = daisy(blankclustersub2)
summary(distance_mat)

# Fitting Hierarchical clustering Model

# Try three different clustering methods
set.seed(123)  # Setting seed
Hierar_cl1 = hclust(distance_mat, method = "complete")
Hierar_cl2 = hclust(distance_mat, method = "centroid")
Hierar_cl3 = hclust(distance_mat, method = "average")

# Goodness of fit

# Cophenetic correlation between methods

coph1 <- cor(cophenetic(Hierar_cl1), distance_mat)
coph2 <- cor(cophenetic(Hierar_cl2), distance_mat)
coph3 <- cor(cophenetic(Hierar_cl3), distance_mat)

# Plotting dendrograms

# Cutting tree by no. of anticipated SuperGroup clusters

fit1 = cutree(Hierar_cl1, k = 4)
table(fit1)

fit2 = cutree(Hierar_cl2, k = 4)
table(fit2)

fit3 = cutree(Hierar_cl3, k = 4)
table(fit3)

citation("clustMixType")
# view cluster contents

# Complete linkage
compcluster1 = clustersub[which(fit1 == 1), ]
compcluster1$Group = "Complete"
compcluster2 = clustersub[which(fit1 == 2), ]
compcluster2$Group = "Complete"
compcluster3 = clustersub[which(fit1 == 3), ]
compcluster3$Group = "Complete"
compcluster4 = clustersub[which(fit1 == 4), ]
compcluster4$Group = "Complete"


# Average linkage
avecluster1 = clustersub[which(fit3 == 1), ]
avecluster1$Group = "Average"
avecluster2 = clustersub[which(fit3 == 2), ]
avecluster2$Group = "Average"
avecluster3 = clustersub[which(fit3 == 3), ]
avecluster3$Group = "Average"
avecluster4 = clustersub[which(fit3 == 4), ]
avecluster4$Group = "Average"

# Look for differences in structure between linkage methods
cluster1dat = rbind(compcluster1, avecluster1)
cluster2dat = rbind(compcluster2, avecluster2)
cluster3dat = rbind(compcluster3, avecluster3)
cluster4dat = rbind(compcluster4, avecluster4)

# Cluster 1
prop.table(summary(avecluster1$`Open Access`))
prop.table(summary(compcluster1$`Open Access`))
prop.table(summary(avecluster1$Quartile))
prop.table(summary((compcluster1$Quartile)))
prop.table(summary((avecluster1$`Top 10% (CiteScore Percentile)`)))
prop.table(summary((compcluster1$`Top 10% (CiteScore Percentile)`)))

# Cluster 2
prop.table(summary(avecluster2$`Open Access`))
prop.table(summary(compcluster2$`Open Access`))
prop.table(summary(avecluster2$Quartile))
prop.table(summary((compcluster2$Quartile)))
prop.table(summary((avecluster2$`Top 10% (CiteScore Percentile)`)))
prop.table(summary((compcluster2$`Top 10% (CiteScore Percentile)`)))

# Cluster 3
prop.table(summary(avecluster3$`Open Access`))
prop.table(summary(compcluster3$`Open Access`))
prop.table(summary(avecluster3$Quartile))
prop.table(summary((compcluster3$Quartile)))
prop.table(summary((avecluster3$`Top 10% (CiteScore Percentile)`)))
prop.table(summary((compcluster3$`Top 10% (CiteScore Percentile)`)))

# Cluster 4
prop.table(summary(avecluster4$`Open Access`))
prop.table(summary(compcluster4$`Open Access`))
prop.table(summary(avecluster4$Quartile))
prop.table(summary((compcluster4$Quartile)))
prop.table(summary((avecluster4$`Top 10% (CiteScore Percentile)`)))
prop.table(summary((compcluster4$`Top 10% (CiteScore Percentile)`)))

# Cluster 1

a = sort(summary(avecluster1$Publisher), decreasing = TRUE)[1:6]
a = data.frame(a)
a$Journal = row.names(a)
kable(a, format = "latex")
b = sort(summary(compcluster1$Publisher), decreasing = TRUE)[1:6]
b = data.frame(b)
kable(b, format = "latex")

c11 = ggplot(data = cluster1dat, aes(x = Group, y = RANK, fill = Group)) +
  geom_boxplot(width = 0.8, position = position_dodge(), alpha = 0.75)  +
  labs(x = "", y = "Rank within Sub-Subject Area \n") + theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.y = element_text(colour="black", face = "bold", size = 10),
        legend.position = "none")

c12 = ggplot(data = cluster1dat, aes(x = Group, y = `CiteScore 2020`, fill = Group)) +
  geom_boxplot(width = 0.8, position = position_dodge(), alpha = 0.75)  +
  labs(x = "", y = "CiteScore (2020) \n") + theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.y = element_text(colour="black", face = "bold", size = 10),
        legend.position = "none")

c13 = ggplot(data = cluster1dat, aes(x = Group, y = `Citation Count`, fill = Group)) +
  geom_point()  +
  labs(x = "", y = "Citation Count \n") + theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.y = element_text(colour="black", face = "bold", size = 10),
        legend.position = "none")

ggarrange(c11, c12, c13, nrow = 1, ncol = 3)

# Cluster 2

a = sort(summary(avecluster2$Publisher), decreasing = TRUE)[1:6]
a = data.frame(a)
a$Journal = row.names(a)
kable(a, format = "latex")
b = sort(summary(compcluster2$Publisher), decreasing = TRUE)[1:6]
b = data.frame(b)
kable(b, format = "latex")

c21 = ggplot(data = cluster2dat, aes(x = Group, y = RANK, fill = Group)) +
  geom_boxplot(width = 0.8, position = position_dodge(), alpha = 0.75)  +
  labs(x = "", y = "Rank within Sub-Subject Area \n") + theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.y = element_text(colour="black", face = "bold", size = 10),
        legend.position = "none")

c22 = ggplot(data = cluster2dat, aes(x = Group, y = `CiteScore 2020`, fill = Group)) +
  geom_boxplot(width = 0.8, position = position_dodge(), alpha = 0.75)  +
  labs(x = "", y = "CiteScore (2020) \n") + theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.y = element_text(colour="black", face = "bold", size = 10),
        legend.position = "none")

c23 = ggplot(data = cluster2dat, aes(x = Group, y = `Citation Count`, fill = Group)) +
  geom_point()  +
  labs(x = "", y = "Citation Count \n") + theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.y = element_text(colour="black", face = "bold", size = 10),
        legend.position = "none")

ggarrange(c21, c22, c23, nrow = 1, ncol = 3)

# Cluster 3

a = sort(summary(avecluster3$Publisher), decreasing = TRUE)[1:6]
a = data.frame(a)
a$Journal = row.names(a)
kable(a, format = "latex")
b = sort(summary(compcluster3$Publisher), decreasing = TRUE)[1:6]
b = data.frame(b)
kable(b, format = "latex")

c31 = ggplot(data = cluster3dat, aes(x = Group, y = RANK, fill = Group)) +
  geom_boxplot(width = 0.8, position = position_dodge(), alpha = 0.75)  +
  labs(x = "", y = "Rank within Sub-Subject Area \n") + theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.y = element_text(colour="black", face = "bold", size = 10),
        legend.position = "none")

c32 = ggplot(data = cluster3dat, aes(x = Group, y = `CiteScore 2020`, fill = Group)) +
  geom_boxplot(width = 0.8, position = position_dodge(), alpha = 0.75)  +
  labs(x = "", y = "CiteScore (2020) \n") + theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.y = element_text(colour="black", face = "bold", size = 10),
        legend.position = "none")

c33 = ggplot(data = cluster3dat, aes(x = Group, y = `Citation Count`, fill = Group)) +
  geom_point()  +
  labs(x = "", y = "Citation Count \n") + theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.y = element_text(colour="black", face = "bold", size = 10),
        legend.position = "none")

ggarrange(c31, c32, c33, nrow = 1, ncol = 3)

# Cluster 4

b = sort(summary(compcluster4$Publisher), decreasing = TRUE)[1:6]
b = data.frame(b)
kable(b, format = "latex")

c41 = ggplot(data = cluster4dat, aes(x = Group, y = RANK)) +
  geom_boxplot(fill = "pink", width = 0.8, position = position_dodge(), alpha = 0.75)  +
  labs(x = "", y = "Rank within Sub-Subject Area \n") + theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.y = element_text(colour="black", face = "bold", size = 10),
        legend.position = "none")

c42 = ggplot(data = cluster4dat, aes(x = Group, y = `CiteScore 2020`)) +
  geom_boxplot(fill = "pink", width = 0.8, position = position_dodge(), alpha = 0.75)  +
  labs(x = "", y = "CiteScore (2020) \n") + theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.y = element_text(colour="black", face = "bold", size = 10),
        legend.position = "none")

c43 = ggplot(data = cluster4dat, aes(x = Group, y = `Citation Count`)) +
  geom_point()  +
  labs(x = "", y = "Citation Count \n") + theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.y = element_text(colour="black", face = "bold", size = 10),
        legend.position = "none")

ggarrange(c41, c42, c43, nrow = 1, ncol = 3)

# Final clustering method
Hierar_cl32 = hclust(distance_mat2, method = "average")
fit32 = cutree(Hierar_cl32, k = 3)
table(fit32)

avecluster12 = clustersub2[which(fit32 == 1), ]
avecluster12$Group = "Average"
avecluster22 = clustersub2[which(fit32 == 2), ]
avecluster22$Group = "Average"
avecluster32 = clustersub2[which(fit32 == 3), ]
avecluster32$Group = "Average"

# Compare to known labels

# Social and Physical Sciences
avelab1 = avecluster12 %>% 
  group_by(SuperGroupLabel) %>%
  summarise(no_rows = length(SuperGroupLabel))
avelab1 = avelab1[order(avelab1$no_rows, decreasing = TRUE), ][1:5, ]
avelab1$Cluster = 1
avelab1$no_rows = avelab1$no_rows/nrow(avecluster12)

# Social and Physical Sciences
avelab2 = avecluster22 %>% 
  group_by(SuperGroupLabel) %>%
  summarise(no_rows = length(SuperGroupLabel))
avelab2 = avelab2[order(avelab2$no_rows, decreasing = TRUE), ][1:5, ]
avelab2$Cluster = 2
avelab2$no_rows = avelab2$no_rows/nrow(avecluster22)

# Social and Physical Sciences
avelab3 = avecluster32 %>% 
  group_by(SuperGroupLabel) %>%
  summarise(no_rows = length(SuperGroupLabel))
avelab3 = avelab3[order(avelab3$no_rows, decreasing = TRUE), ][1:5, ]
avelab3$Cluster = 3
avelab3$no_rows = avelab3$no_rows/nrow(avecluster32)

# Clustering is not happening in a way that corresponds with supergroups
# Clustering is based on quality and influence metrics only
# These are what was observed in the association rules EDA

# Final cluster stats

avelabframe = rbind(avelab1, avelab2, avelab3, avelab4)
avelabcount1 = ggplot(data = avelab1, aes(x = SuperGroupLabel, y = no_rows)) +
  geom_bar(stat = "identity") +
  labs(x = "", y = "Cluster 1") + theme_bw() + coord_flip() +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.text.x=element_blank(),
        axis.title.y = element_text(colour="black", face = "bold", size = 10))
avelabcount2 = ggplot(data = avelab2, aes(x = SuperGroupLabel, y = no_rows)) +
  geom_bar(stat = "identity") +
  labs(x = "", y = "Cluster 2") + theme_bw() + coord_flip() +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.text.x=element_blank(),
        axis.title.y = element_text(colour="black", face = "bold", size = 10))
avelabcount3 = ggplot(data = avelab3, aes(x = SuperGroupLabel, y = no_rows)) +
  geom_bar(stat = "identity") +
  labs(x = "", y = "Cluster 3") + theme_bw() + coord_flip() +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.text.x=element_blank(),
        axis.title.y = element_text(colour="black", face = "bold", size = 10))

ggarrange(avelabcount1, avelabcount2, avelabcount3, nrow = 1, ncol = 3)

## Non-hierarchical clustering ##

# Estimate lambda from the data
lam = lambdaest(blankclusterdat, num.method = 2, fac.method = 2)


# Conduct k-prototype clustering with k=4
kprot = kproto(blankclusterdat, 4, lambda = lam, iter.max = 100, nstart = 3, na.rm = TRUE)


# Check contents of clusters
i1 = which(kprot$cluster == 1)
i2 = which(kprot$cluster == 2)
i3 = which(kprot$cluster == 3)
i4 = which(kprot$cluster == 4)


nhclust1 = clusterdat[i1, ]
nhclust1$Cluster = 1
summary(nhclust1)

prop.table(summary(nhclust1$Quartile))
prop.table(summary(nhclust1$`Top 10% (CiteScore Percentile)`))
prop.table(summary(nhclust1$`Open Access`))


nhclust2 = clusterdat[i2, ]
nhclust2$Cluster = 2

prop.table(summary(nhclust2$Quartile))
prop.table(summary(nhclust2$`Top 10% (CiteScore Percentile)`))
prop.table(summary(nhclust2$`Open Access`))


nhclust3 = clusterdat[i3, ]
nhclust3$Cluster = 3

prop.table(summary(nhclust3$Quartile))
prop.table(summary(nhclust3$`Top 10% (CiteScore Percentile)`))
prop.table(summary(nhclust3$`Open Access`))

nhclust4 = clusterdat[i4, ]
nhclust4$Cluster = 4

prop.table(summary(nhclust4$Quartile))
prop.table(summary(nhclust4$`Top 10% (CiteScore Percentile)`))
prop.table(summary(nhclust4$`Open Access`))

# Visualise differences between clusters

nhclusters = rbind(nhclust1, nhclust2, nhclust3, nhclust4)
nhclusters$Cluster = as.factor(nhclusters$Cluster)

nhrank = ggplot(data = nhclusters, aes(x = Cluster, y = RANK, fill = Cluster)) +
  geom_boxplot(width = 0.8, position = position_dodge(), alpha = 0.75)  +
  labs(x = "", y = "Rank within Sub-Subject Area \n") + theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.y = element_text(colour="black", face = "bold", size = 10),
        legend.position = "none")

nhscore = ggplot(data = nhclusters, aes(x = Cluster, y = `CiteScore 2020`, fill = Cluster)) +
  geom_boxplot(width = 0.8, position = position_dodge(), alpha = 0.75)  +
  labs(x = "", y = "CiteScore (2020) \n") + theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.y = element_text(colour="black", face = "bold", size = 10),
        legend.position = "none")

nhcount = ggplot(data = nhclusters, aes(x = Cluster, y = `Citation Count`, fill = Cluster)) +
  geom_point()  +
  labs(x = "", y = "Citation Count \n") + theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.y = element_text(colour="black", face = "bold", size = 10),
        legend.position = "none")

pub1 = nhclust1 %>% 
  group_by(Publisher) %>%
  summarise(no_rows = length(Publisher))
pub1 = pub1[order(pub1$no_rows, decreasing = TRUE), ][1:5, ]
pub1$Cluster = 1
pub1$no_rows = c(5, 4, 3, 2, 1)

pub2 = nhclust2 %>% 
  group_by(Publisher) %>%
  summarise(no_rows = length(Publisher))
pub2 = pub2[order(pub2$no_rows, decreasing = TRUE), ][1:5, ]
pub2$Cluster = 2
pub2$no_rows = c(5, 4, 3, 2, 1)

pub3 = nhclust3 %>% 
  group_by(Publisher) %>%
  summarise(no_rows = length(Publisher))
pub3 = pub3[order(pub3$no_rows, decreasing = TRUE), ][1:5, ]
pub3$Cluster = 3
pub3$no_rows = c(5, 4, 3, 2, 1)


pub4 = nhclust4 %>% 
  group_by(Publisher) %>%
  summarise(no_rows = length(Publisher))
pub4 = pub4[order(pub4$no_rows, decreasing = TRUE), ][1:5, ]
pub4$Cluster = 4
pub4$no_rows = c(5, 4, 3, 2, 1)
pub4$Publisher = as.character(pub4$Publisher)
pub4[2, 1]= "MDPI"
pub4$Publisher = as.factor(pub4$Publisher)

pubframe = rbind(pub1, pub2, pub3, pub4)
pubcount1 = ggplot(data = pub1, aes(x = Publisher, y = no_rows)) +
  geom_bar(stat = "identity") +
  labs(x = "", y = "Cluster 1") + theme_bw() + coord_flip() +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.text.x=element_blank(),
        axis.title.y = element_text(colour="black", face = "bold", size = 10))
pubcount2 = ggplot(data = pub2, aes(x = Publisher, y = no_rows)) +
  geom_bar(stat = "identity") +
  labs(x = "", y = "Cluster 2") + theme_bw() + coord_flip() +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.text.x=element_blank(),
        axis.title.y = element_text(colour="black", face = "bold", size = 10))
pubcount3 = ggplot(data = pub3, aes(x = Publisher, y = no_rows)) +
  geom_bar(stat = "identity") +
  labs(x = "", y = "Cluster 3") + theme_bw() + coord_flip() +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.text.x=element_blank(),
        axis.title.y = element_text(colour="black", face = "bold", size = 10))
pubcount4 = ggplot(data = pub4, aes(x = Publisher, y = no_rows)) +
  geom_bar(stat = "identity") +
  labs(x = "", y = "Cluster 4") + theme_bw() + coord_flip() +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.text.x=element_blank(),
        axis.title.y = element_text(colour="black", face = "bold", size = 10))

ggarrange(pubcount1, pubcount2, nrow = 1, ncol = 2)
ggarrange(pubcount3, pubcount4, nrow = 1, ncol = 2)

# Compare to known labels

# Physical sciences
lab1 = nhclust1 %>% 
  group_by(SuperGroupLabel) %>%
  summarise(no_rows = length(SuperGroupLabel))
lab1 = lab1[order(lab1$no_rows, decreasing = TRUE), ][1:5, ]
lab1$Cluster = 1
lab1$no_rows = lab1$no_rows/nrow(nhclust1)

# Social sciences and physical sciences
lab2 = nhclust2 %>% 
  group_by(SuperGroupLabel) %>%
  summarise(no_rows = length(SuperGroupLabel))
lab2 = lab2[order(lab2$no_rows, decreasing = TRUE), ][1:5, ]
lab2$Cluster = 2
lab2$no_rows = lab2$no_rows/nrow(nhclust2)

# Sciences in general
lab3 = nhclust3 %>% 
  group_by(SuperGroupLabel) %>%
  summarise(no_rows = length(SuperGroupLabel))
lab3 = lab3[order(lab3$no_rows, decreasing = TRUE), ][1:5, ]
lab3$Cluster = 3
lab3$no_rows = lab3$no_rows/nrow(nhclust3)

# Physical sciences
lab4 = nhclust4 %>% 
  group_by(SuperGroupLabel) %>%
  summarise(no_rows = length(SuperGroupLabel))
lab4 = lab4[order(lab4$no_rows, decreasing = TRUE), ][1:5, ]
lab4$Cluster = 4
lab4$no_rows = lab4$no_rows/nrow(nhclust4)

labframe = rbind(lab1, lab2, lab3, lab4)
labcount1 = ggplot(data = lab1, aes(x = SuperGroupLabel, y = no_rows)) +
  geom_bar(stat = "identity") +
  labs(x = "", y = "Cluster 1") + theme_bw() + coord_flip() +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.text.x=element_blank(),
        axis.title.y = element_text(colour="black", face = "bold", size = 10))
labcount2 = ggplot(data = lab2, aes(x = SuperGroupLabel, y = no_rows)) +
  geom_bar(stat = "identity") +
  labs(x = "", y = "Cluster 2") + theme_bw() + coord_flip() +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.text.x=element_blank(),
        axis.title.y = element_text(colour="black", face = "bold", size = 10))
labcount3 = ggplot(data = lab3, aes(x = SuperGroupLabel, y = no_rows)) +
  geom_bar(stat = "identity") +
  labs(x = "", y = "Cluster 3") + theme_bw() + coord_flip() +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.text.x=element_blank(),
        axis.title.y = element_text(colour="black", face = "bold", size = 10))
labcount4 = ggplot(data = lab4, aes(x = SuperGroupLabel, y = no_rows)) +
  geom_bar(stat = "identity") +
  labs(x = "", y = "Cluster 4") + theme_bw() + coord_flip() +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.text.x=element_blank(),
        axis.title.y = element_text(colour="black", face = "bold", size = 10))

ggarrange(labcount1, labcount2, nrow = 1, ncol = 2)
ggarrange(labcount3, labcount4, nrow = 1, ncol = 2)







