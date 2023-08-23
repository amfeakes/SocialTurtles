#!/usr/bin/env Rscript

#Thesis 
#Author: Amy Feakes
#Script: calculate_sri.R
#Description: calulating the SRI using the edge lists 

#Clear workspace
rm(list=ls())

#Dependencies

library(asnipe)
library(igraph)
library(ape)
library(lme4)
library(dplyr)
library(assocInd)

#import edgelist
edges_df <- read.csv("../data/edgelist_apr_jun.csv", header=T)
edges_df <- edges_df[-c(1,5)]

#import encounters df
encounters_df <- read.csv("../data/individaul_apr_jun.csv", header=T)

#### Data prep ####

#empty list to store the results
results <- data.frame(id1 = character(),
                      id2 = character(),
                      block_together = numeric(),
                      id1_only = numeric(),
                      id2_only = numeric(),
                      same_survey_diff_blocks = numeric(),
                      survey_without = numeric())

#loop through each row
for (i in 1:nrow(edges_df)) {
  # get values needed from current db
  input_id1 <- edges_df$ID1[i]
  input_id2 <- edges_df$ID2[i]
  block_weight <- edges_df$block_weight[i]
  
  #results list to create - results[[i]] <- list(input_id1 = input_id1, input_id2 = input_id2, block_weight = block_weight)
  #filter encounter df for two ids
  df_filtered <- encounters_df[encounters_df$ID %in% c(input_id2, input_id1), ]
  #same survey different block 
  same_dive_diff_block <- sum(duplicated(df_filtered$Dive.ID) & !duplicated(df_filtered[c("Dive.ID", "Block")]))
  #id1 without id2 
  id1_in_dive_not_id2 <- (sum(df_filtered$ID == input_id1)) - block_weight - same_dive_diff_block
  #id2 without id1
  id2_in_dive_not_id1 <- (sum(df_filtered$ID == input_id2)) - block_weight - same_dive_diff_block
  #surveys without id1 or id 2
  survey_without <- (76 - block_weight - same_dive_diff_block - id1_in_dive_not_id2 - id2_in_dive_not_id1)
  
  #results list
  results_current_row <- c(input_id1, input_id2, block_weight, id1_in_dive_not_id2,
                           id2_in_dive_not_id1,same_dive_diff_block, survey_without)
  results <- rbind(results, results_current_row)
}

#save as new csv
write.csv(results,"../data/sri_prep.csv")

#import that csv
df<- read.csv("../data/sri_prep.csv", header=T)
df <- df[-c(1)]
colnames(df) <- c("id1", "id2", "block_together", "id1only", "id2only","samesurvey","survey_without")


####Calculating SRI ####
#create new df
sri_df <- data.frame()
#create new df
normal_sri_df <- data.frame()
for ( i in 1:nrow(df)){
  current_row <- SRI(df$block_together[i], df$id1only[i], 
                     df$id2only[i], df$samesurvey[i])
  #calculate normalised SRI
  normal_sri <- (current_row[1]/(df$survey_without[i]/76))
  #add sri to row
  sri_df <- rbind(sri_df, current_row)
  #add noramlised sri to df
  normal_sri_df <- rbind(normal_sri_df, normal_sri)
  
}
colnames(sri_df) <- c("est_association_strength","standard_error")

#combine the two databases
sri_with_id <- cbind(c(1:124),edges_df$ID1, edges_df$ID2, sri_df, normal_sri_df)
write.csv(sri_with_id,"../data/sri_results.csv")
colnames(sri_with_id) <- c("no","id1","id2","est_association_strength","standard_error", "normalised_sri")

n_sri_df <- read.csv("../data/sri_results.csv")
n_sri_df <- n_sri_df[-c(1)]
colnames(n_sri_df) <- c("no","id1","id2","est_association_strength","standard_error", "normalised_sri")


#### Adding attribute data ####
df <- read.csv("../data/sri_results.csv")
df <- df[-c(1)]
colnames(df) <- c("no","id1","id2","est_association_strength","standard_error", "normalised_sri")
id <- read.csv("../data/block_matrix_extras.csv", header=T)

df_w_attribute <- data.frame()

for( i in 1:nrow(df)){
  id1_current <- df$id1[i]
  id2_current <- df$id2[i]
  
  #find these values in id
  filter_id1 <- subset(id, id$ID == id1_current)
  id1_sex <- filter_id1$Sex
  filter_id2 <- subset(id, id$ID == id2_current)
  id2_sex <- filter_id2$Sex
  
  df_w_attribute <- rbind(df_w_attribute, data.frame(id1_current, id2_current, id1_sex, id2_sex, df$est_association_strength[i]))
}

write.csv(df_w_attribute,"../data/sri_w_attribute.csv")
