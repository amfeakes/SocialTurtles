#!/usr/bin/env Rscript

#Thesis 
#Author: Amy Feakes
#Script: calculate_sri.R
#Description: calulating the SRI using the edge lists 

#Clear workspace
rm(list=ls())

#Dependencies
- 


database <- read.csv("../data/individaul_apr_jun.csv", header=T)
#database <- database[-c(2,3,8,9)]
str(database)

#database <- hc001_df
# create an empty edge list
edge_list <- data.frame(ID1 = character(),
                        ID2 = character(),
                        block_weight = numeric(),
                        weight_2m = numeric(),
                        stringsAsFactors = FALSE)

# loop through each row
for (i in 1:nrow(database)) {
  # current row
  current_row <- database[i, ]
  # print("current_row")
  # print(current_row)
  # check against every other row
  for (j in (i + 1):nrow(database)) {
    # get other row
    other_row <- database[j, ]
    # print("OTHER ROW")
    # print(other_row)
    print(current_row$Dive.ID)
    print(other_row$Dive.ID)
    print(current_row$Block)
    print(other_row$Block)
    # check  DiveID and Block are not missing and are the same
    if (!is.na(current_row$Dive.ID) && !is.na(other_row$Dive.ID) &&
        !is.na(current_row$Block) && !is.na(other_row$Block) &&
        current_row$Dive.ID == other_row$Dive.ID && current_row$Block == other_row$Block) {
      
      # check  ID1 and ID2 are different
      if (current_row$ID != other_row$ID) {
        # check  there is already a row in the edge list with the same IDs
        existing_row <- edge_list[(edge_list$ID1 == current_row$ID & edge_list$ID2 == other_row$ID) |
                                    (edge_list$ID1 == other_row$ID & edge_list$ID2 == current_row$ID), ]
        
        if (nrow(existing_row) == 0) {
          # new row in the edge list
          new_row <- data.frame(ID1 = current_row$ID,
                                ID2 = other_row$ID,
                                block_weight = 1,
                                weight_2m = 0,
                                stringsAsFactors = FALSE)
          # print("creating new row")
          # print(new_row)
          # add the new row to the edge list
          edge_list <- rbind(edge_list, new_row)
        } else {
          # add 1 to the block_weight of the existing row
          # print("Exisiting row:", existing_row)
          existing_row$block_weight <- existing_row$block_weight + 1
          # print("added to exisiting row")
          # print(existing_row)
          #updatr edge list 
          #edge_list[edge_list$ID1 == existing_row$ID1 & edge_list$ID2 == existing_row$ID.2, "block_weight"] <- existing_row$block_weight
          existing_row_index <- which(edge_list$ID1 == existing_row$ID1 & edge_list$ID2 == existing_row$ID2)
          edge_list$block_weight[existing_row_index] <- existing_row$block_weight
        }
      }
    }
  }
  
  # check ID2 column has a value
  if (!is.na(current_row$ID.2) && current_row$ID.2 != "" && current_row$ID != current_row$ID.2) {
    # print("there is a value in ID2")
    # print(current_row)
    # Find or create a row in the edge list with ID and ID2 values
    existing_row <- edge_list[(edge_list$ID1 == current_row$ID & edge_list$ID2 == current_row$ID.2) |
                                (edge_list$ID1 == current_row$ID.2 & edge_list$ID2 == current_row$ID), ]
    
    if (nrow(existing_row) == 0) {
      # create a new row in the edge list
      new_row <- data.frame(ID1 = current_row$ID,
                            ID2 = current_row$ID.2,
                            block_weight = 0,
                            weight_2m = 0.5,
                            stringsAsFactors = FALSE)
      print("new_row")
      # add the new row to the edge list
      edge_list <- rbind(edge_list, new_row)
    } else {
      # add 1 to the weight_2m of the existing row
      print("exisiting row before additon")
      # print(existing_row)
      existing_row$weight_2m <- existing_row$weight_2m + 0.5
      #here it is not put into the edge list maybe? 
      existing_row_index <- which(edge_list$ID1 == existing_row$ID1 & edge_list$ID2 == existing_row$ID2)
      edge_list$weight_2m[existing_row_index] <- existing_row$weight_2m
      #edge_list[edge_list$ID1 == existing_row$ID1 & edge_list$ID2 == existing_row$ID2, "weight_2m"] <- existing_row$weight_2m

      # print("exisiting row after additon")
      # print(existing_row)
      # print("edge list to checka about adition")
      # print(edge_list)
    }
  }
  
  # check if ID2 column has a value
  if (!is.na(current_row$ID.3) && current_row$ID.3 != "" && current_row$ID != current_row$ID.3) {
    # print("there is a value in ID2")
    # print(current_row)
    # find or create a row in the edge list with ID and ID2 values
    existing_row <- edge_list[(edge_list$ID1 == current_row$ID & edge_list$ID2 == current_row$ID.3) |
                                (edge_list$ID1 == current_row$ID.3 & edge_list$ID2 == current_row$ID), ]
    
    if (nrow(existing_row) == 0) {
      # new row in the edge list
      new_row <- data.frame(ID1 = current_row$ID,
                            ID2 = current_row$ID.3,
                            block_weight = 0,
                            weight_2m = 0.5,
                            stringsAsFactors = FALSE)
      print("new_row")
      # add the new row to the edge list
      edge_list <- rbind(edge_list, new_row)
    } else {
      # add 1 to the weight_2m of the existing row
      print("exisiting row before additon")
      # print(existing_row)
      existing_row$weight_2m <- existing_row$weight_2m + 0.5
      #here it is not put into the edge list maybe? 
      existing_row_index <- which(edge_list$ID1 == existing_row$ID1 & edge_list$ID2 == existing_row$ID2)
      edge_list$weight_2m[existing_row_index] <- existing_row$weight_2m
    }
  }
}

# print the resulting edge list
print(edge_list)

#next steps, add columns for different dates and for location

write.csv(edge_list,file='../data/edgelist_apr_jun.csv')
edges_ <- read.csv('../data/edgelist_apr_jun.csv')

#create edge list for just 2m_weights 
#edgelist_2m <- subset(edge_list, weight_2m == 1)
#write.csv(edgelist_2m,file='../data/edgelist2m_apr_jun.csv')

