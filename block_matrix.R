#!/usr/bin/env Rscript

#Thesis 
#Author: Amy Feakes
#Script: block_matrix.R
#Description: Calculating distribution across blocks of each individual with attribute data

#Clear workspace
rm(list=ls())

#Dependencies

#importing data
database <- read.csv("../data/individaul_apr_jun.csv", header=T)

#create an empty matrix 
block_list  <- data.frame(ID = character(),
                          Shallow = numeric(),
                          Turtle = numeric(),
                          Split = numeric(),
                          Yellow = numeric(),
                          Lobster = numeric(),
                          Ridge = numeric(),
                          stringsAsFactors = FALSE)


blocks <- c("Turtle", "Shallow", "Split", "Lobster", "Ridge", "Yellow")
  
  #tally data
tally_df <- data.frame(ID = character(0), stringsAsFactors = FALSE)
  for (block in blocks) {
    tally_df[[block]] <- integer(0)
  }
  
  #loop through each row
  for (i in 1:nrow(database)){
    id <- database$ID[i]
    block <- database$Block[i]
    
    #check if tally is already there
    if (id %in% tally_df$ID) {
      #add to tally
      tally_df[tally_df$ID == id, block] <- tally_df[tally_df$ID == id, block] + 1
    } else {
      #new row 
      new_row <- data.frame(ID = id, stringsAsFactors = FALSE)
      for (block in blocks) {
        new_row[[block]] <- ifelse(block == database$Block[i], 1, 0)
      }
      tally_df <- rbind(tally_df, new_row)
    }
  }
  
  #print
  print(tally_df)
  

#for each row in database - add values of turt-yellow 
  #then rbind this row to tall_df
  
  total <- data.frame(Total_Sightings = numeric())
  
  for(i in 1:nrow(tally_df)){
    total_sighitngs <- sum(tally_df$Turtle[i],tally_df$Shallow[i], tally_df$Split[i], 
        tally_df$Lobster[i], tally_df$Ridge[i], tally_df$Yellow[i])
    total <- rbind(total, total_sighitngs)
  }

#cbind the tally and the total sightings 
  tally_df <- cbind(tally_df, total)
  colnames(tally_df)[8] <- "Total_sightings"
  
#add attribute data 
attribute_df <- data.frame(Size = c(), Sex = c())
  for(i in 1:nrow(tally_df)){
    current_id <- tally_df$ID[i]
    filtered_encounters <- subset(database, database$ID == current_id)
    size_current <- filtered_encounters$Size[1]
    sex_current <- filtered_encounters$Sex[1]
    current_data <- cbind(size_current, sex_current)
    attribute_df <- rbind(attribute_df, current_data)
  }

#add total number of blocks visit as no_blocks 
no_blocks <- data.frame

for(i in 1:nrow(tally_df)){
  tally_df$no_blocks[i] <- sum(tally_df[i,][, c("Turtle", "Shallow", "Split", "Lobster", "Ridge", "Yellow")] > 0)
  
}

#merge in attribute data
tally_w_attr <- cbind(tally_df,attribute_df)

colnames(tally_w_attr)[10] <- "Size"
colnames(tally_w_attr)[11] <- "Sex"

write.csv(tally_w_attr, "../data/block_matrix_extras.csv")
