#!/usr/bin/env Rscript

#Thesis 
#Author: Amy Feakes
#Script: MCMC_glmm_models.R
#Description: using MCMCglmm to understand life-stage and sex varitaions in degree

#Clear workspace
rm(list=ls())

#Dependencies
library(igraph)
library(dplyr)
library(tidyr)
library(ggplot2)
library(asnipe)
library(MCMCglmm)


#use the encounters db 
df <- read.csv("../data/individaul_apr_jun.csv", header=T)

#test by filtering for first HC001 
#then to do it we can loop by each unique(df$Dive.ID)

#create empty df for results 
df_2 <- data.frame()
#create a list of all the possible dive ID's
all_group_labels <- paste0("HC", sprintf("%03d", 1:76))

#loop through each Dive ID
for(i in all_group_labels){
  current_dive_id_df <- subset(df, Dive.ID == i)
  #adding a column for degree
  current_dive_id_df <- current_dive_id_df %>%
    mutate(degree_val = 0)
  print(current_dive_id_df)
  #make the dive into an edgelist 
  
  #### EDGELIST ROUND #### 
  edge_list <- data.frame(ID1 = character(),
                          ID2 = character(),
                          block_weight = numeric(),
                          weight_2m = numeric(),
                          stringsAsFactors = FALSE)
  
  # loop through each row in the database
  for (i in 1:nrow(current_dive_id_df)) {
    #  the current row
    current_row <- current_dive_id_df[i, ]
    # print("current_row")
    # print(current_row)
    # check against every other row
    for (j in (i + 1):nrow(current_dive_id_df)) {
      # Get the other row
      other_row <- current_dive_id_df[j, ]
      # print("OTHER ROW")
      # print(other_row)
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
            #  new row in the edge list
            new_row <- data.frame(ID1 = current_row$ID,
                                  ID2 = other_row$ID,
                                  block_weight = 1,
                                  weight_2m = 0,
                                  stringsAsFactors = FALSE)
            # print("creating new row")
            # print(new_row)
            # add the new row to the edge list
            edge_list <- rbind(edge_list, new_row)
            print(edge_list)
          }
        }
      }
    }
  }
  
  if (nrow(edge_list) < 1) {
    print("No edges, moving onto the next dive ID")
    print(current_dive_id_df$Dive.ID)
    # Add your other commands here
  } else {
    #### CALCULATE DEGREE CODE ####
    #calculate degrees from the edgelist
    dive_id_edge <- edge_list%>%select(ID1, ID2, block_weight)%>%
      group_by(ID1,ID2)%>%
      expand(edge=c(1:block_weight))%>%select(-edge)
    #
    dive_id_net <- graph_from_edgelist(dive_id_edge%>%as.matrix(),directed = FALSE)
    dive_id_net
    degree_dive_id <- degree(dive_id_net)
    print(degree_dive_id)
    #add the degree value to the coloumn in the df 
    
    #take the names from the degree table
    degree_names <- names(degree_dive_id)
    
    #
    for (i in seq_along(degree_names)) {
      name <- degree_names[i]
      value <- degree_dive_id[[i]]  #degree value of current ID 
      #find the row in current df where ID == name 
      # Update the 'df$degree_val' column for the corresponding row where 'df$ID' matches the name
      current_dive_id_df$degree_val[current_dive_id_df$ID == name] <- value
    }
    #now take the updated df and add to a new one 
    df_2 <- rbind(df_2, current_dive_id_df)
  }
  
}

##This is all corect, lets save the output
write.csv(df_2, "../data/degree_per_enounter.csv")
hist(df_2$degree_val)


##This is all corect, lets save the output
write.csv(df_2, "../data/degree_per_enounter.csv")
df_2 <- read.csv("../data/degree_per_enounter.csv")


#### ALL SEXES AND LIFE STAGES ####

m1 <- MCMCglmm(degree_val ~ Sex, random=~ID + Dive.ID + Block,
               data=df_2)
plot(m1$Sol) 
summary(m1) #non significant fixed effects
hist(mcmc(m1$VCV)[,"ID"]) #non significant
hist(mcmc(m1$VCV)[,"Dive.ID"]) #significant 
hist(mcmc(m1$VCV)[,"Block"]) #non significant 

m2 <- MCMCglmm(degree_val ~ Sex, random=~ID, data=df_2, burnin=5000, nitt=20000)
plot(m2$Sol) #autocorrelation ish
autocorr(m2$Sol) #good
summary(m2) # significant fixed effects of male 
hist(mcmc(m2$VCV)[,"ID"]) #significant 

#post.mean l-95% CI u-95% CI eff.samp
#(Intercept)   1.21503  1.03072  1.40044     1500
#SexJuvenile  -0.04325 -0.34779  0.26307     1500
#SexMale       0.52763  0.13537  0.88170     1346
#pMCMC    
#(Intercept) <7e-04 ***
#  SexJuvenile 0.7653    
#SexMale     0.0107 *  
set.seed(1234)
m3 <- MCMCglmm(degree_val ~ Sex, random=~Dive.ID, data=df_2, burnin=5000, nitt=20000)
plot(m3$Sol) #autocorrelation ish
autocorr(m3$Sol) #all good less that 0.01
summary(m3) # significant fixed effects of male 
hist(mcmc(m3$VCV)[,"Dive.ID"]) #significant, not tending to 0 

#pick this 
#post.mean l-95% CI u-95% CI eff.samp
#(Intercept)   1.17767  0.95433  1.38526     2072
#SexJuvenile  -0.03511 -0.29327  0.24201     1500
#SexMale       0.34708  0.03738  0.70904     1500
#pMCMC    
#(Intercept) <7e-04 ***
#  SexJuvenile 0.7880    
#SexMale     0.0413 *  
#post.mean l-95% CI u-95% CI eff.samp
#Dive.ID    0.3039    0.133   0.5076     1440

###more iterations 
set.seed(1234)
m3 <- MCMCglmm(degree_val ~ Sex, random=~Dive.ID, data=df_2, burnin=10000, nitt=250000)
plot(m3$Sol) #autocorrelation ish
autocorr(m3$Sol) #all good less that 0.01
summary(m3) # significant fixed effects of male 
hist(mcmc(m3$VCV)[,"Dive.ID"]) #significant, not tending to 0 

#
#post.mean  l-95% CI  u-95% CI eff.samp
#(Intercept)  1.177361  0.956819  1.394780    24662
#SexJuvenile -0.032441 -0.301269  0.228640    23506
#SexMale      0.346718  0.001614  0.688018    23514
#pMCMC    
#(Intercept) <4e-05 ***
#  SexJuvenile 0.8133    
#SexMale     0.0466 *  

m4 <- MCMCglmm(degree_val ~ Sex, random=~Dive.ID+ID, data=df_2)
plot(m4$Sol) #autocorrelation, best yet
summary(m4) # non significant fixed effects of male 
hist(mcmc(m4$VCV)[,"Dive.ID"]) #significant
hist(mcmc(m4$VCV)[,"ID"]) #non significant


#### ADULT JUVENILE LIFE STAGE ####
#loop through 
life_stage <- data.frame()
for(i in 1:nrow(df_2)){
  if(df_2$Sex[i] == "Male"|| df_2$Sex[i] == "Female"){
    current_life <- "Adult"
  }
  else{
    current_life <- "Juvenile"
  }
  life_stage <- rbind(life_stage, current_life)
}

colnames(life_stage) <- c("life_stage")
ls_df <- cbind(df_2,life_stage)

m2.1 <- MCMCglmm(degree_val ~ life_stage, random=~ID, data=ls_df)
plot(m2.1$Sol) #autocorrelation,  good
autocorr(m2.1$Sol) #good
summary(m2.1) # non significant fixed effects of juvenile
hist(mcmc(m2.1$VCV)[,"ID"]) # significant

m2.2 <- MCMCglmm(degree_val ~ life_stage, random=~Dive.ID, data=ls_df, burnin=5000, nitt =25000)
plot(m2.2$Sol) #autocorrelation 
autocorr(m2.2$Sol) #good
summary(m2.2) # non significant fixed effects of juveniles 
hist(mcmc(m2.2$VCV)[,"Dive.ID"]) #significant

m2.3 <- MCMCglmm(degree_val ~ life_stage, random=~ID+Dive.ID, data=ls_df)
plot(m2.3$Sol) #autocorrelation
autocorr(m2.3$Sol)
summary(m2.3) # non significant fixed effects of juveniles 
hist(mcmc(m4$VCV)[,"Dive.ID"]) #significant
hist(mcmc(m4$VCV)[,"ID"]) #non significant


#### MALE OR FEMALE (ADULTS) ####

#subset database with just adults 
adult_df <- subset(ls_df, life_stage =="Adult")

m3.1 <- MCMCglmm(degree_val ~ Sex, random=~ID, data=adult_df)
plot(m3.1$Sol) #autocorrelation
autocorr(m3.1$Sol) #good 
summary(m3.1) # non significant fixed effects of males 
hist(mcmc(m3.1$VCV)[,"ID"]) #non significant
set.seed(1234)
m3.2 <- MCMCglmm(degree_val ~ Sex, random=~Dive.ID, data=adult_df, burnin=5000, nitt=25000)
plot(m3.2$Sol) #autocorrelation
autocorr(m3.2$Sol) #good 
summary(m3.2) #  significant fixed effects of males 
hist(mcmc(m3.2$VCV)[,"Dive.ID"]) # significant

#post.mean l-95% CI u-95% CI eff.samp
#(Intercept)  1.187065 0.969330 1.386083     2000
#SexMale      0.363690 0.001251 0.713831     1743
#pMCMC    
#(Intercept) <5e-04 ***
#  SexMale      0.044 *  

#same but more iterations 

set.seed(1234)
m3.2 <- MCMCglmm(degree_val ~ Sex, random=~Dive.ID, data=adult_df, burnin=10000, nitt=250000)
plot(m3.2$Sol) #autocorrelation
autocorr(m3.2$Sol) #good 
summary(m3.2) #  significant fixed effects of males 
hist(mcmc(m3.2$VCV)[,"Dive.ID"]) # significant
#this is better 
#post.mean l-95% CI u-95% CI eff.samp
#(Intercept)   1.18410  0.96703  1.40708    24000
#SexMale       0.37357  0.01238  0.74508    24000
#pMCMC    
#(Intercept) <4e-05 ***
#  SexMale     0.0463 *  

#post.mean l-95% CI u-95% CI eff.samp
#Dive.ID     0.2678  0.08099   0.4739    20558
#DIC: 571.4227


m3.3 <- MCMCglmm(degree_val ~ Sex, random=~Dive.ID + ID, data=adult_df, burnin=5000, nitt = 25000)
plot(m3.3$Sol) #autocorrelation
autocorr(m3.3$Sol) #good 
summary(m3.3) # non significant fixed effects of males 
hist(mcmc(m3.3$VCV)[,"Dive.ID"]) # significant
hist(mcmc(m3.3$VCV)[,"Block"]) # non significant

