#!/usr/bin/env Rscript

#Thesis 
#Author: Amy Feakes
#Script: permute_blocks.R
#Description: permutation to test the repeabtility of the social preference 

#Clear workspace
rm(list=ls())

#Dependencies

library(dplyr)
library(assocInd)
library(cowplot)
library(ggplot2)
library(forcats)
library(data.table)


####Original SRI values into a matrix ####

#load data
sridf <- read.csv("../data/sri_results.csv")
sridf <- sridf[-c(1)]
colnames(sridf) <- c("no","id1","id2","est_association_strength","standard_error", "normalised_sri")

#get unique ID values from encounters df
encounters <- read.csv("../data/individaul_apr_jun.csv", header=T)
unique_ids <- unique(encounters$ID)

#empty matrix
assoc_matrix <- matrix("", nrow = length(unique_ids), ncol = length(unique_ids))

colnames(assoc_matrix) <- rownames(assoc_matrix) <- unique_ids
assoc_df <- as.data.frame(assoc_matrix)

print(assoc_df)

#list of all ID's from SRI data
unique_ids <- unique(c(sridf$id1, sridf$id2))
#list of id's from main database
unique_ids_from_database <- unique(encounters$ID)

#combine IDs 
all_unique_ids <- unique(c(unique_ids, unique_ids_from_database))

#matrix
assoc_matrix <- matrix(0, nrow = length(all_unique_ids), ncol = length(all_unique_ids))
rownames(assoc_matrix) <- colnames(assoc_matrix) <- all_unique_ids


#put values of SRI into matrix 
for (i in 1:nrow(sridf)) {
  row_idx1 <- match(sridf$id1[i], all_unique_ids)
  col_idx1 <- match(sridf$id2[i], all_unique_ids)
  
  row_idx2 <- match(sridf$id2[i], all_unique_ids)
  col_idx2 <- match(sridf$id1[i], all_unique_ids)
  
  assoc_matrix[row_idx1, col_idx1] <- sridf$est_association_strength[i]
  assoc_matrix[row_idx2, col_idx2] <- sridf$est_association_strength[i]
}
# Print the association matrix
orginal_vals <- print(assoc_matrix)


#### Running the block permutation ####
#import the data
encounters <- read.csv("../data/individaul_apr_jun.csv", header=T)
matrix_list <- list()
edge_list_list <- list()
unique_ids <- unique(encounters$ID)

#set the number of iterations 
for(i in 1:5000){
#get the encounters list, with ID and block 
#permute the block per Dive ID
  #permutate
  cat("next permutation:", i, "\n")
  set.seed(i)
  shuffled_encounters <- encounters %>%
    group_by(Dive.ID) %>%
    mutate(Block = sample(Block)) %>%
    ungroup()
  
#then from that new encounters list extract the edge list
    database <- shuffled_encounters
    #creating edge list with a new row for each pair!!  
    # empty edge list
    edge_list <- data.frame(ID1 = character(),
                            ID2 = character(),
                            Dive_ID = character(),
                            stringsAsFactors = FALSE)
    
    # each row of database
    for (i in 1:(nrow(database) - 1)) {
      #current row
      current_row <- database[i,]
      
      #check against all other rows
      for (j in (i + 1):nrow(database)) {
        # Get the other row
        other_row <- database[j,]
        
        #check for the same Dive ID and block
        if (!is.na(current_row$Dive.ID) && !is.na(other_row$Dive.ID) &&
            !is.na(current_row$Block) && !is.na(other_row$Block) &&
            current_row$Dive.ID == other_row$Dive.ID && current_row$Block == other_row$Block) {
          
          # check id2 and id1 are different
          if (current_row$ID != other_row$ID) {
            new_row <- data.frame(ID1 = current_row$ID,
                                  ID2 = other_row$ID,
                                  Dive_ID = current_row$Dive.ID,
                                  Block = current_row$Block,
                                  stringsAsFactors = FALSE)
            #add to a new edge list
            edge_list <- rbind(edge_list, new_row)
          }
        }
      }
    }

    edge_list_list <- c(edge_list_list, list(edge_list))
    
#new edge list
    #find a list of all unique dyads from the edge list
    pairs_df <- edge_list %>%
      mutate(pair = paste0(pmin(ID1, ID2), pmax(ID1, ID2))) %>%
      distinct(pair, .keep_all = TRUE) %>%
      select(-pair)
    
    #setting up dataframe 
    pre_sri_vals <- data.frame()
    #for each row in the filtered permuted data
    for(j in 1:nrow(pairs_df)){
      #take the pair of ID of current row
      comb <- c(pairs_df$ID1[j], pairs_df$ID2[j])
      
      #takes rows with Id A and B only
      subset_data <- edge_list %>%
        filter(ID1 == comb[1] | ID2 == comb[1] | ID1 == comb[2]  | ID2 == comb[2])
      
      #calculates number of rows were they are seen as pairs
      paired <- subset_data %>%
        filter((ID1 == comb[1] & ID2 == comb[2]) | (ID1 == comb[2] & ID2 == comb[1]))
      #number of times pair exists in data
      pairs_val <- nrow(paired)
      #list of just ID A
      A_no_B <- subset_data %>%
        filter(ID1 == comb[1]| ID2 == comb[1]) %>% #takes id A in column ID1 and ID2
        filter(!ID1 %in% c(comb[2]) & !ID2 %in% c(comb[2])) %>% #removes times where it is paired with B
        filter(!(Dive_ID %in% paired$Dive_ID)) #removes any other times it is in same Dive ID as pair
      #same applied to ID B
      B_no_A <- subset_data %>%
        filter(ID1 == comb[2] | ID2 == comb[2]) %>%
        filter(!ID1 %in% c(comb[1]) & !ID2 %in% c(comb[1])) %>%
        filter(!(Dive_ID %in% paired$Dive_ID))
      
      #calculating number of Dives ID A is seen on 
      Dive_A <- unique(A_no_B$Dive_ID)
      Dive_B <- unique(B_no_A$Dive_ID)
      
      #number of ID's that are the same between the list - times in same dive but not together
      not_interacting <- length(intersect(Dive_A, Dive_B))
      
      #counts number of times in dive and other isn' there 
      A_withouth_B <- length(Dive_A) - not_interacting
      B_without_A <- length(Dive_B) - not_interacting
      
      #create a row with this data 
      sri_info <- cbind(pairs_val, A_withouth_B, B_without_A, not_interacting)
      
      #add to df with all info of pairs
      pre_sri_vals <- rbind(pre_sri_vals, sri_info)
    }
    
    pairs_sri <- cbind(pairs_df,pre_sri_vals)
    #calculate the SRI values 
    sri_df <- data.frame()
    #create new df
    for ( l in 1:nrow(pairs_sri)){
      current_row <- SRI(pairs_sri$pairs_val[l], pairs_sri$A_withouth_B[l], 
                         pairs_sri$B_without_A[l], pairs_sri$not_interacting[l])
      #add sri to row
      sri_df <- rbind(sri_df, current_row)
    }
    
    #combine the df
    pairs_sri <- cbind(pairs_df, sri_df)
    #lable the column 
    colnames(pairs_sri)[5] <- "sri"
    colnames(pairs_sri)[6] <- "sri_se"
    
    #create new matrix
    assoc_matrix <- matrix("", nrow = length(unique_ids), ncol = length(unique_ids))
    
    colnames(assoc_matrix) <- rownames(assoc_matrix) <- unique_ids
    assoc_df <- as.data.frame(assoc_matrix)
    
    #print(assoc_df)
    
    #list of all ID's from SRI data
    unique_ids <- unique(c(pairs_sri$ID1, pairs_sri$ID2))
    #list of id's from main database
    unique_ids_from_database <- unique(encounters$ID)
    
    #combine IDs 
    all_unique_ids <- unique(c(unique_ids, unique_ids_from_database))
    
    #matrix
    assoc_matrix <- matrix(0, nrow = length(all_unique_ids), ncol = length(all_unique_ids))
    rownames(assoc_matrix) <- colnames(assoc_matrix) <- all_unique_ids
    
    #put values of SRI into matrix 
    for (i in 1:nrow(pairs_sri)) {
      row_idx1 <- match(pairs_sri$ID1[i], all_unique_ids)
      col_idx1 <- match(pairs_sri$ID2[i], all_unique_ids)
      
      row_idx2 <- match(pairs_sri$ID2[i], all_unique_ids)
      col_idx2 <- match(pairs_sri$ID1[i], all_unique_ids)
      
      assoc_matrix[row_idx1, col_idx1] <- pairs_sri$sri[i]
      assoc_matrix[row_idx2, col_idx2] <- pairs_sri$sri[i]
    }
    # Print the association matrix
    
    #save the matrix as a separate one for each permutations 
    matrix_list <- c(matrix_list, list(assoc_matrix))
} #end of big loop 

#saving the files 
saveRDS(edge_list_list, file="../data/edge_list_permutations.RData")
saveRDS(matrix_list, file="../data/matrices_permutations.RData")
edge_list_list <- load("../data/edge_list_permutations.RData")
matrix_list<- load("../data/matrices_permutations.RData")

#### Calculating from permutations ####
#matrix_list - the list of all permutations

#### old method ####
#mean SRI from permutations 
mean_matrix <- Reduce("+", matrix_list) / length(matrix_list)
write.csv(mean_matrix, "../data/permutation_means.csv")

print(mean_matrix)

ci_matrix_lower <- matrix(NA, nrow = nrow(mean_matrix), ncol = ncol(mean_matrix))
ci_matrix_upper <- matrix(NA, nrow = nrow(mean_matrix), ncol = ncol(mean_matrix))

#round 2 of calculating CI 
for (i in 1:nrow(mean_matrix)) {
  for (j in 1:ncol(mean_matrix)) {
    ci <- t.test(sapply(matrix_list, function(mat) mat[i, j]))$conf.int
    ci_matrix_lower[i, j] <- ci[1]
    ci_matrix_upper[i, j] <- ci[2]
  }
}


# Calculate 95% CI for each cell and store in the original format
for (i in 1:nrow(mean_matrix)) {
  for (j in 1:ncol(mean_matrix)) {
    ci <- t.test(sapply(matrix_list, function(mat) mat[i, j]))$conf.int
    ci_matrix_lower[i, j] <- ci[1]
    ci_matrix_upper[i, j] <- ci[2]
  }
}

# Add row and column names to the matrices
row_names <- rownames(mean_matrix)
col_names <- colnames(mean_matrix)
rownames(ci_matrix_lower) <- row_names
colnames(ci_matrix_lower) <- col_names
rownames(ci_matrix_upper) <- row_names
colnames(ci_matrix_upper) <- col_names

# Print or manipulate ci_matrix_lower and ci_matrix_upper as needed
print(ci_matrix_lower)
print(ci_matrix_upper)

#put in table format
#orignal matrix
matrix1 <- orginal_vals
matrix2 <- ci_matrix_lower
matrix3 <- ci_matrix_upper
matrix4 <- mean_matrix

data_frame1 <- data.frame(
  X = rep(rownames(matrix1), each = ncol(matrix1)),
  Y = rep(colnames(matrix1), times = nrow(matrix1)),
  Value = as.vector(matrix1)
)

data_frame2 <- data.frame(
  X = rep(rownames(matrix2), each = ncol(matrix2)),
  Y = rep(colnames(matrix2), times = nrow(matrix2)),
  Value = as.vector(matrix2)
)

data_frame3 <- data.frame(
  X = rep(rownames(matrix3), each = ncol(matrix3)),
  Y = rep(colnames(matrix3), times = nrow(matrix3)),
  Value = as.vector(matrix3)
)

data_frame4 <- data.frame(
  X = rep(rownames(matrix4), each = ncol(matrix4)),
  Y = rep(colnames(matrix4), times = nrow(matrix4)),
  Value = as.vector(matrix4)
)

# Combine data frames
combined_data <- cbind(data_frame1, data_frame2[3], data_frame3[3], data_frame4[3])

# Create a data table
result_table <- data.table(combined_data)
 
#### ####

#the resulting table
print(result_table)
colnames(result_table) <- c("ID1", "ID2","original_sri", "ci_lower", "ci_upper", "mean_perm")
write.csv()
#lets remove all pairs which didn't happen 
filtered_results <- result_table[result_table$original_sri > 0, ]
#remove relpicas
results_final <- filtered_results %>%
  mutate(pair = paste0(pmin(ID1, ID2), pmax(ID1, ID2))) %>%
  distinct(pair, .keep_all = TRUE) %>%
  select(-pair)

#outcome for significant or not
outcomes <- data.frame()

print(results_final)

#### ####



####create flattened matrices ####
flattened_matrices <- data.frame()

matrices_perm <- readRDS("../data/matrices_perm.rds")

# loop to flatten each permutation matrix 
for (i in 1:5000) {
  matrix_i <- matrices_perm[[i]]
  
  flattened_values <- as.vector(matrix_i)
  
  #df for the current permutation in loop 
  perm_df <- data.frame(matrix_row = rep(rownames(matrix_i), each = ncol(matrix_i)),
                        matrix_col = rep(colnames(matrix_i), times = nrow(matrix_i)),
                        value = flattened_values,
                        permutation = i)
  
  #add to results table
  flattened_matrices <- rbind(flattened_matrices, perm_df)
}

print(flattened_matrices)
write.csv(flattened_matrices, "../data/flattened_matrices.csv")
flattened_matrices <- read.csv("../data/flattened_matrices.csv")

####Working with observed pairs ####
#output is flattened_matrices

#create a list of pairs which we are interested in, from SRI df
unique_combinations <- sridf[, c("id1", "id2","est_association_strength")]

#list to store subsets 
subsets <- list()

for (i in 1:nrow(unique_combinations)) {
  current_combination <- unique_combinations[i, ]
  
  # Find rows that match the current combination in either order
  current_subset <- flattened_matrices[
    (flattened_matrices$matrix_row == current_combination$id1 & flattened_matrices$matrix_col == current_combination$id2) |
      (flattened_matrices$matrix_row == current_combination$id2 & flattened_matrices$matrix_col == current_combination$id1), ]
  
  filtered_subset <- current_subset %>%
    group_by(permutation) %>%
    slice(1) %>%
    ungroup()
  
  subsets[[i]] <- filtered_subset
}

subsets[[i]]
saveRDS(subsets, "../data/subestted_pairs")
subsets <- readRDS( "../data/subestted_pairs")
# set confidence levels 
confidence_level <- 0.95

#set up new datafram with info on pairs
perm_ci <- data.frame(matrix_row = character(0), matrix_col = character(0), 
                      mean_value = numeric(0), std_dev = numeric(0), 
                      lower_bound = numeric(0), upper_bound = numeric(0),
                      est_association_strength = numeric(0),
                      significance = character(0), dist_from_mean = numeric(0))

#loop through each perm subset to find ci
for (subset_df in subsets) {
  matrix_row <- subset_df$matrix_row[1]  
  matrix_col <- subset_df$matrix_col[1] 
  mean_value <- mean(subset_df$value)
  std_dev <- sd(subset_df$value)
  z_score <- qnorm((1 + confidence_level) / 2)
  margin_of_error <- z_score * (std_dev / sqrt(nrow(subset_df)))
  
  lower_bound <- mean_value - margin_of_error
  upper_bound <- mean_value + margin_of_error
  
  #get original sri from previous dataframe 
  est_association_strength <- unique_combinations[
    (unique_combinations$id1 == matrix_row & unique_combinations$id2 == matrix_col) |
      (unique_combinations$id1 == matrix_col & unique_combinations$id2 == matrix_row), 
    "est_association_strength"]
  
  #look at significance - from confidence intervals 
  significance <- ifelse(est_association_strength < lower_bound, "less",
                         ifelse(est_association_strength > upper_bound, "more", "between"))
  
  #calculate difference from original to mean from permutation
  dist_from_mean <- est_association_strength - mean_value
  
  #form in a row and add row to dataframe
  new_row <- data.frame(matrix_row = matrix_row, matrix_col = matrix_col,
                        mean_value = mean_value, std_dev = std_dev,
                        lower_bound = lower_bound, upper_bound = upper_bound,
                        est_association_strength = est_association_strength,
                        significance = significance, dist_from_mean = dist_from_mean)
  perm_ci <- rbind(perm_ci, new_row)
}

write.csv(perm_ci, "../data/pairs_data.csv")
pairs_data <- read.csv("../data/pairs_data.csv")

table(pairs_data$significance)
#more 79, less 42, between 3. 

####Plotting examples of pair with bounds ####

#filter to pick one of each example 
palette <- c("#BDD5EA", "#8BBB9E", "#064C74", "#2784B9", "#8B5D33", "#3F6C51")

example1 <- pairs_data[c(80),]
example1_sri <- data.frame(matrix_row = example1$matrix_row, mean_val = example1$est_association_strength)
example2 <- pairs_data[c(61),]
example2_sri <- data.frame(matrix_row = example2$matrix_row, mean_val = example2$est_association_strength)
example3 <- pairs_data[c(6),]
example3_sri <- data.frame(matrix_row = example3$matrix_row, mean_val = example3$est_association_strength)


error_plot1 <- ggplot(example1, aes(x = matrix_row, y = as.numeric(mean_value))) +
  geom_errorbar(aes(ymin = as.numeric(lower_bound), ymax = as.numeric(upper_bound), width = 0.2)) +
  geom_point(color = palette[2]) +
  geom_point(data = example1_sri, aes(x = matrix_row, y = mean_val), color = palette[4], size = 3) +
  labs(x = "G387 - G172", y = "SRI ") +
  theme_minimal() + 
  theme(axis.text.x = element_blank())+
  coord_cartesian(ylim = c(0.035, 0.05))

error_plot2 <- ggplot(example2, aes(x = matrix_row, y = as.numeric(mean_value))) +
  geom_errorbar(aes(ymin = as.numeric(lower_bound), ymax = as.numeric(upper_bound), width = 0.2)) +
  geom_point(color = palette[2]) +
  geom_point(data = example2_sri, aes(x = matrix_row, y = mean_val), color = palette[4], size = 3) +
  labs(x = "G43 - G172", y = "SRI ") +
  theme_minimal() + 
  theme(axis.text.x = element_blank())+
  coord_cartesian(ylim = c(0.035, 0.045))

error_plot3 <- ggplot(example3, aes(x = matrix_row, y = as.numeric(mean_value))) +
  geom_errorbar(aes(ymin = as.numeric(lower_bound), ymax = as.numeric(upper_bound), width = 0.2)) +
  geom_point(color = palette[2]) +
  geom_point(data = example3_sri, aes(x = matrix_row, y = mean_val), color = palette[4], size = 3) +
  labs(x = "G08 - G240", y = "SRI ") +
  theme_minimal() + 
  theme(axis.text.x = element_blank())+
  coord_cartesian(ylim = c(0.025, 0.04))


####plotting the attributes to pair frequency ####
sri_w_a <- read.csv("../data/sri_w_attribute")


sri_w_a$combination <- ifelse(sri_w_a$id1_sex < sri_w_a$id2_sex,
                              paste(sri_w_a$id1_sex, sri_w_a$id2_sex, sep = "-"),
                              paste(sri_w_a$id2_sex, sri_w_a$id1_sex, sep = "-"))

colnames(sri_w_a) <- c("no","id1","id2", "id1_sex","id2_sex","est_association_strength","combination")

# Count normalized combinations
combinations_sexes <- table(sri_w_a$combination)
combinations_sexes <- as.data.frame(combinations_sexes)

palette <- c("#BDD5EA", "#8BBB9E", "#064C74", "#2784B9", "#8B5D33", "#3F6C51")

sig_pairs <- cbind(pairs_data, sri_w_a$combination)
colnames(sig_pairs)[11] <- "combination"

combination_table <- table(sig_pairs$combination, sig_pairs$significance)
comb_df <- as.data.frame(combination_table)

plot_1 <- ggplot(comb_df, aes(x = Var1, y = Freq, fill = Var2 )) +
  geom_bar(stat = "identity", position = "stack", width = 0.7) +
  labs(x = "Pair type", y = "Frequency") +
  scale_fill_manual(values = c(palette[3],palette[2],palette[4]), 
                    labels = c("Between Bound", "Below Bound", "Above Bound"), 
                    breaks = c("between", "less", "more")) +
  theme_minimal() +
  guides(fill = guide_legend(title = "Repeatability of Preference")) +
  coord_cartesian(ylim = c(0, 45)) +
  theme(legend.position = "bottom")



#plot with stacked assoications of the results of repleabilty permutation 


####plotting significant to pairs#####
sig_pairs <- cbind(pairs_data, sri_w_a$combination)
colnames(sig_pairs)[11] <- "combination"

combination_table <- table(sig_pairs$combination, sig_pairs$significance)
comb_df <- as.data.frame(combination_table)

plotC <- ggplot(comb_df, aes(x = factor(Var1), y=Freq,fill=Var2)) +
  geom_bar(position = "dodge", stat="identity") +
  scale_y_continuous(labels = scales::comma_format(scale = 1), breaks = scales::pretty_breaks(n = 10)) +
  labs(x = "Pair type", y = "Frequency", fill = "Significance levels") +
  guides(fill = guide_legend(title = "SRI Value")) +
  theme_minimal() +
  scale_fill_manual(values = c("less" = palette[2], "between" = palette[3], "more" = palette[4]),
                    labels = c("Between Bound", "Below Bound", "Above Bounds"), 
                    breaks = c("between", "less", "more"),)

####Bar plot of SRI ####
str(sig_pairs)

#subgroup meanSRI from permutation and orginngal sri 

original_sri <- data.frame(Combination = sig_pairs$combination, SRI = sig_pairs$est_association_strength, Value = "original" )
perm_sri <- data.frame(Combination = sig_pairs$combination, SRI = sig_pairs$mean_value, Value = "perm" )
sri_subgroups <- rbind(original_sri, perm_sri)

value_labels <- c("perm" = "Mean SRI from permutations", "original" = "Observed SRI values")

plot_box <- ggplot(sri_subgroups, aes(x = Combination, y = SRI, fill = Value)) +
  geom_boxplot() +
  labs(x = "Combination", y = "SRI Value") +
  scale_fill_manual(values = c("perm" = palette[1], "original" = palette[2]),
                    labels = value_labels) +  # Use the labels argument
  theme_minimal() +
  guides(fill = guide_legend(title = "SRI Value")) +
  theme(legend.text = element_text(size=10), legend.position = "bottom")+
  ylim(0, 0.2)  # Set y-axis limits



####Plotting grids####
#plots are 
#plot c is boung 
#row 1 and plot 1 
row1 <- plot_grid(plot_1)
row2 <- plot_grid(error_plot1,error_plot2,error_plot3, ncol=3)
row3 <- plot_grid(plot_box)

row1_labeled <- row1 +
  annotate("text", x = 0.01, y = 0.98, label = "a)", size = 4, hjust = 0, vjust = 1) 
row2_labeled <- row2 +
  annotate("text", x = 0.015, y = 0.98, label = "b)", size = 4, hjust = 0, vjust = 1)
row3_labeled <- row3 +
  annotate("text", x = 0.015, y = 0.98, label = "c)", size = 4, hjust = 0, vjust = 1)


panel_plot5 <- plot_grid(row1_labeled,row2_labeled, row3_labeled, ncol=1, rel_heights = c(2,2.5,2.5))
print(panel_plot5)

panel_plot5 <- panel_plot5 +   theme(
  plot.background = element_rect(fill = "white", color = "white", size = 2),
  plot.margin = margin(20, 20, 20, 20),
  panel.spacing = unit(1.5, "lines")
)

ggsave("../results/figure5.png", plot = panel_plot5, width = 8.27, height = 9.96, units = "in")



