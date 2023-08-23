#!/usr/bin/env Rscript

#Thesis 
#Author: Amy Feakes
#Script: group_size.R
#Description: extract size of groups and frequency from database and plot it 

#Clear workspace
rm(list=ls())

#Dependencies
library(dplyr)
library(cowplot)
library(ggplot2)

#importing the database
encounters <- read.csv("../data/individaul_apr_jun.csv", header=T)

#number of groups across 76 surveys 
length(unique(encounters$Group))
#210 different groups

#table of frequnecies 
groupsize <-table(encounters$Group)

table(groupsize == 1)
table(groupsize==2)
table(groupsize==3)
table(groupsize==4)
table(groupsize==5)
table(groupsize==6)

#setting up a dataframe using information from the table
groupsize_table <- data.frame( group_size = c(2,3,4,5,6),
                               quantity = c(60,16,7,1,1))

#plotting the data frame 
palette <- c("#BDD5EA", "#8BBB9E", "#064C74", "#2784B9", "#8B5D33", "#3F6C51")

plot1 <-ggplot(groupsize_table, aes(x = factor(group_size), y = quantity, fill = factor(group_size))) +
  geom_bar(stat = "identity", fill=palette[2]) +
  labs(x = "Group Size", y = "Frequency of Groups") +
  scale_fill_discrete(name = "Group Size") +
  theme_minimal()+
  scale_x_discrete(labels = c("2", "3", "4", "5", "6"),
  breaks = factor(2:6))+
  geom_text(aes(label = quantity), vjust = -0.3)+
  coord_cartesian(ylim = c(0,65))

##looking at alone and grouping 
group_size <- c()
for(i in 1:nrow(encounters)){
  current_dive <- encounters$Dive.ID[i]
  current_block <- encounters$Block[i]
  number_in_group <- 0
  filtered_encounters <- encounters %>%
    filter(row_number() != i)
  for(j in 1:nrow(filtered_encounters)){
    if(filtered_encounters$Dive.ID[j] == current_dive &
       filtered_encounters$Block[j] == current_block){
      number_in_group <- number_in_group + 1
    }
  }
  
  group_size <- rbind(group_size, number_in_group)
}

#add this to the database 
encounters$grouped <- group_size

#create a new database
unique_ids <- unique(encounters$ID)
#results df
id_preference <- data.frame()  

# loop through each unique ID
for (id in 1:length(unique_ids)) {
  id_value <- unique_ids[id]
  id_data <- encounters %>%
    filter(ID == id_value)  # filter for row of current id
  total_count <- nrow(id_data)
  alone <- sum(id_data$grouped[,1] == 0)
  together <- sum(id_data$grouped[,1] > 0)

  # row of stats for specific id
  id_results <- data.frame(ID = id_value, Size = id_data$Size[1], Sex= id_data$Sex[1], Encounters = total_count,
                           Alone = alone,
                           Group = together)
  
  #add to dataframe
  id_preference <- rbind(id_preference, id_results)
}

# print the results
print(id_preference)

#now plot this 
palette <- c("#BDD5EA", "#8BBB9E", "#064C74", "#2784B9", "#8B5D33", "#3F6C51")
filtered_preference <- id_preference %>%
    filter(Encounters > 2)
sight_values <- c(-20, -15, -10, -5, 0 ,5, 10, 15,20)
female <- filtered_preference %>%
  filter(Sex == "Female")
plot_f <- ggplot(female, aes(y = ID)) +
  geom_bar(aes(x = Group, fill = "Group"), position = "dodge", stat = "identity") +
  geom_bar(aes(x = -Alone, fill = "Alone"), position = "dodge", stat = "identity") +
  scale_fill_manual(values = c("Group" = palette[1], "Alone" = palette[3]),
                    guide = guide_legend(title = NULL)) +
  labs(x = "Count", y = "ID") +
  scale_x_continuous(breaks=sight_values,
                     labels = abs(sight_values))+
  theme_minimal() +
  theme(legend.position = "none")  +
  ggtitle("Females")

print(plot_f)
male <- filtered_preference %>%
  filter(Sex == "Male")
plot_m <- ggplot(male, aes(y = ID)) +
  geom_bar(aes(x = Group, fill = "Group"), position = "dodge", stat = "identity") +
  geom_bar(aes(x = -Alone, fill = "Alone"), position = "dodge", stat = "identity") +
  scale_fill_manual(values = c("Group" = palette[1], "Alone" = palette[3])) +
  labs(x = "Count", y = "ID") +
  scale_x_continuous(breaks=sight_values,
                     labels = abs(sight_values))+
  theme_minimal() +
  theme(legend.position = "none")  +
  ggtitle("Males")

print(plot_m)
juv <- filtered_preference %>%
  filter(Sex == "Juvenile")
plot_j <- ggplot(juv, aes(y = ID)) +
  geom_bar(aes(x = Group, fill = "Group"), position = "dodge", stat = "identity") +
  geom_bar(aes(x = -Alone, fill = "Alone"), position = "dodge", stat = "identity") +
  scale_fill_manual(values = c("Group" = palette[1], "Alone" = palette[3]),
                    guide = guide_legend(title = "Encountered")) +
  labs(x = "Count", y = "ID") +
  scale_x_continuous(breaks=sight_values,
                     labels = abs(sight_values))+
  theme_minimal() +
  theme(legend.position = "left") +
  ggtitle("Juveniles")
print(plot_j)



#### Plotting block of groups ####

encounters <- read.csv("../data/individaul_apr_jun.csv", header=T)

groups_encounters <- encounters %>%
  group_by(Group) %>%
  filter(n() > 1) %>%
  ungroup()

first_rows <- groups_encounters %>%
  group_by(Group) %>%
  slice(1) %>%
  ungroup()

# Print the resulting dataframe
table(first_rows$Block)

#group size ans location of group
summary_of_groups <- data.frame( Block=character(0),
                                Size=numeric(0))
unique_groups <- unique(groups_encounters$Group)
list(unique_groups)
for(i in unique_groups){
  count <- sum(groups_encounters$Group == i)
  block_val <- groups_encounters$Block[groups_encounters$Group == i][1]
  summary_of_groups <- rbind(summary_of_groups, data.frame( Block = block_val, Size = count))
}

#violin plot 
palette <- c("#BDD5EA", "#8BBB9E", "#064C74", "#2784B9", "#8B5D33", "#3F6C51")

plot1b <- ggplot(summary_of_groups, aes(x = Block, y = as.numeric(Size), fill = Block)) +
  geom_violin(trim = FALSE, linewidth=0) +
  geom_point(position = position_jitter(seed = 1, width = 0.3), size=1) +
  labs(x = "Blocks", y = "Size of the Group", fill = "Blocks") +
  theme_minimal() +
  scale_fill_manual(values = palette) +
  theme(legend.position = "none")  +
  scale_y_continuous(breaks = 1:6)

#freq plot
block_freq <-  table(summary_of_groups$Block)
block_freq <- as.data.frame(block_freq)

plot_1a <- ggplot(block_freq, aes(x = Var1, y=Freq, fill=Var1)) +
  geom_bar(stat="identity", fill= palette[6])+
  labs(x = "Block", y = "Frequency of Groups") +
  geom_text(aes(label = Freq), vjust = -0.3) +
  theme_minimal()+
  coord_cartesian(ylim = c(0,45))

#modelling 

m1 <- lm(Block~Size, data=summary_of_groups)
summary(m1)


####plotting grid!####

#using plot 1
#plot1 a 
#plotm/j/f

row1 <- plot_grid(plot1)
row2 <- plot_grid(plot_1a)
row3 <- plot_grid(plot_j,plot_f,plot_m, ncol=3)

row1_labeled <- row1 +
  annotate("text", x = 0.01, y = 0.98, label = "a)", size = 4, hjust = 0, vjust = 1) 
row2_labeled <- row2 +
  annotate("text", x = 0.01, y = 0.98, label = "b)", size = 4, hjust = 0, vjust = 1) 
row3_labeled <- row3 +
  annotate("text", x = 0.015, y = 0.98, label = "c)", size = 4, hjust = 0, vjust = 1)

panel_plot3 <- plot_grid(row1_labeled,row2_labeled,row3_labeled, ncol=1, rel_heights = c(2,2,4))
print(panel_plot3)

panel_plot3 <- panel_plot3 +   theme(
  plot.background = element_rect(fill = "white", color = "white", size = 2),
  plot.margin = margin(10, 10, 10, 10),
  panel.spacing = unit(1.5, "lines")
)

ggsave("../results/figure4.png", plot = panel_plot3, width = 8.27, height = 9.96, units = "in")


