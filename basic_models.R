#!/usr/bin/env Rscript

#Thesis 
#Author: Amy Feakes
#Script: basic_models.R
#Description:Basic descriptive and meta data plots 

#Clear workspace
rm(list=ls())

#Dependencies
library(ggplot2)
library(dplyr)
library(tidyr)
library(cowplot)


#import the data 
encounters <- read.csv("../data/individaul_apr_jun.csv", header=T)
id_freq <- read.csv("../data/block_matrix_extras.csv", header=T)


####Results summary stats ####
#number of individuals in each dive and mean 
no_ind <- table(encounters$Dive.ID)
mean(no_ind) #4.37
sd(no_ind) / sqrt(length(no_ind)) #0.256

#number of groups in each dive
df <- encounters
group_counts <- df %>%
  group_by(Dive.ID) %>%
  summarize(Num_Different_Groups = length(unique(Group)))
mean(group_counts$Num_Different_Groups) # 2.79
sd(group_counts$Num_Different_Groups) / sqrt(length(group_counts$Num_Different_Groups)) 

#number in each group 
unique(df$Group) #210 different groups
group_size <- table(df$Group)
#groups of just one individual 
group_one <- names(group_size[group_size == 1])
no_group_one <- length(group_one) #125
#percentage of groups with 1 individual 
perc_one <- (no_group_one/210)*100 #59 

#125 individuals not in a group, of 332 encounters 
likelihood_to_be_alone <- (125/332)*100
#37.650

#looking at id
sightings <- table(df$ID)
mean(sightings) #mean no of sightings 6.51
se_size <- sd(sightings) / sqrt(length(sightings))
sighted_once <- length(names(sightings[sightings ==1])) #20
perc_sight_once <- (sighted_once/51)*100 #39.21%

####FIGURE 3A####
palette <- c("#BDD5EA", "#8BBB9E", "#064C74", "#2784B9", "#8B5D33", "#3F6C51")
#x axis - number of sightings
#y axis - frequency 
  plot1 <- ggplot(id_freq, aes(x = Total_sightings)) +
    geom_bar(position="dodge", fill = palette[6])+
    labs(x = "Number of Sightings per Individual", y = "Frequency") +
    theme_minimal()+
    scale_x_continuous(breaks = seq(min(id_freq$Total_sightings), max(id_freq$Total_sightings), by = 5))
  
#looking at life history 
table(id_freq$Sex)
#number of adults 
adults <- sum(26,7) #33
per_adults <- (adults/51)*100 #64.71%
male_per <- (7/51)*100 #13.72%

####FIGURE 3B ####
palette <- c("#BDD5EA", "#8BBB9E", "#064C74", "#2784B9", "#8B5D33", "#3F6C51")
#pale blue , sea green, indigo dye, picton blue, brown ,dark red brown ,
id_freq$Sex <- factor(id_freq$Sex, levels = c("Juvenile", "Female", "Male"))

plot2<- ggplot(id_freq, aes(x = factor(Size), fill = Sex)) +
  geom_bar(position = "stack") +
  scale_x_discrete(breaks = seq(35, 110, by = 5)) +
  scale_y_continuous(labels = scales::comma_format(scale = 1), breaks = scales::pretty_breaks(n = 10)) +
  labs(x = "Size (cm)", y = "Frequency of Individuals", fill = "Life-stage/Sex") +
  theme_minimal() +
  scale_fill_manual(values = c(palette[1],palette[2],palette[4]))

#Size of individuals 
sizes <- table(id_freq$Size)
mean_size <- mean(id_freq$Size) #71.96
se_size <- sd(id_freq$Size) / sqrt(length(id_freq$Size))

#size of males 
males_df <- subset(id_freq,id_freq$Sex == "Male")
juv_df <- subset(id_freq,id_freq$Sex == "Juvenile")
fe_df <- subset(id_freq,id_freq$Sex == "Female")
mean(males_df$Size) #97.14cm
mean(juv_df$Size) #52.22cm
mean(fe_df$Size) #78.85cm 

#turtle demographics 
table(id_freq$Sex)
#percent of adults males
male_perc <- (7/33)*100 #21%

#look at encounters
table(encounters$Sex)


#### FIGURE 3C ####
per_survey <- table(encounters$Dive.ID)
per_survey_df <- as.data.frame(per_survey)

plot3 <- ggplot(per_survey_df, aes(x = Freq)) +
  geom_bar(position="dodge", fill= palette[3])+
    labs(x = "Number of Sightings per Event", y = "Frequency") +
  theme_minimal() +
  scale_x_continuous(breaks = 1:10, labels = 1:10) 


##spatial patterns 
turtle <- sum(id_freq$Turtle)
shallow <- sum(id_freq$Shallow)
split <- sum(id_freq$Split)
yellow <- sum(id_freq$Yellow)
ridge <- sum(id_freq$Ridge)
lobster <- sum(id_freq$Lobster)
#127,45,75,17,40,28
#create df 
block_visits <- data.frame( block = c("Lobster", "Ridge", "Shallow", "Split", "Turtle", "Yellow"),
                            no_encounters= c(28,40,45,75,127,17))
####FIGURE 3D ####

plot4 <- ggplot(data = block_visits, aes(x = block, y = no_encounters, fill = block)) +
  geom_bar(stat = "identity", fill=palette[4]) +
  theme_minimal() +
  geom_text(aes(label = no_encounters), vjust = -0.3) + 
  labs(x = "Block", y = "Frequency of Encounters")

####Combining plots ####
#labels
row1 <- plot_grid(plot1, plot3, ncol = 2)
row2 <- plot_grid(plot2)
row3 <- plot_grid(plot4)

row1_labeled <- row1 +
  annotate("text", x = 0.01, y = 0.98, label = "a)", size = 4, hjust = 0, vjust = 1) + 
  annotate("text", x = 0.51, y = 0.98, label = "b)", size = 4, hjust = 0, vjust = 1)
row2_labeled <- row2 +
  annotate("text", x = 0.015, y = 0.98, label = "c)", size = 4, hjust = 0, vjust = 1)
row3_labeled <- row3 +
  annotate("text", x = 0.015, y = 0.98, label = "d)", size = 4, hjust = 0, vjust = 1)


panel_plot2 <- plot_grid(row1_labeled,row2_labeled, row3_labeled, ncol=1, rel_heights = c(1.5,1.5,2))
print(panel_plot2)

panel_plot2 <- panel_plot2 +   theme(
  plot.background = element_rect(fill = "white", color = "white", size = 2),
  plot.margin = margin(20, 20, 20, 20),
  panel.spacing = unit(1.5, "lines")
)

ggsave("../results/figure3.png", plot = panel_plot2, width = 8.27, height = 9.96, units = "in")


####Event details ####
dive_info <- read.csv("../data/dive_data.csv")

table(dive_info$researcher)
table(dive_info$vis)
table(dive_info$researcher)

table(dive_info$length)
ma <- lm(no_turts~length, data=dive_info)
summary(ma)

table(dive_info$start_hour)

#did start time impact encounters 
mb <- lm(no_turts~factor(start_hour), data=dive_info)
summary(mb) #non significant

#creating the plots 

#month of surveys 
date_of_survey <- data.frame(month=c("April", "May", "June"),
                             freq=c(27,23,26))
month_order <- c("April", "May", "June")
date_of_survey$month <- factor(date_of_survey$month, levels = month_order)
plot_A <- ggplot(data = date_of_survey, aes(x = month, y = freq)) +
  geom_bar(stat = "identity", fill=palette[4]) +
  theme_minimal() +
  geom_text(aes(label = freq), vjust = -0.3) + 
  labs(x = "Month", y = "Frequency of Events")+
  coord_cartesian(ylim = c(0,30))

#current of survey
current_freq <- table(dive_info$cur_direction)
current_freq <- as.data.frame(current_freq)

plot_B <- ggplot(current_freq, aes(x = Var1, y=Freq, fill=Var1)) +
  geom_bar(stat="identity", fill= palette[2])+
  labs(x = "Current", y = "Frequency") +
  geom_text(aes(label = Freq), vjust = -0.3) +
  theme_minimal()+
  coord_cartesian(ylim = c(0,55))

#visibility 
vis_freq <- table(dive_info$vis)
vis_freq <- as.data.frame(vis_freq)
vis_order <- c("1-5m", "6-10m", "11-15m", "16-20m")
vis_freq$Var1 <- factor(vis_freq$Var1, levels = vis_order)
plot_C <- ggplot(vis_freq, aes(x = Var1, y=Freq, fill=Var1)) +
  geom_bar(stat="identity", fill= palette[1])+
  labs(x = "Horizontal Visibility", y = "Frequency") +
  geom_text(aes(label = Freq), vjust = -0.3) +
  theme_minimal()+
  coord_cartesian(ylim = c(0,50))


#start time 
start_freq <- table(dive_info$start_hour)
start_freq <- as.data.frame(start_freq)

plot_D <- ggplot(start_freq, aes(x = Var1, y=Freq, fill=Var1)) +
  geom_bar(stat="identity", fill= palette[3])+
  labs(x = "Starting Hour of Event", y = "Frequency") +
  geom_text(aes(label = Freq), vjust = -0.3) +
  theme_minimal()+
  coord_cartesian(ylim = c(0,35))


##plot these four as a block 
#labels
row1 <- plot_grid(plot_A, plot_D, ncol = 2)
row2 <- plot_grid(plot_B, plot_C, ncol = 2)

row1_labeled <- row1 +
  annotate("text", x = 0.01, y = 0.98, label = "a)", size = 4, hjust = 0, vjust = 1) + 
  annotate("text", x = 0.51, y = 0.98, label = "b)", size = 4, hjust = 0, vjust = 1)
row2_labeled <- row2 +
  annotate("text", x = 0.01, y = 0.98, label = "c)", size = 4, hjust = 0, vjust = 1) + 
  annotate("text", x = 0.51, y = 0.98, label = "d)", size = 4, hjust = 0, vjust = 1)

panel_plot3 <- plot_grid(row1_labeled,row2_labeled, ncol=1, rel_heights = c(1.5,1.5))
print(panel_plot3)

panel_plot3 <- panel_plot3 +   theme(
  plot.background = element_rect(fill = "white", color = "white", size = 2),
  plot.margin = margin(20, 20, 20, 20),
  panel.spacing = unit(1.5, "lines")
)

ggsave("../results/figure_appendix.png", plot = panel_plot3, width = 8.27, height = 7, units = "in")



#### Site fidelity ####
table(id_freq$no_blocks)

#individuals encountered more than 3 times 
filtered_id <- subset(id_freq, id_freq$Total_sightings >2) #24 individuals

table(filtered_id$no_blocks)
no_one_block <- subset(filtered_id, filtered_id$no_blocks == 1) 


#### Basic LM to look at temporal and spatial patterns. ####

##Individuals on blocks 
m1 <- lm(no_blocks~Total_sightings, data=id_freq)
summary(m1)

#Size of indvidual on block 
m2 <- lm(Size~Block, data=encounters)
summary(m2)

m3 <- lm((Sex =="Male")~Block, data=encounters)
summary(m3)
m3 <- lm((Sex =="Female")~Block +Size, data=encounters)
summary(m3)
m3 <- lm((Sex =="Juvenile")~Block +Size , data=encounters)
summary(m3)

odds_ratios <- exp(coef(m3))

# Calculate the probabilities
probabilities <- odds_ratios / (1 + odds_ratios)

# Calculate the percentage likelihood
percentage_likelihood <- probabilities * 100

# Create a data frame with the transformed values
transformed_results <- data.frame(
  Stage = rep("Juvenile", length(percentage_likelihood)),
  Block = colnames(odds_ratios),
  Percentage_Likelihood = percentage_likelihood
)

# Print the transformed results
print(transformed_results)

#Sightings number vary with size or sex 
m3 <- lm(Total_sightings ~ Size, data=id_freq)
m4 <- lm(Total_sightings ~ Sex, data=id_freq)
#no significance
summary(m3)
summary(m4)



####Appendix plots ####
#Violin plot of sizes on blocks 

plotA <- ggplot(encounters, aes(x = Block, y = Size, fill = Block)) +
  geom_violin(trim = FALSE, linewidth=0) +
  geom_point(position = position_jitter(seed = 1, width = 0.3), size=1) +
  labs(x = "Blocks", y = "Size (cm)", fill = "Blocks") +
  theme_minimal() +
  scale_fill_manual(values = palette) +
  theme(legend.position = "none")  



