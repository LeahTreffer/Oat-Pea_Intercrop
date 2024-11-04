
library(tidyverse)
library(readr)
library(readxl)
library(data.table)
library(dplyr)

# new script 
# if starting from scratch

#SpringOatPeaIntercrop_2024_NY_subplot <- read_excel("~/Intercrop JLJ/2024_spring/SpringOatPeaIntercrop_2024_NY_subplot.xlsx", 
#sheet = "SpringOatPeaIntercrop_2024_NY_s")
SpringOatPeaIntercrop_2024_NY_subplot <- read_excel("/Users/leahtreffer/Downloads/SpringOatPeaIntercrop_2024_NY_subplot.xlsx", 
                                                    sheet = "SpringOatPeaIntercrop_2024_NY_s")


#SpringOatPeaIntercrop_2024_NY_subplot_pairs <- read_excel("~/Intercrop JLJ/2024_spring/SpringOatPeaIntercrop_2024_NY_subplot.xlsx", 
#sheet = "pair_id")
SpringOatPeaIntercrop_2024_NY_subplot_pairs <- read_excel("/Users/leahtreffer/Downloads/SpringOatPeaIntercrop_2024_NY_subplot.xlsx", 
                                                          sheet = "pair_id")


data <- SpringOatPeaIntercrop_2024_NY_subplot %>% 
  left_join(SpringOatPeaIntercrop_2024_NY_subplot_pairs) %>% ## add the plot pairs
  select(subplot_name,subplot_id,block_number,pair,plot_number,subplot_number) %>% 
  mutate(pair_plot = str_c(pair,"_",plot_number)) %>% # create pair_plot for sorting
  mutate(pair_subplot = str_c(pair,"_",subplot_number)) # create pair_subplot for sorting



biomass_1_2 <- data %>%
  mutate(rand = runif(nrow(data))) %>% # add random numbers to arrange by, this isn't reproducible, make permanent here or in excel?
  filter(subplot_number != 2) %>% # remove subplot 2
  group_by(pair_subplot) %>%
  slice_head(n=1) %>% # it is grouped by pair_subplot, remove one 
  arrange(pair,rand) %>% # arrange by pair and then random
  bind_cols(biomass_sample_1_2 = rep(c( 1, 2), times = 100)) %>% # assign sample biomass sample 1 or 2
  select(pair_subplot,biomass_sample_1_2)



biomass_3_4 <- data %>%
  mutate(rand = runif(nrow(data))) %>% # add random numbers to arrange by, this isn't reproducible, make permanent here or in excel?
  filter(subplot_number != 2) %>% # remove subplot 2
  group_by(pair_subplot) %>%
  slice_head(n=1) %>% # it is grouped by pair_subplot, remove one 
  arrange(pair,rand) %>% # arrange by pair and then random
  bind_cols(biomass_sample_3_4 = rep(c( 3, 4), times = 100)) %>% # assign sample biomass sample 3 or 4
  select(pair_subplot,biomass_sample_3_4)


biomass_sample_management_factor <- 
  data %>% 
  left_join(biomass_1_2, join_by(pair_subplot)) %>% 
  left_join(biomass_3_4, join_by(pair_subplot)) %>% 
  mutate(biomass_1 = if_else(biomass_sample_1_2 == 1,1,NA)) %>% # reassign so each sample is different column / management factor for T3 upload
  mutate(biomass_2 = if_else(biomass_sample_1_2 == 2,1,NA)) %>%
  mutate(biomass_3 = if_else(biomass_sample_3_4 == 3,1,NA)) %>%
  mutate(biomass_4 = if_else(biomass_sample_3_4 == 4,1,NA)) %>% 
  select(subplot_name,subplot_id,block_number,plot_number,subplot_number,biomass_1,biomass_2,biomass_3,biomass_4)  ##



biomass_sample_management_factor %>% 
  print(n=600)  ### look over


##  write.csv(biomass_sample_management_factor, file="biomass_sample_management_factor.csv", na="", row.names = FALSE)  ## save csv without NA's for T3/OAT


####


## First divide in half
## we harvest half the samples at each time point
## half of the samples will be harvested at time point 1 and 3 
## other half will be harvested at time point 2 and 4

# Get unique pairs
unique_pairs <- unique(data$pair)

# Randomly assign pairs to two groups
set.seed(1234)  # Setting seed for reproducibility
group_assignment <- sample(c(rep("Group1", length(unique_pairs)/2), rep("Group2", length(unique_pairs)/2)))
## Group1 can be the plots being harvested at time point 1 and 3
## Group2 can be the plots being harvested at time point 2 and 4

#check that half are in each group
sum(group_assignment == "Group1")

# Create a data.table with pair and their assigned group
pair_group <- data.table(pair = unique_pairs, group = group_assignment)

# Merge the group assignment back to the original data
data <- merge(data, pair_group, by = "pair")


## Now we need to randomly assign which subplot is harvested at which time point
## half of group1 into time 1 and half of group2 into time 2
## then both of the other halves are randomized into 

# subset
group1_df <- data[data$group == "Group1",]
group2_df <- data[data$group == "Group2",]

# Get unique pairs
unique_pairs2 <- unique(group1_df$pair)

# Randomly assign pairs to two groups
set.seed(1234)  # Setting seed for reproducibility
group_assignment2 <- sample(c(rep("subplot_1", length(unique_pairs2)/2), rep("subplot_3", length(unique_pairs2)/2)))

#check that half are in each group
sum(group_assignment2 == "subplot_1")

# Create a data.table with pair and their assigned group
pair_group2 <- data.table(pair = unique_pairs2, first_harvest = group_assignment2)

# Merge the group assignment back to the original data
group1_df <- merge(group1_df, pair_group2, by = "pair")

# Create the other_harvest column with the opposite values
# these are to be harvested later (will re-randomize to either third or fourth harvest timepoint)
group1_df$other_harvest <- ifelse(group1_df$first_harvest == "subplot_1", "subplot_3", "subplot_1")

second_randomization1 <- group1_df[,c("pair", "subplot_name", "subplot_id", "block_number", "plot_number", "subplot_number", "pair_plot", "other_harvest")]


# Get unique pairs
unique_pairs3 <- unique(group2_df$pair)

# Randomly assign pairs to two groups
set.seed(1234)  # Setting seed for reproducibility
group_assignment3 <- sample(c(rep("subplot_1", length(unique_pairs3)/2), rep("subplot_3", length(unique_pairs3)/2)))

#check that half are in each group
sum(group_assignment3 == "subplot_1")

# Create a data.table with pair and their assigned group
pair_group3 <- data.table(pair = unique_pairs3, second_harvest = group_assignment3)

# Merge the group assignment back to the original data
group2_df <- merge(group2_df, pair_group3, by = "pair")

# Create the second_harvest column with the opposite values
group2_df$other_harvest <- ifelse(group2_df$second_harvest == "subplot_1", "subplot_3", "subplot_1")

second_randomization2 <- group2_df[,c("pair", "subplot_name", "subplot_id", "block_number", "plot_number", "subplot_number", "pair_plot", "other_harvest")]

# combine the left over plots from both time 1 and time 2
# re-randomize these into time 3 and time 4
second_randomization <- rbind(second_randomization1,second_randomization2)
second_randomization_unique_pairs <- unique(second_randomization$pair)
set.seed(1234)  # Setting seed for reproducibility
assignment <- sample(c(rep("third", length(second_randomization_unique_pairs)/2), rep("fourth", length(second_randomization_unique_pairs)/2)))

#check that half are in each group
sum(assignment == "third")

# Create a data.table with pair and their assigned group
second_randomization_pair_group <- data.table(pair = second_randomization_unique_pairs, harvest_time = assignment)

# Merge the group assignment back to the original data
test <- merge(second_randomization, second_randomization_pair_group, by = "pair")

test$third_harvest <- NA
test$fourth_harvest <- NA

test$name <- gsub("^.*NY_[0-9]+_", "", test$subplot_name) # extract subplot name from the long plot name
# make column with plot_subplot
# this will be what is shown in the harvest column for each time point 
test$plot_subplot <- paste0(test$plot_number,"_",test$subplot_number) 

# for each harvest time point, have the plot_subplot for the plots we need to harvest
# if the randomly assigned subplot matches the name for that row, then add the plot_subplot name to the harvest column
test <- test %>%
  mutate(harvest_3 = ifelse(harvest_time == "third" & other_harvest == name, plot_subplot, NA))
test <- test %>%
  mutate(harvest_4 = ifelse(harvest_time == "fourth" & other_harvest == name, plot_subplot, NA))

test <- test %>%
  mutate(third_harvest = ifelse(harvest_3 == plot_subplot, name, NA))
test <- test %>%
  mutate(fourth_harvest = ifelse(harvest_4 == plot_subplot, name, NA))

test <- test %>%
  mutate(`ManagementFactor:SpringOatPeaIntercrop_2024_NY_biomass_T3` = ifelse(third_harvest == name, "1", NA))
test <- test %>%
  mutate(`ManagementFactor:SpringOatPeaIntercrop_2024_NY_biomass_T4` = ifelse(fourth_harvest == name, "1", NA))

#clean up
testt <- test[,c("subplot_name", "subplot_id", "block_number", "plot_number", "subplot_number", "pair_plot", "plot_subplot", "third_harvest", "fourth_harvest", "harvest_3", "harvest_4","ManagementFactor:SpringOatPeaIntercrop_2024_NY_biomass_T3", "ManagementFactor:SpringOatPeaIntercrop_2024_NY_biomass_T4")]

# format time 1 and time 2

# need columns in both data frames to be the exact same
# so make empty columns for the harvests that aren't happening for that group
group1_df$second_harvest <- NA
group2_df$first_harvest <- NA

#put groups back into one data frame
group1 <- group1_df[,c("subplot_name", "subplot_id", "block_number", "plot_number", "subplot_number", "pair_plot", "pair_subplot", "first_harvest", "second_harvest")]
group2 <- group2_df[,c("subplot_name", "subplot_id", "block_number", "plot_number", "subplot_number", "pair_plot", "pair_subplot", "first_harvest", "second_harvest")]
grouped <- rbind(group1, group2)
grouped <- grouped %>% arrange(plot_number)

grouped$name <- gsub("^.*NY_[0-9]+_", "", grouped$subplot_name) # extract subplot name from the long plot name
# make column with plot_subplot
# this will be what is shown in the harvest column for each time point 
grouped$plot_subplot <- paste0(grouped$plot_number,"_",grouped$subplot_number) 

# for each harvest time point, have the plot_subplot for the plots we need to harvest
# if the randomly assigned subplot matches the name for that row, then add the plot_subplot name to the harvest column
grouped <- grouped %>%
  mutate(harvest_1 = ifelse(first_harvest == name, plot_subplot, NA))
grouped <- grouped %>%
  mutate(harvest_2 = ifelse(second_harvest == name, plot_subplot, NA))

grouped <- grouped %>%
  mutate(`ManagementFactor:SpringOatPeaIntercrop_2024_NY_biomass_T1` = ifelse(first_harvest == name, "1", NA))
grouped <- grouped %>%
  mutate(`ManagementFactor:SpringOatPeaIntercrop_2024_NY_biomass_T2` = ifelse(second_harvest == name, "1", NA))

#clean up
groupedd <- grouped[,c("subplot_name", "subplot_id", "block_number", "plot_number", "subplot_number", "pair_plot", "plot_subplot", "first_harvest", "second_harvest", "harvest_1", "harvest_2","ManagementFactor:SpringOatPeaIntercrop_2024_NY_biomass_T1", "ManagementFactor:SpringOatPeaIntercrop_2024_NY_biomass_T2")]

new <- merge(testt, groupedd[,c("subplot_name", "first_harvest", "second_harvest", "harvest_1", "harvest_2","ManagementFactor:SpringOatPeaIntercrop_2024_NY_biomass_T1", "ManagementFactor:SpringOatPeaIntercrop_2024_NY_biomass_T2")], by="subplot_name")
new <- new %>% arrange(plot_number)
new <- new[,c("subplot_name", "subplot_id", "block_number", "plot_number", "subplot_number", "pair_plot", "plot_subplot", "first_harvest", "second_harvest", "third_harvest", "fourth_harvest", "harvest_1", "harvest_2","harvest_3", "harvest_4", "ManagementFactor:SpringOatPeaIntercrop_2024_NY_biomass_T1", "ManagementFactor:SpringOatPeaIntercrop_2024_NY_biomass_T2", "ManagementFactor:SpringOatPeaIntercrop_2024_NY_biomass_T3", "ManagementFactor:SpringOatPeaIntercrop_2024_NY_biomass_T4")]

##  write.csv(new, file="/Users/leahtreffer/Library/CloudStorage/GoogleDrive-lkt38@cornell.edu/My Drive/Oat Pea/SpringOatPeaIntercrop_biomass_harvests.csv", na="", row.names = FALSE)  ## save csv without NA's for T3/OAT

####### 

# new script 
# use to correct old script
# re-randomize time 3 and time 4

# download the above csv from BOX : BreedingForIntercropping > 2024_fieldwork > bag_labels > SpringOatPeaIntercrop_diversity_biomassharvest.csv
old <- read.csv("/Users/leahtreffer/Downloads/SpringOatPeaIntercrop_diversity_biomassharvest.csv")

# add pair column 
old <- merge(old, SpringOatPeaIntercrop_2024_NY_subplot_pairs, by="subplot_name") # add the plot pairs

old$name <- gsub("^.*NY_[0-9]+_", "", old$subplot_name) # extract subplot name from the long plot name
# make column with plot_subplot
# this will be what is shown in the harvest column for each time point 
old$plot_subplot <- paste0(old$plot_number,"_",old$subplot_number)

# if T1 = 1 then old$first_harvest = name
old <- old %>%
  mutate(first_harvest = ifelse(ManagementFactor.SpringOatPeaIntercrop_2024_NY_biomass_T1 == "1", name, NA))
# if T2 = 1 then old$second_harvest = name
old <- old %>%
  mutate(second_harvest = ifelse(ManagementFactor.SpringOatPeaIntercrop_2024_NY_biomass_T2 == "1", name, NA))
#if old.T3 = 1 or old.T4 = 1 then old$other_harvest = name
old <- old %>%
  mutate(other_harvest = ifelse(old.ManagementFactor.SpringOatPeaIntercrop_2024_NY_biomass_T3 == "1" | old.ManagementFactor.SpringOatPeaIntercrop_2024_NY_biomass_T4 == "1", name, NA))

df <- old %>%
  filter(!is.na(other_harvest))

new_randomization_unique_pairs <- unique(df$pair)
set.seed(1234)  # Setting seed for reproducibility
new_assignment <- sample(c(rep("third", length(new_randomization_unique_pairs)/2), rep("fourth", length(new_randomization_unique_pairs)/2)))

#check that half are in each group
sum(new_assignment == "third")

# Create a data.table with pair and their assigned group
new_randomization_pair_group <- data.table(pair = new_randomization_unique_pairs, harvest_time = new_assignment)

# Merge the group assignment back to the original data
oldish <- merge(old, new_randomization_pair_group, by = "pair")

# for each harvest time point, have the plot_subplot for the plots we need to harvest
# if the randomly assigned subplot matches the name for that row, then add the plot_subplot name to the harvest column
oldish <- oldish %>%
  mutate(harvest_3 = ifelse(harvest_time == "third" & other_harvest == name, plot_subplot, NA))
oldish <- oldish %>%
  mutate(harvest_4 = ifelse(harvest_time == "fourth" & other_harvest == name, plot_subplot, NA))
oldish <- oldish %>%
  mutate(harvest_1 = ifelse(first_harvest == name, plot_subplot, NA))
oldish <- oldish %>%
  mutate(harvest_2 = ifelse(second_harvest == name, plot_subplot, NA))

oldish <- oldish %>%
  mutate(third_harvest = ifelse(harvest_3 == plot_subplot, name, NA))
oldish <- oldish %>%
  mutate(fourth_harvest = ifelse(harvest_4 == plot_subplot, name, NA))

oldish <- oldish %>%
  mutate(`ManagementFactor:SpringOatPeaIntercrop_2024_NY_biomass_T3` = ifelse(third_harvest == name, "1", NA))
oldish <- oldish %>%
  mutate(`ManagementFactor:SpringOatPeaIntercrop_2024_NY_biomass_T4` = ifelse(fourth_harvest == name, "1", NA))

#clean up
not_oldish <- not_oldish %>% arrange(plot_number, subplot_number)

not_oldish2 <- not_oldish[,c("pair", "subplot_name", "subplot_id", "block_number", "plot_number", "subplot_number", "pair_plot", "pair_subplot", "first_harvest", "second_harvest", "third_harvest", "fourth_harvest", "harvest_1", "harvest_2", "harvest_3", "harvest_4","ManagementFactor.SpringOatPeaIntercrop_2024_NY_biomass_T1", "ManagementFactor.SpringOatPeaIntercrop_2024_NY_biomass_T2", "ManagementFactor:SpringOatPeaIntercrop_2024_NY_biomass_T3", "ManagementFactor:SpringOatPeaIntercrop_2024_NY_biomass_T4")]

not_oldish2 <- not_oldish[,c("pair", "subplot_name", "subplot_id", "block_number", "plot_number", "subplot_number", "pair_plot", "pair_subplot","ManagementFactor.SpringOatPeaIntercrop_2024_NY_biomass_T1", "ManagementFactor.SpringOatPeaIntercrop_2024_NY_biomass_T2", "ManagementFactor:SpringOatPeaIntercrop_2024_NY_biomass_T3", "ManagementFactor:SpringOatPeaIntercrop_2024_NY_biomass_T4")]

## write.csv(not_oldish2, file="/Users/leahtreffer/Library/CloudStorage/GoogleDrive-lkt38@cornell.edu/My Drive/Oat Pea/SpringOatPeaIntercrop_diversity_biomass_harvests.csv", na="", row.names = FALSE)  ## save csv without NA's for T3/OAT
