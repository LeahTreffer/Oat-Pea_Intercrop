library(readr)
library(dplyr)
library(tidyverse)
library(readxl)
library(data.table)

setwd("~/GitHub/OatPea_intercrop")

data <- read.csv("data/SpringOatPeaIntercrop_2024_NY_phenotypes.csv")
SpringOatPeaIntercrop_2024_NY_subplot <- read_excel("data/SpringOatPeaIntercrop_2024_NY_subplot.xlsx",
                                                    sheet = "SpringOatPeaIntercrop_2024_NY_s")
SpringOatPeaIntercrop_2024_NY_subplot_pairs <- read_excel("data/SpringOatPeaIntercrop_2024_NY_subplot.xlsx",
                                                          sheet = "pair_id")

data2 <- SpringOatPeaIntercrop_2024_NY_subplot %>%
  left_join(SpringOatPeaIntercrop_2024_NY_subplot_pairs) %>% ## add the plot pairs
  select(subplot_name,subplot_id,block_number,pair,plot_number,subplot_number) %>%
  mutate(pair_plot = str_c(pair,"_",plot_number)) %>% # create pair_plot for sorting
  mutate(pair_subplot = str_c(pair,"_",subplot_number)) # create pair_subplot for sorting

colnames(data2)[1] <- "observationUnitName"

data3 <- merge(data, data2, by="observationUnitName")

#shorten column names of the pea ID columns
#Remove characters before the last underscore in the columns giving pea names
colnames(data3)[60:71] <- sub(".*_", "", colnames(data3)[60:71])

df <- data3

# Replace 1 with the column name
df[, 60:71] <- lapply(names(df)[60:71], function(col) {
  ifelse(df[[col]] == 1, col, NA)
})

# Combine columns 60 to 71 into a single column
df$peaGermplasm <- apply(df[, 60:71], 1, function(x) {
  na.omit(x)[1]
})

# basic correlation
# oat biomass > pea biomass + weed biomass

#biomass colums: 24 32:35, 39:42, 55:58
biomass <- df[,c(1, 20, 79, 32:35, 39:42, 55:58, 72:78)] #observationUnitNam3, germplasm, traits, plot_subplot ids
biomass$oat_biomass <- apply(biomass[, c("Above ground dry biomass - g|day 154|COMP:0000067", "Above ground dry biomass - g|day 178|COMP:0000068", "Above ground dry biomass - g|day 199|COMP:0000069", "Above ground dry biomass - g|day 217|COMP:0000070")], 1, function(x) {
  paste0(na.omit(x), collapse = "")
})
biomass$pea_biomass <- apply(biomass[, c("Pea above ground dry biomass - g|day 154|COMP:0000083", "Pea above ground dry biomass - g|day 178|COMP:0000084", "Pea above ground dry biomass - g|day 199|COMP:0000085", "Pea above ground dry biomass - g|day 217|COMP:0000086")], 1, function(x) {
  paste0(na.omit(x), collapse = "")
})
biomass$weed_biomass <- apply(biomass[, c("Weed above ground dry biomass - g|day 154|COMP:0000079", "Weed above ground dry biomass - g|day 178|COMP:0000080", "Weed above ground dry biomass - g|day 199|COMP:0000081", "Weed above ground dry biomass - g|day 217|COMP:0000082")], 1, function(x) {
  paste0(na.omit(x), collapse = "")
})
biomass <- biomass[,c("germplasmName", "peaGermplasm", "observationUnitName", "block_number", "pair", "pair_plot", "pair_subplot","oat_biomass","pea_biomass","weed_biomass")]
# Replace blank cells with NA
#biomass2 <- biomass %>%
  #mutate(across(everything(), ~na_if(., "")))

# Remove rows with any NA values
#biomass3 <- biomass2 %>%
  #filter(complete.cases(.))

#remove the subplot 2 rows since biomass wasn't taken from them
biomass2 <- biomass[!grepl("_subplot_2$", biomass$observationUnitName), ]

# Replace blank cells with NA in the specified columns
biomass2[, 8:10] <- lapply(biomass2[, 8:10], function(col) {
  ifelse(col == "", NA, col)
})

biomass2$germplasmName <- as.factor(biomass2$germplasmName)
biomass2$peaGermplasm <- as.factor(biomass2$peaGermplasm)
biomass2$oat_biomass <- as.numeric(as.character(biomass2$oat_biomass))
biomass2$pea_biomass <- as.numeric(as.character(biomass2$pea_biomass))
biomass2$weed_biomass <- as.numeric(as.character(biomass2$weed_biomass))

# Fit a linear model with oat biomass as the response variable and peas and weeds biomass as predictors
model <- lm(oat_biomass ~ pea_biomass + weed_biomass, data = biomass2)

# Display the summary of the model
summary(model)

#Interpretation:
#The summary(model) output will provide you with the coefficients for the pea and weed biomass covariates, indicating how each is correlated with oat biomass when controlling for the other.
#The R-squared value will tell you how much of the variance in oat biomass is explained by the combined effects of pea and weed biomass.

# fit a linear mixed model with oat biomass as the responce and oat and pea genotype as predictors
model2 <- lm(oat_biomass ~ germplasmName * peaGermplasm, data = biomass2)

# Display the summary of the model
summary(model2)

# fit a linear mixed model with oat biomass as the responce and oat and pea genotype and weed biomass as predictors
model3 <- lm(oat_biomass ~ germplasmName + peaGermplasm + weed_biomass, data = biomass2)

# Display the summary of the model
summary(model3)


## oat genotype on oat biomass, weed as covariant
lm_oat_biomass <- lm(oat_biomass ~ germplasmName + weed_biomass, data = biomass2)

# Summary of the linear model to check the significance of oat genotype
summary(lm_oat_biomass)

## oat genotype on pea biomass, weed as covariant
# Fit a linear model with oat genotype and weed biomass as a covariate for pea biomass
lm_pea_biomass <- lm(pea_biomass ~ germplasmName + weed_biomass, data = biomass2)

# Summary of the linear model to check the significance of oat genotype
summary(lm_pea_biomass)


###
model_oatbiomass <- aov(oat_biomass ~ germplasmName + pea_biomass * weed_biomass, biomass2)
summary(model_oatbiomass)
  # significant differences in oat biomass between different germplasms (0.05)
  # direct effect of pea biomass on oat biomass: amount of pea biomass in a plot significantly impacts the oat biomass (0.001)
  # direct effect of weed biomass on oat biomass: weed biomass has a notable impact on oat biomass (0.000)
  # pea weed interaction: relationship between pea biomass and oat biomass varies depending on the weed biomass level (0.000)

model_peabiomass <- aov(pea_biomass ~ peaGermplasm + oat_biomass * weed_biomass, biomass2)
summary(model_peabiomass)
  # significant differences in pea biomass between different pea germplasms (0.000)
  # direct effect of oat biomass on pea biomass: amount of oat biomass in a plot significantly impacts the pea biomass (0.000)
  # no direct effect of weed biomass on pea biomass: weed biomass does not have an effect on pea biomass (0.6)
  # no oat weed interaction: the relationshiop between oat biomass and pea biomass does not vary depending on weed biomass (0.289)

model_oatbiomass2 <- aov(oat_biomass ~ germplasmName + peaGermplasm + pea_biomass * weed_biomass, biomass2)
summary(model_oatbiomass2)
  # no significant difference in oat biomass between different oat genotypes (0.77) or by the pea genotype (0.5) in the plot
  # direct effect of pea biomass on oat biomass: amount of pea biomass in a plot significantly impacts the oat biomass (0.001)
  # direct effect of weed biomass on oat biomass: weed biomass has a notable impact on oat biomass (0.000)
  # pea weed interaction: relationship between pea biomass and oat biomass varies depending on the weed biomass level (0.001)

model_peabiomass2 <- aov(pea_biomass ~ peaGermplasm + germplasmName + oat_biomass * weed_biomass, biomass2)
summary(model_peabiomass2)
  # significant differences in pea biomass between different pea germplasms (0.000)
  # no significant different in pea biomass between different oat genotypes (0.965)
  # direct effect of oat biomass on pea biomass: amount of oat biomass in a plot significantly impacts the pea biomass (0.000)
  # no direct effect of weed biomass on pea biomass: weed biomass does not have an effect on pea biomass (0.927)
  # no oat weed interaction: the relationshiop between oat biomass and pea biomass does not vary depending on weed biomass (0.236)

model_oatbiomass3 <- aov(oat_biomass ~ germplasmName * peaGermplasm + pea_biomass * weed_biomass, biomass2)
summary(model_oatbiomass3)
  # no significant difference in oat biomass between different oat genotypes (0.8699) or by the pea genotype (0.594) in the plot
  # direct effect of pea biomass on oat biomass: amount of pea biomass in a plot significantly impacts the oat biomass (0.000)
  # direct effect of weed biomass on oat biomass: weed biomass has a notable impact on oat biomass (0.000)
  # no significant effect from pea genotype interaction with oat genotype (0.885)
  # pea weed interaction: relationship between pea biomass and oat biomass varies depending on the weed biomass level (0.01)

model_peabiomass3 <- aov(pea_biomass ~  peaGermplasm * germplasmName + oat_biomass * weed_biomass, biomass2)
summary(model_peabiomass3)
  # significant differences in pea biomass between different pea germplasms (0.000)
  # no significant differences in pea biomass between different oat germplasms (0.971)
  # direct effect of oat biomass on pea biomass: amount of oat biomass in a plot significantly impacts the pea biomass (0.000)
  # no direct effect of weed biomass on pea biomass: weed biomass does not have an effect on pea biomass (0.928)
  # no significant effect from pea genotype interaction with oat genotype (0.674)
  # no oat weed interaction: the relationshiop between oat biomass and pea biomass does not vary depending on weed biomass (0.422)


### Model Assumptions
##Verify that the assumptions of ANOVA (normality, homogeneity of variances, and independence) are met:
  # Normality: Check the residuals for normality using a Q-Q plot.
  # Homogeneity of Variances: Use Levene's test to check for equal variances.
  # Independence: Ensure that the residuals are independent.

