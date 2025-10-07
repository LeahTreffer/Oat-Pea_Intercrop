# Data from 2025 NY oat-pea intercrop trials
# Field : Helfner
# 20 plots; these were the first 10 'border' plots on each side of the 400 plot B4I experiment
# 5 oat accessions
# 5 pea accessions
# intercrops and monocrops of oat and pea
# each accession exists has a monoculture and as one intercrop with an accession of the other species
# each plot divided into 3 subplots; subplot 1 always north side and subplot 3 always the south side
# seeding rate for the monocultures was 54g oat mono, 8 seed per square ft pea mono
# intercrops were planted at a percentage of those seeding rates: 40% oat and 60% pea

# PAR measurements taken using a LiCor LI-188B
# Integration = 10
# Range = 100x
# Quantum Sensor Unit: µmol PAR m-2 s-1
# sensor slowly moved horizontally through canopy within each subplot
# Timepoint 1 : June 2-3 2025
# Timepoint 2 : June 24-25 2025
# Timepoint 3 : July 14-15 2025
# measurements taken by Leah and recorded by hand (T1), and in Fieldbook (T2,T3) by Mirza
# PAR measurement taken with sensor on the ground and above canopy
# in T2 and T3 another PAR measurement was taken _ cm from the ground

# Raw data : https://cornell.app.box.com/folder/342321167330
# data exists elsewhere for plant height, biomass, stand count (https://oat.triticeaetoolbox.org/breeders/trial/6830; "data/B4I_2025_PM3D_NY_phenotypes.csv")
## Above ground dry biomass - g|timepoint X|COMP:0000110
## Pea Aboveground Dry Biomass - g|timepoint X|COMP:0000118
## Weed above ground dry biomass - g|timepoint X|COMP:0000126
## Plant establishment - plants/ft2|CO_350:0005124
## Pea Plant Establishment - plants/ft2|CO_xxx:0003011
## Plant height - cm|timepoint X|COMP:0000114
## Pea Plant Height - cm|timepoint X|COMP:0000122
# Since seeding rates are different between interfrop and monoculture, we will want to have the standcounts (emergence) data

################################################################################

# Photosynthetically Active Radiation (PAR) data from a Li-Cor Quantum Sensor
# Above canopy PAR represents the total light available above the crop canopy (i.e., incident light).
# Below canopy PAR indicates the amount of light that passes through the canopy and is available for the plants below it.

# Light Interception (LI): percentage of light that is absorbed by the canopy compared to the incident light
# LI = ((PAR_above - PAR_below) / PAR_above) * 100

# Light Interception Efficiency (LIE): measure of how efficiently the crops intercept light compared to a baseline, such as monocrop systems or theoretical maximum light interception.
# LIE = mean_intercrop_LI / mean_monocrop_LI

################################################################################

library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(corrplot)
library(lme4)
library(multcomp)
library(emmeans)

LiCor <- read_excel("data/T1_T2_T3_light_interception.xlsx", sheet = 'Long')

LiCor$plot_number <- as.factor(LiCor$plot_number)
LiCor$subplot_number <- as.factor(LiCor$subplot_number)
LiCor$Timepoint <- as.factor(LiCor$Timepoint)
LiCor$Crop <- as.factor(LiCor$Crop)
LiCor$oat_accession_name <- as.factor(LiCor$oat_accession_name)
LiCor$pea_accession_name <- as.factor(LiCor$pea_accession_name)

### Light Interception (LI)
LiCor$total_light_interception <- ((LiCor$Light_top - LiCor$Light_ground) / LiCor$Light_top) * 100
LiCor$top_light_interception <- ((LiCor$Light_top - LiCor$Light_mid) / LiCor$Light_top) * 100
LiCor$bottom_light_interception <- ((LiCor$Light_mid - LiCor$Light_ground) / LiCor$Light_mid) * 100

intercrop_data <- LiCor[LiCor$System == "intercrop", ]
monocrop_data <- LiCor[LiCor$System == "monoculture", ]
mono_pea <- LiCor[LiCor$Crop == "pea", ]
mono_oat <- LiCor[LiCor$Crop == "oat", ]

### Light Interception Efficiency (LIE)
mean_intercrop_LI <- mean(intercrop_data$total_light_interception, na.rm = TRUE)
mean_monocrop_LI <- mean(monocrop_data$total_light_interception, na.rm = TRUE)
mean_mono_pea_LI <- mean(mono_pea$total_light_interception, na.rm = TRUE)
mean_mono_oat_LI <- mean(mono_oat$total_light_interception, na.rm = TRUE)
LIE <- mean_intercrop_LI / mean_monocrop_LI
LIE_p <- mean_intercrop_LI / mean_mono_pea_LI
LIE_o <- mean_intercrop_LI / mean_mono_oat_LI
# intercrops intercept more light on average than monocultures
## intercrops intercept more light than pea monocultures, but less light than oat monocultures

rm(mono_oat,mono_pea,monocrop_data,mean_monocrop_LI,mean_mono_pea_LI,mean_mono_oat_LI)

# This does not account for differences in seeding rate between monocultures and intercrops, so below will adjust based on stand counts
## In excel, I sorted oats by oat name>system(inter/mono)>plot so that I had a list with oat accessions grouped together with the two intercrop plots followed by its monoculture version
## I then made a new column (Oat_prcnt) and set the value for the monoculture to 1. This represents that the monoculture was planted at 100% seeding rate. Whatever establishment exists for a 100% seeding rate is concidered 100% stand count
## For each accession's subplots I took the subplot's stand count and divided it by the same subplot in the monoculture version of the accession. This gives observed seeding rate or stand count as a percentage of monoculture
## The same process was done for peas
## Prct from each subplot is X% stand count
## average the two intercrop X% stand counts
## change monoculture LIE value to X% of full mono value

# ChatGPT code to finish adjusting light values based on stand count
# Oat monoculture adjustment
# Step 1: Get inter means for oat_prcnt by accession × subplot
oat_inter_means <- LiCor %>%
  filter(System == "intercrop", Timepoint == 1) %>%
  group_by(oat_accession_name, subplot_number) %>%
  summarise(mean_oat_prcnt = mean(Oat_Prcnt, na.rm = TRUE), .groups = "drop")
# Step 2: Adjust monoculture interception
oat_mono_adj <- LiCor %>%
  filter(System == "monoculture") %>%
  left_join(oat_inter_means, by = c("oat_accession_name", "subplot_number")) %>%
  mutate(
    adj_light_interception = total_light_interception * mean_oat_prcnt,
    adj_top_light_interception = top_light_interception * mean_oat_prcnt,
    adj_bottom_light_interception = bottom_light_interception * mean_oat_prcnt
  )
# Pea monoculture Adjustment
# Step 1: Get inter means for pea_prcnt by accession × subplot
pea_inter_means <- LiCor %>%
  filter(System == "intercrop", Timepoint == 1) %>%
  group_by(pea_accession_name, subplot_number) %>%
  summarise(mean_pea_prcnt = mean(Pea_Prcnt, na.rm = TRUE), .groups = "drop")
# Step 2: Adjust monoculture interception
pea_mono_adj <- LiCor %>%
  filter(System == "monoculture") %>%
  left_join(pea_inter_means, by = c("pea_accession_name", "subplot_number")) %>%
  mutate(
    adj_light_interception = total_light_interception * mean_pea_prcnt,
    adj_top_light_interception = top_light_interception * mean_pea_prcnt,
    adj_bottom_light_interception = bottom_light_interception * mean_pea_prcnt
  )


# Add Adj oat mono values to data table
LiCor <- LiCor %>%
  left_join(
    oat_mono_adj %>%
      dplyr::select(oat_accession_name, plot_number, subplot_number, Timepoint, adj_light_interception),
    by = c("oat_accession_name", "plot_number", "subplot_number", "Timepoint")
  )
# Add Adj pea mono values to data table
LiCor <- LiCor %>%
  left_join(
    pea_mono_adj %>%
      dplyr::select(pea_accession_name, plot_number, subplot_number, Timepoint, adj_light_interception),
    by = c("pea_accession_name", "plot_number", "subplot_number", "Timepoint")
  )
#combine adjustment columns into one column
LiCor$adj_light_interception <- apply(LiCor[, c("adj_light_interception.x", "adj_light_interception.y")], 1, function(x)
  paste(na.omit(x), collapse = " ")
)

LiCor$adj_light_interception <- as.numeric(LiCor$adj_light_interception)

mono_pea <- LiCor[LiCor$Crop == "pea", ]
mono_oat <- LiCor[LiCor$Crop == "oat", ]
mean_mono_pea_LI <- mean(mono_pea$adj_light_interception, na.rm = TRUE)
mean_mono_oat_LI <- mean(mono_oat$adj_light_interception, na.rm = TRUE)
mean_mono_LI <- mean(c(mono_pea$adj_light_interception,mono_oat$adj_light_interception), na.rm=TRUE)
LIE_adj <- mean_intercrop_LI / mean_mono_LI
LIE_adj_p <- mean_intercrop_LI / mean_mono_pea_LI
LIE_adj_o <- mean_intercrop_LI / mean_mono_oat_LI
# intercrop plots were more efficient at light interception than the monocultures
# intercrops intercepted twice (2.18) as much light as the same stand count of monoculture

# Should intercrop be divided in half to account for the monoculture just being one species?
mean_intercrop_LI2 <- mean((intercrop_data$total_light_interception/2), na.rm = TRUE)
LIE_adj2 <- mean_intercrop_LI2 / mean_mono_LI
LIE_adj_o2 <- mean_intercrop_LI2 / mean_mono_oat_LI
LIE_adj_p2 <- mean_intercrop_LI2 / mean_mono_pea_LI
# intercrop plots were more efficent at light inerception than the monocultures
# intercrops intercepted slightly more light than the monocultures

### Histograms

hist(LiCor$Light_ground)
hist(LiCor$Light_mid)
hist(LiCor$Light_top)
hist(LiCor$total_light_interception)
hist(LiCor$top_light_interception)
hist(LiCor$bottom_light_interception)

### Correlations

# correlation matrix with pairwise complete obs
corr_matrix <- cor(LiCor[, c("Oat_biomass", "Pea_Biomass", "Weed_Biomass",
                             "Oat_Height", "Pea_Height", "total_light_interception",
                             "top_light_interception", "bottom_light_interception",
                             "adj_light_interception")],
  use = "pairwise.complete.obs"
)

corrplot(corr_matrix, method = "number", type = 'upper')

## intercrop plots only

# correlation matrix with pairwise complete obs
corr_matrix_int <- LiCor %>%
  filter(System == "intercrop") %>%
  dplyr::select(
    Oat_biomass, Pea_Biomass, Weed_Biomass,
    Oat_Height, Pea_Height,
    total_light_interception, top_light_interception,
    bottom_light_interception
  ) %>%
  cor(use = "pairwise.complete.obs")

corrplot(corr_matrix_int, method = "number", type = 'upper')

### LM Light Interception
model <- lm(total_light_interception~ plot_number + subplot_number + Timepoint + Crop, data=LiCor)
anova(model)
summary(model)

model2 <- lm(total_light_interception~ plot_number + subplot_number + Timepoint + Crop + oat_accession_name*Timepoint + pea_accession_name*Timepoint, data=LiCor)
anova(model2)
summary(model2)

model3 <- lm(total_light_interception~ plot_number + subplot_number + Timepoint + Crop + plot_number*Timepoint, data=LiCor)
anova(model3)
summary(model3)

AIC(model, model2, model3)

# Pairwise comparisons for Plot
emmeans(model, pairwise ~ plot_number)
# Pairwise comparisons for Subplot
emmeans(model, pairwise ~ subplot_number)

model_aov <- aov(total_light_interception~ plot_number + subplot_number + Timepoint + Crop, data=LiCor)

# differences in light interception between plots
tukey <- glht(model_aov, linfct = mcp(plot_number = "Tukey"))
summary(tukey)
plot(tukey)

# differences in light interception between subplots
tukey <- glht(model_aov, linfct = mcp(subplot_number = "Tukey"))
summary(tukey)
plot(tukey)

### LM Oat Biomass
model <- lm(Oat_biomass~ plot_number + subplot_number + Timepoint + Crop, data=LiCor)
anova(model)
summary(model)

model2 <- lm(Oat_biomass~ plot_number + subplot_number + Timepoint + Crop + oat_accession_name*Timepoint + pea_accession_name*Timepoint, data=LiCor)
anova(model2)
summary(model2)

model3 <- lm(Oat_biomass~ plot_number + subplot_number + Timepoint + Crop + plot_number*Timepoint, data=LiCor)
anova(model3)
summary(model3)

model4 <- lm(Oat_biomass~ plot_number + subplot_number + Timepoint + Crop + total_light_interception, data=LiCor)
anova(model4)
summary(model4)

AIC(model, model2, model4)

################################## Old Stuff ##################################


model <- lm(`Below Canopy Value` ~ Plot + Subplot + `Above Canopy Value`, data=LiCor)
anova(model)
summary(model)




model <- lm(`Below Canopy Value` ~ Plot * Subplot + `Above Canopy Value`, data=LiCor)
anova(model)
summary(model)

model <- lm(`Below Canopy Value` ~ Plot/Subplot + `Above Canopy Value`, data = LiCor)


# Pairwise comparisons for Plot
emmeans(model, pairwise ~ Plot)
# Pairwise comparisons for Subplot
emmeans(model, pairwise ~ Subplot)


library(multcomp)
tukey <- glht(model_aov, linfct = mcp(Subplot = "Tukey"))
summary(tukey)
plot(tukey)



model_fixed <- lm(`Below Canopy Value` ~ Plot * Subplot + `Above Canopy Value`, data = LiCor)
anova(model_fixed)

