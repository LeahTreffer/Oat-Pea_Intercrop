# Data from 2025 NY oat-pea intercrop trials
# Field : Helfner
# 20 plots; these were the first 10 'border' plots on each side of the 400 plot B4I experiment
# 5 oat accessions
# 5 pea accessions
# intercrops and monocrops of oat and pea
# each accession exists has a monoculture and as one intercrop with an accession of the other species
# each plot divided into 3 subplots; subplot 1 always north side and subplot 3 always the south side

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
# data exists elsewhere for plant height, biomass, stand count (taken in Fieldbook, might be uploaded eventually to T3)

################################################################################

# Photosynthetically Active Radiation (PAR) data from a Li-Cor Quantum Sensor
# Above canopy PAR represents the total light available above the crop canopy (i.e., incident light).
# Below canopy PAR indicates the amount of light that passes through the canopy and is available for the plants below it.

# Light Interception: percentage of light that is absorbed by the canopy compared to the incident light
# LI = ((PAR_above - PAR_below) / PAR_above) * 100

# Light Interception Efficiency (LIE): measure of how efficiently the crops intercept light compared to a baseline, such as monocrop systems or theoretical maximum light interception.
# LIE = mean_intercrop_LI / mean_monocrop_LI

################################################################################

library(readxl)
library(ggplot2)

LiCor <- read_excel("data/T1_T2_T3_light_interception.xlsx", sheet = 'Long')

LiCor$plot_number <- as.factor(LiCor$plot_number)
LiCor$subplot_number <- as.factor(LiCor$subplot_number)
LiCor$Timepoint <- as.factor(LiCor$Timepoint)
LiCor$Light_ground <- as.numeric(LiCor$Light_ground)
LiCor$Light_mid <- as.numeric(LiCor$Light_mid)
LiCor$Light_top <- as.numeric(LiCor$Light_top)

LiCor$total_light_interception <- ((LiCor$Light_top - LiCor$Light_ground) / LiCor$Light_top) * 100
LiCor$top_light_interception <- ((LiCor$Light_top - LiCor$Light_mid) / LiCor$Light_top) * 100
LiCor$bottom_light_interception <- ((LiCor$Light_mid - LiCor$Light_ground) / LiCor$Light_mid) * 100

intercrop_data <- LiCor[LiCor$System == "intercrop", ]
monocrop_data <- LiCor[LiCor$System == "monoculture", ]
mono_pea <- LiCor[LiCor$Crop == "pea", ]
mono_oat <- LiCor[LiCor$Crop == "oat", ]
mean_intercrop_LI <- mean(intercrop_data$total_light_interception, na.rm = TRUE)
mean_monocrop_LI <- mean(monocrop_data$total_light_interception, na.rm = TRUE)
mean_mono_pea_LI <- mean(mono_pea$total_light_interception, na.rm = TRUE)
mean_mono_oat_LI <- mean(mono_oat$total_light_interception, na.rm = TRUE)
LIE <- mean_intercrop_LI / mean_monocrop_LI
LIE_p <- mean_intercrop_LI / mean_mono_pea_LI
LIE_o <- mean_intercrop_LI / mean_mono_oat_LI
# intercrops intercept more light on average than monocultures
## intercrops intercept more light than pea monocultures, but less light than oat monocultures

hist(LiCor$Light_ground)
hist(LiCor$Light_mid)
hist(LiCor$Light_top)
hist(LiCor$total_light_interception)
hist(LiCor$top_light_interception)
hist(LiCor$bottom_light_interception)



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

