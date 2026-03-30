# Data from 2025 NY oat-pea intercrop trials
# Field : Helfner, Ithaca NY
# 20 plots; these were the first 10 'border' plots on each side of the 400 plot B4I experiment
# 5 oat accessions
# 5 pea accessions
# intercrops and monocrops of oat and pea
# each accession exists has a monoculture and as one intercrop with an accession of the other species
# each plot divided into 3 subplots; subplot 1 always north side and subplot 3 always the south side
# seeding rate for the monocultures was 54g oat mono, 8 seed per square ft pea mono
# intercrops were planted at a percentage of those seeding rates: 40% oat and 60% pea
# biomass samples taken using quadrant, harvest quadrant area from one subplot of each plot per timepoint
# Timepoint 1 : June 2-3 2025
# Timepoint 2 : June 24-25 2025
# Timepoint 3 : July 14-15 2025

# PAR measurements taken using a LiCor LI-188B
# Integration = 10
# Range = 100x
# Quantum Sensor Unit: µmol PAR m-2 s-1
# sensor slowly moved horizontally through canopy within each subplot
# PAR measurement taken with sensor on the ground and above canopy
# in T2 and T3 another PAR measurement was taken _ cm from the ground
# At each timepoint, all three subplots had PAR measurments taken across subplot area (if biomass existed in the subplot; since one subplot was harvested at each timepoint, proceeding timepoints had one subplot per plot fewer available to measure)
# measurements taken by Leah and recorded by hand (T1), and in Fieldbook (T2,T3) by Mirza

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

# Data taken at the subplot level
# Are these independent or repeated measures?
# hierarchical (nested) data

################################################################################

# Photosynthetically Active Radiation (PAR) data from a Li-Cor Quantum Sensor
# Above canopy PAR represents the total light available above the crop canopy (i.e., incident light).
# Below canopy PAR indicates the amount of light that passes through the canopy and is available for the plants below it.

# Light Interception (LI): percentage of light that is absorbed by the canopy compared to the incident light
# LI = ((PAR_above - PAR_below) / PAR_above) * 100

# Light Interception Efficiency (LIE): measure of how efficiently the crops intercept light compared to a baseline, such as monocrop systems or theoretical maximum light interception.
# LIE = mean_intercrop_LI / mean_monocrop_LI

################################################################################

# Not calculated but of interest for future analysis found in Pan et al (2022) https://doi.org/10.1007/s42729-021-00676-w
# Capture Ratio (CR)
# CR = (top incidence - top reflection - bottom incidence + bottom reflection)/top incidence

# Reflection Ratio (RR)
# RR = bottom incidence / top incidence

# Penetration Ratio (PR)
# PR = 1 - CR - RR

# Another paper to read for analysis methods: Gu et al (2025) DOI 10.1002/jsfa.70004

################################################################################

library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(corrplot)
library(lmerTest)
library(lme4)
library(multcomp)
library(emmeans)
library(here)
library(MASS)
library(flextable)
library(tibble)

# Function to convert p-values to significance stars
p_to_stars <- function(p) {
  case_when(
    is.na(p) ~ "",
    p < 0.001 ~ "***",
    p < 0.01  ~ "**",
    p < 0.05  ~ "*",
    p < 0.1   ~ ".",
    TRUE      ~ ""
  )
}

here::i_am("code/2025_LiCor.R")

LiCor <- read_excel(here("data", "T1_T2_T3_light_interception.xlsx"), sheet = 'Long')

LiCor$plot_number <- as.factor(LiCor$plot_number)
LiCor$subplot_number <- as.factor(LiCor$subplot_number)
LiCor$Timepoint <- as.factor(LiCor$Timepoint)
LiCor$Crop <- as.factor(LiCor$Crop)
LiCor <- LiCor %>%
  mutate(oat_accession_name = na_if(oat_accession_name, "NO_OATS_PLANTED"))%>%
  mutate(pea_accession_name = na_if(pea_accession_name, "NO_PEAS_PLANTED"))
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
## change monoculture LIE value to X% of full mono value (if monoculture had the same stand count as an intercrop plot, what would the LIE be)

# ChatGPT code to finish adjusting light values based on stand count
# Oat monoculture adjustment
# Step 1: Get intercrop plot means for oat_prcnt by accession × subplot
oat_inter_means <- LiCor %>%
  filter(System == "intercrop", Timepoint == 1) %>%
  group_by(oat_accession_name, subplot_number) %>%
  summarise(mean_oat_prcnt = mean(Oat_Prcnt, na.rm = TRUE), .groups = "drop") # average stand count (% of mono count)
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
# Step 1: Get intercrop means for pea_prcnt by accession × subplot
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
      dplyr::select(oat_accession_name, plot_number, subplot_number, Timepoint, adj_light_interception, adj_top_light_interception, adj_bottom_light_interception),
    by = c("oat_accession_name", "plot_number", "subplot_number", "Timepoint")
  )
# Add Adj pea mono values to data table
LiCor <- LiCor %>%
  left_join(
    pea_mono_adj %>%
      dplyr::select(pea_accession_name, plot_number, subplot_number, Timepoint, adj_light_interception, adj_top_light_interception, adj_bottom_light_interception),
    by = c("pea_accession_name", "plot_number", "subplot_number", "Timepoint")
  )
#combine adjustment columns into one column
LiCor$adj_light_interception <- apply(LiCor[, c("adj_light_interception.x", "adj_light_interception.y")], 1, function(x)
  paste(na.omit(x), collapse = " ")
)
LiCor$adj_top_light_interception <- apply(LiCor[, c("adj_top_light_interception.x", "adj_top_light_interception.y")], 1, function(x)
  paste(na.omit(x), collapse = " ")
)
LiCor$adj_bottom_light_interception <- apply(LiCor[, c("adj_bottom_light_interception.x", "adj_bottom_light_interception.y")], 1, function(x)
  paste(na.omit(x), collapse = " ")
)

# combine adjusted mono values and true intercrop values (intercrop values were not adjusted; monoculture was adjusted to intercrop)
LiCor$adjTotLI <- ifelse(!is.na(LiCor$adj_light_interception) & LiCor$adj_light_interception != "", LiCor$adj_light_interception, LiCor$total_light_interception)
LiCor$adjTopLI <- ifelse(!is.na(LiCor$adj_top_light_interception) & LiCor$adj_top_light_interception != "", LiCor$adj_top_light_interception, LiCor$top_light_interception)
LiCor$adjBtmLI <- ifelse(!is.na(LiCor$adj_bottom_light_interception) & LiCor$adj_bottom_light_interception != "", LiCor$adj_bottom_light_interception, LiCor$bottom_light_interception)

LiCor$adjTotLI <- as.numeric(LiCor$adjTotLI)
LiCor$adjTopLI <- as.numeric(LiCor$adjTopLI)
LiCor$adjBtmLI <- as.numeric(LiCor$adjBtmLI)

mono_pea <- LiCor[LiCor$Crop == "pea", ]
mono_oat <- LiCor[LiCor$Crop == "oat", ]
mean_mono_pea_LI <- mean(mono_pea$adjTotLI, na.rm = TRUE)
mean_mono_oat_LI <- mean(mono_oat$adjTotLI, na.rm = TRUE)
mean_mono_LI <- mean(c(mono_pea$adjTotLI,mono_oat$adjTotLI), na.rm=TRUE)
LIE_adj <- mean_intercrop_LI / mean_mono_LI
LIE_adj_o <- mean_intercrop_LI / mean_mono_oat_LI
LIE_adj_p <- mean_intercrop_LI / mean_mono_pea_LI
# intercrop plots were more efficient at light interception than the monocultures
# intercrops intercepted twice (2.18) as much light as the same stand count of monoculture
# true regardless of monoculture species; intercrops intercepted more than oats (LIE=2.22) and peas (LIE=2.14)

# Should intercrop be divided in half to account for the monoculture just being one species?
# i don't think so unless interested in interception from one species specifically; I adjusted monoculture LI values to match stand counts of intercrop
#mean_intercrop_LI2 <- mean((intercrop_data$total_light_interception/2), na.rm = TRUE)
#LIE_adj2 <- mean_intercrop_LI2 / mean_mono_LI
#LIE_adj_o2 <- mean_intercrop_LI2 / mean_mono_oat_LI
#LIE_adj_p2 <- mean_intercrop_LI2 / mean_mono_pea_LI
# intercrop plots were slightly more efficent at light inerception than the monocultures (LIE = 1.09)
# intercrop plots intercepted more light than either species component in monocultures; oat LIE 1.11 and pea LIE 1.07

### Histograms

hist(LiCor$Light_ground)
hist(LiCor$Light_mid)
hist(LiCor$Light_top)
hist(LiCor$total_light_interception)
hist(LiCor$top_light_interception)
hist(LiCor$bottom_light_interception)
hist(LiCor$adjTotLI)
hist(LiCor$adjTopLI)
hist(LiCor$adjBtmLI)

shapiro.test(LiCor$adjTotLI)
shapiro.test(LiCor$adjTopLI)
shapiro.test(LiCor$adjBtmLI)

# not normally distributed and log and sqrt transformations don't improve it
# need to figure this out

###### Correlations
## all plots - intercrop and monocultures
# correlation matrix with pairwise complete obs
corr_matrix <- cor(LiCor[, c("Oat_biomass", "Pea_Biomass", "Weed_Biomass",
                             "Oat_Height", "Pea_Height", "adjTotLI",
                             "adjTopLI", "adjBtmLI")],
  use = "pairwise.complete.obs"
)

corrplot(corr_matrix, method = "number", type = 'upper')

LiCor %>%
  ggplot(aes(x = Oat_biomass, y = adjTotLI)) +
  geom_point(aes(color = Timepoint, shape = Crop), size = 3.5)+
  labs(x = "Oat Biomass (g)", y = "Total Light Interception (%)") +
  theme(axis.title = element_text(size = 16),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))

## intercrop plots only
# correlation matrix with pairwise complete obs
corr_matrix_int <- LiCor %>%
  filter(System == "intercrop") %>%
  dplyr::select(
    Oat_biomass, Pea_Biomass, Weed_Biomass,
    Oat_Height, Pea_Height,
    adjTotLI, adjTopLI,
    adjBtmLI
  ) %>%
  cor(use = "pairwise.complete.obs")

corrplot(corr_matrix_int, method = "number", type = 'upper')
# expected positive correlation of biomass and height
# negative correlation for oat height and LI in bottom half of stand (as oats got taller, less light was intercepted in the lower portion of the canopy)
# negative correlation for both species biomass and LI in bottom half of stand (as crop biomass increased, less light was intercepted in the lower portion of the canopy)
# positive correlation for LI in both sections of the canopy and total LI (more light interception in any part of the canopy increases total LI)
# slight negative correlation for top and bottom LI (as more light is intercepted by the top, less is intersepted by the bottom - perhaps those leaves are senesing and/or less light even getting down there)
# interesting that there is a slight positive correlation of weed biomass and total light interception (perhaps weed biomass contributed to canopy LI rather than oat and pea out-competing weeds/ biomass shading out weeds)
# interesting that correlations weren't higher
library(ggpmisc)
LiCor %>%
  filter(System == "intercrop") %>%
  ggplot(aes(x = Oat_biomass, y = adjTotLI)) +
  geom_point(aes(color = Timepoint), size = 3)+
  geom_smooth(aes(color = Timepoint), method = "lm", se = FALSE) +
#  stat_poly_eq(
#    aes(label = ..rr.label.., group = Timepoint),
#    formula = y ~ x,
#    parse = TRUE,
#    label.y.npc = c(90, 80, 70)  # one per Timepoint
#  ) +
  labs(x = "Oat Biomass (g)", y = "Total Light Interception (%)") +
  theme(axis.title = element_text(size = 16),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))
# not much change in LI across biomass by weight at time point 2 and 3
# for intercrop plots, variation for LI present in time point 1


### Difference in light interception by cropping system?
# Cropping System by timepoint
modelCROP <- lmer(adjTotLI ~ Timepoint * Crop + (1|plot_number) + (1|plot_number:subplot_number), data=LiCor)
  CROP_resid<-resid(modelCROP)
  qqnorm(CROP_resid)
  qqline(CROP_resid)
  plot(modelCROP)
anova(modelCROP)
aov.table <- anova(modelCROP)
table = flextable(data = aov.table %>%
                        #filter(`Pr(>F)` < 0.05) %>%
                        mutate(`Pr(>F)` = as.numeric(`Pr(>F)`),          # ensure numeric
                                Signif = p_to_stars(`Pr(>F)`),            # add stars column
                                `Pr(>F)` = formatC(`Pr(>F)`, digits = 3))%>%  # format p-value nicely %>%
                        dplyr::select(`Pr(>F)`,Signif) %>%
                        rename(`  ` = Signif) %>%
                        rownames_to_column(" "))
print(table)

# Boxplot for Light Interception by cropping system
custom_letters <- c("a", "b", "c", "d", "e", "f", "g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z")  # Customize the letters as needed
emms <- emmeans(modelCROP, ~ Crop*Timepoint, type = "response") # Obtain estimated marginal means
emms_df <- as.data.frame(summary(emms))
cld_data <- cld(emms, Letters=custom_letters)
cld_df <-as.data.frame(cld_data)
merged_df <- merge(emms_df, cld_df,by = c("Timepoint", "Crop"))

totLI_CROP <- ggplot(LiCor, aes(x = Crop, y = adjTotLI, colour = Timepoint)) +
  geom_boxplot(position = position_dodge(width = 0.8)) +
  geom_text(data = merged_df, aes(x = Crop, y = emmean.x, colour = Timepoint, label = .group),
            position = position_dodge(width = 0.8),
            vjust = -0.5,  # slightly above each box
            size = 5,      # font size
            #hjust = 1.5)
            )+
  labs(title = "Relationship of Total Light Interception and Cropping System", x = "Cropping System", y = "Light Interception (%)")+
  theme(axis.text=element_text(size=14), #change font size of axis text
        axis.title=element_text(size=18))
# switch visual x axis and fill
totLI_CROP <- ggplot(LiCor, aes(x = Timepoint, y = adjTotLI, colour = Crop)) +
  geom_boxplot(position = position_dodge(width = 0.8)) +
  geom_text(data = merged_df, aes(x = Timepoint, y = emmean.x, colour = Crop, label = .group),
            position = position_dodge(width = 0.8),
            vjust = -0.5,  # slightly above each box
            size = 5)+      # font size
  labs(title = "Relationship of Total Light Interception to Cropping System and Timepoint", x = "Timepoint", y = "Light Interception (%)")+
  theme(axis.text=element_text(size=14), #change font size of axis text
        axis.title=element_text(size=18))
# intercrop LI always higher than mono LI at all timepoints
## higher in T2 and T3 than in T1, but no difference between the LI in T2 and T3
# no difference in pea LI vs oat LI within a timepoint
## oat mono LI different (higher) in T2 than T1 or T3
## pea mono LI different (higher) in T2 and T3 than in T1, which no difference between T2 and T3


# treat subplot as technical replicates
# average subplots for each plot within timepoint
LiCor2 <- LiCor %>%
  group_by(Timepoint, plot_number, Crop)%>%
  summarize(plotlvl_tot_LI = mean(adjTotLI, na.rm=TRUE))

modelcrop <- lmer(plotlvl_tot_LI ~ Timepoint * Crop + (1|plot_number), data=LiCor2)
  crop_resid<-resid(modelcrop)
  qqnorm(crop_resid)
  qqline(crop_resid)
  plot(modelcrop)
AIC(modelcrop,modelCROP)
# actually this fits a lot better
anova(modelcrop)
aov.table <- anova(modelcrop)
table = flextable(data = aov.table %>%
                    #filter(`Pr(>F)` < 0.05) %>%
                    mutate(`Pr(>F)` = as.numeric(`Pr(>F)`),          # ensure numeric
                           Signif = p_to_stars(`Pr(>F)`),            # add stars column
                           `Pr(>F)` = formatC(`Pr(>F)`, digits = 3))%>%  # format p-value nicely %>%
                    dplyr::select(`Pr(>F)`,Signif) %>%
                    rename(`  ` = Signif) %>%
                    rownames_to_column(" "))
print(table)

custom_letters <- c("a", "b", "c", "d", "e", "f", "g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z")  # Customize the letters as needed
emms <- emmeans(modelcrop, ~ Crop*Timepoint, type = "response") # Obtain estimated marginal means
emms_df <- as.data.frame(summary(emms))
cld_data <- cld(emms, Letters=custom_letters)
cld_df <-as.data.frame(cld_data)
merged_df <- merge(emms_df, cld_df,by = c("Timepoint", "Crop"))
totLI_crop <- ggplot(LiCor2, aes(x = Timepoint, y = plotlvl_tot_LI, colour = Crop)) +
  geom_boxplot(position = position_dodge(width = 0.8)) +
  geom_text(data = merged_df, aes(x = Timepoint, y = emmean.x, colour = Crop, label = .group),
            position = position_dodge(width = 0.8),
            vjust = -0.5,  # slightly above each box
            size = 5)+      # font size
  labs(title = "Relationship of Total Light Interception to Cropping System and Timepoint", x = "Timepoint", y = "Light Interception (%)")+
  theme(axis.text=element_text(size=14), #change font size of axis text
        axis.title=element_text(size=18))



### Difference of biomass
# Oat_biomass
# Pea_Biomass
# Weed_Biomass
# transformed because normal broke equal variance assumption

LiCor2 <- LiCor %>%
  group_by(plot_number, Timepoint) %>%
  summarise(
    plotlvl_tot_LI = mean(adjTotLI, na.rm = TRUE),
    across(c(Oat_biomass, Pea_Biomass, Weed_Biomass, Crop),
           ~ if (all(is.na(.))) NA else first(na.omit(.))),
    .groups = "drop"
)

modeloat<- lmer(log(Oat_biomass) ~ plotlvl_tot_LI * Timepoint * Crop + (1|plot_number), data=LiCor2)
  oat_resid<-resid(modeloat)
  qqnorm(oat_resid)
  qqline(oat_resid)
  plot(modeloat)
anova(modeloat)
# log transformed: oat biomass differed by Timepoint, almost sig three way interaction
# linear: no effects

modelpea<- lmer(log(Pea_Biomass) ~ plotlvl_tot_LI * Timepoint * Crop + (1|plot_number), data=LiCor2)
  pea_resid<-resid(modelpea)
  qqnorm(pea_resid)
  qqline(pea_resid)
  plot(modelpea)
anova(modelpea)

modelweed <- lmer(sqrt(Weed_Biomass) ~ plotlvl_tot_LI * Timepoint * Crop + (1|plot_number), data=LiCor2)
  weed_resid<-resid(modelweed)
  qqnorm(weed_resid)
  qqline(weed_resid)
  plot(modelweed)
anova(modelweed)
# weed biomass almost significant different by Timepoint * Crop

# total biomass
LiCor3 <- LiCor2 %>%
  mutate(totBiomass = rowSums(cbind(Oat_biomass, Pea_Biomass), na.rm = TRUE))
modelbiomass <- lmer(log(totBiomass) ~ plotlvl_tot_LI * Timepoint * Crop + (1|plot_number), data=LiCor3)
  bio_resid<-resid(modelbiomass)
  qqnorm(bio_resid)
  qqline(bio_resid)
  plot(modelbiomass)
anova(modelbiomass)
# timepoint is significant


# Oat Biomass T
custom_letters <- c("a", "b", "c", "d", "e", "f", "g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z")  # Customize the letters as needed
emms <- emmeans(modeloat, ~ Timepoint, type = "response") # Obtain estimated marginal means
emms_df <- as.data.frame(summary(emms))
cld_data <- cld(emms, Letters=custom_letters)
cld_df <-as.data.frame(cld_data)
merged_df <- merge(emms_df, cld_df,by = c("Timepoint"))
totLI_oatT <- ggplot(LiCor2, aes(x = Timepoint, y = Oat_biomass)) +
  geom_boxplot(position = position_dodge(width = 0.8)) +
  geom_text(data = merged_df, aes(x = Timepoint, y = response.x, label = .group),
            position = position_dodge(width = 0.8),
            vjust = -0.5,  # slightly above each box
            size = 5)+      # font size
  labs(title = "Relationship of Oat Biomass and Timepoint", x = "Timepoint", y = "Oat Biomass (g)")+
  theme(axis.text=element_text(size=14), #change font size of axis text
        axis.title=element_text(size=18))
# Oat Biomass T:L:C (not significant)
totLI_oatTLC <- ggplot(LiCor2) +
  geom_point(aes(x = plotlvl_tot_LI, y = Oat_biomass, shape = Crop), size=5) +
  facet_wrap(~Timepoint, scales = "fixed", labeller = label_both, ncol = 3) +
  scale_shape_manual(values = c(1, 18, 12),labels = c("Oat Monoculture","Pea Monoculture", "Intercrop"))+
  labs(title = "Relationship of Oat Biomass to Total Light Interception to Cropping System and Timepoint", x = "Light Interception (%)", y = "Oat Biomass (g)")+
  theme(axis.text=element_text(size=14), #change font size of axis text
        axis.title=element_text(size=18),
        legend.text=element_text(size=12), #change font size of legend text
        legend.title=element_text(size=16), #change font size of legend title
        strip.text.x = element_text(size = 18),
        legend.key.width = unit(1.5, "cm")) #change font size of facet title)

# Weed Biomass T:C (not significant)
totLI_weedTC <- ggplot(LiCor2) +
  geom_boxplot(aes(x = Timepoint, y = Weed_Biomass, fill = Crop)) +
  scale_fill_manual(values = c("#DDCC77", "#882255", "#117733"),labels = c("Oat Monoculture","Pea Monoculture", "Intercrop"))+
  labs(title = "Relationship of Weed Biomass and Timepoint Cropping System", x = "Timepoint", y = "Weed Biomass (g)")+
  theme(axis.text=element_text(size=14), #change font size of axis text
        axis.title=element_text(size=18),
        legend.text=element_text(size=12), #change font size of legend text
        legend.title=element_text(size=16), #change font size of legend title
        legend.key.width = unit(1.5, "cm")) #change font size of facet title)

# Total Biomass T
emms <- emmeans(modelbiomass, ~ Timepoint, type = "response") # Obtain estimated marginal means
emms_df <- as.data.frame(summary(emms))
cld_data <- cld(emms, Letters=custom_letters)
cld_df <-as.data.frame(cld_data)
merged_df <- merge(emms_df, cld_df,by = c("Timepoint"))
totLI_oatT <- ggplot(LiCor3, aes(x = Timepoint, y = totBiomass)) +
  geom_boxplot(position = position_dodge(width = 0.8)) +
  geom_text(data = merged_df, aes(x = Timepoint, y = response.x, label = .group),
            position = position_dodge(width = 0.8),
            vjust = -0.5,  # slightly above each box
            size = 5)+      # font size
  labs(title = "Relationship of Total Crop Biomass and Timepoint", x = "Timepoint", y = "Biomass (g)")+
  theme(axis.text=element_text(size=14), #change font size of axis text
        axis.title=element_text(size=18))


## Linear Models of each Timepoint Individually

LiCorT1 <- LiCor2 %>% filter(Timepoint=="1")

modeloatT1<- lm(Oat_biomass ~ plotlvl_tot_LI * Crop, data=LiCorT1)
  oaT1_resid<-resid(modeloatT1)
  qqnorm(oaT1_resid)
  qqline(oaT1_resid)
  #plot(modeloatT1)
anova(modeloatT1)
# light interception and cropping system were significant individually but not as an interaction

modelpeaT1<- lm(Pea_Biomass ~ plotlvl_tot_LI * Crop, data=LiCorT1)
  peat1_resid<-resid(modelpeaT1)
  qqnorm(peat1_resid)
  qqline(peat1_resid)
  #plot(modelpeaT1)
anova(modelpeaT1)
# light interception was significant

modelweedT1<- lm(Weed_Biomass ~ plotlvl_tot_LI * Crop, data=LiCorT1)
  weedT1_resid<-resid(modelweedT1)
  qqnorm(weedT1_resid)
  qqline(weedT1_resid)
  #plot(modelweedT1)
anova(modelweedT1)
# LI * Crop interaction was significant

LiCorT2 <- LiCor2 %>% filter(Timepoint=="2")

modeloatT2<- lm(Oat_biomass ~ plotlvl_tot_LI * Crop, data=LiCorT2)
  oaT2_resid<-resid(modeloatT2)
  qqnorm(oaT2_resid)
  qqline(oaT2_resid)
  #plot(modeloatT2)
anova(modeloatT2)
# cropping system was significant

modelpeaT2<- lm(Pea_Biomass ~ plotlvl_tot_LI * Crop, data=LiCorT2)
  pea1_resid<-resid(modelpeaT2)
  qqnorm(pea1_resid)
  qqline(pea1_resid)
  #plot(modelpeaT2)
anova(modelpeaT2)
# light interception was significant

modelweedT2<- lm(Weed_Biomass ~ plotlvl_tot_LI * Crop, data=LiCorT2)
  weed1_resid<-resid(modelweedT2)
  qqnorm(weed1_resid)
  qqline(weed1_resid)
  #plot(modelweedT2)
anova(modelweedT2)
# LI was significant

LiCorT3 <- LiCor2 %>% filter(Timepoint=="3")

modeloatT3<- lm(Oat_biomass ~ plotlvl_tot_LI * Crop, data=LiCorT3)
  oaT3_resid<-resid(modeloatT3)
  qqnorm(oaT3_resid)
  qqline(oaT3_resid)
  #plot(modeloatT3)
anova(modeloatT3)
# LI was significant

modelpeaT3<- lm(Pea_Biomass ~ plotlvl_tot_LI * Crop, data=LiCorT3)
  peaT3_resid<-resid(modelpeaT3)
  qqnorm(peaT3_resid)
  qqline(peaT3_resid)
  #plot(modelpeaT3)
anova(modelpeaT3)
# cropping system was significant

modelweedT3<- lm(Weed_Biomass ~ plotlvl_tot_LI * Crop, data=LiCorT3)
  weedT3_resid<-resid(modelweedT3)
  qqnorm(weedT3_resid)
  qqline(weedT3_resid)
  #plot(modelweedT3)
anova(modelweedT3)
# cropping system was significant



aov.tableO1 <- anova(modeloatT1)
tableO1 = flextable(data = aov.tableO1 %>%
                      #filter(`Pr(>F)` < 0.05) %>%
                      mutate(`Pr(>F)` = as.numeric(`Pr(>F)`),          # ensure numeric
                             Signif = p_to_stars(`Pr(>F)`),            # add stars column
                             `Pr(>F)` = formatC(`Pr(>F)`, digits = 3))%>%  # format p-value nicely %>%
                      dplyr::select(`Pr(>F)`,Signif) %>%
                      rename(`  ` = Signif) %>%
                      rownames_to_column("term")%>%
                      dplyr::mutate(term = dplyr::case_when(
                        term == "plotlvl_tot_LI" ~ "Total LI",
                        term == "Crop" ~ "Cropping System",
                        term == "plotlvl_tot_LI:Crop" ~ "Total LI:Cropping System")))%>%
                      set_header_labels(term = "")
print(tableO1)

aov.tableO2 <- anova(modeloatT2)
tableO2 = flextable(data = aov.tableO2 %>%
                      #filter(`Pr(>F)` < 0.05) %>%
                      mutate(`Pr(>F)` = as.numeric(`Pr(>F)`),          # ensure numeric
                             Signif = p_to_stars(`Pr(>F)`),            # add stars column
                             `Pr(>F)` = formatC(`Pr(>F)`, digits = 3))%>%  # format p-value nicely %>%
                      dplyr::select(`Pr(>F)`,Signif) %>%
                      rename(`  ` = Signif) %>%
                      rownames_to_column("term")%>%
                      dplyr::mutate(term = dplyr::case_when(
                        term == "plotlvl_tot_LI" ~ "Total LI",
                        term == "Crop" ~ "Cropping System",
                        term == "plotlvl_tot_LI:Crop" ~ "Total LI:Cropping System")))%>%
                      set_header_labels(term = "")
print(tableO2)

aov.tableO3 <- anova(modeloatT3)
tableO3 = flextable(data = aov.tableO3 %>%
                      #filter(`Pr(>F)` < 0.05) %>%
                      mutate(`Pr(>F)` = as.numeric(`Pr(>F)`),          # ensure numeric
                             Signif = p_to_stars(`Pr(>F)`),            # add stars column
                             `Pr(>F)` = formatC(`Pr(>F)`, digits = 3))%>%  # format p-value nicely %>%
                      dplyr::select(`Pr(>F)`,Signif) %>%
                      rename(`  ` = Signif) %>%
                      rownames_to_column("term")%>%
                      dplyr::mutate(term = dplyr::case_when(
                        term == "plotlvl_tot_LI" ~ "Total LI",
                        term == "Crop" ~ "Cropping System",
                        term == "plotlvl_tot_LI:Crop" ~ "Total LI:Cropping System")))%>%
  set_header_labels(term = "")
print(tableO3)


aov.tableP1 <- anova(modelpeaT1)
tableP1 = flextable(data = aov.tableP1 %>%
                      #filter(`Pr(>F)` < 0.05) %>%
                      mutate(`Pr(>F)` = as.numeric(`Pr(>F)`),          # ensure numeric
                             Signif = p_to_stars(`Pr(>F)`),            # add stars column
                             `Pr(>F)` = formatC(`Pr(>F)`, digits = 3))%>%  # format p-value nicely %>%
                      dplyr::select(`Pr(>F)`,Signif) %>%
                      rename(`  ` = Signif) %>%
                      rownames_to_column("term")%>%
                      dplyr::mutate(term = dplyr::case_when(
                        term == "plotlvl_tot_LI" ~ "Total LI",
                        term == "Crop" ~ "Cropping System",
                        term == "plotlvl_tot_LI:Crop" ~ "Total LI:Cropping System")))%>%
  set_header_labels(term = "")
print(tableP1)

aov.tableP2 <- anova(modelpeaT2)
tableP2 = flextable(data = aov.tableP2 %>%
                      #filter(`Pr(>F)` < 0.05) %>%
                      mutate(`Pr(>F)` = as.numeric(`Pr(>F)`),          # ensure numeric
                             Signif = p_to_stars(`Pr(>F)`),            # add stars column
                             `Pr(>F)` = formatC(`Pr(>F)`, digits = 3))%>%  # format p-value nicely %>%
                      dplyr::select(`Pr(>F)`,Signif) %>%
                      rename(`  ` = Signif) %>%
                      rownames_to_column("term")%>%
                      dplyr::mutate(term = dplyr::case_when(
                        term == "plotlvl_tot_LI" ~ "Total LI",
                        term == "Crop" ~ "Cropping System",
                        term == "plotlvl_tot_LI:Crop" ~ "Total LI:Cropping System")))%>%
  set_header_labels(term = "")
print(tableP2)

aov.tableP3 <- anova(modelpeaT3)
tableP3 = flextable(data = aov.tableP3 %>%
                      #filter(`Pr(>F)` < 0.05) %>%
                      mutate(`Pr(>F)` = as.numeric(`Pr(>F)`),          # ensure numeric
                             Signif = p_to_stars(`Pr(>F)`),            # add stars column
                             `Pr(>F)` = formatC(`Pr(>F)`, digits = 3))%>%  # format p-value nicely %>%
                      dplyr::select(`Pr(>F)`,Signif) %>%
                      rename(`  ` = Signif) %>%
                      rownames_to_column("term")%>%
                      dplyr::mutate(term = dplyr::case_when(
                        term == "plotlvl_tot_LI" ~ "Total LI",
                        term == "Crop" ~ "Cropping System",
                        term == "plotlvl_tot_LI:Crop" ~ "Total LI:Cropping System")))%>%
  set_header_labels(term = "")
print(tableP3)


aov.tableW1 <- anova(modelweedT1)
tableW1 = flextable(data = aov.tableW1 %>%
                      #filter(`Pr(>F)` < 0.05) %>%
                      mutate(`Pr(>F)` = as.numeric(`Pr(>F)`),          # ensure numeric
                             Signif = p_to_stars(`Pr(>F)`),            # add stars column
                             `Pr(>F)` = formatC(`Pr(>F)`, digits = 3))%>%  # format p-value nicely %>%
                      dplyr::select(`Pr(>F)`,Signif) %>%
                      rename(`  ` = Signif) %>%
                      rownames_to_column("term")%>%
                      dplyr::mutate(term = dplyr::case_when(
                        term == "plotlvl_tot_LI" ~ "Total LI",
                        term == "Crop" ~ "Cropping System",
                        term == "plotlvl_tot_LI:Crop" ~ "Total LI:Cropping System")))%>%
  set_header_labels(term = "")
print(tableW1)

aov.tableW2 <- anova(modelweedT2)
tableW2 = flextable(data = aov.tableW2 %>%
                      #filter(`Pr(>F)` < 0.05) %>%
                      mutate(`Pr(>F)` = as.numeric(`Pr(>F)`),          # ensure numeric
                             Signif = p_to_stars(`Pr(>F)`),            # add stars column
                             `Pr(>F)` = formatC(`Pr(>F)`, digits = 3))%>%  # format p-value nicely %>%
                      dplyr::select(`Pr(>F)`,Signif) %>%
                      rename(`  ` = Signif) %>%
                      rownames_to_column("term")%>%
                      dplyr::mutate(term = dplyr::case_when(
                        term == "plotlvl_tot_LI" ~ "Total LI",
                        term == "Crop" ~ "Cropping System",
                        term == "plotlvl_tot_LI:Crop" ~ "Total LI:Cropping System")))%>%
  set_header_labels(term = "")
print(tableW2)

aov.tableW3 <- anova(modelweedT3)
tableW3 = flextable(data = aov.tableW3 %>%
                      #filter(`Pr(>F)` < 0.05) %>%
                      mutate(`Pr(>F)` = as.numeric(`Pr(>F)`),          # ensure numeric
                             Signif = p_to_stars(`Pr(>F)`),            # add stars column
                             `Pr(>F)` = formatC(`Pr(>F)`, digits = 3))%>%  # format p-value nicely %>%
                      dplyr::select(`Pr(>F)`,Signif) %>%
                      rename(`  ` = Signif) %>%
                      rownames_to_column("term")%>%
                      dplyr::mutate(term = dplyr::case_when(
                        term == "plotlvl_tot_LI" ~ "Total LI",
                        term == "Crop" ~ "Cropping System",
                        term == "plotlvl_tot_LI:Crop" ~ "Total LI:Cropping System")))%>%
  set_header_labels(term = "")
print(tableW3)

table_merge <- cbind(aov.tableW1, aov.tableW2, aov.tableW3)
colnames(table_merge) <- make.unique(colnames(table_merge))
table_merge

my_modelsNG <- c(modeloatT1, modeloatT2, modeloatT3, modelpeaT1, modelpeaT2, modelpeaT3,modelweedT1, modelweedT2, modelweedT3)
anovasNG <- purrr::map(my_modelsNG, ~anova(.))
names(anovasNG) <- c("Year 1 Summer", "Year 1 Fall", "Year 2 Summer", "Year 2 Fall", "Year 3 Summer", "Year 3 Fall")

# Function to format p-values as stars
format_p_value <- function(p_val) {
  stars <- ifelse(p_val < 0.001, "***", ifelse(p_val < 0.01, "**", ifelse(p_val < 0.05, "*", "")))
  return(stars)
}

# Apply the function to your ANOVA results
NG_stars_tibble <- map(anovasNG, as_tibble, rownames="term") %>%
  bind_rows(.id = "harvest") %>%
  mutate(stars = map_chr(`Pr(>F)`, format_p_value))

NG_stars_tibble <- NG_stars_tibble %>%
  mutate(term = ifelse(term == "P", "E", term)) %>%
  #  mutate(term = ifelse(term == "N", "Nitrogen Rate (N)", term)) %>%
  mutate(term = ifelse(term == "CROP", "C", term)) %>%
  mutate(term = ifelse(term == "N:P", "N:E", term))%>%
  mutate(term = ifelse(term == "P:CROP", "E:C", term))%>%
  mutate(term = ifelse(term == "N:P:CROP", "N:E:C", term))%>%
  filter(term != "Residuals")

# Print the tibble
print(NG_stars_tibble)

NG_stars <-NG_stars_tibble |>
  select(harvest, term, stars) |>
  pivot_wider(names_from = term, values_from = stars)|>
  mutate(model = c("log", "log", "linear", "log", "log", "log"))|>
  relocate(model, .after = harvest)

gt_NG <- gt(NG_stars)

gt_NG |> gtsave("figures/forage/gt_TF.png", expand = 10)











### Difference in light interception by accession?
# accession by timepoint G:T
modelACC <- lmer(adjTotLI ~ oat_accession_name*pea_accession_name*Timepoint + (1|plot_number:subplot_number), data=LiCor)
  ACC_resid<-resid(modelACC)
  qqnorm(ACC_resid)
  qqline(ACC_resid)
  plot(modelACC)

modelac <- glmer(adjTotLI ~ oat_accession_name*pea_accession_name + Timepoint*subplot_number + (1 | plot_number),
               family = gaussian(link = "identity"),
               data = LiCor)
  ac_resid<-resid(modelac)
  qqnorm(ac_resid)
  qqline(ac_resid)
  plot(modelac)

AIC(modelac,modelACC)
# modelACC has lower AIC score
anova(modelACC)

aov.table <- anova(modelACC)
table = flextable(data = aov.table %>%
                    #filter(`Pr(>F)` < 0.05) %>%
                    mutate(`Pr(>F)` = as.numeric(`Pr(>F)`),          # ensure numeric
                           Signif = p_to_stars(`Pr(>F)`),            # add stars column
                           `Pr(>F)` = formatC(`Pr(>F)`, digits = 3))%>%  # format p-value nicely %>%
                    dplyr::select(`Pr(>F)`,Signif) %>%
                    rename(`  ` = Signif) %>%
                    rownames_to_column(" "))
print(table)

# Boxplot for Light Interception by oat accession
custom_letters <- c("a", "b", "c", "d", "e", "f", "g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z")  # Customize the letters as needed
emms <- emmeans(modelACC, ~ oat_accession_name*pea_accession_name, type = "response") # Obtain estimated marginal means
emms_df <- as.data.frame(summary(emms))
cld_data <- cld(emms, Letters=custom_letters)
cld_df <-as.data.frame(cld_data)
merged_df <- merge(emms_df, cld_df,by = c("oat_accession_name", "pea_accession_name"))
# Custom X-axis labels
totLI_ACC <- ggplot(LiCor, aes(x=factor(oat_accession_name, levels=c('IL17-1704', 'ND Spilde', 'ND190393', 'ND210038', 'SD Momentum', 'NO_OATS_PLANTED')), y = adjTotLI, colour = pea_accession_name)) +
  geom_boxplot(position = position_dodge(width = 0.8)) +
  geom_text(data = merged_df, aes(x = oat_accession_name, y = emmean.x, colour = pea_accession_name, label = .group),
            position = position_dodge(width = 0.8),
            vjust = -0.5, size = 5)+#,hjust = 1.5)+
  labs(title = "Relationship of Total Light Interception and oat accession", x = "oat accession", y = "Light Interception (%)")+
  theme(axis.text=element_text(size=14), #change font size of axis text
        axis.title=element_text(size=18))
# Boxplot for Light Interception by pea accession and Timepoint
custom_letters <- c("a", "b", "c", "d", "e", "f", "g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z")  # Customize the letters as needed
emms <- emmeans(modelACC, ~ pea_accession_name*Timepoint | oat_accession_name, type = "response") # Obtain estimated marginal means
emms_df <- as.data.frame(summary(emms))
cld_data <- cld(emms, Letters=custom_letters)
cld_df <-as.data.frame(cld_data)
merged_df <- merge(emms_df, cld_df,by = c("Timepoint", "pea_accession_name"))
# Custom X-axis labels
totLI_ACC <- ggplot(LiCor, aes(x=Timepoint, y = adjTotLI, colour = pea_accession_name)) +
  geom_boxplot(position = position_dodge(width = 0.8)) +
  geom_text(data = merged_df, aes(x = Timepoint, y = emmean.x, colour = pea_accession_name, label = .group),
            position = position_dodge(width = 0.8),
            vjust = -0.5, size = 5)+#,hjust = 1.5)+
  labs(title = "Relationship of Total Light Interception and pea accession and Timepoint", x = "Timepoint", y = "Light Interception (%)")+
  theme(axis.text=element_text(size=14), #change font size of axis text
        axis.title=element_text(size=18))


















