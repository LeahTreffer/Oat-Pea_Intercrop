hist(biomass2$oat_biomass)# bimodal
hist(log(biomass2$pea_biomass))
hist(sqrt(biomass2$weed_biomass))
hist(biomass2$total_biomass) #bimodal
hist(diversity_plots2$avg_biomass_s)
hist(diversity_plots2$oat_grain_yield)
hist(log(diversity_plots2$pea_grain_yield))
hist(diversity_plots2$total_yield)
hist(sqrt(diversity_plots2$Lodging))

biomass2$pea_biomass_log <- log(biomass2$pea_biomass +0.1)
biomass2$weed_biomass_sqrt <- sqrt(biomass2$weed_biomass)
diversity_plots2$pea_grain_yield_log <- log(diversity_plots2$pea_grain_yield +0.1)
diversity_plots2$Lodging_sqrt <- sqrt(diversity_plots2$Lodging)


model <- lm(oat_biomass ~ pea_biomass_log + weed_biomass_sqrt + block_number, data = biomass2)
summary(model)
# as pea biomass and weed biomass increase, oat biomass increases
summary_model <- broom::tidy(model)%>%
  filter(p.value < 0.05)
summary_model %>%
  mutate(p.value = format(p.value, scientific = TRUE)) %>%
  kable(digits = 5, caption = "Summary of Linear Model for Oat Biomass", 
        col.names = c("Term", "Estimate", "Std. Error", "t value", "P value")) %>%
  kable_styling(full_width = FALSE, position = "center", bootstrap_options = c("striped", "hover", "condensed"))

model2 <- lm(oat_biomass ~ germplasmName + peaName + pea_biomass_log + weed_biomass_sqrt + block_number, data = biomass2)
summary(model2)
significant_summary <- broom::tidy(model2) %>%
  filter(p.value < 0.05)
significant_summary %>%
  select(term, estimate, std.error, statistic, p.value) %>%
  mutate(p.value = format(p.value, scientific = TRUE)) %>%
  kable(digits = 3, caption = "Significant Covariates in Linear Model for Oat Biomass",
        col.names = c("Term", "Estimate", "Std. Error", "t value", "P value")) %>%
  kable_styling(full_width = FALSE, position = "center", bootstrap_options = c("striped", "hover", "condensed"))

model3 <- lm(pea_biomass_log ~ germplasmName + peaName + oat_biomass + weed_biomass_sqrt + block_number, data = biomass2)
summary(model3)
# Tidy the model output and filter for significant covariates (p < 0.05)
significant_summary <- broom::tidy(model3) %>%
  filter(p.value < 0.05)
significant_summary %>%
  select(term, estimate, std.error, statistic, p.value) %>%
  mutate(p.value = format(p.value, scientific = TRUE)) %>%
  kable(digits = 3, caption = "Significant Covariates in Linear Model for Pea Biomass",
        col.names = c("Term", "Estimate", "Std. Error", "t value", "P value")) %>%
  kable_styling(full_width = FALSE, position = "center", bootstrap_options = c("striped", "hover", "condensed"))


### are group means different than each other
model_oatbiomass <- aov(oat_biomass ~ germplasmName + peaName + pea_type + pea_biomass_log + weed_biomass_sqrt, biomass2)
summary(model_oatbiomass)

significant_summary2 <- broom::tidy(model_oatbiomass) %>%
  filter(p.value < 0.05)
significant_summary2 %>%
  mutate(p.value = format(p.value, scientific = TRUE)) %>%
  kable(digits = 3, caption = "ANOVA Results for Oat Biomass Model",
        col.names = c("Term", "Degrees of Freedom", "Sum of Squares", "Mean Square", "F value", "P value")) %>%
  kable_styling(full_width = FALSE, position = "center", bootstrap_options = c("striped", "hover", "condensed"))

model_totalbiomass <- aov(total_biomass ~ germplasmName + peaName + pea_type + oat_biomass + pea_biomass_log + weed_biomass_sqrt + block_number, biomass2)
summary(model_totalbiomass)
sig_summ_total <- broom::tidy(model_totalbiomass) %>%
  filter(p.value < 0.05)
sig_summ_total %>%
  mutate(p.value = format(p.value, scientific = TRUE)) %>%
  kable(digits = 3, caption = "ANOVA Results for Total Plot Biomass Model",
        col.names = c("Term", "Degrees of Freedom", "Sum of Squares", "Mean Square", "F value", "P value")) %>%
  kable_styling(full_width = FALSE, position = "center", bootstrap_options = c("striped", "hover", "condensed"))


model_peabiomass <- aov(pea_biomass_log ~ germplasmName + peaName + pea_type + oat_biomass + weed_biomass_sqrt, biomass2)
summary(model_peabiomass)

significant_summary3 <- broom::tidy(model_peabiomass) %>%
  filter(p.value < 0.05)
significant_summary3 %>%
  mutate(p.value = format(p.value, scientific = TRUE)) %>%
  kable(digits = 3, caption = "ANOVA Results for Pea Biomass Model",
        col.names = c("Term", "Degrees of Freedom", "Sum of Squares", "Mean Square", "F value", "P value")) %>%
  kable_styling(full_width = FALSE, position = "center", bootstrap_options = c("striped", "hover", "condensed"))




model_yield <- aov(total_yield ~ pea_type + peaName + germplasmName + avg_biomass_s, data=diversity_plots2) 
summary(model_yield)

significant_yield_summary <- broom::tidy(model_yield) %>%
  filter(p.value < 0.05)
significant_yield_summary %>%
  #mutate(p.value = format(p.value, scientific = TRUE)) %>%
  kable(digits = 3, caption = "ANOVA Results for Total Grain Yield Model",
        col.names = c("Term", "Degrees of Freedom", "Sum of Squares", "Mean Square", "F value", "P value")) %>%
  kable_styling(full_width = FALSE, position = "center", bootstrap_options = c("striped", "hover", "condensed"))

# oat grain 
model_yield2 <- aov(oat_grain_yield ~ pea_type + peaName + germplasmName + avg_biomass_s, data=diversity_plots2) 
summary(model_yield2)

# pea grain 
model_yield3 <- aov(pea_grain_yield_log ~ pea_type + peaName + germplasmName + avg_biomass_s, data=diversity_plots2) 
summary(model_yield3)

significant_yield_summary <- broom::tidy(model_yield) %>%
  filter(p.value < 0.05)
significant_yield_summary %>%
  #mutate(p.value = format(p.value, scientific = TRUE)) %>%
  kable(digits = 3, caption = "ANOVA Results for Total Grain Yield Model",
        col.names = c("Term", "Degrees of Freedom", "Sum of Squares", "Mean Square", "F value", "P value")) %>%
  kable_styling(full_width = FALSE, position = "center", bootstrap_options = c("striped", "hover", "condensed"))

# are any of the oat genotypes different in their mean yield, controlling for pea genotype and weed biomass 
model_oat_yield <- lm(oat_grain_yield ~ germplasmName + peaName + pea_grain_yield_log + avg_biomass_s + blockNumber, data =  diversity_plots2)
summary(model_oat_yield)

significant_Oyield_summary <- broom::tidy(model_oat_yield) %>%
  filter(p.value < 0.05)
significant_Oyield_summary %>%
  #mutate(p.value = format(p.value, scientific = TRUE)) %>%
  kable(digits = 3, caption = "Significant Covariates in Linear Model for Oat Grain",
        col.names = c("Term", "Estimate", "Std. Error", "t value", "P value")) %>%
  kable_styling(full_width = FALSE, position = "center", bootstrap_options = c("striped", "hover", "condensed"))


model_pea_yield <- lm(pea_grain_yield_log ~ germplasmName + peaName + oat_grain_yield + avg_biomass_s + blockNumber, data =  diversity_plots2)
summary(model_pea_yield)

significant_Pyield_summary <- broom::tidy(model_pea_yield) %>%
  filter(p.value < 0.05)
significant_Pyield_summary %>%
  #mutate(p.value = format(p.value, scientific = TRUE)) %>%
  kable(digits = 3, caption = "Significant Covariates in Linear Model for Pea Grain",
        col.names = c("Term", "Estimate", "Std. Error", "t value", "P value")) %>%
  kable_styling(full_width = FALSE, position = "center", bootstrap_options = c("striped", "hover", "condensed"))