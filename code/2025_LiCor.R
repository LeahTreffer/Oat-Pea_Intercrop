library(readxl)
LiCor <- read_excel("Downloads/2025 Spring oat-pea LiCor.xlsx")

LiCor$Plot <- as.factor(LiCor$Plot)
LiCor$Subplot <- as.factor(LiCor$Subplot)
LiCor$Plot.Subplot <- as.factor(LiCor$Plot.Subplot)


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

