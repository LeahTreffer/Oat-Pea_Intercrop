library(readxl)
diversity <- read_excel("data/2023.9.7_oat_pea_cornell_2023.xlsx",
                                             sheet = "diversity_data_2023")
View(diversity)

library(readr)
phenotype <- read_csv("data/phenotype.csv")

# Trait headings will change and subplots will be added after uploaded to T3
# Will be able to directly pull from T3


standav <- diversity[,c(1:5,25:33)]
standav$pea_stand <- rowMeans(subset(standav, select = c(pea_stand_1_m2_6.2.23, pea_stand_2_m2_6.2.23, pea_stand_3_m2_6.2.23)), na.rm = TRUE)
standav$oat_stand <- rowMeans(subset(standav, select = c(oat_stand_1_m2_6.2.23, oat_stand_2_m2_6.2.23, oat_stand_3_m2_6.2.23)), na.rm = TRUE)


standav$color <- standav$pea_type
standav$color[standav$color =="field"] <- "#009E73"
standav$color[standav$color =="forage"] <- "#E69F00"

boxplot(pea_stand~peaName, data=standav, ylab="Emergence", xlab="", las=2, main="pea emergence", col=standav$color)
legend("topleft", c("field", "forage"), border="black", fill = c("#009E73", "#E69F00"), box.lty=0, cex = 0.65)

boxplot(oat_stand~oatName, data=standav, ylab="Emergence", xlab="", las=2, main="oat emergence", col=standav$color)
legend("topleft", c("field", "forage"), border="black", fill = c("#009E73", "#E69F00"), box.lty=0, cex = 0.65)

boxplot(pea_stand~plot_id, data=standav, ylab="Emergence", xlab="", las=2, main="pea emergence", col=standav$color)
boxplot(oat_stand~plot_id, data=standav, ylab="Emergence", xlab="", las=2, main="oat emergence", col=standav$color)



##

oneway.test(oat_stand ~ oatName, data = standav, var.equal = TRUE)
oneway.test(pea_stand ~ oatName, data = standav, var.equal = TRUE)
oneway.test(oat_stand ~ peaName, data = standav, var.equal = TRUE)
oneway.test(pea_stand ~ peaName, data = standav, var.equal = TRUE)

cor.test(standav$oat_stand,standav$pea_stand) # correlation between alfalfa emergence and intercrop companion emergence

two.way <- aov(oat_stand ~ oatName + peaName, data = standav)
summary(two.way)

blocking <- aov(oat_stand ~ oatName + peaName + block_number, data = standav)
summary(blocking)

interaction <- aov(oat_stand ~ oatName*peaName, data = standav)
summary(interaction)

library(AICcmodavg)

model.set <- list(two.way, interaction, blocking)
model.names <- c("two.way", "interaction", "blocking")

aictab(model.set, modnames = model.names)


###

library(corrplot)

DivTab <- diversity[,c(2,28:95)] # make table of plot names and all traits
DivTab <- as.data.frame(DivTab)

DivTab$pea_flowering_6.13.23[DivTab$pea_flowering_6.13.23 =="Yes"] <- "1"
DivTab$pea_flowering_6.13.23[DivTab$pea_flowering_6.13.23 =="No"] <- "0"
DivTab$oat_ear_emergence_6.13.23[DivTab$oat_ear_emergence_6.13.23 =="Yes"] <- "1"
DivTab$oat_ear_emergence_6.13.23[DivTab$oat_ear_emergence_6.13.23 =="No"] <- "0"
DivTab$pea_flowering_6.20.2023[DivTab$pea_flowering_6.20.2023 =="Yes"] <- "1"
DivTab$pea_flowering_6.20.2023[DivTab$pea_flowering_6.20.2023 =="No"] <- "0"
DivTab$pea_fruit_6.20.2023[DivTab$pea_fruit_6.20.2023 =="Yes"] <- "1"
DivTab$pea_fruit_6.20.2023[DivTab$pea_fruit_6.20.2023 =="No"] <- "0"
DivTab$oat_ear_emergence_6.20.2023[DivTab$oat_ear_emergence_6.20.2023 =="Yes"] <- "1"
DivTab$oat_ear_emergence_6.20.2023[DivTab$oat_ear_emergence_6.20.2023 =="No"] <- "0"

DivTab <- sapply(DivTab, as.numeric)
DivTab <- as.data.frame(DivTab)

rownames(DivTab) <- DivTab[,1]
DivTab <- DivTab[,-1]

# reorder so table is grouped by trait
colnames(DivTab)
DivTab2 <- DivTab[,c(1:6,7:12,18:23,25:30,34:39,24,31,50,44:49,13:17,32:33,40:43,51:68)]

CorTab <- cor(DivTab2, use="pairwise.complete.obs") # use=’pairwise.complete.obs’ because of NAs

#write.csv(CorTab, '~/GitHub/OatPea_intercrop/output/trait_correlations.csv', row.names = TRUE)
CorTab <- read.csv('~/GitHub/OatPea_intercrop/output/trait_correlations.csv')

corrplot(CorTab)
corrplot.mixed(CorTab)



library(tidyr)
data_long <- gather(standav, stand_part, measurement, oat_stand_1_m2_6.2.23:pea_stand_3_m2_6.2.23, factor_key=TRUE)
data_long

data_long$crop <- data_long$stand_part
data_long$stand_part <- as.character(data_long$stand_part)
data_long$crop[data_long$stand_part == "oat_stand"] <- "oat"
data_long$crop[data_long$stand_part == "pea_stand"] <- "pea"

data_long$crop2 <- str_sub(data_long$crop, 1, -19)
data_long$time <- str_sub(data_long$crop, 11, -11)
data_long$time <- as.numeric(data_long$time)

library(gcookbook) # Load gcookbook for the tg data set
library(ggplot2)

# Map supp to colour
ggplot(data_long, aes(x = time, y = measurement, colour = plot_name)) +
  geom_line()

ggplot(data_long, aes(x = time, y = measurement, linetype = plot_name)) +
  geom_line()

ggplot(data_long, aes(x = time, y = measurement, color = plot_name)) +
  geom_line()+
  theme(legend.position = "none")




colnames(phenotype)[31] <- "AGDBM_157"
colnames(phenotype)[32] <- "AGDBM_172"
colnames(phenotype)[33] <- "AGDBM_192"
colnames(phenotype)[34] <- "AGDBM_214"
colnames(phenotype)[45] <- "Pea_AGDBM_157"
colnames(phenotype)[46] <- "Pea_AGDBM_172"
colnames(phenotype)[47] <- "Pea_AGDBM_192"
colnames(phenotype)[48] <- "Pea_AGDBM_214"

colnames(phenotype)[35] <- "Ear_EMG_164"
colnames(phenotype)[36] <- "Ear_EMG_171"

colnames(phenotype)[37] <- "GrainWeight"

colnames(phenotype)[38] <- "GrainYield"
colnames(phenotype)[53] <- "PeaYield"

colnames(phenotype)[39] <- "LDGSEV_181"
colnames(phenotype)[40] <- "LDGSEV_186"
colnames(phenotype)[41] <- "LDGSEV_221"

colnames(phenotype)[42] <- "OAT_Maturity_191"
colnames(phenotype)[43] <- "OAT_Maturity_200"
colnames(phenotype)[44] <- "OAT_Maturity_206"

colnames(phenotype)[49] <- "Pea_Flower_164"
colnames(phenotype)[50] <- "Pea_Flower_171"

colnames(phenotype)[51] <- "Pea_Fruit"
colnames(phenotype)[52] <- "Pea_GrainWeight"

colnames(phenotype)[54] <- "Pea_Maturity_191"
colnames(phenotype)[55] <- "Pea_Maturity_200"
colnames(phenotype)[56] <- "Pea_Maturity_206"

colnames(phenotype)[57] <- "Pea_EST"
colnames(phenotype)[63] <- "Oat_EST"

colnames(phenotype)[58] <- "Pea_PTHT_164"
colnames(phenotype)[59] <- "Pea_PTHT_180"
colnames(phenotype)[60] <- "Pea_PTHT_186"
colnames(phenotype)[61] <- "Pea_PTHT_200"
colnames(phenotype)[62] <- "Pea_PTHT_208"
colnames(phenotype)[64] <- "Oat_PTHT_164"
colnames(phenotype)[65] <- "Oat_PTHT_180"
colnames(phenotype)[66] <- "Oat_PTHT_186"
colnames(phenotype)[67] <- "Oat_PTHT_200"
colnames(phenotype)[68] <- "Oat_PTHT_208"

colnames(phenotype)[70] <- "AUS_FieldPea"
colnames(phenotype)[71] <- "Admiral"
colnames(phenotype)[72] <- "Frostmaster"
colnames(phenotype)[73] <- "Icicle"
colnames(phenotype)[74] <- "Maxum"
colnames(phenotype)[75] <- "NDDawn"
colnames(phenotype)[76] <- "NDVictory"
colnames(phenotype)[77] <- "NDP140510Y"
colnames(phenotype)[78] <- "NDP150231Y"
colnames(phenotype)[79] <- "NDP150412G"
colnames(phenotype)[80] <- "O4010"
colnames(phenotype)[81] <- "Delta"




fit2 = lm(GrainYield ~ PeaYield, data = phenotype)
fit2
summary(fit2)
plot(fit2)


plot(phenotype$GrainYield, phenotype$PeaYield)

# https://www.statology.org/piecewise-regression-in-r/

library(segmented)

fit <- lm(PeaYield ~ GrainYield, data=phen)

segmented.fit <- segmented(fit, seg.Z = ~GrainYield,psi=9)
summary(segmented.fit)

plot(phenotype$GrainYield, phenotype$PeaYield, col='steelblue')
plot(segmented.fit, add=T)

plot(phenotype$GrainYield, phenotype$PeaYield, col="navy")


Subs1 <- subset(phenotype, (!is.na(phenotype$AGDBM_157)))

# Change Pea ID from NA,1 code to one column with names
## Define the conditions and replacement values
conditions <- c("1")
replacement_values <- c("Delta") # change this for every column
## Use replace() to replace the names in the 'Names' column
Subs1$Delta <- replace(Subs1$Delta, Subs1$Delta %in% conditions, replacement_values) # change this for every column
# one column for Pea Type
data_long1 <- unite(Subs1, col='Pea_Type', c(AUS_FieldPea:Delta), sep='', na.rm = TRUE)
