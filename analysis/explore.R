library(readxl)
diversity <- read_excel("data/2023.9.7_oat_pea_cornell_2023.xlsx",
                                             sheet = "diversity_data_2023")
View(diversity)

# Trait headings will change and subplots will be added after uploaded to T3
# Will be able to directly pull from T3


standav <- diversity[,c(1:5,25:33)]
standav$pea_stand <- rowMeans(subset(standav, select = c(pea_stand_1_m2_6.2.23, pea_stand_2_m2_6.2.23, pea_stand_3_m2_6.2.23)), na.rm = TRUE)
standav$oat_stand <- rowMeans(subset(standav, select = c(oat_stand_1_m2_6.2.23, oat_stand_2_m2_6.2.23, oat_stand_3_m2_6.2.23)), na.rm = TRUE)


standav$color <- standav$pea_type
standav$color[standav$color =="field"] <- "#009E73"
standav$color[standav$color =="forage"] <- "#E69F00"

boxplot(pea_stand~peaName, data=standav, ylab="Emergence", xlab="", las=2, main="pea emergence", col=standav$color)
boxplot(oat_stand~oatName, data=standav, ylab="Emergence", xlab="", las=2, main="oat emergence", col=standav$color)
boxplot(pea_stand~plot_id, data=standav, ylab="Emergence", xlab="", las=2, main="pea emergence", col=standav$color)
boxplot(oat_stand~plot_id, data=standav, ylab="Emergence", xlab="", las=2, main="oat emergence", col=standav$color)

# is early height influenced by pea type (forage/grain)
glm(pea_height_1_6.29.23 ~ pea_type, data=diversity)

