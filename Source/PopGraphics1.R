# packages <- c("ggplot2", "gcookcook", "maps", 'corrplot', 'rgl')
# install.packages(packages)


library(ggplot2)
library(maps)
library(gcookbook)
library(corrplot)
library(rgl)
library(sp)
library(rgdal)
library(RColorBrewer)
library(gridExtra)

source("http://www.sthda.com/upload/rquery_cormat.r")

data.pop.raw = read.csv("CSV//AgePop.csv", header = TRUE, sep=';')

dpt_IDF <- c(75, 77, 78, 91, 92, 93, 94, 95)

ylim.idf =c(48.104485,49.257756)
xlim.idf = c(1.414525, 3.589818)


#On supprime les 6 derniÃ¨res lignes de la DataFrame
#Elles correspondent Ã  la moyenne sur la France et les dÃ©partements d'OutreMer
fin <- length(data.pop.raw$P10_POP) 
data.pop <- data.pop.raw[-c(fin:(fin-5)),]
data.pop$P10_jeune = data.pop$P10_0014_POP +  data.pop$P10_1529_POP + data.pop$P10_3044_POP
data.pop$P99_jeune = data.pop$P99_0014_POP +  data.pop$P99_1529_POP + data.pop$P99_3044_POP

data.pop$P10_vieux = data.pop$P10_4559_POP +  data.pop$P10_6074_POP + data.pop$P10_75P_POP
data.pop$P99_vieux = data.pop$P99_4559_POP +  data.pop$P99_6074_POP + data.pop$P99_75P_POP
data.pop.idf <- data.pop[data.pop$CODGEO %in% dpt_IDF,]

data_sorted <- data.pop[order(data.pop['P10_1529_POP']),]

Premiers <- tail(data_sorted, 10)
ColonesAges <- c(5,7,9, 11, 13, 15)
PremiersInteret <- Premiers[,ColonesAges]

mat.premier <- t(as.matrix(PremiersInteret))

n <- 6
png('Images\\PartAge10DeptPeuplés.png')
colors.barplot.age <- rainbow(n, s = 1, v = 1, start = 0, end = max(1, n - 1)/n, alpha = 1)
legends <- c("0 à 14 ans","15 à 29 ans","30 à 44 ans", "45 à 59 ans", "60 à 74 ans", "75 ans et plus")
barplot(mat.premier, names.arg = Premiers$CODGEO, las=2, col=colors.barplot.age)
legend("topleft", legend = legends, fill = colors.barplot.age)
title("Pourcentage de catégorie de Population\n des 10 départements les plus peuplés")
dev.off()

#============================================================
#           Quantité de Population par départements
#============================================================

france1<-map(database="france")
#Récupère les numéros des départements correspondant à la carte
num <- read.csv("CSV//DeptNum.csv", header= TRUE, sep=';')
num_cor <- num$Num
#Renomme les départements de la carte avec leur numéro
france1$names <- num_cor

data.pop.sorted <- data.pop[order(data.pop$P10_POP),]
dpt <- data.pop.sorted$CODGEO
matche<-match.map(france1,dpt,exact=TRUE)
greys <- colorRampPalette(brewer.pal(9,"Greys"))(100)
colors<-greys[matche]

png('Images//CartePopulation2010.png')
map(database="france", fill=TRUE,col=colors,resolution=0)
grandesvilles <- read.csv("CSV//grandesvilles.csv", header=TRUE, sep=';')
# Mcdo <- read.csv("CSV//McDonald's France.csv", header=TRUE, sep=',')
#Ajoute les plus grandes villes sur la carte
points(grandesvilles$Longitude, grandesvilles$Latitude, pch=20, col = "red")
# points(Mcdo$longitude, Mcdo$latitude, pch=10, col ='green')
#Précise le nom de ces villes
text(grandesvilles$Longitude, grandesvilles$Latitude+0.2, grandesvilles$Nom, col="red", cex = 1)
title("Part de cadre dans la population de chaque département")
max <- trunc(data.pop.sorted$P10_POP[length(data.pop.sorted$P10_POP)])
min <- trunc(data.pop.sorted$P10_POP[1])
leg <- rev(trunc(seq(min%/%10000*10000,max%/%10000*10000, 250000)))
legend("bottomright", legend =leg, pch = 20, col = greys[seq(91,0, -91/11)], title="(en %)")
dev.off()

#============================================================
#           Départements les plus jeunes et les plus vieux
#============================================================

#==================================
# =========== 1999 ================
#==================================

data.1529.sorted99 <- data.pop[order(data.pop$P99_1529_POP),]
dpt1529.99 <- data.1529.sorted99$CODGEO
matche.1529.99<-match.map(france1,dpt1529.99,exact=TRUE)
blues <- colorRampPalette(brewer.pal(9,"Blues"))(100)
colors.1529.99<-blues[matche.1529.99]

png('Images//CartePopulation15291999.png')
map(database="france", fill=TRUE,col=colors.1529.99,resolution=0)
grandesvilles <- read.csv("CSV//grandesvilles.csv", header=TRUE, sep=';')
points(grandesvilles$Longitude, grandesvilles$Latitude, pch=20, col = "red")
#Précise le nom de ces villes
min <- trunc(data.1529.sorted99$P99_1529_POP[1])
max <- trunc(data.1529.sorted99$P99_1529_POP[length(data.1529.sorted99$P99_1529_POP)])
text(grandesvilles$Longitude, grandesvilles$Latitude+0.2, grandesvilles$Nom, col="red", cex = 1)
title("Pourcentage de 15-29 ans dans les departements en 1999")
legend("bottomright", legend = rev(trunc(seq(min, max, 1))), pch = 20, col = rev(blues[seq(91,0, -91/11)]), title="(en %)")
dev.off()

#==================================
# =========== 2010 ================
#==================================


data.1529.sorted10 <- data.pop[order(data.pop$P10_1529_POP),]
dpt1529.10 <- data.1529.sorted10$CODGEO
matche.1529.10<-match.map(france1,dpt1529.10,exact=TRUE)
blues <- colorRampPalette(brewer.pal(9,"Blues"))(100)
colors.1529.10<-blues[matche.1529.10]

png('Images//CartePopulation15292010.png')
map(database="france", fill=TRUE,col=colors.1529.10,resolution=0)
grandesvilles <- read.csv("CSV//grandesvilles.csv", header=TRUE, sep=';')
# Mcdo <- read.csv("CSV//McDonald's France.csv", header=TRUE, sep=',')
#Ajoute les plus grandes villes sur la carte
points(grandesvilles$Longitude, grandesvilles$Latitude, pch=20, col = "red")
# points(Mcdo$longitude, Mcdo$latitude, pch=10, col ='green')
#Précise le nom de ces villes
text(grandesvilles$Longitude, grandesvilles$Latitude+0.2, grandesvilles$Nom, col="red", cex = 1)
min <- trunc(data.1529.sorted10$P10_1529_POP[1])
max <- trunc(data.1529.sorted10$P10_1529_POP[length(data.1529.sorted10$P10_1529_POP)])
title("Pourcentage de 15-29 ans dans les departements en 2010")
legend("bottomright", legend = rev(trunc(seq(min, max, 1.5))), pch = 20, col = rev(blues[seq(91,0, -91/11)]), title="(en %)")
dev.off()

#============================================================
#           Jeune en fonction de la population
#============================================================

ggplot(data.pop, aes(x=data.pop$P10_jeune, y=data.pop$P10_POP)) +
  geom_point(shape=1) +
  geom_smooth(method=lm) + 
  geom_text(aes(label=data.pop$CODGEO), size=4, hjust=-1, vjust=-1) +
  ggtitle("Pourcentage de jeunes par département\nen fonction de la population")+
  labs(x = "Pourcentage de 15-29 ans", y = "Population totale")
ggsave(file="Pourcentage_de_jeune_population.png")

# ----- 2010
ggplot(data.pop, aes(x=data.pop$P10_1529_POP, y=data.pop$P10_75P_POP)) +
  geom_point(aes(size=data.pop$P10_POP),shape=1) +
  scale_size_continuous(range = c(3, 10)) +
  geom_smooth(method=lm) + 
  geom_text(aes(label=data.pop$CODGEO), size=4, hjust=-1, vjust=-1) +
  ggtitle("Pourcentage de très jeunes par rapport aux très vieux par département en 2010")+
  labs(x = "Pourcentage de 15-29 ans", y = "Pourcentage de 75 ans et plus")
ggsave(file="Images\\Pourcentage_très_jeunes_vieux_2010.png")

# ----- 1999
ggplot(data.pop, aes(x=data.pop$P99_1529_POP, y=data.pop$P99_75P_POP)) +
  geom_point(aes(size=data.pop$P10_POP),shape=1) +
  scale_size_continuous(range = c(3, 10)) +
  geom_smooth(method=lm) + 
  geom_text(aes(label=data.pop$CODGEO), size=4, hjust=-1, vjust=-1) +
  ggtitle("Pourcentage de très jeunes par rapport aux très vieux par départementen 1999")+
  labs(x = "Pourcentage de 15-29 ans", y = "Pourcentage de 75 ans et plus")
ggsave(file="Images\\Pourcentage_très_jeunes_vieux_1999.png")
#=================================
#       Pop jeune vieux 
#                IDF
# ===============================

# 1999 

ggplot(data.pop, aes(x=data.pop.idf$P99_jeune, y=data.pop.idf$P99_vieux)) +
  geom_point(aes(size=data.pop.idf$P10_POP),shape=1) +
  scale_size_continuous(range = c(3, 10)) +
  geom_smooth(method=lm) + 
  geom_text(aes(label=data.pop.idf$CODGEO), size=4, hjust=-1, vjust=-1) +
  ggtitle("Pourcentage de jeunes par rapport aux vieux par département en 1999")+
  labs(x = "Pourcentage de 00-44 ans", y = "Pourcentage de 45 ans et plus")
ggsave(file="Images\\Pourcentage_idf_jeune_vieux_1999.png")

#2010

ggplot(data.pop, aes(x=data.pop.idf$P10_jeune, y=data.pop.idf$P10_vieux)) +
  geom_point(aes(size=data.pop.idf$P10_POP),shape=1) +
  scale_size_continuous(range = c(3, 10)) +
  geom_smooth(method=lm) + 
  geom_text(aes(label=data.pop.idf$CODGEO), size=4, hjust=-1, vjust=-1) +
  ggtitle("Pourcentage de jeunes par rapport aux vieux par département en 2010")+
  labs(x = "Pourcentage de 00-44 ans", y = "Pourcentage de 45 ans et plus")
ggsave(file="Images\\Pourcentage_idf_jeune_vieux_2010.png")

# ggplot(data.pop, aes(x=data.pop$P10_POP)) + 
#   geom_histogram(binwidth = 0.5)
data.pop$f0 <- 1 *
  
P99_hist_1529 <- as.data.frame(cbind(data.pop$CODGEO,data.pop$P99_1529_POP))
P99_hist_1529$type <- '99'
P10_hist_1529 <- as.data.frame(cbind(data.pop$CODGEO,data.pop$P10_1529_POP))
P10_hist_1529$type <- '10'
colnames(P99_hist_1529) <- c("CODGEO", "P1529", "type")
colnames(P10_hist_1529) <- c("CODGEO", "P1529", "type")
hist_1519 <- rbind(P99_hist_1529, P10_hist_1529)

ggplot(hist_1519, aes(hist_1519$P1529, fill=hist_1519$type)) + 
  geom_histogram(binwidth = 1, position = 'identity', alpha = 0.5) +
  ggtitle("Distribution du pourcentage de 15-29 ans par département en 2010 et 1999")+
  geom_vline(aes(xintercept=mean(subset(hist_1519, type=="10")$P1529, na.rm=T)),  color="red", linetype="dashed", size=1) +
  geom_vline(aes(xintercept=mean(subset(hist_1519, type=="99")$P1529, na.rm=T)),  color="blue", linetype="dashed", size=1) + 
  labs(y = "Count", x = "Pourcentage de 15-29 ans")
ggsave('Images\\Distribution_1529_2010_1999.png')

ggplot(data.pop, aes(x=data.pop$P10_1529_POP)) + 
  geom_histogram(binwidth = 1)+
  ggtitle("Distribution du pourcentage de 15-29 ans par département en 2010")+
  geom_vline(aes(xintercept=mean(data.pop$P10_1529_POP, na.rm=T)),  color="red", linetype="dashed", size=1) + 
  labs(y = "Count", x = "Pourcentage de 15-29 ans")
ggsave(file="Images\\Distribution1529_Dept2010.png")

ggplot(data.pop, aes(x=data.pop$P99_1529_POP)) + 
  geom_histogram(binwidth = 1)+
  ggtitle("Distribution du pourcentage de 15-29 ans par département en 1999")+
  geom_vline(aes(xintercept=mean(data.pop$P99_1529_POP, na.rm=T)),  color="red", linetype="dashed", size=1) + 
  labs(x = "Pourcentage de 15-29 ans", y = "Count")
ggsave(file="Images\\Distribution1529_Dept1999.png")

#============================================================
#           Emploi par département de la Pop active
#============================================================

emploi <- read.csv("CSV//EmploiPopActive.csv", sep = ';', header=TRUE)
emploi_column <- c("CODGEO", "LIBGEO", "P10_ACT1564", "P10_CHOM1564", "P10_CHOM_ACT1564",  "P99_CHOM_ACT1564")
emploi <- emploi[emploi_column]
emploi['Diff1099'] <- emploi['P10_CHOM_ACT1564'] - emploi['P99_CHOM_ACT1564']
emploi['RatioActifsChomeurs'] <- (emploi['P10_ACT1564'] - emploi['P10_CHOM1564']) / emploi['P10_CHOM1564']

ggplot(emploi, aes(x=emploi$P10_ACT1564, y=emploi$P10_CHOM1564)) +
  geom_point(shape=1) +
  geom_smooth(method=lm) + 
  geom_text(aes(label=emploi$CODGEO), size=4, hjust=-1, vjust=-1)

#On voit qu'il n'y a pas vraiment de départements qui sortent du lot

ggplot(emploi, aes(x=emploi$Diff1099)) + 
  geom_histogram(binwidth = 0.5)+
  ggtitle("Distribution de la différence de chomeur entre 1999 et 2010")+
  geom_vline(aes(xintercept=mean(emploi$Diff1099, na.rm=T)),  color="red", linetype="dashed", size=1) + 
  labs(x = "Différence de chomeurs entre 1999 et 2010", y = "Count")
ggsave(file="Images\\Distribution_diffChomeur_9910.png")

pairs(emploi)

#On voit une légère corélation entre le pourcentage de chomeurs en 2010 et en 1999 
#On voit qu'en général la plupart des départements garde une même dynamique

# =====================================================================
# ---------------------------- IDF -----------------------------------
# =====================================================================

emploi_idf = emploi[emploi$CODGEO %in% dpt_IDF,]
data_idf = data.pop[data.pop$CODGEO %in% dpt_IDF,]

# ----- 2010
ggplot(data_idf, aes(x=data_idf$P10_1529_POP, y=data_idf$P10_75P)) +
  geom_point(aes(size=data_idf$P10_POP),shape=1) +
  scale_size_continuous(range = c(3, 10)) +
  geom_smooth(method=lm) + 
  geom_text(aes(label=data_idf$CODGEO), size=4, hjust=-1, vjust=-1)+  
  ggtitle("Pourcentage de très jeunes par rapport au très vieux par département en 2010") +
  labs(x = "Pourcentage de 15-29 ans ", y = "Pourcentage de 75 ans et plus")
ggsave(file="Images\\Pourcentage_idf_Tres_jeunes_vieux_2010.png")

# ------ 1999
ggplot(data_idf, aes(x=data_idf$P99_1529_POP, y=data_idf$P99_75P)) +
  geom_point(aes(size=data_idf$P10_POP),shape=1) +
  scale_size_continuous(range = c(3, 10)) +
  geom_smooth(method=lm) + 
  geom_text(aes(label=data_idf$CODGEO), size=4, hjust=-1, vjust=-1)+
  ggtitle("Pourcentage de très jeunes par rapport au très vieux par département en 1999") +
  labs(x = "Pourcentage de 15-29 ans ", y = "Pourcentage de 75 ans et plus")
ggsave(file="Images\\Pourcentage_idf_Tres_jeunes_vieux_1999.png")

ggplot(data_idf, aes(x=data_idf$P10_1529_POP, y=data_idf$P10_POP)) +
  geom_point(shape=1) +
  geom_smooth(method=lm) + 
  geom_text(aes(label=data_idf$CODGEO), size=4, hjust=-1, vjust=-1)


#============================================================
#           Chomage dans les départements francais
#============================================================


P99_hist_chom <- as.data.frame(cbind(emploi$CODGEO,emploi$P99_CHOM_ACT1564))
P99_hist_chom$type <- '99'
P10_hist_chom <- as.data.frame(cbind(emploi$CODGEO,emploi$P10_CHOM_ACT1564))
P10_hist_chom$type <- '10'
colnames(P99_hist_chom) <- c("CODGEO", "chomage", "type")
colnames(P10_hist_chom) <- c("CODGEO", "chomage", "type")
hist_chom <- rbind(P99_hist_chom, P10_hist_chom)

ggplot(hist_chom, aes(hist_chom$chomage, fill=hist_chom$type)) + 
  geom_histogram(binwidth = 1, position = 'identity', alpha = 0.5) +
  ggtitle("Distribution du taux de chomages par département en 2010 et 1999")+
  geom_vline(aes(xintercept=mean(subset(hist_chom, type=="10")$chomage, na.rm=T)),  color="red", linetype="dashed", size=1) +
  geom_vline(aes(xintercept=mean(subset(hist_chom, type=="99")$chomage, na.rm=T)),  color="blue", linetype="dashed", size=1) + 
  labs(y = "Count", x = "Taux de Chomeurs")
ggsave('Images\\Distribution_Chomage_2010_1999.png')

france1<-map(database="france")
#Récupère les numéros des départements correspondant à la carte
num <- read.csv("CSV//DeptNum.csv", header= TRUE, sep=';')
num_cor <- num$Num
#Renomme les départements de la carte avec leur numéro
france1$names <- num_cor

emploi.chomage2010 <- emploi[order(emploi$P10_CHOM_ACT1564),]
emploi.chomage1999 <- emploi[order(emploi$P99_CHOM_ACT1564),]
dpt.chom99 <- emploi.chomage1999$CODGEO
dpt.chom10 <- emploi.chomage2010 $CODGEO
matche.chomage1999<-match.map(france1,dpt.chom99,exact=TRUE)
matche.chomage2010<-match.map(france1,dpt.chom10,exact=TRUE)

yellows <- colorRampPalette(brewer.pal(9,"YlOrRd"))(100)
colors.chomage2010<-yellows[matche.chomage2010]
colors.chomage1999<-yellows[matche.chomage1999]
grandesvilles <- read.csv("CSV//grandesvilles.csv", header=TRUE, sep=';')

# ------------- 2010 
png('Images\\Taux_chomage_2010.png')
map(database="france", fill=TRUE,col=colors.chomage2010,resolution=0)
points(grandesvilles$Longitude, grandesvilles$Latitude, pch=20, col = "black")
text(grandesvilles$Longitude, grandesvilles$Latitude+0.2, grandesvilles$Nom, col="black", cex = 0.5)
title("Taux de chomeurs par départements en 2010")
min <- trunc(emploi.chomage2010$P10_CHOM_ACT1564[1])
max <- trunc(emploi.chomage2010$P10_CHOM_ACT1564[91])
legend("bottomleft", legend = rev(trunc(seq(min,max, 1))), pch = 20, col = yellows[seq(91,0, -91/10)], title="(en %)")
dev.off()
#------------- 1999
png('Images\\Taux_chomage_1999.png')
map(database="france", fill=TRUE,col=colors.chomage1999,resolution=0)
points(grandesvilles$Longitude, grandesvilles$Latitude, pch=20, col = "black")
text(grandesvilles$Longitude, grandesvilles$Latitude+0.2, grandesvilles$Nom, col="black", cex = 0.5)
title("Taux de chomeurs par département en 1999")
min <- trunc(emploi.chomage1999$P99_CHOM_ACT1564[1])
max <- trunc(emploi.chomage1999$P99_CHOM_ACT1564[91])
legend("bottomleft", legend = rev(trunc(seq(min, max, 1.5))), pch = 20, col = yellows[seq(91,0, -91/10)], title="(en %)")
dev.off()

#============================================================
#          Différence de salaire entre les départements
#============================================================

salary <- read.csv('CSV//Salaires2012_clean.csv', sep=';', header=TRUE)
salary_dept <- aggregate(salary,by = list(car=salary$Dept), FUN=mean)
pairs(salary_dept)
head(salary_dept)

S <- salary_dept[c(1, 5,6,7,8,9)]
S$CODGEO <- S$car
Pop <- data[c(2, 4)]

Salaire.Pop <- merge(Pop,S, by ='CODGEO')
Salaire.Pop.interet <- Salaire.Pop[,c(2,4, 5, 6, 7, 8)]
colnames(Salaire.Pop.interet) <- c("Population","Moyen", "Cadres", "Prof", "Employés", "Ouvriers")
cor.mat.salary <- cor(Salaire.Pop.interet)
png('Images\\Correlation_Salaire2012.png')
corrplot(cor.mat.salary,method="shade",shade.col=NA,tl.col="black",tl.srt=45, addCoef.col="black")
dev.off()
#On voit une corrélation entre les différents salaires selon les emplois
#Ce qui implique qu'en général les salaires entre les différentes classes sociales
#Ne subissent pas des écarts très importants

ggplot(Salaire.Pop, aes(x=Salaire.Pop$SC, y=Salaire.Pop$SO)) +
  geom_point(aes(size=Salaire.Pop$P10_POP),shape=1) +
  scale_size_continuous(range = c(3, 10)) +
  geom_smooth(method=lm) + 
  geom_text(aes(label=Salaire.Pop$CODGEO), size=4, hjust=-1, vjust=-1) +
  ggtitle("Salaire des cadres en fonctions de celui des ouvriers en 2010")+
  labs(y = "Salaire horaire des Ouvriers", x = "Salaire horaire des Cadres")
ggsave("Images\\Salaire_ouvriers_cadres.png")
#On voit que le département ou les cadres sont le mieux payé est Paris 
#Et que le département ou les Ouvries sont le mieux payé est le 92 

# ----------------------- IDF ----------------------------------------

#Plus précisément pour les départements d'ile de France


Salaire_Pop_idf = Salaire.Pop[Salaire.Pop$CODGEO %in% dpt_IDF,]

ggplot(Salaire_Pop_idf, aes(x=Salaire_Pop_idf$SC, y=Salaire_Pop_idf$SO)) +
  geom_point(aes(size=Salaire_Pop_idf$P10_POP),shape=1) +
  scale_size_continuous(range = c(3, 10)) +
  geom_smooth(method=lm) + 
  geom_text(aes(label=Salaire_Pop_idf$CODGEO), size=4, hjust=-1, vjust=-1)+
  ggtitle("Salaire des cadres en fonctions de celui des ouvriers en IDF en 2010")+
  labs(y = "Salaire horaire des Ouvriers", x = "Salaire horaire des Cadres")

ggsave("Images\\Salaire_ouvriers_cadres_IDF.png")
#On voit ici qu'il existe une très grande variation de salaire entre
#les département d'ile de France

test <- merge(Salaire.Pop, emploi, by='CODGEO')
test_interet <- test[c(1,2, 4, 12)]
pairs(test_interet)

#Plot CHOMEUR EN FONCITON DU SALAIRE 


#On observe une certaine corrélation entre le salaire et la Population du département

ggplot(test, aes(x=test$S, y=test$P10_POP)) +
  scale_size_continuous(range = c(3, 10)) +
  geom_smooth(method=lm) + 
  geom_text(aes(label=test$CODGEO), size=4, hjust=-1, vjust=-1) +
  ggtitle("Salaire moyen en fonction de la population")+
  labs(y = "Population", x = "Salaire horaire moyen")
ggsave("Images\\Salaire_Population.png")

# ==============================================================
# ------------- Mise en commun de toutes les données ------------
# ===============================================================

total_idf <- merge(data_idf, emploi_idf, by='CODGEO')
total_idf_final <-  merge(total_idf,Salaire_Pop_idf, by='CODGEO')
total_idf_final <- total_idf_final[c(1, 4, 5, 7, 9, 11, 13, 15, 18, 20,26, 27, 28, 29, 30)]
rquery.cormat(total_idf_final[c(2,3,4,5,6,7, 8,9,10,11, 12, 13, 14, 15)])

#On observe pas de corrélation flagrante intérressante entre les différentes 
#Variable utilisées jusque la


# ===================================================================
# ------------- Logement ----------------
# ===================================================================

Logement <- read.csv("CSV//Logement.csv", header=TRUE, sep=';')
fin_logement <- length(Logement$CODGEO)
Logement <- Logement[-c(fin_logement:(fin_logement-5)),-c(1, 3, 6, 8)]

Data_interet_logement <- data[c(2, 4,5, 7, 9, 11, 13, 15)]

Logement_Pop <- merge(Data_interet_logement, Logement, by='CODGEO')
rquery.cormat(Logement_Pop[c(2,3,4,5,6,7, 8,9,10, 11)])

pairs(Logement_Pop)

#On voit une corrélation entre le pourcentage de population 'jeune' 
# Et la part d'appartement dans les départements. On sait que les jeunes 
# préfèrent les appartements pour leur prix et parcequ'ils sont souvent 
# dans des villes plus dynamiques donc ou il y a le plus de travail

Salaire_Logement_pop <- merge(Logement_Pop, Salaire_Pop, by='CODGEO')
pairs(Salaire_Logement_pop)
rquery.cormat(Salaire_Logement_pop[c(2,3,4,5,6,7,8,9,10,11, 12, 14, 15, 16, 17, 18)])


# ---------------------- IDF ------------------------------------

Logement_pop_idf <- Logement_Pop[Logement_Pop$CODGEO %in% dpt_IDF,]

pairs(Logement_pop_idf)
rquery.cormat(Logement_pop_idf[c(2,3,4,5,6,7,8,9,10,11)])

ggplot(Logement_pop_idf, aes(x=Logement_pop_idf$P10_MAISON_LOG, y=Logement_pop_idf$P10_APPART_LOG)) +
  geom_point(aes(size=Logement_pop_idf$P10_LOG),shape=1) +
  scale_size_continuous(range = c(3, 10)) +
  geom_smooth(method=lm) + 
  geom_text(aes(label=Logement_pop_idf$CODGEO), size=4, hjust=-1, vjust=-1)


# ===================================================================
# -------------------- Taux Natalité/Mortalité ----------------------
# ===================================================================

TxNat <- read.csv('CSV//TxNat.csv', header=TRUE, sep=';')
TxMort <- read.csv('CSV//TxMort.csv', header=TRUE, sep=';')

TxNatMort_raw <- merge(TxNat, TxMort, by='CODGEO')
TxNatMort <- TxNatMort_raw[,-c(2, 3, 9, 10)]
pairs(TxNatMort)
rquery.cormat(TxNatMort[c(2,3,4,5,6,7,8,9,10,11)])

#On voit bien que le taux de mortalité est très corrélé avec les taux
#de mortalité la même année (en effet il est inversement proportionel)
#On en déduit que plus un département possède un taux élevé de natalité
#Il possède aussi un très faible taux de mortalité
TxNatMort_idf <- TxNatMort[TxNatMort$CODGEO %in% dpt_IDF,]


ggplot(TxNatMort_idf, aes(x=TxNatMort_idf$TNAT9910, y=TxNatMort_idf$TMOR9910)) +
  geom_point(aes(size=data_idf$P10_POP),shape=1) +
  scale_size_continuous(range = c(3, 10)) +
  geom_smooth(method=lm) + 
  geom_text(aes(label=TxNatMort_idf$CODGEO), size=4, hjust=-1, vjust=-1)

ggplot(TxNatMort_idf, aes(x=TxNatMort_idf$TNAT9099, y=TxNatMort_idf$TMOR9099)) +
  geom_point(aes(size=data_idf$P10_POP),shape=1) +
  scale_size_continuous(range = c(3, 10)) +
  geom_smooth(method=lm) + 
  geom_text(aes(label=TxNatMort_idf$CODGEO), size=4, hjust=-1, vjust=-1)

ggplot(TxNatMort_idf, aes(x=TxNatMort_idf$TNAT8290, y=TxNatMort_idf$TMOR8290)) +
  geom_point(aes(size=data_idf$P10_POP),shape=1) +
  scale_size_continuous(range = c(3, 10)) +
  geom_smooth(method=lm) + 
  geom_text(aes(label=TxNatMort_idf$CODGEO), size=4, hjust=-1, vjust=-1)

ggplot(TxNatMort_idf, aes(x=TxNatMort_idf$TNAT7582, y=TxNatMort_idf$TMOR7582)) +
  geom_point(aes(size=data_idf$P10_LOG),shape=1) +
  scale_size_continuous(range = c(3, 10)) +
  geom_smooth(method=lm) + 
  geom_text(aes(label=TxNatMort_idf$CODGEO), size=4, hjust=-1, vjust=-1)

ggplot(TxNatMort_idf, aes(x=TxNatMort_idf$TNAT6875, y=TxNatMort_idf$TMOR6875)) +
  geom_point(aes(size=data_idf$P10_LOG),shape=1) +
  scale_size_continuous(range = c(3, 10)) +
  geom_smooth(method=lm) + 
  geom_text(aes(label=TxNatMort_idf$CODGEO), size=4, hjust=-1, vjust=-1)


TxNatMort_2010 <- TxNatMort[c(1,2, 7)]
Tx_log <- merge(TxNatMort_2010, Logement_Pop, by='CODGEO')
pairs(Tx_log)
rquery.cormat(Tx_log[c(2,3,4,5,6,7,8,9,10,11,12,13)])

#On voit ici que plus le département possède un population jeune
#plus le taux de natalité est important ce qui est plutot normal dans l'ensemble
#Mais aussi que plus la population est vieille plus le taux de mortalité est élevé
#On a donc au fil des ans des départements qui vont devenir de plus en plus vieux
#Et d'autre qui vont devenir de plus en plus jeune ce qui va impacté très fortement 
#L'aspect économique de ces derniers.
Tx_Salaire <- merge(TxNatMort_2010, Salaire_Pop, by ='CODGEO')
pairs(Tx_Salaire)
rquery.cormat(Tx_Salaire[c(2,3,4,6,7,8,9,10)])


#Interpréation : 

# ==================================================================
# ------------- Différents part d'emploi dans les depts ------------
# ==================================================================

emploi.pop.raw <- read.csv('CSV//EmploiPop.csv', sep=';', header=TRUE)
fin.emploi.pop <- length(emploi.pop.raw$CODGEO)
emploi.pop <- emploi.pop.raw[-c(fin.emploi.pop:(fin.emploi.pop-5)),]
columns.2010 <- c(2, 4, 5, 7, 9, 11, 13, 15, 17, 19)
columns.1999 <- c(2, 4, 6, 8, 10, 12, 14, 16 , 18, 20)
emploi.pop.2010 <- emploi.pop[,columns.2010]
emploi.pop.1999 <- emploi.pop[,columns.1999]

pairs(emploi.pop.2010)
pairs(emploi.pop.1999)

cor.mat.emploi.2010 <- cor(emploi.pop.2010[,c(2,3,4,5,6,7,8,9,10)])
cor.mat.emploi.1999 <- cor(emploi.pop.1999[,c(2,3,4,5,6,7,8,9,10)])
corrplot(cor.mat.emploi.2010,method="shade",shade.col=NA,tl.col="black",tl.srt=45, addCoef.col="black")
corrplot(cor.mat.emploi.1999,method="shade",shade.col=NA,tl.col="black",tl.srt=45, addCoef.col="black")

# ==============================================


cadre.p.2010 <- emploi.pop.2010[order(-emploi.pop.2010$C10_CADRE),]
prof.p.2010 <- emploi.pop.2010[order(-emploi.pop.2010$C10_PROF),]
cadre.p.1999 <- emploi.pop.1999[order(-emploi.pop.1999$C99_CADRE),]
prof.p.1999 <- emploi.pop.1999[order(-emploi.pop.1999$C99_PROF),]

dpt.cadre.2010 <- cadre.p.2010$CODGEO
dpt.prof.2010 <- prof.p.2010$CODGEO
dpt.cadre.1999 <- cadre.p.1999$CODGEO
dpt.prof.1999 <- prof.p.1999$CODGEO

N.emploi <- length(emploi_pop_2010$CODGEO)

ylim_idf =c(48.104485,49.257756)
xlim_idf = c(1.414525, 3.589818)

# =================== CADRE ==========================

match.cadre.2010 <- match.map(france1,dpt.cadre.2010)
match.cadre.1999 <- match.map(france1,dpt.cadre.1999)

reds <- rev(colorRampPalette(brewer.pal(9,"Reds"))(100))
color.cadre.2010 <- reds[match.cadre.2010]
color.cadre.1999 <- reds[match.cadre.1999]

#------------ 2010
png("Images\\Carte_cadre_2010.png")
map(database="france", fill=TRUE, col=color.cadre.2010)
title("Part de cadre dans la population de chaque département en 2010")
min <- cadre.p.2010$C10_CADRE[length(cadre.p.2010$C10_CADRE)]
max <- cadre.p.2010$C10_CADRE[1]
legend("bottomleft", legend = trunc(seq(trunc(min), trunc(max), trunc(cadre.p.2010$C10_CADRE[1])/10)), pch = 20, col = reds[seq(91,0, -91/11)], title="(en %)")
dev.off()
#------------ 1999
png("Images\\Carte_cadre_1999.png")
map(database="france", fill=TRUE, col=color.cadre.1999)
title("Part de cadre dans la population de chaque département en 1999")
min <- cadre.p.1999$C99_CADRE[length(cadre.p.1999$C99_CADRE)]
max <- cadre.p.1999$C99_CADRE[1]
legend("bottomleft", legend = trunc(seq(min, max, trunc(max)/10)), pch = 20, col = reds[seq(91,0, -91/11)], title="(en %)")
dev.off()
# =================== PROF ==========================

match.prof.2010 <- match.map(france1,dpt.prof.2010)
match.prof.1999 <- match.map(france1,dpt.prof.1999)

blues <- rev(colorRampPalette(brewer.pal(9,"Blues"))(100))

color.prof.2010 <- blues[match.prof.2010]
color.prof.1999 <- blues[match.prof.1999]

# -------------- 2010 
png("Images\\Carte_prof_2010.png")
map(database="france", fill=TRUE, col=color.prof.2010)
title("Part de prof dans la population de chaque département en 2010")
min <- trunc(prof.p.2010$C10_PROF[length(prof.p.2010$C10_PROF)])
max <- trunc(prof.p.2010$C10_PROF[1])
legend("bottomleft", legend = trunc(seq(min, max, 1)), pch = 20, col = blues[seq(91,0, -91/11)], title="(en %)" )
dev.off()
# -------------- 1999
png("Images\\Carte_prof_1999.png")
map(database="france", fill=TRUE, col=color.prof.1999)
title("Part de prof dans la population de chaque département en 1999")
min <- trunc(prof.p.1999$C99_PROF[length(prof.p.1999$C99_PROF)])
max <- trunc(prof.p.1999$C99_PROF[1])
legend("bottomleft", legend = trunc(seq(min, max, 1)), pch = 20, col = blues[seq(91,0, -91/11)], title="(en %)" )
dev.off()