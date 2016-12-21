library(ggplot2)
library(maps)
library(maptools)

data = read.csv("CSV//AgePop.csv", header = TRUE, sep=';')

#On supprime les 6 derniÃ¨res lignes de la DataFrame
#Elles correspondent Ã  la moyenne sur la France et les dÃ©partements d'OutreMer
fin <- length(data$P10_POP)
data <- data[-c(fin:(fin-5)),]


#On
Pop <- as.numeric(data$P10_POP)

deptName <- data$LIBGEO
names(Pop)<- deptName

barplot(Pop)

#=====================================================================
                              #Library(Maps)
#=====================================================================

france1<-map(database="france")
#Récupère les numéros des départements correspondant à la carte
num <- read.csv("CSV//DeptNum.csv", header= TRUE, sep=';')
num_cor <- num$Num
#Renomme les départements de la carte avec leur numéro
france1$names <- num_cor

dpt <- data$CODGEO
matche<-match.map(france1,dpt,exact=TRUE)
grey.colors<-function(n) grey(rev(0:(n-1)/1.5)/n)
colors<-gray.colors(max(data$P10_POP))[data$P10_POP[matche]]
map(database="france", fill=TRUE,col=colors,resolution=0)


Pop1529_2010 <- as.numeric(data$P10_1529_POP)
colors<-gray.colors(max(Pop1529_2010))[Pop1529_2010[matche]]
map(database="france", fill=TRUE,col=colors,resolution=0)

grandesvilles <- read.csv("CSV//grandesvilles.csv", header=TRUE, sep=';')
grandesvilles

#Ajoute les plus grandes villes sur la carte
points(grandesvilles$Longitude, grandesvilles$Latitude, pch=20, col = "red")
#Précise le nom de ces villes
text(grandesvilles$Longitude, grandesvilles$Latitude+0.2, grandesvilles$Nom, col="red", cex = 0.5)

#=====================================================================
                              #Library(MapTools)
#=====================================================================

france<-readShapeSpatial("SHP//LIMITE_DEPARTEMENT.SHP", proj4string=CRS("+proj=longlat"))
departements<-readShapeSpatial("SHP//DEPARTEMENT.SHP")

#Affiche les différentes régions
#plot(departements,col=as.numeric(departements$CODE_REG))


plot(france,lwd=2)
test<-match(departements$CODE_DEPT,data$CODGEO)
couleurs<-Pop[test]/max(Pop)
couleurs[is.na(couleurs)] <- 0
plot(departements,col=gray(1-couleurs), add=TRUE)

#Ajoute les plus grandes villes sur la carte
points(grandesvilles$Longitude, grandesvilles$Latitude, pch=20, col = "red")
#Précise le nom de ces villes
text(grandesvilles$Longitude, grandesvilles$Latitude+0.2, grandesvilles$Nom, col="red", cex = 0.5)

