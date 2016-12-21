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
library(ggmap)
library(httr)

source("http://www.sthda.com/upload/rquery_cormat.r")

developper.key <-  "AIzaSyB3kZDu0v2yw_fgi9UJGNZPejKE5qNuPiA"

getDept <- function(x){
  return (substr(x, 1, 2))
}

getMonth <- function(x){
  return(substr(x,6,7))
}

patents <- read.csv("CSV//prior_pat_fr.csv", sep=',', header=TRUE)

local.adress <- paste(patents$adr_final[1], patents$zip_code_comp[1], sep = ' ')
local.adress.final <- paste(local.adress, patents$city_comp[1], sep = ' ')


patents$Dept <- getDept(patents$zip_code_comp)
patents$Month <- getMonth(patents$appln_filing_date)
patents$YearMonth <- as.numeric(paste(patents$appln_filing_year, patents$Month, sep=''))


aggdata <-aggregate(patents$frac_invt, by=list(patents$appln_filing_year,patents$Dept), FUN=sum, na.rm=TRUE)
aggdata[aggdata$Group.2<=95,]

aggdata.year.month <-aggregate(patents$frac_invt, by=list(patents$YearMonth), FUN=sum, na.rm=TRUE)


barplot_year <- ggplot(patents, aes(y=patents$frac_invt, x=as.factor(patents$YearMonth))) + 
    geom_bar(stat='identity') 

ggplot(patents, aes(y=patents$frac_invt, x=patents$appln_filing_year)) + 
  geom_bar(stat='identity') +
  ggtitle("Dépots de brevet par an") +
  labs(x="Années" , y = "Nombre de brevet")
ggsave("Images\\NombreDeBrevetParAns.png")

colnames(aggdata.zipcode) <- c("CODGEO", "NBPatens")

#On récupère les 10 départements ou l'on dépose le plus de brevets
best.dept.patents <- tail(aggdata.zipcode[order(aggdata.zipcode$NBPatens),],10)$CODGEO
patents.best <- patents[patents$Dept %in% best.dept.patents,]

ggplot(patents.best, aes(y=patents.best$frac_invt, x=patents.best$Dept)) + 
  geom_bar(stat='identity', fill='red') +
  ggtitle("Dépots de brevet pour les 10 plus gros départements") +
  labs(x="Années" , y = "Départements")
ggsave("Images\\NombreDeBrevetParAns10BEST.png")

aggdata.zipcode <- aggregate(patents$frac_invt, by=list(patents$Dept), FUN=sum, na.rm=TRUE)
aggdata.zipcode <- aggdata.zipcode[aggdata.zipcode$Group.1<=95,]


france1<-map(database="france")
#Récupère les numéros des départements correspondant à la carte
num <- read.csv("CSV//DeptNum.csv", header= TRUE, sep=';')
num_cor <- num$Num
#Renomme les départements de la carte avec leur numéro
france1$names <- num_cor

patents.best.df <-aggdata.zipcode[order(aggdata.zipcode$NBPatens),]
dpt <- patents.best.df$CODGEO
matche<-match.map(france1,dpt,exact=TRUE)
blues <- colorRampPalette(brewer.pal(9,"Blues"))(100)
colors <- blues[matche]

png("Images\\carteDepotDeBrevet.png")
map(database="france", fill=TRUE,col=colors,resolution=0)
#Ajoute les plus grandes villes sur la carte
points(grandesvilles$Longitude, grandesvilles$Latitude, pch=20, col = "red")
#Précise le nom de ces villes
text(grandesvilles$Longitude, grandesvilles$Latitude+0.4, grandesvilles$Nom, col="red", cex = 1)
dev.off()
data_Pop = read.csv("CSV//AgePop.csv", header = TRUE, sep=';')

colnames(aggdata.zipcode) <- c('CODGEO', 'NB_PATENTS')
aggdata.zipcode.pop <- merge(aggdata.zipcode, data_Pop, by='CODGEO')

ggplot(aggdata.zipcode.pop, aes(x=aggdata.zipcode.pop$P10_POP, y=aggdata.zipcode.pop$NB_PATENTS)) +
  geom_point(aes(size=aggdata.zipcode.pop$P10_POP),shape=1) +
  scale_size_continuous(range = c(3, 10)) +
  geom_smooth(method=lm) + 
  geom_text(aes(label=aggdata.zipcode.pop$CODGEO), size=4, hjust=-1, vjust=-1)+
  ggtitle("Nombre de brevet en fonction de la population ") +
  labs(x="Population", y='Nombre de brevet')
ggsave("Images\\PopNbPatents.png")

ggplot(aggdata.zipcode.pop, aes(x=aggdata.zipcode.pop$NB_PATENTS, y=aggdata.zipcode.pop$P10_3044_POP)) +
  geom_point(aes(size=aggdata.zipcode.pop$P10_POP),shape=1) +
  scale_size_continuous(range = c(3, 10)) +
  geom_smooth(method=lm) + 
  geom_text(aes(label=aggdata.zipcode.pop$CODGEO), size=4, hjust=-1, vjust=-1)

cor_mat <- cor(aggdata.zipcode.pop[,c(2,5,6, 8, 10, 12, 14, 16)])
corrplot(cor_mat)
png("Images\\CorrélationPatentsPopulation.png")
corrplot(cor_mat,method="shade",shade.col=NA,tl.col="black",tl.srt=45, addCoef.col="black")
dev.off()

pairs(aggdata.zipcode.pop[,c(2,5,6, 8, 10, 12, 14, 16)])

data_salaire <- read.csv('CSV//EmploiPop.csv', sep=';', header=TRUE)
aggdata.zipcode.salaire <- merge(aggdata.zipcode, data_salaire, by='CODGEO')
cor_mat_salaire <- cor(aggdata.zipcode.salaire[,c(2,5,6, 8, 10, 12, 14, 16)])

png("Images\\CorrélationSalairePatents.png")
corrplot(cor_mat_salaire,method="shade",shade.col=NA,tl.col="black",tl.srt=45, addCoef.col="black")
dev.off()

ggplot(aggdata.zipcode.salaire, aes(x=aggdata.zipcode.salaire$NBPatens, y=aggdata.zipcode.salaire$C10_CADRE)) +
  geom_point(aes(size=aggdata.zipcode.salaire$P10_POP15P),shape=1) +
  scale_size_continuous(range = c(3, 10)) +
  geom_smooth(method=lm) + 
  geom_text(aes(label=aggdata.zipcode.salaire$CODGEO), size=4, hjust=-1, vjust=-1) +
  ggtitle("Nombre de brevets en fonction de la part de cadre") + 
  labs(x='Nombre de brevet', y='Pourcentage de cadre')
ggsave("Images\\EmploiPatents.png")

