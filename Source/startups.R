
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
library(stringr)
library(plyr)
library(jsonlite)

age.Pop <- read.csv("CSV\\AgePop.csv" , sep =';')

getUrlAdress <- function(x){
  tmp <- gsub("-", " ", x)
  tmp <- gsub("  ", " ", tmp)
  adress <- gsub(" ", "+", tmp)
  url <- 'https://maps.googleapis.com/maps/api/geocode/json?address='
  developper.key <-  "&components=country:FR&key=AIzaSyB3kZDu0v2yw_fgi9UJGNZPejKE5qNuPiA"
  url.tmp <- paste(url,adress, sep='')
  return(paste(url.tmp, developper.key, sep=''))
}

startups.with.type.Togeocode <- read.csv('CSV\\StartUpDataBase_type.csv', sep=';')
startups.with.type.Togeocode$UrlAdress <- getUrlAdress(startups.with.type.Togeocode$Adresse.du.siège)
# startups$Lat <- "NA"
# startups$Long <- "NA"
startups.with.type.Togeocode$Lat <- "NA"
startups.with.type.Togeocode$Long <- "NA"


for(i in 1:length(startups.with.type.Togeocode$Téléphone)){
  tryCatch({
    tmp <- GeocodeAdress(startups.with.type.Togeocode[i,]$UrlAdress)
    startups.with.type.Togeocode[i,]$Lat <- tmp[1] 
    startups.with.type.Togeocode[i,]$Long <- tmp[2] 
  }, error=function(e){cat("ERROR :","There is some error", "\n")})
}

# 
# 
# GeocodeAdress <- function(x){
#   request <- fromJSON(x)
#   print(request$results$geometry$location$lat)
#   return(getLatLong(request))
# }
# 
# getLatLong <- function(x){
#   return(x$results$geometry$location$lat, x$results$geometry$location$lng))
# }
# 
# 
# getZipCode <- function(x){
#   return(str_extract(x, "([0-9][0-9][0-9][0-9][0-9])"))
# }
# getDept <- function(x){
#   return (substr(x, 1, 2))
# }
# getYear <- function(x){
#   return (substr(x, 7, 10))
# }


# startups <- read.csv('CSV\\StartUpDataBase.csv', sep=';', header=TRUE)
# 
# url.test <- 'https://maps.googleapis.com/maps/api/geocode/json?address=2+avenue+Alphonse+Bordereau+77500+CHELLES&components=country:FR&key=AIzaSyB3kZDu0v2yw_fgi9UJGNZPejKE5qNuPiA'

# ========================================================================
# ========================================================================
# ========================================================================

dpt.idf <- c(75, 77, 78, 91, 92, 93, 94, 95)

# startups["ZipCode"] <- getZipCode(startups$adresse_siege)
# startups['Dept'] <- getDept(startups$ZipCode)
# startups['Nb'] <- 1
# startups['Year'] <- getYear(startups$Date_creation)

startups.with.type <- read.csv('CSV\\StartUpDataBase_type.csv', sep=';')
startups.with.coordinates <- read.csv('CSV\\Startups_geocodéssep.csv', sep=',')
startups <- cbind(startups.with.coordinates, startups.with.type$Classe)
test <- merge(startups.with.type, startups.with.coordinates, by="adresse_siege", all.y = FALSE, all.x=TRUE)

startups.with.type["ZipCode"] <- getZipCode(startups.with.type$Adresse.du.siège)
startups.with.type['Dept'] <- getDept(startups.with.type$ZipCode)
startups.with.type['Nb'] <- 1
startups.with.type['Year'] <- getYear(startups.with.type$Date.de.création)

startups.idf <- startups[startups$Dept %in% dpt.idf,]

aggdata.dept <-aggregate(startups$Nb, by=list(startups$Dept), FUN=sum, na.rm=TRUE)
colnames(aggdata.dept) <- c("CODGEO", 'NBStartup')
aggdata.dept.idf <- aggdata.dept[aggdata.dept$CODGEO %in% dpt.idf,]

dpt.best.creation <- tail(aggdata.dept[order(aggdata.dept$NBStartup),],10)$CODGEO
startup.best.dept <- startups[startups$Dept %in% dpt.best.creation,]


ggplot(startups, aes(y=startups$Nb, x=startups$Dept)) + 
  geom_bar(stat='identity') +
  labs(title = "New plot title")
  

ggplot(startup.best.dept, aes(y=startup.best.dept$Nb, x=startup.best.dept$Dept)) + 
  geom_bar(stat='identity') 

ggplot(startups.idf, aes(y=startups.idf$Nb, x=startups.idf$Dept)) + 
  geom_bar(stat='identity')

#On voit très bien que Paris est nettement devant

x.scale.creation <- seq(1980, 2015, 1)

#Distribution des création de startups en France depuis 1875 
startups.small <- startups.with.type[startups.with.type$Year %in% x.scale.creation,]
ggplot(startups.small, aes(y=startups.small$Nb, x=startups.small$Year, fill=startups.small$Classe)) + 
  geom_bar(stat='identity') +
  theme(axis.text.x = element_text(angle = 90, hjust = 0)) +
  ggtitle("Création de startups en France depuis 1980") +
  labs(x="Années" , y = "Nombre de startups")
ggsave("Images\\NombreDeStartupsFrance.png")

ggplot(startups.with.type, aes(y=startups.with.type$Nb, x=startups.with.type$Year, fill=startups.with.type$Classe)) + 
  geom_bar(stat='identity') +
  theme(axis.text.x = element_text(angle = 90, hjust = 0)) +
  ggtitle("Création de startups en France depuis 1875") +
  labs(x="Années" , y = "Nombre de startups")
ggsave("Images\\NombreDeStartupsFranceType.png")

ggplot(startups.with.type, aes(y=startups.with.type$Nb, x=startups.with.type$Classe,  fill=startups.with.type$Classe)) + 
  geom_bar(stat='identity') +
  theme(axis.text.x = element_text(angle = 90, hjust = 0)) +
  ggtitle("Type de Startups") +
  labs(x="Type" , y = "Nombre de startups")
ggsave("Images\\TypeDeStartups.png")

ggplot(startups.with.type, aes(y=startups.with.type$Nb, x=startups.with.type$Dept, fill=startups.with.type$Classe)) + 
  geom_bar(stat='identity') +
  theme(axis.text.x = element_text(angle = 90, hjust = 0)) +
  ggtitle("Type de Startups") +
  labs(x="Type" , y = "Nombre de startups")
ggsave("Images\\StartupsParDepartements.png")


#En idf
ggplot(startups.idf, aes(y=startups.idf$Nb, x=startups.idf$Year)) + 
  geom_bar(stat='identity')

startup.pop$RatioCcreation <- startup.pop$NBStartup/startup.pop$P10_POP
df.ratio.creation <- startup.pop[,c(1,11)]

#Département les plus créatif en fonction de la population
ordered.Dept.creation <- startup.pop[order(startup.pop$RatioCcreation),]$CODGEO

colnames.tmp <- colnames(startup.pop)
colnames(startup.pop) <- c("CODGEO",  "NbStartups","LIBGEO", "Population", "0 - 14 ans", "15 - 29 ans","30 - 40 ans",
                                   "45 - 59 ans", "60 - 74 ans ", "75 ans et plus","NbStartups/Pop")
mat.cor.startup.pop <- cor(startup.pop[,-c(1,3)])
colnames.tmp -> colnames(startup.pop)
corrplot(mat.cor.startup.pop,method="shade",shade.col=NA,tl.col="black",tl.srt=45, addCoef.col="black")

#=======================================================
#On supprime Paris qui tasse toutes nos autres données


startup.pop.without.Paris <- startups.with.type[startups.with.type$Dept!=75,]
aggdata.dept.WOP <-aggregate(startup.pop.without.Paris$Nb, by=list(startup.pop.without.Paris$Dept), FUN=sum, na.rm=TRUE)
colnames(aggdata.dept.WOP) <- c('CODGEO', 'NBStartups')
data.pop <- read.csv("CSV//AgePop.csv", header = TRUE, sep=';')
data.pop10 <- data_Pop[,c(2, 3, 4, 5, 7, 9, 11, 13, 15)]

startup.pop <- merge(aggdata.dept.WOP, data.pop10, by='CODGEO')
startup.pop.ratio <- merge(startup.pop, df.ratio.creation, by='CODGEO')


mat.cor.startup.pop.WO.P <- cor(startup.pop.ratio[,-c(1,3)])

png("CorrélationAgeRatioStartups.png")
corrplot(mat.cor.startup.pop.WO.P,method="shade",shade.col=NA,tl.col="black",tl.srt=45, addCoef.col="black")
dev.off()

ggplot(startup.pop.without.Paris, aes(y=startup.pop.without.Paris$Nb, x=startup.pop.without.Paris$Dept, fill=startup.pop.without.Paris$Classe)) + 
  geom_bar(stat='identity') +
  theme(axis.text.x = element_text(angle = 90, hjust = 0)) +
  ggtitle("Type de Startups") +
  labs(x="Type" , y = "Nombre de startups")
ggsave("Images\\StartupsParDepartementsWithoutParis.png")

#Garder que les 10 départements les plus important
best.dept10 <- tail(startup.pop[order(startup.pop$NBStartups),]$CODGEO,10)
starups.best.10 <- startup.pop.without.Paris[startup.pop.without.Paris$Dept %in% best.dept10,]

ggplot(starups.best.10, aes(y=starups.best.10$Nb, x=starups.best.10$Dept, fill=starups.best.10$Classe)) + 
  geom_bar(stat='identity') +
  theme(axis.text.x = element_text(angle = 90, hjust = 0)) +
  ggtitle("Type de Startups") +
  labs(x="Type" , y = "Nombre de startups")
ggsave("Images\\DixDépartementMeilleurCreationStartups.png")


ggplot(startup.pop.ratio, aes(x=startup.pop.ratio$NBStartups, y=startup.pop.ratio$P10_POP)) +
  geom_point(aes(size=startup.pop.ratio$P10_3044_POP),shape=1) +
  scale_size_continuous(range = c(3, 10)) +
  geom_smooth(method=lm) + 
  geom_text(aes(label=startup.pop.ratio$CODGEO), size=4, hjust=-1, vjust=-1)

startup.pop.idf <- startup.pop[startup.pop$CODGEO %in% dpt.idf,]
ggplot(startup.pop.idf, aes(x=startup.pop.idf$NBStartup, y=startup.pop.idf$P10_POP)) +
  geom_point(aes(size=startup.pop.idf$P10_POP15P),shape=1) +
  scale_size_continuous(range = c(3, 10)) +
  geom_smooth(method=lm) + 
  geom_text(aes(label=startup.pop.idf$CODGEO), size=4, hjust=-1, vjust=-1)

#================================= Salary

salary <- read.csv('CSV//Salaires2012_clean.csv', sep=';', header=TRUE)
salary_dept <- aggregate(salary,by = list(car=salary$Dept), FUN=mean)
salary.clean <- salary_dept[,c(1, 5, 6, 7, 8, 9)]
colnames(salary.clean) <- c('CODGEO', "S", "SC", "SP", "SE", "SO")
startups.salary <- merge(aggdata.dept.WOP, salary.clean, by='CODGEO')

startups.salaray.ratio <- merge(startups.salary, df.ratio.creation, by="CODGEO")

mat.cor.startup.salary.WOP <- cor(startups.salaray.ratio[,-1])
corrplot(mat.cor.startup.salary.WOP,method="shade",shade.col=NA,tl.col="black",tl.srt=45, addCoef.col="black")


load('patent.data')
aggdata.zipcode <- aggregate(patents$frac_invt, by=list(patents$Dept), FUN=sum, na.rm=TRUE)
patents.zipcode <- aggdata.zipcode <- aggdata.zipcode[aggdata.zipcode$Group.1<=95,]
colnames(patents.zipcode) <- c('CODGEO', 'NbPatents')

patents.startup.pop <- merge(patents.zipcode, startup.pop, by='CODGEO' )
patents.startup.pop$RatioPatent <- patents.startup.pop$NbPatents/patents.startup.pop$P10_POP

colnames.tmp <- colnames(patents.startup.pop)
colnames(patents.startup.pop) <- c("CODGEO", "NbPatents", "NbStartups",
            "LIBGEO", "Population", "0 - 14 ans", "15 - 29 ans","30 - 40 ans",
            "45 - 59 ans", "60 - 74 ans ", "75 ans et plus","NbStartups/Pop", "NbPatents/Pop")
mat.cor.patent.startup.pop <- cor(patents.startup.pop[,-c(1, 4)])

colnames(patents.startup.pop) <- colnames.tmp

mat.cor.startup.pop <- cor(startup.pop[,-1])
corrplot(mat.cor.startup.pop,method="shade",shade.col=NA,tl.col="black",tl.srt=45, addCoef.col="black")



patents$type <- "patent"
startups$type <- "startup"
patents.startup <- rbind.fill(patents, startups)

ggplot(patents.startup.pop, aes(x=patents.startup.pop$NBStartup, y=patents.startup.pop$P10_POP)) +
  geom_point(aes(size=patents.startup.pop$P10_POP15P),shape=1) +
  scale_size_continuous(range = c(3, 10)) +
  geom_smooth(method=lm) + 
  geom_text(aes(label=patents.startup.pop$CODGEO), size=4, hjust=-1, vjust=-1)


france1<-map(database="france")
#Récupère les numéros des départements correspondant à la carte
num <- read.csv("CSV//DeptNum.csv", header= TRUE, sep=';')
num_cor <- num$Num
#Renomme les départements de la carte avec leur numéro
france1$names <- num_cor





grandesvilles <- read.csv("CSV//grandesvilles.csv", header=TRUE, sep=';')

startup.pop <- merge(aggdata.dept, age.Pop, by='CODGEO')[,c(1,2, 5,10)]

startup.zipcode <- startup.pop[order(startup.pop$NBStartup),]
startup.3044 <- startup.pop[order(startup.pop$P10_3044_POP),]

dpt.startup <- startup.zipcode$CODGEO
dpt.3044 <- startup.zipcode$CODGEO
match.startup<-match.map(france1,dpt.startup,exact=TRUE)
match.3044<-match.map(france1,dpt.startup,exact=TRUE)

reds <- colorRampPalette(brewer.pal(9,"Reds"))(100)
blues <- colorRampPalette(brewer.pal(9,"BLues"))(100)

colors.startup<-reds[match.startup]
colors.3044<-blues[match.3044]

png("Images\\NombreStartupsParDept.png")

map(database="france",resolution=0, col=colors.startup, fill=TRUE)
points(startups$Long, startups$Lat, pch=20, col = "green", cex=1)
points(grandesvilles$Longitude, grandesvilles$Latitude, pch=20, col = "black")
text(grandesvilles$Longitude, grandesvilles$Latitude+0.4, grandesvilles$Nom, pch=20, col = "black")
title("Nombre de startups par départements")
legend("bottomleft", legend = rev(seq(1, 1500, 150)), pch = 20, col = reds[seq(91,0, -91/11)])
dev.off()

png("Images\\CarteNombreStartupsParDept3044.png")

map(database="france",resolution=0, col=colors.3044, fill=TRUE)
points(startups$Long, startups$Lat, pch=20, col = "red", cex=1)
points(grandesvilles$Longitude, grandesvilles$Latitude, pch=20, col = "black")
text(grandesvilles$Longitude, grandesvilles$Latitude+0.4, grandesvilles$Nom, pch=20, col = "black")
title("Taux de 30 - 44 ans")
legend("bottomleft", legend = rev(seq(10, 20, 1)), pch = 20, col = blues[seq(91,0, -91/11)])
dev.off()

# ------- IDF ---------
ylim_idf =c(48.104485,49.257756)
xlim_idf = c(1.414525, 3.589818)

map(database="france",resolution=0, xlim=xlim_idf, ylim = ylim_idf)
points(startups$Long, startups$Lat, pch=20, col = "red", cex=1)
points(grandesvilles$Longitude, grandesvilles$Latitude, pch=20, col = "black")
text(grandesvilles$Longitude, grandesvilles$Latitude, grandesvilles$Nom, pch=20, col = "black")

