data = read.csv("CSV//AgePop.csv", header = TRUE, sep=';')

#On supprime les 6 derniÃ¨res lignes de la DataFrame
#Elles correspondent Ã  la moyenne sur la France et les dÃ©partements d'OutreMer
fin <- length(data$P10_POP)
data <- data[-c(fin:(fin-5)),]

#On
Pop <- as.numeric(data$P10_POP)
deptName <- data$LIBGEO
names(Pop)<- deptName

Pop_sorted <- sort(Pop, decreasing = FALSE)

barplot(Pop_sorted, las=2)

POP10_0014per <- data$P10_0014_POP
POP10_1529per <- data$P10_1529_POP
POP10_3044per <- data$P10_3044_POP
POP10_4559per <- data$P10_4559_POP
POP10_6074per <- data$P10_6074_POP
POP10_75Pper <- data$P10_75P_POP

names(POP10_0014per)<- deptName
names(POP10_1529per)<- deptName

#On trie les département par pourcentage de jeune 
POP10_1529per_sorted <- sort(POP10_0014per, decreasing=TRUE)

Intervalle <- 5
Premiers<- c(1:Intervalle)
fin <- length(POP10_1529per_sorted)
Derniers <- c(fin:(fin-Intervalle))

PD_1529 <- POP10_1529per_sorted[Premiers]
DD_1529 <- POP10_1529per_sorted[Derniers]

TD_1529 <- sort(c(DD_1529, PD_1529))

barplot(TD_1529, las=2)



