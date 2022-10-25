library(MASS)


# question 1


data("silicium")
#rm(list = ls())
setwd("~/Desktop")
#values=read.table("silicium.txt",header=TRUE)
getwd()
silicium <- read.table("silicium.txt", header = TRUE)

attach(silicium)
summary(silicium)
#rownames(silicium)
#colnames(silicium)
plot(silicium)



# question 2



X = as.matrix(silicium[, -7])
# on enl�ve Camber
siliciumtransposee = t(X) %*% X
# on calcule alors la transpos�e de ce qu'on a obtenu


# on observe que :
# La matrice est orthogonale. 
# Les exp�riences sont r�pliqu�es
# car beaucoup de variabilit� sur la sortie
# c'est un plan d'exp�rience fractionnaire 2^(6 - 2)


X2 = as.matrix(silicium[1:16, -7])
# on consid�re les lignes 1 � 16 tout en enlevant encore la colonne 7
# pour ne pas prendre en compte les r�p�titions d'exp�rience
X2[,1] * X2[, 2] * X2[,3] == X2[,5]
X2[,1] * X2[, 3] * X2[,4] == X2[,6]
# Ctime interaction triple de Ltemp, Ltime et Lpress
# Catmos interaction triple de Ltemp, Lpress et Ctemp
# Le plan est de r�solution 4 ( car 6-2)

#on va maintenant faire une regression lin�aire comme lors des pr�c�dents BE
siliciumLm = lm(Camber ~ Ltemp + Ltime + Lpress + Ctemp + Ctime + Catmos, data = silicium)
# puis faire une anova
siliAnova = anova(siliciumLm)
# Ltime et Ctemp ne sont pas significatives, on peut les supprimer

# question 3

summary(siliciumLm)
# stdError = sqrt(residualStandardError^2 / 64)

#on refait de m�me en enlevant Ltime et Ctemp qui ne sont pas significatives
siliciumLmsansautresfacteurs = lm(Camber ~ Ltemp + Lpress + Ctime + Catmos, data = silicium)
summary(siliciumLmsansautresfacteurs)
# Tous les co�fficients sont les m�mes et les variables passent le test de Student
#on garde donc comme �a

# On regarde les coefficients de la colonne Estimate
# coef positif=> on minimise
# coef n�gatif => on maximise la valeur associ�e 
# �a devrait minimiser la courbure

sili3
moyenneCourbureSilicium = mean(CourbureSilicium)

frame = data.frame(Ltemp = -1, Lpress = -1, Ctime = 1, Catmos = 1)
predict(siliciumLmsansautresfacteurs, frame, interval="confidence", level=0.95)
# L'intervalle de confiance est donc [-17.9689; 25.12515]

par(mfrow = c(2, 2))
plot(siliciumLmsansautresfacteurs)

# on cherche � ajouter des interactions dans notre mod�le

E<-silicium[,1]*silicium[,3]
F<-silicium[,5]*silicium[,6]

Nouveaumodele<-data.frame(X,E,F)
#CourbureSilicium2 = Nouveaumodele[which(Ltemp == -1 & Lpress == -1 & Ctime == 1 & Catmos == 1), 7]
#moyenneCourbureSilicium2 = mean(CourbureSilicium2)

Lmnouveaumodele=lm(Camber ~ Ltemp + Lpress + Ctime + Catmos + E + F, data = Nouveaumodele)
frame2 = data.frame(Ltemp = -1, Lpress = -1, Ctime = 1, Catmos = 1, E= -1, F =-1)
predict(Lmnouveaumodele, frame2, interval="confidence", level=0.95)
