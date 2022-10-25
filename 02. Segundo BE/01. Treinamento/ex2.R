neu <- read.table("~/Documents/CENTRALE/4A/MOD/Stats/BE3/neuralgia.txt",header = TRUE)

#partage du fichier en un fichier d'apprentissage et un fichier test
u <- sample(1:60,48)
apprenti <- neu[u,]
test <- neu[-u,]

#régression logistique 
apprenti$Treatment = as.factor(apprenti$Treatment)
apprenti$Sex = as.factor(apprenti$Sex)
neu_logit=glm(Pain ~ Treatment + Sex + Age + Duration,family=binomial(link="logit"),data=apprenti)

#Anova
anova(neu_logit,test="Chisq")
Anova(neu_logit,test.statistic = "LR", type= 'III')
Anova(neu_logit,test.statistic = "Wald", type= 'III')

#procédure fowrward critère AIC
neu_logit2=glm(Pain ~ 1,family=binomial,data=apprenti)
forward <- step(neu_logit2, direction="forward", scope=list(upper=~(Treatment + Sex + Age + Duration)), trace = TRUE)
forward$anova
Anova(forward,test.statistic="LR",type = 'III')

#modèle réduit
neu_logitr=glm(Pain ~ Treatment + Sex + Age ,family=binomial(link="logit"),data=apprenti)

#previsions sur les données test pour les deux modèles
predictcomplet <- exp(predict(neu_logit,newdata =test))/(1+exp(predict(neu_logit,newdata =test)))
predictreduit <- exp(predict(neu_logitr,newdata =test))/(1+exp(predict(neu_logitr,newdata =test)))

#matrice de confusion
table(predictcomplet>0.5,test$Pain)
table(predictreduit>0.5,test$Pain)

#on fixe la modèle réduit, on prend 50 ensembles d'apprentissages
compa <- vector("numeric",50) 
for (i in 1:50) {
  v<- sample(1:60,48)
  apprent <- neu[v,]
  tes <- neu[-v,]
  apprent$Treatment = as.factor(apprent$Treatment)
  apprent$Sex = as.factor(apprent$Sex)
  neu_log=glm(Pain ~ Treatment + Sex + Age ,family=binomial(link="logit"),data=apprent)
  predicti <- exp(predict(neu_log,newdata =tes))/(1+exp(predict(neu_log,newdata =tes)))
  tabli <- table(predicti>0.5,tes$Pain)
  compa[i]= (tabli[1,1]+tabli[2,2])/12
}
#summary(compa)
#on fait varier le pourcentage d'apprentissage
prop10 <- vector("numeric",50)
for (i in 1:50) {
  v<- sample(1:60,54)
  apprent <- neu[v,]
  tes <- neu[-v,]
  apprent$Treatment = as.factor(apprent$Treatment)
  apprent$Sex = as.factor(apprent$Sex)
  neu_log=glm(Pain ~ Treatment + Sex + Age ,family=binomial(link="logit"),data=apprent)
  predicti <- exp(predict(neu_log,newdata =tes))/(1+exp(predict(neu_log,newdata =tes)))
  tabli <- table(predicti>0.5,tes$Pain)
  prop10[i]= (tabli[1,1]+tabli[2,2])/6
}

prop30 <- vector("numeric",50)
for (i in 1:50) {
  v<- sample(1:60,42)
  apprent <- neu[v,]
  tes <- neu[-v,]
  apprent$Treatment = as.factor(apprent$Treatment)
  apprent$Sex = as.factor(apprent$Sex)
  neu_log=glm(Pain ~ Treatment + Sex + Age ,family=binomial(link="logit"),data=apprent)
  predicti <- exp(predict(neu_log,newdata =tes))/(1+exp(predict(neu_log,newdata =tes)))
  tabli <- table(predicti>0.5,tes$Pain)
  prop30[i]= (tabli[1,1]+tabli[2,2])/18
}

prop40 <- vector("numeric",50)
for (i in 1:50) {
  v<- sample(1:60,36)
  apprent <- neu[v,]
  tes <- neu[-v,]
  apprent$Treatment = as.factor(apprent$Treatment)
  apprent$Sex = as.factor(apprent$Sex)
  neu_log=glm(Pain ~ Treatment + Sex + Age ,family=binomial(link="logit"),data=apprent)
  predicti <- exp(predict(neu_log,newdata =tes))/(1+exp(predict(neu_log,newdata =tes)))
  tabli <- table(predicti>0.5,tes$Pain)
  prop40[i]= (tabli[1,1]+tabli[2,2])/24
}

moy<-c(mean(prop10),mean(compa),mean(prop30),mean(prop40))
plot(moy, ylim=0:1)

