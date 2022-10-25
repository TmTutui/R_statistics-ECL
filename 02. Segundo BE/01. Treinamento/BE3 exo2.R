library("MASS")
library("car")
library("boot")

#Exercice 2

#Question 1
setwd('D:/Alexei/Documents/Centrale - Cours/Stat/BE3')
getwd()
Neuraglia <- read.table("neuralgia.txt",header = TRUE)
plot(Neuraglia)
summary(Neuraglia)

#Question 2
u= sample(1:60,48)
NeuragliaApp = Neuraglia[u,]
NeuragliaTest = Neuraglia[-u,]

#Question 3
reglNeuralgia = glm(Pain ~ ., family=binomial(link=logit), data = NeuragliaApp)

#Question 4
reglNeuralgiaAnovaChisq <- anova(reglNeuralgia, test = "Chisq")
reglNeuralgiaAnovaChisq

reglNeuralgiaAnovaLR = Anova(reglNeuralgia, type = "III", test.statistic = "LR")
reglNeuralgiaAnovaLR

reglNeuralgiaAnovaWald = Anova(reglNeuralgia, type = "III", test.statistic = "Wald")
reglNeuralgiaAnovaWald

summary(reglNeuralgiaAnovaChisq)
summary(reglNeuralgiaAnovaLR)
summary(reglNeuralgiaAnovaWald)

#Question 5

constant = glm(Pain~1,family=binomial,data=Neuraglia)
forward <- step(constant, Pain~Treatment + Sex + Age + Duration, direction="forward")
forward$anova
Anova(forward,test.statistic="LR",type= 'III')

#Question 6
reglNeuragliaTest= glm(Pain ~.-Duration, family=binomial(link=logit), data = NeuragliaApp)
prediction <- predict(reglNeuragliaTest, newdata = NeuragliaTest, type = 'response')
prediction

table(prediction>0.5,NeuragliaTest$Pain)

reglNeuragliaTestBis= glm(Pain ~.-Duration-Sex, family=binomial(link=logit), data = NeuragliaApp)
predictionBis <- predict(reglNeuragliaTestBis, newdata = NeuragliaTest, type = 'response')
predictionBis

table(predictionBis>0.5,NeuragliaTest$Pain)

#Question 7

i=1
a=0
while (i<51) 
{u2= sample(1:60,48)
NeuragliaApp2 = Neuraglia[u2,]
NeuragliaTest2 = Neuraglia[-u2,]
reglNeuragliaTest2= glm(Pain ~.-Duration, family=binomial(link=logit), data = NeuragliaApp2)
prediction2 <- predict(reglNeuragliaTest2, newdata = NeuragliaTest2, type = 'response')
a<-table(prediction2>0.5,NeuragliaTest2$Pain)[2,1]+table(prediction2>0.5,NeuragliaTest2$Pain)[1,2]+a
i=i+1
}
a
#moyenne_instance_mal_classés = 124/50


