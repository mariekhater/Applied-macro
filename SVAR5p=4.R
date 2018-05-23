install.packages("rJava")
install.packages("xlsx")
install.packages("mFilter")
install.packages("fUnitRoots")
install.packages("vars")
install.packages("xlsxjars")
install.packages("haven")
install.packages("AER")


library(tseries)
library(xlsx)
library(mFilter)
library(xts)
library(fUnitRoots) #test Adf avec constante, tendance ou aucun
library(vars) #estimate a VAR model
library(car)
library(haven)
library(AER)

#J'ai pas trouv? un d?flateur du PIB trimestriel 
rawIPC <- read.csv("IPC-indice.csv", sep="," , dec=".")

IPC <- ts(rawIPC[13:116,17] , start =c(1983,1) , frequency =4) # indice des prix ? la conso (deflateur)
plot(IPC)

rawPIBval <- t_pib_val #PIB en valeur

PIBval <- ts(rawPIBval[143:246,2], start=c(1983,1) , frequency=4)
PIB <- PIBval/IPC
plot(PIB)


rawAPU <- t_compteapu_val # en valeur

depensestot <- ts(rawAPU[143:246,4], start=c(1983,1) , frequency=4)
plot(depensestot)
# On d?finit une variable d?penses publiques G (cf papier Biau et Girard) ou on exclut les transferts
#aux m?nages et aux entreprises ainsi que la charge de la dette
consint <- ts(rawAPU[143:246,5], start=c(1983,1) , frequency=4)
remunerationsal <- ts(rawAPU[143:246,6], start=c(1983,1) , frequency=4)
autresdepfctnmt <- ts(rawAPU[143:246,8], start=c(1983,1), frequency=4)
FBCF <- ts(rawAPU[143:246,15], start=c(1983,1) , frequency=4)
autresfinan <- ts(rawAPU[143:246,16], start=c(1983,1) , frequency=4)
depenses <- consint + remunerationsal + autresdepfctnmt + FBCF + autresfinan
G <- depenses/IPC
plot(G)

#On d?finit les recettes publiques TA (cf papier Biau et Girard) comme la somme de G et de la capacit? de financement
recettes <- ts(rawAPU[143:246,18], start=c(1983,1) , frequency=4)
capfinancement <- ts(rawAPU[143:246,2], start=c(1983,1) , frequency=4)
recettes <- capfinancement + depenses
TA <- recettes/IPC
plot(TA)
#On n'a pas d?duit l'impot sur les soci?t?s (pour le moment) contrairement au papier de Biau et Girard
#Normalement faisable avec comptabilit? nationale item D51

#On ajoute le taux d'intérêt interbancaire
taux<-ts(PIBOR_EURIBOR[1:104,3], start=c(1983,1) , frequency=4)
plot(taux)

PIBlog <-log(PIB)
Glog <- log(G)
TAlog <- log(TA)
IPClog<-log(IPC)


par(mfrow=c(3,2))
plot(PIBlog)
plot(Glog)
plot(TAlog)
plot(IPClog)
plot(taux)

#il semble y avoir une tendance d?terministe lin?aire. Pour le taux, éventuellement une tendance linéaire à partir de 2009
adfTest(PIBlog, type="ct") #test avec constante et tendance
adfTest(Glog,  type="ct")
adfTest(TAlog, type="ct")
adfTest(IPClog, type="ct") # pas de racine unitaire si test avec constante et tendance
adfTest(taux, type="ct")
adfTest(taux, type="c")
#On ne peut pas rejeter l'existence d'une racine unitaire pour les 4 séries sauf IPClog
View(IPClog)
#On prend tout en diff?rence premi?re *100 pour avoir en pourcentage : argument Hamilton (1994)
#le passage en diff?rence accro?t la pr?cision de l'estimation du mod?le,
#dans une situation o? l'?chantillon est de petite taille

deltaPIB <- diff(PIBlog)*100
deltaTA <- diff(TAlog)*100
deltaG <- diff(Glog)*100
deltaIPC<-diff(IPClog)*100
deltataux<-diff(taux)*100

par(mfrow=c(3,2))
plot(deltaPIB)
plot(deltaTA)
plot(deltaG)
plot(deltaIPC)
plot(deltataux)

#pour les taux, il y a clairement un point singulier après la crise

#On teste la stationnarité des 5
adfTest(deltaPIB, type="nc") #test avec ni constante ni tendance
adfTest(deltaTA,  type="nc")
adfTest(deltaG, type="nc")
adfTest(deltaIPC, type="nc")
adfTest(deltataux, type="nc")

#On rejette l'existence d'une racine unitaire avec un seuil sup?rieur ? 99 pourcent

#Conclusion : nos 5 séries sont I(1)

#On souhaite maintenant tester l'existence d'une relation de cointégration
#Eventuellement entre deltaTA et deltaG (deltaTA-deltaG aurait un sens économique)
plot(TAlog-Glog) #bof stationnaire : coupure en 1993 avec creusement fort du déficit pour ensuite ajustement budgétaire
#On va quand meme tester la stationnarité de la différence
adfTest(TAlog-Glog, type="c")
adfTest(TAlog-Glog, type="ct")
#On ne peut pas rejeter pour les deux tests que c'est non stationnaire

#Conclusion: On a 5 variables I(1) et pas de relation de cointégration apparente 
#Donc on va travailler avec un modèle VAR en différence première

#Estimation d'un VAR en différence premiére

Y <- data.frame(deltaTA, deltaG, deltaPIB, deltaIPC, deltataux)
names(Y)<-c("deltaTA","deltaG","deltaPIB","deltaIPC","deltataux")
#On détermine d'abord le nombre de retard
VARselect(Y, lag.max = 8, type = "const") #On choisit 8 car on suppose que les variables du modèle ne peuvent
#avoir d'impact les unes sur les autres aprés deux années)
#on a 1 ou 2 selon les tests   --> on va prendre 4
#(argument économique et parce que ça nous arrange pour le test de Johansen et l'autocorrélation )

#Test de Johansen
jotest=ca.jo(Y, type="eigen", K=4, ecdet="none", spec="transitory")
summary(jotest)
#Il y aurait 4 relations de cointégration à 5 % mais 2 à 1 % 


est <- VAR(Y,p=4, type="const") #
est
summary(est, equation = "deltaTA")
plot(est, names="deltaTA")
summary(est, equation="deltaG")
plot(est, names="deltaG")
summary(est, equation="deltaPIB")
plot(est,names="deltaPIB")

ser11 <- serial.test(est, lags.pt = 16)
ser11$serial
#residus ne sont pas auto-corrélés pour p=2 ; p value maximum pour p = 4
res<- resid(est)
acf(res)

norm3 <- normality.test(est)
norm3$jb.mul
#On rejette que les résidus suivent une loi normale... 

#Passage au VAR structurel 
#Identification de M1 et M2
M1 = matrix( c(1, 0, -99, -99,0, 0, 1, -99,-99,0,-99,-99,1, 1,-99,-99,-99,0,0,-99,-99,-99,0,0,1), nrow=5, ncol=5) #je mets 99 quand c'est une valeur inconnue pour le moment
M2 = matrix( c(1, 99, 0,0,99,99,1, 0,0,99,0,0,1,0,0,0,0,0,1,0,0,0,0,0,1), nrow=5, ncol=5)

# Pour les alpha, on reprend exactement comme dans le papier
M1[1,3]<- -0.8
M1[1,4]<- -0.5
M1[1,5]<- 0
M1[2,3]<- 0
M1[2,4]<- 1
M1[2,5]<- 0


#Argument : d?cisions portant sur les TA pr?c?dent celles sur les d?penses
M2[1,2]<- 0

#On calcule les epsilon(ta) 
eps_ta=res[,1]-0.8*res[,3]-0.5*res[,4]

#Regression MCO sur la deuxi?me equation pour obtenir beta_gt
lineareq2 <- lm(res[,2]+res[,4] ~ eps_ta - 1) #on ne veut pas d'intercept
summary(lineareq2)
#on n'obtient -0.002, Biau et Girard eux obtiennent -0.05.
eps_tg <- residuals(lineareq2)
M2[2,1]<- summary(lineareq2)$coefficients[1, 1]

#Regression MCO (avec variables instrumentales) sur la troisi?me ?quation pour obtenir gamma_yt et gamma_yg
#Les IV sont eps_ta pour u_ta et eps_tg pour u_tg
lineareq3 <- ivreg(res[,3] ~ res[,1] + res[,2] -1| eps_ta +eps_tg)
summary(lineareq3)
M1[3,1]<- - summary(lineareq3)$coefficients[1,1]
M1[3,2] <- - summary(lineareq3)$coefficients[2,1]

eps_y<-residuals(lineareq3)

lineareq4<- ivreg(res[,4]~ res[,1] + res[,2] + res[,3] -1| eps_ta +eps_tg+eps_y)
summary(lineareq4)
M1[4,1]<- - summary(lineareq4)$coefficients[1,1]
M1[4,2]<- - summary(lineareq4)$coefficients[2,1]
M1[4,3]<- - summary(lineareq4)$coefficients[3,1]
M1[4,4]<- 1

eps_p<-residuals(lineareq4)

lineareq5<-ivreg(res[,5]~ res[,3] + res[,4] + eps_ta + eps_tg -1| eps_y +eps_p+eps_ta+eps_tg)
M1[5,3]<- - summary(lineareq5)$coefficients[1,1]
M1[5,4]<- - summary(lineareq5)$coefficients[2,1]
M2[5,1]<- summary(lineareq5)$coefficients[3,1]
M2[5,2]<- summary(lineareq5)$coefficients[4,1]

eps_R<-residuals(lineareq5)


#Les IRFs


P <- inv(M1)%*%M2

results <- matrix(numeric(0), 5,10)

#On construit la matrix rho pour pouvoir apr?s calculer Xt=rho*X(t-1)+epsilont
rho <- matrix(numeric(0),5,5)

for (j in 1:5)
{
  rho[1,j] <- coef(est)$deltaTA[j,1]
}

for (j in 1:5)
{
  rho[2,j] <- coef(est)$deltaG[j,1]
}

for (j in 1:5)
{
  rho[3,j] <- coef(est)$deltaPIB[j,1]
}

for (j in 1:5)
{
  rho[4,j] <- coef(est)$deltaIPC[j,1]
}

for (j in 1:5)
{
  rho[5,j] <- coef(est)$deltataux[j,1]
}


epsilon <- matrix(numeric(0), 5,1)
epsilon[1,1]<-1
epsilon[2,1]<-0
epsilon[3,1]<-0
epsilon[4,1]<-0
epsilon[5,1]<-0


results[1:5,1] <- P%*%epsilon
epsilonbis <- matrix(numeric(0), 5,1)

epsilonbis[1,1]<-1/results[1,1]
epsilonbis[2,1]<-0
epsilonbis[3,1]<-0
epsilonbis[4,1]<-0
epsilonbis[5,1]<-0


resultsbis <- matrix(numeric(0), 5,10)

resultsbis[1:5,1] <- P%*%epsilonbis


for (j in 2:10)
{
  resultsbis[,j] <- rho%*%resultsbis[,j-1]
}

resultsNiveauxLn<-matrix(numeric(0), 5,10)
resultsNiveauxLn[,1]<-resultsbis[,1]

for (j in 2:10)
{
  resultsNiveauxLn[,j]<-resultsbis[,j]+resultsNiveauxLn[,j-1]
}

#Monte Carlo
IRF_TA <- matrix(numeric(0), 10,5)
IRF_G <- matrix(numeric(0), 10,5)
IRF_PIB <- matrix(numeric(0),10,5)
IRF_IPC<- matrix(numeric(0),10,5)
IRF_TAUX<- matrix(numeric(0),10,5)

for (i in 1:5)
{
  #G?n?rer de nouvelles donn?es de meme taille
  Xmonte <- matrix(numeric(0), 5,99)
  Xmonte[,1]=P%*%residuals(est)[sample(1:99, 1),]
  for (j in 2:99)
  {
    Xmonte[,j]=rho%*%Xmonte[,j-1]+P%*%residuals(est)[sample(1:99, 1),]
  }
  #Estime le mod?le sur ces donn?es
  
  Xmonte <- data.frame(Xmonte[1,], Xmonte[2,], Xmonte[3,],Xmonte[4,],Xmonte[5,])
  estmonte <- VAR(Xmonte,p=4, type="cons")
  estmonte_residuals <-residuals(estmonte)
  
  #Passage au VAR structurel 
  #Identification de M1 et M2
  M1monte = matrix( c(1, 0, -99, -99,0, 0, 1, -99,-99,0,-99,-99,1, 1,-99,-99,-99,0,0,-99,-99,-99,0,0,1), nrow=5, ncol=5) #je mets 99 quand c'est une valeur inconnue pour le moment
  M2monte = matrix( c(1, 99, 0,0,99,99,1, 0,0,99,0,0,1,0,0,0,0,0,1,0,0,0,0,0,1), nrow=5, ncol=5)
  
  # Pour les alpha, on reprend exactement comme dans le papier
  M1monte[1,3]<- -0.8
  M1monte[1,4]<- -0.5
  M1monte[1,5]<- 0
  M1monte[2,3]<- 0
  M1monte[2,4]<- 1
  M1monte[2,5]<- 0
  
  
  #Argument : d?cisions portant sur les TA pr?c?dent celles sur les d?penses
  M2monte[1,2]<- 0
  
  #On calcule les epsilon(ta) 
  epsmonte_ta=estmonte_residuals[,1]-0.8*estmonte_residuals[,3]-0.5*estmonte_residuals[,4]
  
  #Regression MCO sur la deuxi?me equation pour obtenir beta_gt
  lineareqmonte2 <- lm(estmonte_residuals[,2]+estmonte_residuals[,4] ~ epsmonte_ta - 1) #on ne veut pas d'intercept
  epsmonte_tg <- residuals(lineareqmonte2)
  M2monte[2,1]<- summary(lineareqmonte2)$coefficients[1, 1]
  
  #Regression MCO (avec variables instrumentales) sur la troisi?me ?quation pour obtenir gamma_yt et gamma_yg
  #Les IV sont eps_ta pour u_ta et eps_tg pour u_tg
  lineareqmonte3 <- ivreg(estmonte_residuals[,3] ~ estmonte_residuals[,1] + estmonte_residuals[,2] -1| epsmonte_ta +epsmonte_tg)
  M1monte[3,1]<- - summary(lineareqmonte3)$coefficients[1,1]
  M1monte[3,2] <- - summary(lineareqmonte3)$coefficients[2,1]
  
  epsmonte_y<-residuals(lineareqmonte3)
  
  lineareqmonte4<- ivreg(estmonte_residuals[,4]~ estmonte_residuals[,1] + estmonte_residuals[,2] + estmonte_residuals[,3] -1| epsmonte_ta +epsmonte_tg+epsmonte_y)
  M1monte[4,1]<- - summary(lineareqmonte4)$coefficients[1,1]
  M1monte[4,2]<- - summary(lineareqmonte4)$coefficients[2,1]
  M1monte[4,3]<- - summary(lineareqmonte4)$coefficients[3,1]
  M1monte[4,4]<- 1
  
  epsmonte_p<-residuals(lineareqmonte4)
  
  lineareqmonte5<-ivreg(estmonte_residuals[,5]~ estmonte_residuals[,3] + estmonte_residuals[,4] + epsmonte_ta + epsmonte_tg -1| epsmonte_y +epsmonte_p+epsmonte_ta+epsmonte_tg)
  M1monte[5,3]<- - summary(lineareqmonte5)$coefficients[1,1]
  M1monte[5,4]<- - summary(lineareqmonte5)$coefficients[2,1]
  M2monte[5,1]<- summary(lineareqmonte5)$coefficients[3,1]
  M2monte[5,2]<- summary(lineareqmonte5)$coefficients[4,1]
  
  epsmonte_R<-residuals(lineareqmonte5)
  
  #IRFs
  Pmonte <- inv(M1)%*%M2monte
  
  resultsmonte <- matrix(numeric(0), 5,10)
  resultsmontebis <- matrix(numeric(0), 5,10)
  resultsmonteNiveauxLn <- matrix(numeric(0), 5,10)
  
  #On construit la matrix rho pour pouvoir apr?s calculer Xt=rho*X(t-1)+epsilont
  rhomonte <- matrix(numeric(0),5,5)
  
  for (j in 1:5)
  {
    rhomonte[1,j] <- coef(estmonte)$Xmonte.1...[j,1]
  }
  
  for (j in 1:5)
  {
    rhomonte[2,j] <- coef(estmonte)$Xmonte.2...[j,1]
  }
  
  for (j in 1:5)
  {
    rhomonte[3,j] <- coef(estmonte)$Xmonte.3...[j,1]
  }
 
  for (j in 1:5)
  {
    rhomonte[4,j] <- coef(estmonte)$Xmonte.4...[j,1]
  }
  
  for (j in 1:5)
  {
    rhomonte[5,j] <- coef(estmonte)$Xmonte.5...[j,1]
  }
   
  epsilonmonte <- matrix(numeric(0), 5,1)
  epsilonmonte[1,1]<-1
  epsilonmonte[2,1]<-0
  epsilonmonte[3,1]<-0
  epsilonmonte[4,1]<-0
  epsilonmonte[5,1]<-0
  
  resultsmonte[,1] <- Pmonte%*%epsilonmonte
  

  epsilonmontebis <- matrix(numeric(0), 5,1)
  epsilonmontebis[1,1]<-1/(resultsmonte[1,1])
  epsilonmontebis[2,1]<-0
  epsilonmontebis[3,1]<-0
  epsilonmontebis[4,1]<-0
  epsilonmontebis[5,1]<-0
  
  
  
  resultsmontebis[,1] <- Pmonte%*%epsilonmontebis
  
  for (j in 2:10)
  {
    resultsmontebis[,j] <- rhomonte%*%resultsmontebis[,j-1]
  }
  
  resultsmonteNiveauxLn[,1]<-resultsmontebis[,1]
  
  for (j in 2:10)
  {
    resultsmonteNiveauxLn[,j]<-resultsmontebis[,j]+resultsmonteNiveauxLn[,j-1]
  }
  
  
  IRF_TA[,i] <-resultsmonteNiveauxLn[1,]
  IRF_G[,i] <- resultsmonteNiveauxLn[2,]
  IRF_PIB[,i]<- resultsmonteNiveauxLn[3,]
  IRF_IPC[,i]<- resultsmonteNiveauxLn[4,]
  IRF_TAUX[,i]<- resultsmonteNiveauxLn[5,]
  
  
}

varIRF_TA <- matrix(numeric(0), 10,1)
for (j in 1:10)
{
  varIRF_TA[j,]<-var(IRF_TA[j,])
}
sc=1.6449 #90% intervalle de confiance
Max_TA <- resultsNiveauxLn[1,] +sc*sqrt(varIRF_TA)
Min_TA <- resultsNiveauxLn[1,] -sc*sqrt(varIRF_TA)


varIRF_G <- matrix(numeric(0), 10,1)
for (j in 1:10)
{
  varIRF_G[j,]<-var(IRF_G[j,])
}
Max_G <- resultsNiveauxLn[2,] +sc*sqrt(varIRF_G)
Min_G <- resultsNiveauxLn[2,] -sc*sqrt(varIRF_G)

varIRF_PIB <- matrix(numeric(0), 10,1)
for (j in 1:10)
{
  varIRF_PIB[j,]<-var(IRF_PIB[j,])
}
Max_PIB <- resultsNiveauxLn[3,] +sc*sqrt(varIRF_PIB)
Min_PIB <- resultsNiveauxLn[3,] -sc*sqrt(varIRF_PIB)

varIRF_IPC <- matrix(numeric(0), 10,1)
for (j in 1:10)
{
  varIRF_IPC[j,]<-var(IRF_IPC[j,])
}
sc=1.6449 #90% intervalle de confiance
Max_IPC <- resultsNiveauxLn[4,] +sc*sqrt(varIRF_IPC)
Min_IPC <- resultsNiveauxLn[4,] -sc*sqrt(varIRF_IPC)

varIRF_TAUX <- matrix(numeric(0), 10,1)
for (j in 1:10)
{
  varIRF_TAUX[j,]<-var(IRF_TAUX[j,])
}
sc=1.6449 #90% intervalle de confiance
Max_TAUX <- resultsNiveauxLn[5,] +sc*sqrt(varIRF_TAUX)
Min_TAUX <- resultsNiveauxLn[5,] -sc*sqrt(varIRF_TAUX)

plot(resultsNiveauxLn[1,],type="o",col="blue",ylim=c(0,2))
lines(Max_TA,type="o",col="red")
lines(Min_TA,type="o",col="red")

plot(resultsNiveauxLn[2,],type="o",col="blue",ylim=c(-1,1))
lines(Max_G,type="o",col="red")
lines(Min_G,type="o",col="red")

plot(resultsNiveauxLn[3,],type="o",col="blue",ylim=c(-1,1))
lines(Max_PIB,type="o",col="red")
lines(Min_PIB,type="o",col="red")

plot(resultsNiveauxLn[4,],type="o",col="blue",ylim=c(-1,1))
lines(Max_IPC,type="o",col="red")
lines(Min_IPC,type="o",col="red")

plot(resultsNiveauxLn[5,],type="o",col="blue",ylim=c(-20,20))
lines(Max_TAUX,type="o",col="red")
lines(Min_TAUX,type="o",col="red")



#Choc sur dépenses
results2<-matrix(numeric(0),5,10)
epsilon2 <- matrix(numeric(0), 5,1)
results2bis<-matrix(numeric(0),5,10)
epsilon2bis <- matrix(numeric(0), 5,1)
epsilon2[1,1]<-0
epsilon2[2,1]<-1
epsilon2[3,1]<-0
epsilon2[4,1]<-0
epsilon2[5,1]<-0


results2[1:5,1] <- P%*%epsilon2

epsilon2bis[1,1]<-0
epsilon2bis[2,1]<-1/results2[2,1]
epsilon2bis[3,1]<-0
epsilon2bis[4,1]<-0
epsilon2bis[5,1]<-0

results2bis[1:5,1] <- P%*%epsilon2


for (j in 2:10)
{
  results2bis[,j] <- rho%*%results2bis[,j-1]
}



results2NiveauxLn<-matrix(numeric(0), 5,10)
results2NiveauxLn[,1]<-results2bis[,1]

for (j in 2:10)
{
  results2NiveauxLn[,j]<-results2bis[,j]+results2NiveauxLn[,j-1]
}


#Monte Carlo
IRF_TA_2 <- matrix(numeric(0), 10,5)
IRF_G_2 <- matrix(numeric(0), 10,5)
IRF_PIB_2 <- matrix(numeric(0),10,5)
IRF_IPC_2 <- matrix(numeric(0),10,5)
IRF_TAUX_2 <- matrix(numeric(0),10,5)



for (i in 1:5)
{
  #G?n?rer de nouvelles donn?es de meme taille
  Xmonte_2 <- matrix(numeric(0), 5,99)
  Xmonte_2[,1]=P%*%residuals(est)[sample(1:99, 1),]
  for (j in 2:99)
  {
    Xmonte_2[,j]=rho%*%Xmonte_2[,j-1]+P%*%residuals(est)[sample(1:99, 1),]
  }
  
  #Estime le mod?le sur ces donn?es
  
  Xmonte_2 <- data.frame(Xmonte_2[1,], Xmonte_2[2,], Xmonte_2[3,], Xmonte_2[4,],Xmonte_2[5,])
  estmonte_2 <- VAR(Xmonte_2,p=4, type="cons")
  estmonte_residuals_2 <-residuals(estmonte_2)
  
  
  
  
  #Passage au VAR structurel 
  #Identification de M1 et M2
  M1monte_2 = matrix( c(1, 0, -99, -99,0, 0, 1, -99,-99,0,-99,-99,1, 1,-99,-99,-99,0,0,-99,-99,-99,0,0,1), nrow=5, ncol=5) #je mets 99 quand c'est une valeur inconnue pour le moment
  M2monte_2 = matrix( c(1, 99, 0,0,99,99,1, 0,0,99,0,0,1,0,0,0,0,0,1,0,0,0,0,0,1), nrow=5, ncol=5)
  
  # Pour les alpha, on reprend exactement comme dans le papier
  M1monte_2[1,3]<- -0.8
  M1monte_2[1,4]<- -0.5
  M1monte_2[1,5]<- 0
  M1monte_2[2,3]<- 0
  M1monte_2[2,4]<- 1
  M1monte_2[2,5]<- 0
  
  
  #Argument : d?cisions portant sur les TA pr?c?dent celles sur les d?penses
  M2monte_2[1,2]<- 0
  
  #On calcule les epsilon(ta) 
  epsmonte_ta_2=estmonte_residuals_2[,1]-0.8*estmonte_residuals_2[,3]-0.5*estmonte_residuals_2[,4]
  
  #Regression MCO sur la deuxi?me equation pour obtenir beta_gt
  lineareqmonte2_2 <- lm(estmonte_residuals_2[,2]+estmonte_residuals_2[,4] ~ epsmonte_ta_2 - 1) #on ne veut pas d'intercept
  epsmonte_tg_2 <- residuals(lineareqmonte2_2)
  M2monte_2[2,1]<- summary(lineareqmonte2_2)$coefficients[1, 1]
  
  #Regression MCO (avec variables instrumentales) sur la troisi?me ?quation pour obtenir gamma_yt et gamma_yg
  #Les IV sont eps_ta pour u_ta et eps_tg pour u_tg
  lineareqmonte3_2 <- ivreg(estmonte_residuals_2[,3] ~ estmonte_residuals_2[,1] + estmonte_residuals_2[,2] -1| epsmonte_ta_2 +epsmonte_tg_2)
  M1monte_2[3,1]<- - summary(lineareqmonte3_2)$coefficients[1,1]
  M1monte_2[3,2] <- - summary(lineareqmonte3_2)$coefficients[2,1]
  
  epsmonte_y_2<-residuals(lineareqmonte3_2)
  
  lineareqmonte4_2<- ivreg(estmonte_residuals_2[,4]~ estmonte_residuals_2[,1] + estmonte_residuals_2[,2] + estmonte_residuals_2[,3] -1| epsmonte_ta_2 +epsmonte_tg_2+epsmonte_y_2)
  M1monte_2[4,1]<- - summary(lineareqmonte4_2)$coefficients[1,1]
  M1monte_2[4,2]<- - summary(lineareqmonte4_2)$coefficients[2,1]
  M1monte_2[4,3]<- - summary(lineareqmonte4_2)$coefficients[3,1]
  M1monte_2[4,4]<- 1
  
  epsmonte_p_2<-residuals(lineareqmonte4_2)
  
  lineareqmonte5_2<-ivreg(estmonte_residuals_2[,5]~ estmonte_residuals_2[,3] + estmonte_residuals_2[,4] + epsmonte_ta_2 + epsmonte_tg_2 -1| epsmonte_y_2 +epsmonte_p_2+epsmonte_ta_2+epsmonte_tg_2)
  M1monte_2[5,3]<- - summary(lineareqmonte5_2)$coefficients[1,1]
  M1monte_2[5,4]<- - summary(lineareqmonte5_2)$coefficients[2,1]
  M2monte_2[5,1]<- summary(lineareqmonte5_2)$coefficients[3,1]
  M2monte_2[5,2]<- summary(lineareqmonte5_2)$coefficients[4,1]
  
  epsmonte_R_2<-residuals(lineareqmonte5_2)
  #IRFs
  Pmonte_2 <- inv(M1monte_2)%*%M2monte_2
  
  resultsmonte_2 <- matrix(numeric(0), 5,10)
  resultsmontebis_2 <- matrix(numeric(0), 5,10)
  resultsmonteNiveauxLn_2 <- matrix(numeric(0), 5,10)
  
  #On construit la matrix rho pour pouvoir apr?s calculer Xt=rho*X(t-1)+epsilont
  rhomonte_2 <- matrix(numeric(0),5,5)
  
  for (j in 1:5)
  {
    rhomonte_2[1,j] <- coef(estmonte_2)$Xmonte_2.1...[j,1]
  }
  
  for (j in 1:5)
  {
    rhomonte_2[2,j] <- coef(estmonte_2)$Xmonte_2.2...[j,1]
  }
  
  for (j in 1:5)
  {
    rhomonte_2[3,j] <- coef(estmonte_2)$Xmonte_2.3...[j,1]
  }
  
  for (j in 1:5)
  {
    rhomonte_2[4,j] <- coef(estmonte_2)$Xmonte_2.4...[j,1]
  }
  
  for (j in 1:5)
  {
    rhomonte_2[5,j] <- coef(estmonte_2)$Xmonte_2.5...[j,1]
  }
  
  epsilonmonte_2 <- matrix(numeric(0), 5,1)
  epsilonmonte_2[1,1]<-0
  epsilonmonte_2[2,1]<-1
  epsilonmonte_2[3,1]<-0
  epsilonmonte_2[4,1]<-0
  epsilonmonte_2[5,1]<-0
  
  
  
  resultsmonte_2[,1] <- Pmonte_2%*%epsilonmonte_2
  
  epsilonmontebis_2 <- matrix(numeric(0), 5,1)
  epsilonmontebis_2[1,1]<-0
  epsilonmontebis_2[2,1]<-1/(resultsmonte_2[2,1])
  epsilonmontebis_2[3,1]<-0
  epsilonmontebis_2[4,1]<-0
  epsilonmontebis_2[5,1]<-0
  
  
  resultsmontebis_2[,1] <- Pmonte_2%*%epsilonmontebis_2
  
  for (j in 2:10)
  {
    resultsmontebis_2[,j] <- rhomonte_2%*%resultsmontebis_2[,j-1]
  }
  
  resultsmonteNiveauxLn_2[,1]<-resultsmontebis_2[,1]
  
  for (j in 2:10)
  {
    resultsmonteNiveauxLn_2[,j]<-resultsmontebis_2[,j]+resultsmonteNiveauxLn_2[,j-1]
  }
  
  
  IRF_TA_2[,i] <-resultsmonteNiveauxLn_2[1,]
  IRF_G_2[,i] <- resultsmonteNiveauxLn_2[2,]
  IRF_PIB_2[,i]<- resultsmonteNiveauxLn_2[3,]
  IRF_IPC_2[,i]<- resultsmonteNiveauxLn_2[4,]
  IRF_TAUX_2[,i]<- resultsmonteNiveauxLn_2[5,]
  
  
}

varIRF_TA_2 <- matrix(numeric(0), 10,1)
for (j in 1:10)
{
  varIRF_TA_2[j,]<-var(IRF_TA_2[j,])
}
sc=1.6449 #90% intervalle de confiance
Max_TA_2 <- results2NiveauxLn[1,] +sc*sqrt(varIRF_TA_2)
Min_TA_2 <- results2NiveauxLn[1,] -sc*sqrt(varIRF_TA_2)


varIRF_G_2 <- matrix(numeric(0), 10,1)
for (j in 1:10)
{
  varIRF_G_2[j,]<-var(IRF_G_2[j,])
}
Max_G_2 <- results2NiveauxLn[2,] +sc*sqrt(varIRF_G_2)
Min_G_2 <- results2NiveauxLn[2,] -sc*sqrt(varIRF_G_2)

varIRF_PIB_2 <- matrix(numeric(0), 10,1)
for (j in 1:10)
{
  varIRF_PIB_2[j,]<-var(IRF_PIB_2[j,])
}
Max_PIB_2 <- results2NiveauxLn[3,] +sc*sqrt(varIRF_PIB_2)
Min_PIB_2 <- results2NiveauxLn[3,] -sc*sqrt(varIRF_PIB_2)


varIRF_IPC_2 <- matrix(numeric(0), 10,1)
for (j in 1:10)
{
  varIRF_IPC_2[j,]<-var(IRF_IPC_2[j,])
}
Max_IPC_2 <- results2NiveauxLn[4,] +sc*sqrt(varIRF_IPC_2)
Min_IPC_2 <- results2NiveauxLn[4,] -sc*sqrt(varIRF_IPC_2)

varIRF_TAUX_2 <- matrix(numeric(0), 10,1)
for (j in 1:10)
{
  varIRF_TAUX_2[j,]<-var(IRF_TAUX_2[j,])
}
Max_TAUX_2 <- results2NiveauxLn[5,] +sc*sqrt(varIRF_TAUX_2)
Min_TAUX_2 <- results2NiveauxLn[5,] -sc*sqrt(varIRF_TAUX_2)



plot(results2NiveauxLn[1,],type="o",col="blue",ylim=c(-5,5))
lines(Max_TA_2,type="o",col="red")
lines(Min_TA_2,type="o",col="red")

plot(results2NiveauxLn[2,],type="o",col="blue",ylim=c(0,4))
lines(Max_G_2,type="o",col="red")
lines(Min_G_2,type="o",col="red")

plot(results2NiveauxLn[3,],type="o",col="blue",ylim=c(-2,2))
lines(Max_PIB_2,type="o",col="red")
lines(Min_PIB_2,type="o",col="red")

plot(results2NiveauxLn[4,],type="o",col="blue",ylim=c(0,4))
lines(Max_IPC_2,type="o",col="red")
lines(Min_IPC_2,type="o",col="red")

plot(results2NiveauxLn[5,],type="o",col="blue",ylim=c(-100,100))
lines(Max_TAUX_2,type="o",col="red")
lines(Min_TAUX_2,type="o",col="red")

