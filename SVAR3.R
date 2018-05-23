library(tseries)
library(xlsx)
library(mFilter)
library(xts)
library(fUnitRoots) #test Adf avec constante, tendance ou aucun
library(vars) #estimate a VAR model
library(car)
library(haven)
library(AER) #pour les régressions à variables instrumentales
library(dse)

#J'ai pas trouvé un déflateur du PIB trimestriel 
rawIPC <- read.csv("IPC-indice.csv", sep="," , dec=".")

IPC <- ts(rawIPC$Value , start =c(1980,1) , frequency =4) # indice des prix à la conso (deflateur)
plot(IPC)

rawPIBval <- read.xlsx("t_pib_val.xls", sheetName = "Niveaux") #PIB en valeur

PIBval <- ts(as.numeric(as.character(rawPIBval[8:284,2])), start=c(1949,1) , frequency=4)
PIB <- PIBval/IPC
plot(PIB)

rawAPU <- read.xlsx("t_compteapu_val.xls", sheetName = "Niveaux") # en valeur

depensestot <- ts(as.numeric(as.character(rawAPU[131:282,4])), start=c(1980,1) , frequency=4)

# On définit une variable dépenses publiques G (cf papier Biau et Girard) ou on exclut les transferts
#aux ménages et aux entreprises ainsi que la charge de la dette
consint <- ts(as.numeric(as.character(rawAPU[131:282,5])), start=c(1980,1) , frequency=4)
remunerationsal <- ts(as.numeric(as.character(rawAPU[131:282,6])), start=c(1980,1) , frequency=4)
autresdepfctnmt <- ts(as.numeric(as.character(rawAPU[131:282,8])), start=c(1980,1), frequency=4)
FBCF <- ts(as.numeric(as.character(rawAPU[131:282,15])), start=c(1980,1) , frequency=4)
autresfinan <- ts(as.numeric(as.character(rawAPU[131:282,16])), start=c(1980,1) , frequency=4)
depenses <- consint + remunerationsal + autresdepfctnmt + FBCF + autresfinan
G <- depenses/IPC
plot(G)

#On définit les recettes publiques TA (cf papier Biau et Girard) comme la somme de G et de la capacité de financement
recettes <- ts(as.numeric(as.character(rawAPU[131:282,18])), start=c(1980,1) , frequency=4)
capfinancement <- ts(as.numeric(as.character(rawAPU[131:282,2])), start=c(1980,1) , frequency=4)
recettes <- capfinancement + depenses
TA <- recettes/IPC
plot(TA)
#On n'a pas déduit l'impot sur les sociétés (pour le moment) contrairement au papier de Biau et Girard
#Normalement faisable avec comptabilité nationale item D51

#Estimation d'un VAR3 : 1980Q1 to 2017Q3 => 151 trimestres

PIBlog <-log(PIB)
Glog <- log(G)
TAlog <- log(TA)

par(mfrow=c(2,2))
plot(PIBlog)
plot(Glog)
plot(TAlog)

#il semble y avoir une tendance déterministe linéaire
adfTest(PIBlog, type="ct") #test avec constante et tendance
adfTest(Glog,  type="ct")
adfTest(TAlog, type="ct")
#On ne peut pas rejeter l'existence d'une racine unitaire pour les trois séries qd on prend en compte tendance et constante

#On souhaite maintenant tester l'existence d'une relation de cointégration
#Eventuellement entre deltaTA et deltaG (deltaTA-deltaG aurait un sens économique)
plot(TAlog-Glog) #bof stationnaire semble y avoir une constante (trend?)
#On va quand meme tester la stationnarité de la différence
adfTest(TAlog-Glog, type="c")
adfTest(TAlog-Glog, type="ct")
#On ne peut pas rejeter pour les deux tests que c'est non stationnaire
#BG trouve de meme (en ce qui concerne la relation de cointégration) pour la France sur des données antérieures et idem USA avec BP(2002)

#Test de Johansen

deltaPIB <- diff(PIBlog)*100
deltaTA <- diff(TAlog)*100
deltaG <- diff(Glog)*100

X <- data.frame(deltaTA, deltaG, deltaPIB)
jotest=ca.jo(X, type="trace", K=3, ecdet="const", spec="longrun")
summary(jotest)
#Il indique 3 relations de cointégration soit que les 3 variables sont stationnaires (doit etre du à la crise de 2009)

#On restreint à avant 2009
inds <- seq(as.Date("1980-01-01"), as.Date("2017-12-31"), by = "quarter")
deltaPIBres <- subset(deltaPIB, inds >= as.Date("1980-01-01") & inds < as.Date("2009-01-01"))
deltaGres <- subset(deltaG, inds >= as.Date("1980-01-01") & inds < as.Date("2009-01-01"))
deltaTAres <- subset(deltaTA, inds >= as.Date("1980-01-01") & inds < as.Date("2009-01-01"))
#On a 116 points
Xres <- data.frame(deltaTAres, deltaGres, deltaPIBres)
jotest2=ca.jo(Xres, type="trace", K=3, ecdet="const", spec="longrun")
summary(jotest2)
#Dans ce cas 2 relations de cointégration mais difficile de donner une interprétation économique
#aux deux relations de long terme

#On prend tout en différence première *100 pour avoir en pourcentage : argument Hamilton (1994)
#le passage en différence accroît la précision de l'estimation du modèle,
#dans une situation où l'échantillon est de petite taille
plot(deltaPIB)
plot(deltaTA)
plot(deltaG)

#On teste la stationnarité des 3
adfTest(deltaPIB, type="nc") #test avec ni constante ni tendance
adfTest(deltaTA,  type="nc")
adfTest(deltaG, type="nc")
#On rejette l'existence d'une racine unitaire avec un seuil supérieur à 99 pourcent

#Conclusion : nos 3 séries sont I(1)

#Conclusion: On a trois variables I(1) et pas de relation de cointégration apparente 
#Donc on va travailler avec un modèle VAR en différence première

#Estimation d'un VAR en différence première
#On détermine d'abord le nombre de retard
VARselect(X, lag.max = 8, type = "both") #On choisit 8 car on suppose que les variables du modèle ne peuvent
#avoir d'impact les unes sur les autres après deux années)
#Tous indiquent 1

X <- X[, c("deltaTA", "deltaG", "deltaPIB")]
est <- VAR(X,p=1, type="both")
est
summary(est, equation = "deltaTA")
plot(est, names= "deltaPIB")
summary(est, equation="deltaG")

ser11 <- serial.test(est, lags.pt = 16, type = "PT.adjusted")
ser11$serial
#p=1 résidus autocorrélés donc j'augmente p progressivement
#H0 étant pas d'autocorrélation

est <- VAR(X,p=7, type="both")
est
summary(est, equation = "deltaTA")
plot(est, names= "deltaPIB")
summary(est, equation="deltaG")

ser11 <- serial.test(est, lags.pt = 16)
ser11$serial
#residus sont auto-corrélés meme quand je passe à 8 lags (pas bon)
#j'obtiens la p-value la plus grande pour p=7
res<- resid(est)
acf(res)

#On restreint à avant 2009 
plot(deltaPIBres)
plot(deltaGres)
plot(deltaTAres)
#semble stationnaire
adfTest(deltaPIBres, type="c")
adfTest(deltaGres, type="c")
adfTest(deltaTAres, type="c")
#oki

#On détermine d'abord le nombre de retard
VARselect(Xres, lag.max = 8, type = "cons") #On choisit 8 car on suppose que les variables du modèle ne peuvent
#avoir d'impact les unes sur les autres après deux années)
#Tous indiquent 1

Xres <- Xres[, c("deltaTAres", "deltaGres", "deltaPIBres")]
estres <- VAR(Xres,p=1, type="cons")
estres
summary(estres, equation = "deltaPIBres")
plot(estres, names= "deltaPIBres")
summary(estres, equation="deltaGres")

serres <- serial.test(estres, lags.pt = 16)
serres$serial
#Et là ça marche dès qu'on utilise 1 lag on obtient une p-value grande qui fait qu'on ne peut 
#pas rejeter Hà :"Pas d'autocorrélation"

est_residuals <- resid(estres)
acf(est_residuals)

norm3 <- normality.test(estres)
norm3$jb.mul
#On ne peut pas rejeter que les résidus suivent une loi normale
#Top mais le diagram of fit est pas ouf surtout qu'il y a un décalage
#Quand on regarde significativité des coefficients trend pas significatif donc on peut l'enlever
#par contre constante a l'air significatif


#Passage au VAR structurel 
#Identification de M1 et M2
M1 = matrix( c(1, 0, -99, 0, 1, -99,-99,-99, 1), nrow=3, ncol=3) #je mets 99 quand c'est une valeur inconnue pour le moment
M2 = matrix( c(1, 99, 0, 99, 1, 0,0,0, 1), nrow=3, ncol=3)

#Argument stabilisateurs automatiques + pol discrétionnaire => élasticité des TA et G aux PIB (on reprend les valeurs du papier)
M1[1,3]<- -0.8
M1[2,3]<-0

#Argument : décisions portant sur les TA précèdent celles sur les dépenses
M2[1,2]<- 0

#On calcule les epsilon(ta) 
eps_ta=est_residuals[,1]-0.8*est_residuals[,3]

#Regression MCO sur la deuxième equation pour obtenir beta_gt
lineareq2 <- lm(est_residuals[,2] ~ eps_ta - 1) #on ne veut pas d'intercept
#on n'obtient -0.02, Biau et Girard eux obtiennent -0.05.
eps_tg <- residuals(lineareq2)
M2[2,1]<- summary(lineareq2)$coefficients[1, 1]
#Tous les coefficients de M2 sont maintenant identifiés !!!!

#Regression MCO (avec variables instrumentales) sur la troisième équation pour obtenir gamma_yt et gamma_yg
#Les IV sont eps_ta pour u_ta et eps_tg pour u_tg
lineareq3 <- ivreg(est_residuals[,3] ~ est_residuals[,1] + est_residuals[,2] -1| eps_ta +eps_tg)
M1[3,1]<- - summary(lineareq3)$coefficients[1,1]
M1[3,2] <- - summary(lineareq3)$coefficients[2,1]
#Tous les coefficients de M1 sont aussi identifiés

#Les IRFs
horizon <- 30
n_iter <- 100

P <- inv(M1)%*%M2

results <- matrix(numeric(0), 3,horizon)
resultsbis <- matrix(numeric(0), 3, horizon)

#On construit la matrix rho pour pouvoir après calculer Xt=rho*X(t-1)+epsilont
rho <- matrix(numeric(0),3,3)

for (j in 1:3)
{
  rho[1,j] <- coef(estres)$deltaTAres[j,1]
}

for (j in 1:3)
{
  rho[2,j] <- coef(estres)$deltaGres[j,1]
}

for (j in 1:3)
{
  rho[3,j] <- coef(estres)$deltaPIBres[j,1]
}


epsilon <- matrix(numeric(0), 3,1)
epsilon[1,1]<-1
epsilon[2,1]<-0
epsilon[3,1]<-0

results[1:3,1] <- P%*%epsilon

#normalisation à 1 euro
epsilonbis <- matrix(numeric(0), 3,1)
epsilonbis[1,1]<-1/results[1,1]
epsilonbis[2,1]<-0
epsilonbis[3,1]<-0

resultsbis[1:3,1] <- P%*%epsilonbis

for (j in 2:horizon)
{
  resultsbis[,j] <- rho%*%resultsbis[,j-1]
}

#calcul en niveau
resultsNiveauxLn<-matrix(numeric(0), 3,horizon)
resultsNiveauxLn[,1]<-resultsbis[,1]

for (j in 2:horizon)
{
  resultsNiveauxLn[,j]<-resultsbis[,j]+resultsNiveauxLn[,j-1]
}



#Monte Carlo
IRF_TA <- matrix(numeric(0), horizon,n_iter)
IRF_G <- matrix(numeric(0), horizon,n_iter)
IRF_PIB <- matrix(numeric(0),horizon,n_iter)
for (i in 1:n_iter)
{
  #Générer de nouvelles donn?es de meme taille
  Xmonte <- matrix(numeric(0), 3,115)
  Xmonte[,1]=P%*%residuals(estres)[sample(1:115, 1),]
  for (j in 2:115)
  {
    Xmonte[,j]=rho%*%Xmonte[,j-1]+P%*%residuals(estres)[sample(1:115, 1),]
  }
  #Estime le modèle sur ces données
  
  Xmonte <- data.frame(Xmonte[1,], Xmonte[2,], Xmonte[3,])
  estmonte <- VAR(Xmonte,p=1, type="cons")
  estmonte_residuals <-residuals(estmonte)
  #Estimer le nouveau P (donc M1 et M2)
  
  Mmonte1 = matrix( c(1, 0, -99, 0, 1, -99,-99,-99, 1), nrow=3, ncol=3) #je mets 99 quand c'est une valeur inconnue pour le moment
  Mmonte2 = matrix( c(1, 99, 0, 99, 1, 0,0,0, 1), nrow=3, ncol=3)
  Mmonte1[1,3]<- -0.8
  Mmonte1[2,3]<-0
  Mmonte2[1,2]<- 0
  
  #On calcule les epsilon(ta) 
  epsmonte_ta=estmonte_residuals[,1]-0.8*estmonte_residuals[,3]
  
  #Regression MCO sur la deuxième equation pour obtenir beta_gt
  linearmonteeq2 <- lm(estmonte_residuals[,2] ~ epsmonte_ta - 1) 
  epsmonte_tg <- residuals(linearmonteeq2)
  Mmonte2[2,1]<- summary(linearmonteeq2)$coefficients[1, 1]
  
  
  #Regression MCO (avec variables instrumentales) sur la troisième équation pour obtenir gamma_yt et gamma_yg
  #Les IV sont eps_ta pour u_ta et eps_tg pour u_tg
  linearmonteeq3 <- ivreg(estmonte_residuals[,3] ~ estmonte_residuals[,1] + estmonte_residuals[,2] -1| epsmonte_ta +epsmonte_tg)
  Mmonte1[3,1]<- - summary(linearmonteeq3)$coefficients[1,1]
  Mmonte1[3,2] <- - summary(linearmonteeq3)$coefficients[2,1]
  #Tous les coefficients de M1 sont aussi identifi?s
  
  #IRFs
  Pmonte <- inv(Mmonte1)%*%Mmonte2
  
  resultsmonte <- matrix(numeric(0), 3,horizon)
  resultsmontebis <- matrix(numeric(0), 3,horizon)
  resultsmonteNiveauxLn <- matrix(numeric(0), 3,horizon)
  
  #On construit la matrix rho pour pouvoir après calculer Xt=rho*X(t-1)+epsilont
  rhomonte <- matrix(numeric(0),3,3)
  
  for (j in 1:3)
  {
    rhomonte[1,j] <- coef(estmonte)$Xmonte.1...[j,1]
  }
  
  for (j in 1:3)
  {
    rhomonte[2,j] <- coef(estmonte)$Xmonte.2...[j,1]
  }
  
  for (j in 1:3)
  {
    rhomonte[3,j] <- coef(estmonte)$Xmonte.3...[j,1]
  }
  
  epsilonmonte <- matrix(numeric(0), 3,1)
  epsilonmonte[1,1]<-1
  epsilonmonte[2,1]<-0
  epsilonmonte[3,1]<-0
  
  resultsmonte[,1] <- Pmonte%*%epsilonmonte
  
  epsilonmontebis <- matrix(numeric(0), 3,1)
  epsilonmontebis[1,1]<-1/(resultsmonte[1,1])
  epsilonmontebis[2,1]<-0
  epsilonmontebis[3,1]<-0
  
  resultsmontebis[,1] <- Pmonte%*%epsilonmontebis
  
  for (j in 2:horizon)
  {
    resultsmontebis[,j] <- rhomonte%*%resultsmontebis[,j-1]
  }
  
  resultsmonteNiveauxLn[,1]<-resultsmontebis[,1]
  
  for (j in 2:horizon)
  {
    resultsmonteNiveauxLn[,j]<-resultsmontebis[,j]+resultsmonteNiveauxLn[,j-1]
  }
  
  
  IRF_TA[,i] <-resultsmonteNiveauxLn[1,]
  IRF_G[,i] <- resultsmonteNiveauxLn[2,]
  IRF_PIB[,i]<- resultsmonteNiveauxLn[3,]
  
  
}

varIRF_TA <- matrix(numeric(0), horizon,1)
for (j in 1:horizon)
{
  varIRF_TA[j,]<-var(IRF_TA[j,])
}
sc=1.6449 #90% intervalle de confiance
Max_TA <- resultsNiveauxLn[1,] +sc*sqrt(varIRF_TA)
Min_TA <- resultsNiveauxLn[1,] -sc*sqrt(varIRF_TA)


varIRF_G <- matrix(numeric(0), horizon,1)
for (j in 1:horizon)
{
  varIRF_G[j,]<-var(IRF_G[j,])
}
Max_G <- resultsNiveauxLn[2,] +sc*sqrt(varIRF_G)
Min_G <- resultsNiveauxLn[2,] -sc*sqrt(varIRF_G)

varIRF_PIB <- matrix(numeric(0), horizon,1)
for (j in 1:horizon)
{
  varIRF_PIB[j,]<-var(IRF_PIB[j,])
}
Max_PIB <- resultsNiveauxLn[3,] +sc*sqrt(varIRF_PIB)
Min_PIB <- resultsNiveauxLn[3,] -sc*sqrt(varIRF_PIB)

plot(resultsNiveauxLn[3,],type="l",col="blue",ylim=c(-1,1))
lines(Max_PIB,type="l",col="red")
lines(Min_PIB,type="l",col="red")

plot(resultsNiveauxLn[2,],type="l",col="blue",ylim=c(-1,1))
lines(Max_G,type="l",col="red")
lines(Min_G,type="l",col="red")

plot(resultsNiveauxLn[1,],type="l",col="blue",ylim=c(0,2))
lines(Max_TA,type="l",col="red")
lines(Min_TA,type="l",col="red")


#Choc sur dépenses
results2<-matrix(numeric(0),3,horizon)
results2bis<-matrix(numeric(0),3,horizon)

epsilon2 <- matrix(numeric(0), 3,1)
epsilon2bis <- matrix(numeric(0), 3,1)


epsilon2[1,1]<-0
epsilon2[2,1]<-1
epsilon2[3,1]<-0

results2[1:3,1] <- P%*%epsilon2


epsilon2bis[1,1]<-0
epsilon2bis[2,1]<-1/results2[2,1]
epsilon2bis[3,1]<-0

results2bis[1:3,1] <- P%*%epsilon2bis

for (j in 2:horizon)
{
  results2bis[,j] <- rho%*%results2bis[,j-1]
}

results2NiveauxLn<-matrix(numeric(0), 3,horizon)
results2NiveauxLn[,1]<-results2bis[,1]

for (j in 2:horizon)
{
  results2NiveauxLn[,j]<-results2bis[,j]+results2NiveauxLn[,j-1]
}


#Monte Carlo
IRF_TA_2 <- matrix(numeric(0), horizon,n_iter)
IRF_G_2 <- matrix(numeric(0), horizon,n_iter)
IRF_PIB_2 <- matrix(numeric(0),horizon,n_iter)
for (i in 1:n_iter)
{
  #Générer de nouvelles données de meme taille
  Xmonte_2 <- matrix(numeric(0), 3,115)
  Xmonte_2[,1]=P%*%residuals(estres)[sample(1:115, 1),]
  for (j in 2:115)
  {
    Xmonte_2[,j]=rho%*%Xmonte_2[,j-1]+P%*%residuals(estres)[sample(1:115, 1),]
  }
  
  #Estime le modèle sur ces données
  
  Xmonte_2 <- data.frame(Xmonte_2[1,], Xmonte_2[2,], Xmonte_2[3,])
  estmonte_2 <- VAR(Xmonte_2,p=1, type="cons")
  estmonte_residuals_2 <-residuals(estmonte_2)
  #Estimer le nouveau P (donc M1 et M2)
  
  Mmonte1_2 = matrix( c(1, 0, -99, 0, 1, -99,-99,-99, 1), nrow=3, ncol=3) #je mets 99 quand c'est une valeur inconnue pour le moment
  Mmonte2_2 = matrix( c(1, 99, 0, 99, 1, 0,0,0, 1), nrow=3, ncol=3)
  Mmonte1_2[1,3]<- -0.8
  Mmonte1_2[2,3]<-0
  Mmonte2_2[1,2]<- 0
  
  #On calcule les epsilon(ta) 
  epsmonte_ta_2=estmonte_residuals_2[,1]-0.8*estmonte_residuals_2[,3]
  
  #Regression MCO sur la deuxi?me equation pour obtenir beta_gt
  linearmonteeq2_2 <- lm(estmonte_residuals_2[,2] ~ epsmonte_ta_2 - 1) 
  epsmonte_tg_2 <- residuals(linearmonteeq2_2)
  Mmonte2_2[2,1]<- summary(linearmonteeq2_2)$coefficients[1, 1]
  
  
  #Regression MCO (avec variables instrumentales) sur la troisième équation pour obtenir gamma_yt et gamma_yg
  #Les IV sont eps_ta pour u_ta et eps_tg pour u_tg
  linearmonteeq3_2 <- ivreg(estmonte_residuals_2[,3] ~ estmonte_residuals_2[,1] + estmonte_residuals_2[,2] -1| epsmonte_ta_2 +epsmonte_tg_2)
  Mmonte1_2[3,1]<- - summary(linearmonteeq3_2)$coefficients[1,1]
  Mmonte1_2[3,2] <- - summary(linearmonteeq3_2)$coefficients[2,1]
  #Tous les coefficients de M1 sont aussi identifi?s
  
  #IRFs
  Pmonte_2 <- inv(Mmonte1_2)%*%Mmonte2_2
  
  resultsmonte_2 <- matrix(numeric(0), 3,horizon)
  resultsmontebis_2 <- matrix(numeric(0), 3,horizon)
  resultsmonteNiveauxLn_2 <- matrix(numeric(0), 3,horizon)
  
  #On construit la matrix rho pour pouvoir apr?s calculer Xt=rho*X(t-1)+epsilont
  rhomonte_2 <- matrix(numeric(0),3,3)
  
  for (j in 1:3)
  {
    rhomonte_2[1,j] <- coef(estmonte_2)$Xmonte_2.1...[j,1]
  }
  
  for (j in 1:3)
  {
    rhomonte_2[2,j] <- coef(estmonte_2)$Xmonte_2.2...[j,1]
  }
  
  for (j in 1:3)
  {
    rhomonte_2[3,j] <- coef(estmonte_2)$Xmonte_2.3...[j,1]
  }
  
  epsilonmonte_2 <- matrix(numeric(0), 3,1)
  epsilonmonte_2[1,1]<-0
  epsilonmonte_2[2,1]<-1
  epsilonmonte_2[3,1]<-0
  
  resultsmonte_2[,1] <- Pmonte_2%*%epsilonmonte_2
  
  epsilonmontebis_2 <- matrix(numeric(0), 3,1)
  epsilonmontebis_2[1,1]<-0
  epsilonmontebis_2[2,1]<-1/(resultsmonte_2[2,1])
  epsilonmontebis_2[3,1]<-0
  
  resultsmontebis_2[,1] <- Pmonte_2%*%epsilonmontebis_2
  
  for (j in 2:horizon)
  {
    resultsmontebis_2[,j] <- rhomonte_2%*%resultsmontebis_2[,j-1]
  }
  
  resultsmonteNiveauxLn_2[,1]<-resultsmontebis_2[,1]
  
  for (j in 2:horizon)
  {
    resultsmonteNiveauxLn_2[,j]<-resultsmontebis_2[,j]+resultsmonteNiveauxLn_2[,j-1]
  }
  
  
  IRF_TA_2[,i] <-resultsmonteNiveauxLn_2[1,]
  IRF_G_2[,i] <- resultsmonteNiveauxLn_2[2,]
  IRF_PIB_2[,i]<- resultsmonteNiveauxLn_2[3,]
  
  
}

varIRF_TA_2 <- matrix(numeric(0), horizon,1)
for (j in 1:horizon)
{
  varIRF_TA_2[j,]<-var(IRF_TA_2[j,])
}
sc=1.6449 #90% intervalle de confiance
Max_TA_2 <- results2NiveauxLn[1,] +sc*sqrt(varIRF_TA_2)
Min_TA_2 <- results2NiveauxLn[1,] -sc*sqrt(varIRF_TA_2)


varIRF_G_2 <- matrix(numeric(0), horizon,1)
for (j in 1:horizon)
{
  varIRF_G_2[j,]<-var(IRF_G_2[j,])
}
Max_G_2 <- results2NiveauxLn[2,] +sc*sqrt(varIRF_G_2)
Min_G_2 <- results2NiveauxLn[2,] -sc*sqrt(varIRF_G_2)

varIRF_PIB_2 <- matrix(numeric(0), horizon,1)
for (j in 1:horizon)
{
  varIRF_PIB_2[j,]<-var(IRF_PIB_2[j,])
}
Max_PIB_2 <- results2NiveauxLn[3,] +sc*sqrt(varIRF_PIB_2)
Min_PIB_2 <- results2NiveauxLn[3,] -sc*sqrt(varIRF_PIB_2)

plot(results2NiveauxLn[3,],type="l",col="blue",ylim=c(-2,2))
lines(Max_PIB_2,type="l",col="red")
lines(Min_PIB_2,type="l",col="red")

plot(results2NiveauxLn[2,],type="l",col="blue",ylim=c(0,4))
lines(Max_G_2,type="l",col="red")
lines(Min_G_2,type="l",col="red")

plot(results2NiveauxLn[1,],type="l",col="blue",ylim=c(-5,5))
lines(Max_TA_2,type="l",col="red")
lines(Min_TA_2,type="l",col="red")

