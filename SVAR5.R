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

IPC <- ts(rawIPC[77:152,17] , start =c(1999,1) , frequency =4) # indice des prix ? la conso (deflateur)
plot(IPC)

rawPIBval <- t_pib_val #PIB en valeur

PIBval <- ts(rawPIBval[207:282,2], start=c(1999,1) , frequency=4)
PIB <- PIBval/IPC
plot(PIB)


rawAPU <- t_compteapu_val # en valeur

depensestot <- ts(rawAPU[207:282,4], start=c(1999,1) , frequency=4)

# On d?finit une variable d?penses publiques G (cf papier Biau et Girard) ou on exclut les transferts
#aux m?nages et aux entreprises ainsi que la charge de la dette
consint <- ts(rawAPU[207:282,5], start=c(1999,1) , frequency=4)
remunerationsal <- ts(rawAPU[207:282,6], start=c(1999,1) , frequency=4)
autresdepfctnmt <- ts(rawAPU[207:282,8], start=c(1999,1), frequency=4)
FBCF <- ts(rawAPU[207:282,15], start=c(1999,1) , frequency=4)
autresfinan <- ts(rawAPU[207:282,16], start=c(1999,1) , frequency=4)
depenses <- consint + remunerationsal + autresdepfctnmt + FBCF + autresfinan
G <- depenses/IPC
plot(G)

#On d?finit les recettes publiques TA (cf papier Biau et Girard) comme la somme de G et de la capacit? de financement
recettes <- ts(rawAPU[207:282,18], start=c(1999,1) , frequency=4)
capfinancement <- ts(rawAPU[207:282,2], start=c(1999,1) , frequency=4)
recettes <- capfinancement + depenses
TA <- recettes/IPC
plot(TA)
#On n'a pas d?duit l'impot sur les soci?t?s (pour le moment) contrairement au papier de Biau et Girard
#Normalement faisable avec comptabilit? nationale item D51

#On ajoute le taux d'intérêt interbancaire
taux<-ts(taux_interbancaire[1:76,3], start=c(1999,1) , frequency=4)
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
adfTest(IPClog, type="ct")
adfTest(taux, type="ct")
adfTest(taux, type="c")
#On ne peut pas rejeter l'existence d'une racine unitaire pour les 4 séries (sauf le taux) quand on prend tendance + constance
#Pour le taux, on ne peut pas rejetter l'existence d'une racine unitaire quand on prend uniquement constante. Si on prend, tendance + constance, on peut rejeter 

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
plot(TAlog-Glog) #bof stationnaire : coupure en 2008 avec creusement fort du déficit pour ensuite ajustement budgétaire
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
#on a 1 -

#Test de Johansen
jotest=ca.jo(Y, type="trace", K=3, ecdet="const", spec="longrun")
summary(jotest)
#Il y aurait 4 relations de cointégration à 5 %... 


est <- VAR(Y,p=3, type="const") #
est
summary(est, equation = "deltaTA")
plot(est, names="deltaTA")
summary(est, equation="deltaG")
plot(est, names="deltaG")
summary(est, equation="deltaPIB")
plot(est,names="deltaPIB")

ser11 <- serial.test(est, lags.pt = 16)
ser11$serial
#residus ne sont pas auto-corrélés pour p=1 ; p value maximum pour p = 3
res<- resid(est)
acf(res)

norm3 <- normality.test(est)
norm3$jb.mul
#On rejette que les résidus suivent une loi normale... 
#problème de significativité des coeff

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
#on n'obtient -0.02, Biau et Girard eux obtiennent -0.05.
eps_tg <- residuals(lineareq2)
M2[2,1]<- summary(lineareq2)$coefficients[1, 1]
#Tous les coefficients de M2 sont maintenant identifi?s !!!!

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

