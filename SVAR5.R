install.packages("rJava")
install.packages("xlsx")
install.packages("mFilter")
install.packages("fUnitRoots")
install.packages("vars")
install.packages("xlsxjars")




library(tseries)
library(xlsx)
library(mFilter)
library(xts)
library(fUnitRoots) #test Adf avec constante, tendance ou aucun
library(vars) #estimate a VAR model


#J'ai pas trouv? un d?flateur du PIB trimestriel 
rawIPC <- read.csv("IPC-indice.csv", sep="," , dec=".")

IPC <- ts(rawIPC[77:141,17] , start =c(1999,1) , frequency =4) # indice des prix ? la conso (deflateur)
plot(IPC)

rawPIBval <- t_pib_val #PIB en valeur

PIBval <- ts(rawPIBval[207:271,2], start=c(1999,1) , frequency=4)
PIB <- PIBval/IPC
plot(PIB)


rawAPU <- t_compteapu_val # en valeur

depensestot <- ts(rawAPU[207:271,4], start=c(1999,1) , frequency=4)

# On d?finit une variable d?penses publiques G (cf papier Biau et Girard) ou on exclut les transferts
#aux m?nages et aux entreprises ainsi que la charge de la dette
consint <- ts(rawAPU[207:271,5], start=c(1999,1) , frequency=4)
remunerationsal <- ts(rawAPU[207:271,6], start=c(1999,1) , frequency=4)
autresdepfctnmt <- ts(rawAPU[207:271,8], start=c(1999,1), frequency=4)
FBCF <- ts(rawAPU[207:271,15], start=c(1999,1) , frequency=4)
autresfinan <- ts(rawAPU[207:271,16], start=c(1999,1) , frequency=4)
depenses <- consint + remunerationsal + autresdepfctnmt + FBCF + autresfinan
G <- depenses/IPC
plot(G)

#On d?finit les recettes publiques TA (cf papier Biau et Girard) comme la somme de G et de la capacit? de financement
recettes <- ts(rawAPU[207:271,18], start=c(1999,1) , frequency=4)
capfinancement <- ts(rawAPU[207:271,2], start=c(1999,1) , frequency=4)
recettes <- capfinancement + depenses
TA <- recettes/IPC
plot(TA)
#On n'a pas d?duit l'impot sur les soci?t?s (pour le moment) contrairement au papier de Biau et Girard
#Normalement faisable avec comptabilit? nationale item D51

#On ajoute le taux d'intérêt interbancaire
taux<-ts(taux_interbancaire[1:65,3], start=c(1999,1) , frequency=4)
plot(taux)

PIBlog <-log(PIB)
Glog <- log(G)
TAlog <- log(TA)
IPClog<-log(IPC)
tauxlog<-log(taux)


par(mfrow=c(3,2))
plot(PIBlog)
plot(Glog)
plot(TAlog)
plot(IPClog)
plot(tauxlog)

#il semble y avoir une tendance d?terministe lin?aire. Pour le taux, éventuellement une tendance linéaire à partir de 2009
adfTest(PIBlog, type="ct") #test avec constante et tendance
adfTest(Glog,  type="ct")
adfTest(TAlog, type="ct")
adfTest(IPClog, type="ct")
adfTest(tauxlog, type="ct")
adfTest(tauxlog, type="c")
#On ne peut pas rejeter l'existence d'une racine unitaire pour les 4 séries (sauf le taux) quand on prend tendance + constance
#Pour le taux, on ne peut pas rejetter l'existence d'une racine unitaire quand on prend uniquement constante. Si on prend, tendance + constance, on peut rejeter 

#On prend tout en diff?rence premi?re *100 pour avoir en pourcentage : argument Hamilton (1994)
#le passage en diff?rence accro?t la pr?cision de l'estimation du mod?le,
#dans une situation o? l'?chantillon est de petite taille

deltaPIB <- diff(PIBlog)*100
deltaTA <- diff(TAlog)*100
deltaG <- diff(Glog)*100
deltaIPC<-diff(IPClog)*100
deltataux<-diff(tauxlog)*100

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

Y <- data.frame(deltaPIB, deltaG, deltaTA, deltaIPC, deltataux)
names(Y)<-c("deltaPIB","deltaG","deltaTA","deltaIPC","deltataux")
#On détermine d'abord le nombre de retard
VARselect(Y, lag.max = 8, type = "const") #On choisit 8 car on suppose que les variables du modèle ne peuvent
#avoir d'impact les unes sur les autres aprés deux années)
#on a 7, 1 ou 8... --> choix fait avec le test sur l'autocorrélation des résidus

#Test de Johansen
jotest=ca.jo(Y, type="trace", K=4, ecdet="const", spec="longrun")
summary(jotest)
#Il y aurait 2 relations de cointégration à 5 % (cohérent avec le papier)


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
#residus ne sont pas auto-corrélés
res<- resid(est)
acf(res)


norm3 <- normality.test(est)
norm3$jb.mul
#On rejette que les résidus suivent une loi normale... 
#problème de significativité des coeff

