library(tseries)
library(xlsx)
library(mFilter)
library(xts)
library(fUnitRoots) #test Adf avec constante, tendance ou aucun
library(vars) #estimate a VAR model


#J'ai pas trouv� un d�flateur du PIB trimestriel 
rawIPC <- read.csv("IPC-indice.csv", sep="," , dec=".")

IPC <- ts(rawIPC$Value , start =c(1980,1) , frequency =4) # indice des prix � la conso (deflateur)
plot(IPC)

rawPIBval <- read.xlsx("t_pib_val.xls", sheetName = "Niveaux") #PIB en valeur

PIBval <- ts(as.numeric(as.character(rawPIBval[8:284,2])), start=c(1949,1) , frequency=4)
PIB <- PIBval/IPC
plot(PIB)

rawAPU <- read.xlsx("t_compteapu_val.xls", sheetName = "Niveaux") # en valeur

depensestot <- ts(as.numeric(as.character(rawAPU[131:282,4])), start=c(1980,1) , frequency=4)

# On d�finit une variable d�penses publiques G (cf papier Biau et Girard) ou on exclut les transferts
#aux m�nages et aux entreprises ainsi que la charge de la dette
consint <- ts(as.numeric(as.character(rawAPU[131:282,5])), start=c(1980,1) , frequency=4)
remunerationsal <- ts(as.numeric(as.character(rawAPU[131:282,6])), start=c(1980,1) , frequency=4)
autresdepfctnmt <- ts(as.numeric(as.character(rawAPU[131:282,8])), start=c(1980,1), frequency=4)
FBCF <- ts(as.numeric(as.character(rawAPU[131:282,15])), start=c(1980,1) , frequency=4)
autresfinan <- ts(as.numeric(as.character(rawAPU[131:282,16])), start=c(1980,1) , frequency=4)
depenses <- consint + remunerationsal + autresdepfctnmt + FBCF + autresfinan
G <- depenses/IPC
plot(G)

#On d�finit les recettes publiques TA (cf papier Biau et Girard) comme la somme de G et de la capacit� de financement
recettes <- ts(as.numeric(as.character(rawAPU[131:282,18])), start=c(1980,1) , frequency=4)
capfinancement <- ts(as.numeric(as.character(rawAPU[131:282,2])), start=c(1980,1) , frequency=4)
recettes <- capfinancement + depenses
TA <- recettes/IPC
plot(TA)
#On n'a pas d�duit l'impot sur les soci�t�s (pour le moment) contrairement au papier de Biau et Girard
#Normalement faisable avec comptabilit� nationale item D51

#Estimation d'un VAR3 : 1980Q1 to 2017Q3 => 151 trimestres

PIBlog <-log(PIB)
Glog <- log(G)
TAlog <- log(TA)

par(mfrow=c(2,2))
plot(PIBlog)
plot(Glog)
plot(TAlog)

#il semble y avoir une tendance d�terministe lin�aire
adfTest(PIBlog, type="ct") #test avec constante et tendance
adfTest(Glog,  type="ct")
adfTest(TAlog, type="ct")
#On ne peut pas rejeter l'existence d'une racine unitaire pour les trois s�ries qd on prend en compte tendance et constante

#On prend tout en diff�rence premi�re *100 pour avoir en pourcentage : argument Hamilton (1994)
#le passage en diff�rence accro�t la pr�cision de l'estimation du mod�le,
#dans une situation o� l'�chantillon est de petite taille

deltaPIB <- diff(PIBlog)*100
deltaTA <- diff(TAlog)*100
deltaG <- diff(Glog)*100

plot(deltaPIB)
plot(deltaTA)
plot(deltaG)

#On teste la stationnarit� des 3
adfTest(deltaPIB, type="nc") #test avec ni constante ni tendance
adfTest(deltaTA,  type="nc")
adfTest(deltaG, type="nc")
#On rejette l'existence d'une racine unitaire avec un seuil sup�rieur � 99 pourcent

#Conclusion : nos 3 s�ries sont I(1)

#On souhaite maintenant tester l'existence d'une relation de coint�gration
#Eventuellement entre deltaTA et deltaG (deltaTA-deltaG aurait un sens �conomique)
plot(TAlog-Glog) #bof stationnaire semble y avoir une constante (trend?)
#On va quand meme tester la stationnarit� de la diff�rence
adfTest(TAlog-Glog, type="c")
adfTest(TAlog-Glog, type="ct")
#On ne peut pas rejeter pour les deux tests que c'est non stationnaire
#BG trouve de meme (en ce qui concerne la relation de coint�gration) pour la France sur des donn�es ant�rieures et idem USA avec BP(2002)

#Conclusion: On a trois variables I(1) et pas de relation de coint�gration apparente 
#Donc on va travailler avec un mod�le VAR en diff�rence premi�re

#Estimation d'un VAR en diff�rence premi�re
Y <- data.frame(deltaPIB, deltaG, deltaTA)
#On d�termine d'abord le nombre de retard
VARselect(Y, lag.max = 8, type = "both") #On choisit 8 car on suppose que les variables du mod�le ne peuvent
#avoir d'impact les unes sur les autres apr�s deux ann�es)
#Tous indiquent 1

Y <- Y[, c("deltaPIB", "deltaG", "deltaTA")]
est <- VAR(Y,p=1, type="both")
est
summary(est, equation = "deltaTA")
plot(est, names= "deltaPIB")
summary(est, equation="deltaG")

ser11 <- serial.test(est, lags.pt = 16, type = "PT.adjusted")
ser11$serial
#p=1 r�sidus autocorr�l�s donc j'augmente p progressivement
#H0 �tant pas d'autocorr�lation

est <- VAR(Y,p=7, type="both")
est
summary(est, equation = "deltaTA")
plot(est, names= "deltaPIB")
summary(est, equation="deltaG")

ser11 <- serial.test(est, lags.pt = 16)
ser11$serial
#residus sont auto-corr�l�s meme quand je passe � 8 lags (pas bon)
#j'obtiens la p-value la plus grande pour p=7
res<- resid(est)
acf(res)

#Je vais regarder si le probl�me c'est les donn�es apr�s 2009
inds <- seq(as.Date("1980-01-01"), as.Date("2017-12-31"), by = "quarter")
deltaPIBres <- subset(deltaPIB, inds >= as.Date("1980-01-01") & inds < as.Date("2009-01-01"))
deltaGres <- subset(deltaG, inds >= as.Date("1980-01-01") & inds < as.Date("2009-01-01"))
deltaTAres <- subset(deltaTA, inds >= as.Date("1980-01-01") & inds < as.Date("2009-01-01"))
#On a 116 points
plot(deltaPIBres)
plot(deltaGres)
plot(deltaTAres)
#semble stationnaire
adfTest(deltaPIBres, type="c")
adfTest(deltaGres, type="c")
adfTest(deltaTAres, type="c")
#oki


Yres <- data.frame(deltaPIBres, deltaGres, deltaTAres)
#On d�termine d'abord le nombre de retard
VARselect(Yres, lag.max = 8, type = "both") #On choisit 8 car on suppose que les variables du mod�le ne peuvent
#avoir d'impact les unes sur les autres apr�s deux ann�es)
#Tous indiquent 1

Yres <- Yres[, c("deltaPIBres", "deltaGres", "deltaTAres")]
estres <- VAR(Yres,p=1, type="both")
estres
summary(estres, equation = "deltaPIBres")
plot(estres, names= "deltaPIBres")
summary(estres, equation="deltaGres")

serres <- serial.test(estres, lags.pt = 16)
serres$serial
#Et l� �a marche d�s qu'on utilise 1 lag on obtient une p-value grande qui fait qu'on ne peut 
#pas rejeter H� :"Pas d'autocorr�lation"

est_residuals <- resid(estres)
acf(est_residuals)

norm3 <- normality.test(estres)
norm3$jb.mul
#On ne peut pas rejeter que les r�sidus suivent une loi normale
#Top mais le diagram of fit est pas ouf surtout qu'il y a un d�calage
#Quand on regarde significativit� des coefficients trend pas significatif donc on peut l'enlever
#par contre constante a l'air significatif










#BROUILLON
#Brouillon
#Je vais essayer de regarder mes s�ries en le d�trendant + avec des indicatrices
detrendPIB <- hpfilter(PIBlog,freq=1600)$cycle
detrendTA <- hpfilter(TAlog, freq=1600)$cycle
detrendG <- hpfilter(Glog, freq=1600)$cycle

adfTest(detrendPIB, type=c("nc"))
adfTest(detrendG, type=c("nc"))
adfTest(detrendTA, type=c("nc"))
#rejette existence d'une racine unitaire

Y2 <- data.frame(detrendPIB, detrendG, detrendTA)
#On d�termine d'abord le nombre de retard
VARselect(Y2, lag.max = 8, type = "const") #On choisit 8 car on suppose que les variables du mod�le ne peuvent
#avoir d'impact les unes sur les autres apr�s deux ann�es)
#Indiquent 2 ou 5

Y2 <- Y2[, c("detrendPIB", "detrendG", "detrendTA")]
est2 <- VAR(Y2,p=8, type="const")
est2
summary(est2, equation = "detrendPIB")
plot(est2, names= "detrendPIB")
#le fit est bien meilleur mais r�sidus sont autocorr�les
summary(est2, equation="detrendG")

ser2 <- serial.test(est, lags.pt = 16, type = "PT.asymptotic")
ser2$serial

norm2 <- normality.test(est2)
norm2$jb.mul
#auto-corr�l� et loins d'etre normaux

inds <- seq(as.Date("1980-01-01"), as.Date("2017-12-31"), by = "quarter")
dummy <- as.numeric(inds >= "2009-01-01" & inds<"2010-01-01")
dummy2 <-  as.numeric(inds >= "1992-02-01" & inds<="1993-04-01")
detrendPIB2 <- residuals(lm(PIBlog ~1 + inds + dummy))
detrendG2 <- residuals (lm(Glog ~ 1+ inds ))
detrendTA2 <- residuals (lm(TAlog ~ 1 + inds + dummy + dummy2))
#juste pour TA adf dnne 

Y3 <- data.frame(detrendPIB2, detrendG2, detrendTA2)
#On d�termine d'abord le nombre de retard
VARselect(Y3, lag.max = 8, type = "const") #On choisit 8 car on suppose que les variables du mod�le ne peuvent
#avoir d'impact les unes sur les autres apr�s deux ann�es)
#Indiquent 2 ou 3

Y3 <- Y3[, c("detrendPIB2", "detrendG2", "detrendTA2")]
est3 <- VAR(Y3,p=5, type="const")
est3
summary(est3, equation = "detrendPIB2")
plot(est3, names= "detrendPIB2")
ser3 <- serial.test(est3, lags.pt = 16, type = "PT.asymptotic")
ser3$serial
est3_residuals <- resid(est3)
acf(est3_residuals)

norm3 <- normality.test(est3)
norm3$jb.mul