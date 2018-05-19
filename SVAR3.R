library(tseries)
library(xlsx)
library(mFilter)
library(xts)
library(fUnitRoots) #test Adf avec constante, tendance ou aucun
library(vars) #estimate a VAR model


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

#On prend tout en différence première *100 pour avoir en pourcentage : argument Hamilton (1994)
#le passage en différence accroît la précision de l'estimation du modèle,
#dans une situation où l'échantillon est de petite taille

deltaPIB <- diff(PIBlog)*100
deltaTA <- diff(TAlog)*100
deltaG <- diff(Glog)*100

plot(deltaPIB)
plot(deltaTA)
plot(deltaG)

#On teste la stationnarité des 3
adfTest(deltaPIB, type="nc") #test avec ni constante ni tendance
adfTest(deltaTA,  type="nc")
adfTest(deltaG, type="nc")
#On rejette l'existence d'une racine unitaire avec un seuil supérieur à 99 pourcent

#Conclusion : nos 3 séries sont I(1)

#On souhaite maintenant tester l'existence d'une relation de cointégration
#Eventuellement entre deltaTA et deltaG (deltaTA-deltaG aurait un sens économique)
plot(TAlog-Glog) #bof stationnaire semble y avoir une constante (trend?)
#On va quand meme tester la stationnarité de la différence
adfTest(TAlog-Glog, type="c")
adfTest(TAlog-Glog, type="ct")
#On ne peut pas rejeter pour les deux tests que c'est non stationnaire
#BG trouve de meme (en ce qui concerne la relation de cointégration) pour la France sur des données antérieures et idem USA avec BP(2002)

#Conclusion: On a trois variables I(1) et pas de relation de cointégration apparente 
#Donc on va travailler avec un modèle VAR en différence première

#Estimation d'un VAR en différence première
Y <- data.frame(deltaPIB, deltaG, deltaTA)
#On détermine d'abord le nombre de retard
VARselect(Y, lag.max = 8, type = "both") #On choisit 8 car on suppose que les variables du modèle ne peuvent
#avoir d'impact les unes sur les autres après deux années)
#Tous indiquent 1

Y <- Y[, c("deltaPIB", "deltaG", "deltaTA")]
est <- VAR(Y,p=1, type="both")
est
summary(est, equation = "deltaTA")
plot(est, names= "deltaPIB")
summary(est, equation="deltaG")

ser11 <- serial.test(est, lags.pt = 16, type = "PT.adjusted")
ser11$serial
#p=1 résidus autocorrélés donc j'augmente p progressivement
#H0 étant pas d'autocorrélation

est <- VAR(Y,p=7, type="both")
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

#Je vais regarder si le problème c'est les données après 2009
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
#On détermine d'abord le nombre de retard
VARselect(Yres, lag.max = 8, type = "both") #On choisit 8 car on suppose que les variables du modèle ne peuvent
#avoir d'impact les unes sur les autres après deux années)
#Tous indiquent 1

Yres <- Yres[, c("deltaPIBres", "deltaGres", "deltaTAres")]
estres <- VAR(Yres,p=1, type="both")
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










#BROUILLON
#Brouillon
#Je vais essayer de regarder mes séries en le détrendant + avec des indicatrices
detrendPIB <- hpfilter(PIBlog,freq=1600)$cycle
detrendTA <- hpfilter(TAlog, freq=1600)$cycle
detrendG <- hpfilter(Glog, freq=1600)$cycle

adfTest(detrendPIB, type=c("nc"))
adfTest(detrendG, type=c("nc"))
adfTest(detrendTA, type=c("nc"))
#rejette existence d'une racine unitaire

Y2 <- data.frame(detrendPIB, detrendG, detrendTA)
#On détermine d'abord le nombre de retard
VARselect(Y2, lag.max = 8, type = "const") #On choisit 8 car on suppose que les variables du modèle ne peuvent
#avoir d'impact les unes sur les autres après deux années)
#Indiquent 2 ou 5

Y2 <- Y2[, c("detrendPIB", "detrendG", "detrendTA")]
est2 <- VAR(Y2,p=8, type="const")
est2
summary(est2, equation = "detrendPIB")
plot(est2, names= "detrendPIB")
#le fit est bien meilleur mais résidus sont autocorréles
summary(est2, equation="detrendG")

ser2 <- serial.test(est, lags.pt = 16, type = "PT.asymptotic")
ser2$serial

norm2 <- normality.test(est2)
norm2$jb.mul
#auto-corrélé et loins d'etre normaux

inds <- seq(as.Date("1980-01-01"), as.Date("2017-12-31"), by = "quarter")
dummy <- as.numeric(inds >= "2009-01-01" & inds<"2010-01-01")
dummy2 <-  as.numeric(inds >= "1992-02-01" & inds<="1993-04-01")
detrendPIB2 <- residuals(lm(PIBlog ~1 + inds + dummy))
detrendG2 <- residuals (lm(Glog ~ 1+ inds ))
detrendTA2 <- residuals (lm(TAlog ~ 1 + inds + dummy + dummy2))
#juste pour TA adf dnne 

Y3 <- data.frame(detrendPIB2, detrendG2, detrendTA2)
#On détermine d'abord le nombre de retard
VARselect(Y3, lag.max = 8, type = "const") #On choisit 8 car on suppose que les variables du modèle ne peuvent
#avoir d'impact les unes sur les autres après deux années)
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