library(tseries)
library(mFilter)      # for HP filter
library(seasonal)     # for seasonal component
library(xlsx)
library(xts)

#J'ai pas trouvé un déflateur du PIB trimestriel 
rawIPC <- read.csv("IPC-indice.csv", sep="," , dec=".")

IPC <- ts(rawIPC$Value , start =c(1980,1) , frequency =4) # indice des prix à la conso (deflateur)
plot(IPC)

rawPIBval <- read.xlsx("t_pib_val.xls", sheetName = "Niveaux") #PIB en valeur

PIBval <- ts(as.numeric(as.character(rawPIBval[8:284,2])), start=c(1949,1) , frequency=4)
PIB <- PIBval/IPC

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

#On définit les recettes publiques TA (cf papier Biau et Girard) comme la somme de G et de la capacité de financement
recettes <- ts(as.numeric(as.character(rawAPU[131:282,18])), start=c(1980,1) , frequency=4)
capfinancement <- ts(as.numeric(as.character(rawAPU[131:282,2])), start=c(1980,1) , frequency=4)
recettes <- capfinancement + depenses
TA <- recettes/IPC
#On n'a pas déduit l'impot sur les sociétés (pour le moment) contrairement au papier de Biau et Girard
#Normalement faisable avec comptabilité nationale item D51

#Je galère à transformer mes daily data en quaterly
rawtauxinteret <- read.xlsx("taux_interbancaire.xlsx",sheetName = "taux_interbancaire")

#Le code ne fonctionne plus à partir de la
tauxinteret <- ts(as.numeric(as.character(rawtauxinteret[6:7066,5])), start=c(1999,1,1) , frequency=365.25)
to.weekly(tauxinteret)

inds <- seq(as.Date("1999-01-01"), as.Date("2018-05-01"), by = "day")
taux <- ts(as.numeric(as.character(rawtauxinteret[6:7066,5])),  
           start = c(1999, as.numeric(format(inds[1], "%j"))),
           frequency = 365)
apply.quarterly(taux, FUN=mean)