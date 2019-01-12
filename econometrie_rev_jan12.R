#########################################################################################
# Projet Économétrie
# Sujet : Inégalités salariales H/F en France
# Données : Enquête en continu 2012 (EEC12), INSEE
# https://www.insee.fr/fr/statistiques/2415221?sommaire=2415511
# Dictionnaire des variables :
# https://www.insee.fr/fr/statistiques/fichier/2415221/contenu_eec12_indiv12.pdf
#########################################################################################

#########################################################################################
# Changelog : historique des modifications et ajouts
# Format à suivre :
# < Prenom en 3 lettres > < DD/MM > : < commentaire >
# Remarque : Sectionner le plus souvent le code pour faciliter la recherche de modifs.
# Remarque : Utiliser "###" pour dissimuler le nom.

# ### 16/11 : - Création du fichier
#             - Importations des données
#             - Préparation de la base complète
# ### 06/12 : - Sélection de variables
# ### 21/12 : - Mise à jour de la liste de variables retenues
#             - Création d'une sauvegarde de la base filtrée pour relancer plus rapidement
#             - Création des variables indicatrices (dummies)
# ### 02/01 : - Nouvelle variable : NIVP au lieu de DDIPL
#             - Non séparation des modalités de MARTRI : MARRIED WIDOW DIVORCED
# ### 03/01 : - Nettoyage de la base (gestion des NA et suppression de variables)
# ### 03/01 : - Régressions MCO
#             - Détection d'hétéroscédasticité dans les deux modèles
#             - Correction de White sur les deux modèles
# ### 04/01 : - Création des variables carrées : AGE_SQUARE, ANCENTR_SQUARE, NBHEUR_SQUARE
#             - Régressions MCO bis
#             - Correction de White bis
#             - Création de tableaux récapitulatifs des MCO
#             - Création de tableaux récapitulatifs des MCO (avec Correction de White)
#             - Création du tableau récapitulatif de la décomposition d'Oaxaca
#             - Mise en forme des tableaux : prêts à être exportés en fichiers .txt
# ### 08/01 : - Statistiques descriptives univariées
#             - Exportation des graphiques en pdf
# ### 09/01 : - Statistiques descriptives bivariées
#             - Identification de la forme de la relation SALHR_LN ~ quanti
# ### 10/01 : - Graphiques pyramidales pour la représentation des distributions par sexe
#             - Graphiques boxplot pour la représentation de la distribution selon les modalités
# ### 12/01 : - Calcul de la matrice des corrélations et la matrice des similarités
#########################################################################################
# Packages (Donner le but du package importé)
library(foreign) # Importation des fichiers dbf
library(formattable) # Formatter des sorties en pourcentage : fonction percent() # 21/12
library(lmtest) # tests sur la régression dont celui de Breusch-Pagan (bptest()) # 03/01
library(sandwich) # correction d'hétéroscédasticité : correction de White # 03/01
library(XML) # 10/01
library(reshape2) # 10/01
library(plyr) # 10/01
library(ggplot2) # visualisation graphique avancée # 10/01
library(corrplot) # représentation graphique de la matrice des corrélations # 12/01

#########################################################################################

#########################################################################################
# Répertoires de travail
setwd("/media/psqrt/Square2/projet_econometrie/")
setwd("/media/psqrt/SanDisk 8GB/0_SEMESTRE 7/Projets S7/Projet_économétrie/")
setwd("C:/Users/Mathilde/OneDrive/Documents/Master/Eco")
setwd("C:/Users/Utilisateur/Documents/cours/Master 1/Eco")
setwd("/home/champain_m/Documents/Eco")
#########################################################################################

#########################################################################################
# Importations des données
morceau1 = read.dbf("indiv121.dbf", as.is = TRUE) # partie 1 de la base
morceau2 = read.dbf("indiv122.dbf", as.is = TRUE) # partie 2
morceau3 = read.dbf("indiv123.dbf", as.is = TRUE)  # partie 3
# varlist et varmod ne sont pas utiles, se référer au pdf (dictionnaire)
# varlist = read.dbf("eec12_indiv12_dbase/varlist.dbf") # liste des variables
# varmod = read.dbf("eec12_indiv12_dbase/varmod.dbf") # indications variables
#########################################################################################

#########################################################################################
# Préparation de la base complète
base = merge(morceau1, morceau2, 
             by = c("IDENT", "NOI", "TRIM"))
base = merge(base, morceau3, 
             by = c("IDENT", "NOI", "TRIM"))
rm(morceau1, morceau2, morceau3)
# base doit contenir 422 133 observations et 555 variables 
# (549 variables + 3 variables d'identification + 3 variables de construction de sondage)
#########################################################################################
# Sélection de variables pertinentes

liste_vars = c("SEXE", "CSPIP", "MATRI", "TPP", "NAFG10N", "PAYNEU27", # maj 01/01 -c("DDIPL")
               "TAU10", "TUU", "LNAIS", "NBAGENF", "AGE", "ANCENTR", "ANCENTR4", 
               "STATUT", "SALRED", "IDENT",
               "CSER", "TPPRED", "EMPNBH", "HHC", "NBHEUR", "NUITC", "PUB3FP",
               "EFEN", "NFR", "REG", "CONTRA",  # maj 21/12 +c(...)
               "NIVP") # maj 01/01 +c("NIVP")

base = base[liste_vars]

save(base, file = "base_28vars.Rda") # sauvegarde de la base propre pour réutilisation
load("base_28vars.Rda") # pour démarrer directement sur la base propre à chaque session
#########################################################################################

# Bac à sable (GUY)

sapply(base, function(x) sum(is.na(x))) # nombre de NA par variable
percent(sapply(base, function(x) sum(is.na(x))/nrow(base)), digits = 3L) # pourcentage de NA par variable
                                                                         # nécessite le package formattable

#########################################################################################
# Création des variables indicatrices (dummies)

#### SEXE ####
unique(base$SEXE)
table(base$SEXE, useNA = "always")
# Rappel : 1 - Homme / 2 - Femme (CHAR type)

# Nombre de variables à créer : 1 (SEXE)
# Puisqu'on s'intéresse à la discrimination envers les femmes : les modalités sont :
# 0 - Homme / 1 - Femme
base$SEXE = as.integer(base$SEXE) - 1
unique(base$SEXE) # ok


#### CSPIP ####
# laissé de côté pour l'instant : 90.21% de NA et CSER plus pertinent

#### CSER ####
unique(base$CSER)
table(base$CSER, useNA = "always")
# Nombre de variables à construire : 5 (CSP_AGRI CSP_ARTI CSP_CADRE CSP_INTERM CSP_OUVRI)
# On sort CSP_EMPLOY car majoritaire
base$CSP_AGRI = 1*(base$CSER == "1")
base$CSP_ARTI = 1*(base$CSER == "2")
base$CSP_CADRE = 1*(base$CSER == "3")
base$CSP_INTERM = 1*(base$CSER == "4")
base$CSP_OUVRI = 1*(base$CSER == "6")


#### DDIPL #### # useless : voir NIVP (maj 01/01)
# laissé de côté pour l'instant : ils distinguent pas les BAC+3, BAC+5 et BAC+8 ...

#### NIVP #### # maj 01/01
unique(base$NIVP)
table(base$NIVP, useNA = "always")
# On a :
# 10 = BAC+5 et plus
# 20 = BAC+3
# 30 = BAC+2
# 40 + 41 = BAC
# 50 + 60 + 71 + 72 + 73 = Pas de bac : servira comme modalité référence
# Nombre de variables à construire : 4 (BAC5 BAC3 BAC2 BAC)
base$BAC5 = 1*(base$NIVP == "10")
base$BAC3 = 1*(base$NIVP == "20")
base$BAC2 = 1*(base$NIVP == "30")
base$BAC = 1*(base$NIVP == "40") + 1*(base$NIVP == "41")

#### MATRI #### (mis à jour 02/01)
unique(base$MATRI)
table(base$MATRI, useNA = "always")
# Rappel : 1 - célibataire 2 - marié ou remarié 3 - veuf 4 - divorcé

# Nombre de variables à créer : 1 (MARRIED)
# Interprétation économique : conséquende du fait d'être marrié ?
# 0 - Célibataire ou divorcé / 1 - Marié, remarié ou veuf
# base$MARRIED = 1*(base$MATRI == 2) + 1*(base$MATRI == 3)
    # maj 02/01
    # Nombre de variables à créer : 3 (MARRIED WIDOW DIVORCED)
base$MARRIED = 1*(base$MATRI == 2)
base$WIDOW = 1*(base$MATRI == 3)
base$DIVORCED = 1*(base$MATRI == 4)

#### TPP ####
unique(base$TPP)
table(base$TPP, useNA = "always")
unique(base$TPPRED)
table(base$TPPRED, useNA = "always")
# TPPRED m'a l'air plus pertinent : donc on lâche TPP

#### TPPRED ####
unique(base$TPPRED)
table(base$TPPRED, useNA = "always")
# Nombre de variables à créer : 1 (TPPRED)
# Modalités : 0 - Temps complet / 1 - Temps partiel
# Justification : être en temps partiel revient à sortir de la norme (effectif minoritaire)
base$TPPRED = as.integer(base$TPPRED) - 1
table(base$TPPRED, useNA = "always")

#### EMPNBH HHC NBHEUR ####
mean(base$EMPNBH, na.rm = TRUE) # raisonnable
mean(base$HHC, na.rm = TRUE) # raisonnable
mean(base$NBHEUR, na.rm = TRUE) # raisonnable
nrow(base[!is.na(base$SALRED) 
          & !is.na(base$NBHEUR) 
          & base$SALRED > 750,])
nrow(base[!is.na(base$SALRED) 
          & !is.na(base$HHC) 
          & base$HHC < 70 
          & base$SALRED > 750,])
nrow(base[!is.na(base$SALRED) 
          & !is.na(base$EMPNBH) 
          & base$EMPNBH < 70 
          & base$SALRED > 750,])
# Quelle variable choisir ?

#### NUITC ####
unique(base$NUITC)
table(base$NUITC, useNA = "always")
# Nombre de variables à créer : 1 (NUITC)
# Modalités : 0 - Jamais / 1 - Occasionnellement ou Habituellement
# Justification : conséquence d'être un travailleur de nuit sur le salaire
base$NUITC = 1*(base$NUITC == "1") + 1*(base$NUITC == "2")
table(base$NUITC, useNA = "always")

#### NAFG10N ####
# Nombre de variables à créer : 9 (on garde les mêmes noms, préfixés par SECT_ENT_)
# Modalités : 0 - Non / 1 - Oui
# Le secteur OQ est retiré parce qu'il est le plus grand en terme d'effectif
# On supprime les lignes avec NAFG10N == "00"
unique(base$NAFG10N)
table(base$NAFG10N, useNA = "always")
base$SECT_ENT_AZ = 1*(base$NAFG10N == "AZ")
base$SECT_ENT_BE = 1*(base$NAFG10N == "BE")
base$SECT_ENT_FZ = 1*(base$NAFG10N == "FZ")
base$SECT_ENT_GI = 1*(base$NAFG10N == "GI")
base$SECT_ENT_JZ = 1*(base$NAFG10N == "JZ")
base$SECT_ENT_KZ = 1*(base$NAFG10N == "KZ")
base$SECT_ENT_LZ = 1*(base$NAFG10N == "LZ")
base$SECT_ENT_MN = 1*(base$NAFG10N == "MN")
base$SECT_ENT_RU = 1*(base$NAFG10N == "RU")

#### PUB3FP ####
# Nombre de variables à créer : 1 (PUBLIC)
# Modalités : 0 - Privé / 1 - Public
# Justification : le secteur privé est majoritaire (donc public hors norme)
# Construction : 0 = "4" et 1 = "1" + "2" + "3"
unique(base$PUB3FP)
table(base$PUB3FP, useNA = "always")
base$PUBLIC = 1*(base$PUB3FP == "1") + 1*(base$PUB3FP == "2") + 1*(base$PUB3FP == "3")
table(base$PUBLIC, useNA = "always")

#### EFEN ####
range(base$EFEN, na.rm = T) # raisonnable
mean(base$EFEN, na.rm = T) # raisonnable
median(base$EFEN, na.rm = T) # raisonnable

#### PAYNEU27 ####
unique(base$PAYNEU27)
table(base$PAYNEU27, useNA = "always")
# Nombre de variables à créer : 1 (PAYSNEU27)
# Modalités : 0 - Oui / 1 - Non
# Justification : conséquence de ne pas être issu de l'UE27 sur le salaire
# Construction : 0 = "1" et 1 = "0"
base$PAYNEU27 = abs(as.integer(base$PAYNEU27) - 1) # bricolo
table(base$PAYNEU27, useNA = "always")

#### LNAIS ####
unique(base$LNAIS)
table(base$LNAIS, useNA = "always")
# Nombre de variables à créer : 1 (LNAIS)
# Modalités : 0 - France / 1 - À l'étranger
# Justification : conséquence de ne pas être issu de la France sur le salaire
# Construction : 0 = "1" et 1 = "2"
base$LNAIS = as.integer(base$LNAIS) - 1
table(base$LNAIS, useNA = "always")

#### NFR ####
unique(base$NFR)
table(base$NFR, useNA = "always")
# Nombre de variables à créer : 1 (NFR)
# Modalités : 0 - Français / 1 - Étranger
# Justification : conséquence d'être un étranger pour le salaire
# Construction : 0 = "1" + "2" et 1 = "3"
base$NFR = 1*(base$NFR == "3")
table(base$NFR, useNA = "always")

#### TAU10 ####
unique(base$TAU10)
table(base$TAU10, useNA = "always")
# Quel découpage ?

#### TUU ####
unique(base$TUU)
table(base$TUU, useNA = "always")
# Nombre de variables à créer : 1 (TUU)
# Modalités : 0 - Commune / 1 - Commune urbaine
# Construction : 0 = "0" et 1 = "1"
base$TUU = as.integer(base$TUU)

#### REG ####
unique(base$REG)
table(base$REG, useNA = "always")
# Nombre de variables à créer : 1 (IDF)
# Modalités : 0 - Hors IdF / 1 - IdF
# Justification : conséquence de résider en IdF sur le salaire ?
# Construction : 0 = "21" + ... + "94 et 1 = "11"
base$IDF = 1*(base$REG == "11")
table(base$IDF, useNA = "always")

#### NBAGENF ####
unique(base$NBAGENF)
table(base$NBAGENF, useNA = "always")
# Nombre de variables à créer : 3 (NBENF1 NBENF2 NBENF3PLUS)
# Modalités : 0 - ne vérifie pas le nombre / 1 - vérifie le nombre
# Justification : on a retiré la modalité 0 enfants parce qu'on s'en sert comme référence
# Construction : NBENF1 : 0 = "0" + "4" + ... + "9" /           1 = "1" + "2" + "3"
#                NBENF2 : 0 = "0" + ... + "3" + "7" + ... "9" / 1 = "4" + "5" + "6"
#                NBENF3 : 0 = "0" + ... + "6" /                 1 = "7" + "8" + "9"
base$NBENF1 = 1*(base$NBAGENF == "1") + 1*(base$NBAGENF == "2") + 1*(base$NBAGENF == "3")
base$NBENF2 = 1*(base$NBAGENF == "4") + 1*(base$NBAGENF == "5") + 1*(base$NBAGENF == "6")
base$NBENF3PLUS = 1*(base$NBAGENF == "7") + 1*(base$NBAGENF == "8") + 1*(base$NBAGENF == "9")

#### AGE ####
base$AGE = as.integer(base$AGE)
mean(base$AGE, na.rm = TRUE) # raisonnable
median(base$AGE, na.rm = TRUE) # raisonnable
range(base$AGE, na.rm = TRUE) # ok
sum(base$AGE > 90) # que faire ?
sum(base$AGE == 99) # que faire ? Rappel : modalité 99 = 99 et plus

#### ANCENTR ####
sum(is.na(base$ANCENTR))
mean(base$ANCENTR, na.rm = TRUE) # raisonnable (en mois)
median(base$ANCENTR, na.rm = TRUE) # raisonnable
range(base$ANCENTR, na.rm = TRUE) # ok

#### ANCENTR4 ####
sum(is.na(base$ANCENTR4))
unique(base$ANCENTR4)
table(base$ANCENTR4, useNA = "always")
sum(is.na(base$ANCENTR4)) == sum(is.na(base$ANCENTR))
# il est possible que ANCENTR4 soit directement construit à partir de ANCENTR
# il vaudrait mieux garder ANCENTR pour sa précision.
# et puis il n'y a pas de fracture du salaire lorsqu'on passe à 5 ou 10 ans d'ancienneté

#### STATUT ####
# nomenclature trop compliquée à mon goût : voir CONTRA

#### CONTRA ####
unique(base$CONTRA)
table(base$CONTRA, useNA = "always")
# Nombre de variables à construire : 2 (CDD, AutreCDD)
# Modalités : CDD : 0 - Non CDD / 1 - CDD
#             AutreCDD : 0 - Non CDD special / 1 - saisonnier, interim, apprentissage, alternance
# CDI en modalité retirée parce qu'elle est majoritaire
base$CDD = 1*(base$CONTRA == "2")
base$AutreCDD = 1*(base$CONTRA == "3") + 1*(base$CONTRA == "4") + 1*(base$CONTRA == "5")
table(base$CDD, useNA = "always")
table(base$AutreCDD, useNA = "always")

#### Conclusion ####
# Variables gardées :
# SEXE, SALRED, PUBLIC, EFEN, PAYNEU27, LNAIS, NFR, TAU10, TUU, IDF, NBENF1, NBENF2,
# NBENF3PLUS, AGE, ANCENTR, CDD, AutreCDD, SECT_ENT_AZ, SECT_ENT_BE, SECT_ENT_FZ,
# SECT_ENT_GI, SECT_ENT_JZ, SECT_ENT_KZ, SECT_ENT_LZ, SECT_ENT_MN, SECT_ENT_RU,
# CSPIP, DDIPL, MARRIED, TPPRED, IDENT, CSP_AGRI, CSP_ARTI, CSP_CADRE, CSP_INTERM,
# CSP_OUVRI, NBHEUR, NUITC
# Variables supprimées :
# PUB3FP, REG, NBAGENF, ANCENTR4, STATUT, CONTRA, MATRI, TPP, NAFG10N, EMPNBH, HHC, CSER


var_keep = c("SEXE", "SALRED", "PUBLIC", "EFEN", "PAYNEU27", "LNAIS", "NFR", "TAU10", 
             "TUU", "IDF", "NBENF1", "NBENF2", "NBENF3PLUS", "AGE", "ANCENTR", "CDD", 
             "AutreCDD", "SECT_ENT_AZ", "SECT_ENT_BE", "SECT_ENT_FZ", "SECT_ENT_GI", 
             "SECT_ENT_JZ", "SECT_ENT_KZ", "SECT_ENT_LZ", "SECT_ENT_MN", "SECT_ENT_RU",
             "CSPIP", "MARRIED", "TPPRED", "IDENT", "CSP_AGRI", "CSP_ARTI", # "DDIPL" # maj 01/01 
             "CSP_CADRE", "CSP_INTERM", "CSP_OUVRI", "NBHEUR", "NUITC",
             "BAC5", "BAC3", "BAC2", "BAC",# maj 01/01
             "WIDOW", "DIVORCED",  # maj 02/01
             "CSER", "CONTRA") # maj 03/01

var_drop = c("PUB3FP", "REG", "NBAGENF", "ANCENTR4", "STATUT", "MATRI", 
             "TPP", "NAFG10N", "EMPNBH", "HHC", # maj 03/01 -c("CSER", "CONTRA")
             "NIVP") # maj 01/01

colnames(base) %in% c(var_keep, var_drop) #ok

# Variables à retraiter :
# "CSPIP",                      trop de valeurs manquantes ? # maj 01/01 : on jette CSPIP (utiliser CSER)
# "EMPNBH", "HHC", "NBHEUR",    quelle variable choisir ? # maj 01/01 : on garde NBHEUR
# "TAU10",                      quel découpage ?
# "AGE", "SALRED",              fixer des limites ?

#### Préparation de la nouvelle base filtrée ####
base = base[, var_keep]
save(base, file = "base_45vars.Rda") # sauvegarde de la base propre pour réutilisation # maj 01/01 38 à 41 # maj 02/02 41 à 43 # maj 03/01 43 à 45
load("base_45vars.Rda") # pour démarrer directement sur la base propre à chaque session # maj 01/01 38 à 41 # maj 02/02 41 à 43 # maj 03/01 43 à 45

# fin création de variables indicatrices (dummies)
#########################################################################################

# Mathilde 03/01 : modification de la base

# aperçu de la base
sapply(base, function(x) sum(is.na(x))) # nombre de NA par variable
percent(sapply(base, function(x) sum(is.na(x))/nrow(base)), digits = 3L) # pourcentage de NA par variable
                                                                         # nécessite le package formattable

base <- base[complete.cases(base$SALRED),]
# Nous supprimons les individus n'ayant pas de salaire renseigné
base <- base[complete.cases(base$NBHEUR),]
# Nous supprimons les individus n'ayant pas de volume horaire renseigné

base <- subset(base, (AGE > 17) & (AGE < 71))
# Je supprime les individus dont l'âge est inférieur à 18 ans ou supérieur à 70 ans.

base <- subset(base, (CSER != 8) | (CSER != 0)) # maj 05/01
# Les individus n'ayant jamais travaillé sont exclus (CSER = 8)

base <- subset(base, NBHEUR < 260) # 60 * 4.345 = volume mensuel
# Nous supprimons les individus ayant un volume horaire de travail de 260 heures ou plus, que nous considérons comme aberrant

# base$SALHR <- base$SALRED / (base$EMPNBH * 4.345)
# Création d'une variable SALHR avec le salaire par taux horaire, càd le salaire mensuel / nombre d'heures par semaine * 4.345 (nb de semaines par mois)
# voir en bas

# Les salaires en dessous de 600€ sont considérés aberrants et retirés, de même que ceux supérieurs à 20000€
base <- subset(base, (SALRED > 600) & (SALRED < 20000))
summary(base)

base <- base[complete.cases(base$CONTRA), ]
base <- base[complete.cases(base$ANCENTR), ]

# pour le salaire horaire, on prendra SALRED/NBHEUR c'est deux variables liées entre elles alors que EMPNBH est dissociée
base$SALHR = base$SALRED / base$NBHEUR

sapply(base, function(x) sum(is.na(x))) # nombre de NA par variable
percent(sapply(base, function(x) sum(is.na(x))/nrow(base)), digits = 3L) # pourcentage de NA par variable
                                                                         # nécessite le package formattable
# on laisse tomber CSPIP, EFEN et TAU10
base = subset(base, select = -c(CSPIP))
base = subset(base, select = -c(EFEN))
base = subset(base, select = -c(TAU10))

save(base, file = "base_finale.Rda") # 30399 obs, 0 NA et 43 variables
load("base_finale.Rda") # 

#########################################################################################

# Réarrangement de l'ordre des variables pour faciliter l'exportation des résultats vers le rapport

    # on drop ces trois variables car inutiles (elles servaient uniquement à filtrer les lignes retenues)
base = subset(base, select = -c(CONTRA))
base = subset(base, select = -c(CSER))
base = subset(base, select = -c(IDENT))
    # création de la variable dépendante (expliquée) SALHR_LN
base$SALHR_LN = log(base$SALHR)

    # création des variables carrées :
base$AGE_SQUARE = base$AGE ** 2
base$ANCENTR_SQUARE = base$ANCENTR ** 2
base$NBHEUR_SQUARE = base$NBHEUR ** 2

save(base, file = "base_finale2.Rda") # 30399 obs, 0 NA et 44 variables
load("base_finale2.Rda") # 

ordre_variables = c("SALHR_LN", # variable expliquée
                    # variables inutiles
                    "SEXE",
                    "SALRED",
                    "SALHR",
                    # variables diplome
                    "BAC",
                    "BAC2",
                    "BAC3",
                    "BAC5",
                    # variables age
                    "AGE", # potentiellement AGE**2 à ajouter
                    "AGE_SQUARE",
                    # variables situation géographique
                    "NFR",
                    "LNAIS",
                    "PAYNEU27",
                    "TUU",
                    "IDF",
                    # variables maritales
                    "MARRIED",
                    "DIVORCED",
                    "WIDOW",
                    # variables enfants
                    "NBENF1",
                    "NBENF2",
                    "NBENF3PLUS",
                    # variables sur l'entreprise et la charge de travail
                    "PUBLIC",
                    "NBHEUR", # potentiellement NBHEUR**2 à ajouter
                    "NBHEUR_SQUARE",
                    "TPPRED",
                    "NUITC",
                    "ANCENTR", # potentiellement ANCENTR**2 à ajouter
                    "ANCENTR_SQUARE",
                    "CDD",
                    "AutreCDD",
                    # variables CSP
                    "CSP_AGRI",
                    "CSP_ARTI",
                    "CSP_CADRE",
                    "CSP_INTERM",
                    "CSP_OUVRI",
                    # variables secteur entreprise
                    "SECT_ENT_AZ",
                    "SECT_ENT_BE",
                    "SECT_ENT_FZ",
                    "SECT_ENT_GI",
                    "SECT_ENT_JZ",
                    "SECT_ENT_KZ",
                    "SECT_ENT_LZ",
                    "SECT_ENT_MN",
                    "SECT_ENT_RU"
                    )

# Réarrangement des colonnes
base = base[, ordre_variables]

save(base, file = "base_finale3.Rda") # 30399 obs, 0 NA et 44 variables
load("base_finale3.Rda") # 

# Préparation des échantillons HOMMES FEMMES

base_hommes = subset(base, SEXE == 0)
base_femmes = subset(base, SEXE == 1)

save(base_hommes, base_femmes, file = "bases_separees.Rda")
load("bases_separees.Rda")

#########################################################################################

# MCO initiale sur les hommes

MCO_hommes = lm(SALHR_LN ~ . - SALRED - SALHR - SEXE - NBENF1 - NBENF2 - NBENF3PLUS, data = base_hommes)
summary(MCO_hommes)

# MCO initiale sur les femmes

MCO_femmes = lm(SALHR_LN ~ . - SALRED - SALHR - SEXE, data = base_femmes)
summary(MCO_femmes)

# Pour les deux régressions, CSP_AGRI et CSP_ARTI ne sont pas pris en compte :
sum(base_hommes$CSP_ARTI)
sum(base_hommes$CSP_AGRI)
sum(base_femmes$CSP_ARTI)
sum(base_femmes$CSP_AGRI)
# Dans ce qui nous reste, il y a ni artisans ni agriculteurs
# On va retirer ces deux variables de l'analyse par souci de propreté


# 2eme MCO sur les hommes

MCO_hommes = lm(SALHR_LN ~ . - SALRED - SALHR - SEXE - CSP_AGRI - CSP_ARTI - NBENF1 - NBENF2 - NBENF3PLUS,
                data = base_hommes)
summary(MCO_hommes)

# 2eme MCO sur les femmes

MCO_femmes = lm(SALHR_LN ~ . - SALRED - SALHR - SEXE - CSP_AGRI - CSP_ARTI,
                data = base_femmes)
summary(MCO_femmes)

# Les deux modèles ont un bon pouvoir explicatif (~+50%) et avec peu de variables non significatives


#########################################################################################

# Hétéroscédasticité ?

bptest(MCO_hommes)
# p-value inférieure à 0.05, il y a hétéroscédasticité chez les hommes

bptest(MCO_femmes)
# p-value inférieure à 0.05, il y a hétéroscédasticité chez les femmes

# vcov(MCO_hommes) ## matrice de variance-cov MCO
# vcovHC(MCO_hommes) ## matrice de variance-cov avec la correction de white
# coeftest(MCO_hommes, vcov = vcovHC) # t-values avec une matrice VCOV corrigée de White
# summary(MCO_hommes) # pour comparer avec les t-values sans correction

# Méthode utilisée par la prof, (TP2 - diapo 23)

#########################################################################################

# association variable-label des variables utilisées

# SAUVEGARDE 05/01 (Labels trop longs pour le rapport : réduction)
# double_variables = c("(Intercept) : Constante du modèle",
#                     # variables diplome
#                     "BAC : Possession d'un baccalauréat sans plus",
#                     "BAC2 : Possession d'un BAC+2 sans plus",
#                     "BAC3 : Possession d'un BAC+3 sans plus",
#                     "BAC5 : Possession d'un BAC+5 sans plus",
#                     # variables age
#                     "AGE : Age de l'individu", # potentiellement AGE**2 à ajouter
#                     "AGE_SQUARE : Age au carré de l'individu",
#                     # variables situation géographique
#                     "NFR : ... est de nationalité étrangère (NFR = 1)",
#                     "LNAIS : ... est né(e) à l'étranger (LNAIS = 1)",
#                     "PAYNEU27 : ... est né(e) en dehors d'un pays de l'UE27 (PAYNEU27 = 1)",
#                     "TUU : ... vit en commune urbaine (TUU = 1)",
#                     "IDF : ... vit en Ile-de-France (IDF = 1)",
#                     # variables maritales
#                     "MARRIED : ... est marié(e) (MARRIED = 1)",
#                     "DIVORCED : ... est divorcé(e) (DIVORCED = 1)",
#                     "WIDOW : ... est veuf(ve) (WIDOW = 1)",
#                     # variables enfants
#                     "NBENF1 : ... a exactement 1 enfant (NBENF1 = 1)",
#                     "NBENF2 : ... a exactement 2 enfants (NBENF2 = 1)",
#                     "NBENF3PLUS : ... a 3 enfants ou plus (NBENF3PLUS = 1)",
#                     # variables sur l'entreprise et la charge de travail
#                     "PUBLIC : entreprise publique (PUBLIC = 1)",
#                     "NBHEUR : nombre d'heures de travail par mois", # potentiellement NBHEUR**2 à ajouter
#                     "NBHEUR_SQUARE : nombre d'heures de travail par mois au carré",
#                     "TPPRED : ... travaille en temps partiel (TPPRED = 1)",
#                     "NUITC : ... travaille de nuit (NUITC = 1)",
#                     "ANCENTR : ancienneté en mois dans l'entreprise actuelle", # potentiellement ANCENTR**2 à ajouter
#                     "ANCENTR_SQUARE : ancienneté en mois au carré dans l'entreprise actuelle",
#                     "CDD : contrat de type CDD (CDD = 1)",
#                     "AutreCDD : contrat de type autre que CDD hors CDI (AutreCDD = 1)",
#                     # variables CSP
#                     "CSP_AGRI : Agriculteur",
#                     "CSP_ARTI : Artisan",
#                     "CSP_CADRE : Cadre",
#                     "CSP_INTERM : Profession intermédiaire",
#                     "CSP_OUVRI : Ouvrier",
#                     # variables secteur entreprise
#                     "SECT_ENT_AZ : Agriculture, syviculture et pêche",
#                     "SECT_ENT_BE : Industrie manufacturière, industries extractives et autres",
#                     "SECT_ENT_FZ : Construction",
#                     "SECT_ENT_GI : Commerce de gros et de détail, transports, hébergement et restauration",
#                     "SECT_ENT_JZ : Information et communication",
#                     "SECT_ENT_KZ : Activités financières et d'assurance",
#                     "SECT_ENT_LZ : Activités immobilières",
#                     "SECT_ENT_MN : Activités spécialisées, scientifiques, techniques et services administratifs",
#                     "SECT_ENT_RU : Autres activités de services"
#                     )

double_variables = c("(Intercept) : Constante du modèle",
                    # variables diplome
                    "BAC : Possession d'un baccalauréat sans plus",
                    "BAC2 : Possession d'un BAC+2 sans plus",
                    "BAC3 : Possession d'un BAC+3 sans plus",
                    "BAC5 : Possession d'un BAC+5 sans plus",
                    # variables age
                    "AGE : Age de l'individu", # potentiellement AGE**2 à ajouter
                    "AGE_SQUARE : Age au carré de l'individu",
                    # variables situation géographique
                    "NFR : ... est de nationalité étrangère",
                    "LNAIS : ... est né(e) à l'étranger",
                    "PAYNEU27 : ... est né(e) en dehors d'un pays de l'UE27",
                    "TUU : ... vit en commune urbaine",
                    "IDF : ... vit en Ile-de-France",
                    # variables maritales
                    "MARRIED : ... est marié(e)",
                    "DIVORCED : ... est divorcé(e)",
                    "WIDOW : ... est veuf(ve)",
                    # variables enfants
                    "NBENF1 : ... a exactement 1 enfant",
                    "NBENF2 : ... a exactement 2 enfants",
                    "NBENF3PLUS : ... a 3 enfants ou plus",
                    # variables sur l'entreprise et la charge de travail
                    "PUBLIC : entreprise publique",
                    "NBHEUR : nombre d'heures de travail par mois", # potentiellement NBHEUR**2 à ajouter
                    "NBHEUR_SQUARE : NBHEUR au carré",
                    "TPPRED : ... travaille en temps partiel",
                    "NUITC : ... travaille de nuit",
                    "ANCENTR : ancienneté en mois dans l'entreprise", # potentiellement ANCENTR**2 à ajouter
                    "ANCENTR_SQUARE : ANCENTR au carré",
                    "CDD : contrat de type CDD",
                    "AutreCDD : contrat de type autre que CDD hors CDI",
                    # variables CSP
                    "CSP_AGRI : Agriculteur",
                    "CSP_ARTI : Artisan",
                    "CSP_CADRE : Cadre",
                    "CSP_INTERM : Profession intermédiaire",
                    "CSP_OUVRI : Ouvrier",
                    # variables secteur entreprise
                    "SECT_ENT_AZ : Agriculture, syviculture et pêche",
                    "SECT_ENT_BE : Industrie manufacturière, industries extractives et autres",
                    "SECT_ENT_FZ : Construction",
                    "SECT_ENT_GI : Commerce de gros et de détail, transports, hébergement et restauration",
                    "SECT_ENT_JZ : Information et communication",
                    "SECT_ENT_KZ : Activités financières et d'assurance",
                    "SECT_ENT_LZ : Activités immobilières",
                    "SECT_ENT_MN : Activités spécialisées, scientifiques, techniques et services administratifs",
                    "SECT_ENT_RU : Autres activités de services"
                    )

# Sélection des labels

label_variables = c(rep(0, length(double_variables)))
nom_variables = c(rep(0, length(double_variables)))
for (iterateur in seq(1, length(double_variables))){
    nom_variables[iterateur] = strsplit(double_variables, " : ")[[iterateur]][1]
    label_variables[iterateur] = strsplit(double_variables, " : ")[[iterateur]][2]
}
nom_variables
label_variables

# Variables + Labels dans une même DF

info_variables = data.frame(variable = nom_variables,
                            label = label_variables)

# petite exportation pour le rapport (annexe)
write.table(info_variables,
            file = "./sorties/info_variables.txt",
            quote = F,
            sep = "&",
            na = "--",
            row.names = F,
            fileEncoding = "UTF-8")
#########################################################################################

# Définition d'une fonction créatrice de tableau récapitulatif (REGRESSIONS)

Recap_coefficients = function(df_resultats, matrice_vcov){
    resultat = coeftest(df_resultats, vcov = matrice_vcov)
    resultat = data.frame(unclass(resultat))
    resultat$variable = rownames(resultat)
    rownames(resultat) = c()
    resultat = merge(resultat, info_variables,
                     by = c("variable"),
                     all = TRUE,
                     sort = FALSE)
    resultat = resultat[c(1, 6, 2, 3, 4, 5)]
    resultat$signif = symnum(resultat$Pr...t..,
                             corr = FALSE,
                             na = FALSE,
                             cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                             symbols = c("***", "**", "*", ".", " "))
    resultat = resultat[match(info_variables$variable, resultat$variable), ]
    return(resultat)
}

resultats_mco_hommes = Recap_coefficients(MCO_hommes, NULL)
resultats_mco_femmes = Recap_coefficients(MCO_femmes, NULL)
resultats_white_hommes = Recap_coefficients(MCO_hommes, vcovHC)
resultats_white_femmes = Recap_coefficients(MCO_femmes, vcovHC)

#########################################################################################

# BAC À SABLE (GUY # 04/01 : tentative réduction résidu chez les femmes)

# load("bases_separees.Rda")
# 
# MCO_hommes2 = lm(SALHR_LN ~ . - SALRED - SALHR - SEXE - CSP_AGRI - CSP_ARTI - NBENF1 - NBENF2 - NBENF3PLUS,
#                 data = base_hommes)
# summary(MCO_hommes2)
# coeftest(MCO_hommes2, vcov = vcovHC)
# 
# # 2eme MCO sur les femmes
# 
# MCO_femmes2 = lm(SALHR_LN ~ . - SALRED - SALHR - SEXE - CSP_AGRI - CSP_ARTI,
#                 data = base_femmes)
# summary(MCO_femmes2)
# coeftest(MCO_femmes2, vcov = vcovHC)
# 
# resultats_mco_hommes = Recap_coefficients(MCO_hommes2, NULL)
# resultats_mco_femmes = Recap_coefficients(MCO_femmes2, NULL)
# resultats_white_hommes = Recap_coefficients(MCO_hommes2, vcovHC)
# resultats_white_femmes = Recap_coefficients(MCO_femmes2, vcovHC)

#########################################################################################

# Création du tableau récapitulatif (DÉCOMPOSITIONS)

    # vidage des colonnes CSP_AGRI et CSP_ARTI pour les deux échantillons
base_hommes[, c("CSP_AGRI", "CSP_ARTI")] = NA
base_femmes[, c("CSP_AGRI", "CSP_ARTI")] = NA
moy_hommes = data.frame(moyennes_hommes = colMeans(base_hommes))
moy_hommes$variable = rownames(moy_hommes)
rownames(moy_hommes) = c()
recap_hommes = merge(resultats_white_hommes[c(1, 2, 3)], moy_hommes,
                     by = c("variable"),
                     sort = FALSE)
colnames(recap_hommes)[3] = "beta_hommes"

moy_femmes = data.frame(moyennes_femmes = colMeans(base_femmes))
moy_femmes$variable = rownames(moy_femmes)
rownames(moy_femmes) = c()
recap_femmes = merge(resultats_white_femmes[c(1, 2, 3)], moy_femmes,
                     by = c("variable"),
                     sort = FALSE)
colnames(recap_femmes)[3] = "beta_femmes"

recap_oaxaca = merge(recap_hommes, recap_femmes,
                     by = c("variable", "label"),
                     sort = FALSE)
recap_oaxaca$diff_femmes = (recap_oaxaca$moyennes_hommes - recap_oaxaca$moyennes_femmes) * recap_oaxaca$beta_femmes
recap_oaxaca$diff_hommes = (recap_oaxaca$moyennes_hommes - recap_oaxaca$moyennes_femmes) * recap_oaxaca$beta_hommes

ecart = mean(base_hommes$SALHR_LN) - mean(base_femmes$SALHR_LN)

recap_oaxaca$pct_femmes = - recap_oaxaca$diff_femmes / ecart * 100
recap_oaxaca$pct_hommes = - recap_oaxaca$diff_hommes / ecart * 100

first = data.frame("SALHR_LN_DIFF", "Écart moyen des SALHR_LN entre hommes et femmes", 
          NA, ecart, 
          NA, ecart,
          NA, NA, 100, 100)
colnames(first) = colnames(recap_oaxaca)

recap = rbind(first, recap_oaxaca)

last = data.frame("Total", "Résidu après avoir ôté l'effet des variables",
                  NA, NA, NA, NA, 
                  ecart - sum(recap_oaxaca$diff_femmes[2:length(recap_oaxaca$diff_femmes)], na.rm = T),
                  ecart - sum(recap_oaxaca$diff_hommes[2:length(recap_oaxaca$diff_hommes)], na.rm = T),
                  100 + sum(recap_oaxaca$pct_femmes[2:length(recap_oaxaca$pct_femmes)], na.rm = T),
                  100 + sum(recap_oaxaca$pct_hommes[2:length(recap_oaxaca$pct_hommes)], na.rm = T))
colnames(last) = colnames(recap_oaxaca)

recap = rbind(recap, last)



#########################################################################################

# Création des tableaux à exporter vers le rapport

r_recap = recap
r_recap[, c(3:8)] = round(recap[, c(3:8)], 4)
r_recap[, c(9:10)] = round(recap[, c(9:10)], 2)

r_resultats_mco_femmes = resultats_mco_femmes
r_resultats_mco_hommes = resultats_mco_hommes
r_resultats_white_femmes = resultats_white_femmes
r_resultats_white_hommes = resultats_white_hommes
r_resultats_mco_femmes[, c(3:6)] = round(resultats_mco_femmes[, c(3:6)], 4)
r_resultats_mco_hommes[, c(3:6)] = round(resultats_mco_hommes[, c(3:6)], 4)
r_resultats_white_femmes[, c(3:6)] = round(resultats_white_femmes[, c(3:6)], 4)
r_resultats_white_hommes[, c(3:6)] = round(resultats_white_hommes[, c(3:6)], 4)

colnames(r_resultats_mco_femmes)[3:6] = c("Estimation", "Erreur-type", "t-stat", "p-value")
colnames(r_resultats_mco_hommes)[3:6] = c("Estimation", "Erreur-type", "t-stat", "p-value")
colnames(r_resultats_white_femmes)[3:6] = c("Estimation", "Erreur-type", "t-stat", "p-value")
colnames(r_resultats_white_hommes)[3:6] = c("Estimation", "Erreur-type", "t-stat", "p-value")

# Exportations en fichiers .txt

write.table(r_recap,
            file = "./sorties/r_recap.txt",
            quote = F,
            sep = "&",
            na = "--",
            row.names = F,
            fileEncoding = "UTF-8")
write.table(r_recap[c(1, 3:10)],
            file = "./sorties/r_recap_simple.txt",
            quote = F,
            sep = "&",
            na = "--",
            row.names = F,
            fileEncoding = "UTF-8")
write.table(r_resultats_mco_femmes,
            file = "./sorties/r_resultats_mco_femmes.txt",
            quote = F,
            sep = "&",
            na = "--",
            row.names = F,
            fileEncoding = "UTF-8")
write.table(r_resultats_mco_hommes,
            file = "./sorties/r_resultats_mco_hommes.txt",
            quote = F,
            sep = "&",
            na = "--",
            row.names = F,
            fileEncoding = "UTF-8")
write.table(r_resultats_white_femmes,
            file = "./sorties/r_resultats_white_femmes.txt",
            quote = F,
            sep = "&",
            na = "--",
            row.names = F,
            fileEncoding = "UTF-8")
write.table(r_resultats_white_hommes,
            file = "./sorties/r_resultats_white_hommes.txt",
            quote = F,
            sep = "&",
            na = "--",
            row.names = F,
            fileEncoding = "UTF-8")

#########################################################################################

# Petites statistiques pouvant servir dans le rapport :

mean(base_femmes$SALRED) # salaire moyen des femmes
mean(base_hommes$SALRED) # salaire moyen des hommes 
mean(base_femmes$SALRED) - mean(base_hommes$SALRED) # écart des salaires moyens
summary(MCO_femmes) # R^2, F-stat et p-value du test de Fishier
summary(MCO_hommes) # R^2, F-stat et p-value du test de Fishier
nrow(base_femmes) # n_femmes
nrow(base_hommes) # n_hommes

#########################################################################################

# Statistiques descriptives (part 1 : univariées)

load("base_28vars.Rda")
base_stat <- base
load("base_finale2.Rda")

#######################################################
# Mathilde 05/01 et 07/01 : statistiques descriptives #
#######################################################

### STATISTIQUES DESCRIPTIVES UNIAVRIEES

##Représentation des variables quantitatives

pdf(file = "./sorties/graphiques_stats_des_univ.pdf")
#Représentation de l'âge
hist(base$AGE, 
     nclass = 10, 
     prob = FALSE, # modif GUY : voir les effectifs c'est plus parlant que des 0.030 0.040
     col = "cornflowerblue", 
     border = "white",
     xlim = c(min(base$AGE), max(base$AGE)), 
     main = NA, 
     xlab = "Âge (années)", 
     ylab = "Effectif") # Effectif au lieu de Densité
# lines(density(base$AGE, na.rm=TRUE), lwd=2, col="lightblue")

#Représentation du salaire
# hist(base$SALRED, nclass=100, prob=TRUE, col="cornflowerblue", border="white",
#      xlim=c(0,20000), main="", xlab="Salaire mensuel (Euros)", ylab="Densité")
# lines(density(base$SALRED, na.rm=TRUE), lwd=2, col="lightblue")
# text(10000, 0.00025, paste("N =", sum(complete.cases(base$SALRED))), cex=1.2)

#Représentation du salaire avec SALRED<5000
base2 <- subset(base, (base$SALRED < 5000))
hist(base2$SALRED, 
     nclass = 20, 
     prob = FALSE, # T to F 
     col = "cornflowerblue", 
     border = "white",
     xlim = c(min(base2$SALRED), max(base2$SALRED)), 
     main=NA, 
     xlab="Salaire mensuel (Euros)", 
     ylab="Effectif") # Densité to Effectif
# lines(density(base2$SALRED, na.rm=TRUE), lwd=2, col="lightblue")
text(x = 4500, 
     y = 1500, 
     paste("N =", sum(complete.cases(base2$SALRED))), 
     cex = 1.2)

#Représentation de l'ancienneté en mois (variable brute)
hist(base$ANCENTR, 
     nclass = 25, 
     prob = FALSE, 
     col = "cornflowerblue", 
     border = "white",
     xlim = c(min(base$ANCENTR), max(base$ANCENTR)), 
     main = NA, 
     xlab = "Ancienneté (mois)", 
     ylab = "Effectif")
# lines(density(base$ANCENTR, na.rm=TRUE), lwd=2, col="lightblue")

#Représentation de l'ancienneté en années
hist((base$ANCENTR/12), 
     nclass = 30, 
     prob = FALSE, 
     col = "cornflowerblue", 
     border = "white",
     xlim = c(min(base$ANCENTR/12), max(base$ANCENTR/12)), 
     main = NA, 
     xlab = "Ancienneté (années)", 
     ylab = "Effectif")
# lines(density((base$ANCENTR/12), na.rm=TRUE), lwd=2, col="lightblue")


#Représentation du nombre d'heures par semaine
hist(base$NBHEUR, 
     nclass = 10, 
     prob = FALSE, 
     col = "cornflowerblue", 
     border = "white",
     xlim = c(min(base$NBHEUR), max(base$NBHEUR)), 
     main = NA, 
     xlab = "nombre d'heures travaillées (par mois)", 
     ylab = "Effectif")

#Représentation du salaire hebdomadaire
hist((base$NBHEUR/4.345), 
     nclass = 25, 
     prob = FALSE, 
     col = "cornflowerblue", 
     border = "white",
     xlim = c(0,50), 
     main = NA, 
     xlab = "nombre d'heures travaillées (par semaine)", 
     ylab = "Effectif")

#Représentation du salaire horaire
# summary(base$SALHR)
# hist(base$SALHR, nclass=10000, prob=TRUE, col="cornflowerblue", border="white",
#      xlim=c(0,50), main="", xlab="Salaire horaire (Euros)", ylab="Densité")
# lines(density(base$SALHR, na.rm=TRUE), lwd=2, col="lightblue")
# text(40, 0.00025, paste("N =", sum(complete.cases(base$SALRED))), cex=1.2)

#Représentation du salaire avec SALHR<50 car un salaire horaire >50 ne concerne que peu d'individus
base2 <- subset(base, (base$SALHR < 50))
hist(base2$SALHR, 
     nclass = 60, 
     prob = FALSE, 
     col = "cornflowerblue", 
     border = "white",
     xlim = c(0, max(base2$SALHR)), 
     main = NA, 
     xlab = "Salaire horaire (Euros)", 
     ylab = "Effectif")
# lines(density(base2$SALHR, na.rm=TRUE), lwd=2, col="lightblue")
text(x = 45, 
     y = 1000, 
     paste("N =", sum(complete.cases(base2$SALRED))), cex=1.2)

##Représentation des variables qualitatives

#Représentation du sexe
table(base$SEXE, useNA="always")
#0 = ♂ ; 1 = ♀
#Version pie chart
pie(table(base$SEXE), 
    labels = paste(paste(c("Homme", "Femme"), round((table(base$SEXE))/sum(table(base$SEXE))*100)), "%", sep = ""),
    col=c('lightgreen', 'cornflowerblue'))
#    main="Répartition des sexes")

#Représentation des individus par secteur de travail
lbls <- c("Privé", "Public")
pct <- round(table(base$PUBLIC)/sum(table(base$PUBLIC))*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # add % to labels
pie(table(base$PUBLIC),
    labels = lbls, 
    col=c('lightgreen', 'cornflowerblue')) # cornflowerblue au lieu de lightblue
#    main="Secteur de travail") 


#Variable IDF (Île de France)
#0 = Province, 1 = Ile de France
pie(table(base$IDF), 
    labels = paste(paste(c("Province" , "Île de France"), round((table(base$IDF))/sum(table(base$IDF))*100)), "%", sep=""),
    col=c('lightgreen', 'cornflowerblue'))
#    main="Répartition du lieu de travail")

#Variable PAYNEU27 (Pays de naissance appartenant à l'Europe des 27)
#0 = oui , 1 = non
pie(table(base$PAYNEU27), 
    labels = paste(paste(c("UE27", "Hors UE27"), round((table(base$PAYNEU27))/sum(table(base$PAYNEU27))*100)), "%", sep=""),
    col=c('lightgreen', 'cornflowerblue'))
#    main="Répartition du pays de naissance")

#Variable LNAIS (lieu de naissance)
#0 = France, 1 = étranger
pie(table(base$LNAIS), 
    labels = paste(paste(c("France", "Etranger"), round((table(base$LNAIS))/sum(table(base$LNAIS))*100)), "%", sep=""),
    col=c('lightgreen', 'cornflowerblue'))
#     main = "Répartition du pays de naissance")

#Variable NFR (Nationalité Française)
#0 = français, 1 = étranger
pie(table(base$NFR), 
    labels = paste(paste(c("Français", "Etranger"), round((table(base$NFR))/sum(table(base$NFR))*100)), "%", sep=""),
    col=c('lightgreen', 'cornflowerblue'))
#   main="Répartition des nationalités")

#Variable TUU (Territoire Urbain)
#0 = Rural, 1 = Urbain
pie(table(base$TUU), 
    labels = paste(paste(c("Rural", "Urbain"), round((table(base$TUU))/sum(table(base$TUU))*100)), "%", sep=""),
    col=c('lightgreen', 'cornflowerblue'))
#   main="Répartition des zones de travail")


#Variable études
base2 <- base
etudes <- c()
for (i in 1:nrow(base2)){
  if (base2$BAC[i] == 1){
    etudes <- c(etudes, "bac")
  } else if (base2$BAC2[i] == 1){
    etudes <- c(etudes, "bac+2")
  } else if (base2$BAC3[i] == 1){
    etudes <- c(etudes, "bac+3")
  } else if (base2$BAC5[i] == 1){
    etudes <- c(etudes, "bac+5")
  } else {
    etudes <- c(etudes, "sans bac")
  }
}
#Répartition des études
#barplot(table(etudes))
pie(table(etudes), 
    # main="Niveau d'étude des individus", 
    labels = paste(paste(c("bac :", "bac+2 :", "bac+3 :", "bac+5 :", "sans bac :"), round((table(etudes))/sum(table(etudes))*100)), "%", sep=""),
    col=c('lightgreen', 'cornflowerblue', 'lightcoral', 'lightgoldenrod1', 'thistle2'))
#A choisir selon lequel est le mieux /!\

#Variables enfants
base2 <- base
enfants <- c()
for (i in 1:nrow(base2)){
  if (base2$NBENF1[i] == 1){
    enfants <- c(enfants, "1")
  } else if (base2$NBENF2[i] == 1){
    enfants <- c(enfants, "2")
  } else if (base2$NBENF3PLUS[i] == 1){
    enfants <- c(enfants, "3 ET +")
  } else {
    enfants <- c(enfants, "0")
  }
}
#Répartition du nombre d'enfants
#barplot(table(enfants))
pie(table(enfants), 
    #main="Nombre d'enfants des individus", 
    labels = paste(paste(c("0 enfant :", "1 enfant :", "2 enfants :", "3 enfants et plus :"), round((table(enfants))/sum(table(enfants))*100)), "%", sep=""),
    col=c('lightgreen', 'cornflowerblue', 'lightcoral', 'lightgoldenrod1'))


#######################
# Création base pour stat desc
#######################
base_stat <- base_stat[complete.cases(base_stat$SALRED),]
# Nous supprimons les individus n'ayant pas de salaire renseignÃ©
base_stat <- base_stat[complete.cases(base_stat$NBHEUR),]
# Nous supprimons les individus n'ayant pas de volume horaire renseignÃ©

base_stat <- subset(base_stat, (AGE >17) & (AGE<71))
#Je supprime les individus dont l'age est inférieur à 18 ans ou supérieur à 70 ans.

base_stat <- subset(base_stat, (CSER != 8))
#Les individus n'ayant jamais travaillé sont exclus (CSER = 8)

base_stat <- subset(base_stat, NBHEUR < 260) # 60 * 4.345 = volume mensuel
#Nous supprimons les individus ayant un volume horaire de travail de 260 heures ou plus, que nous considérons comme aberrant

# base$SALHR <- base$SALRED / (base$EMPNBH * 4.345)
#Création d'une variable SALHR avec le salaire par taux horaire, càd le salaire mensuel / nombre d'heures par semaine * 4.345 (nb de semaines par mois)

#Les salaires en dessous de 600euros sont considérés aberrants et retirés, de même que ceux supérieurs à 20000 euros
base_stat <- subset(base_stat, (SALRED>600) & (SALRED<20000))
summary(base_stat)

base_stat <- base_stat[complete.cases(base_stat$CONTRA),]
base_stat <- base_stat[complete.cases(base_stat$ANCENTR),]

# pour le salaire horaire, on prendra SALRED/NBHEUR c'est deux variables liées entre elles alors que EMPNBH est dissociée
base_stat$SALHR = base_stat$SALRED / base_stat$NBHEUR

sapply(base_stat, function(x) sum(is.na(x))) # nombre de NA par variable
percent(sapply(base_stat, function(x) sum(is.na(x))/nrow(base_stat)), digits = 3L) # pourcentage de NA par variable

#################################
#Stats desc via la base_stat

#Variable MATRI
#Situation matrimoniale
pie(table(base_stat$MATRI), 
    labels = paste(paste(c("Célibataire", "Marié", "Veuf", "Divorcé"), 
                         round((table(base_stat$MATRI))/sum(table(base_stat$MATRI))*100)), "%", sep=""),
    col=c('lightgreen', 'cornflowerblue', 'lightcoral', 'lightgoldenrod1'))
#    main="Répartition de la situation matrimoniale")

#Variable CONTRA
#Type de contrat
contrat <- c()
for (i in 1:nrow(base_stat)){
  if (base_stat$CONTRA[i] == 1){
    contrat <- c(contrat, 'CDI')
  } else if (base_stat$CONTRA[i] == 2){
    contrat <- c(contrat, 'CDD')
  } else {
    contrat <- c(contrat,'Autres contrats')
  }
}
pie(table(contrat), 
    labels = paste(paste(c("Autres", "CDD", "CDI"), 
                         round((table(contrat))/sum(table(contrat))*100)), "%", sep=""),
#    main="Répartition du type de contrat",
    col=c('lightgreen', 'cornflowerblue', 'lightcoral'),
    xlab='Autres = saisonnier, intérim, apprentissage, alternance')

#contrat<-c()
#for (i in 1:nrow(base_stat)){
#  if (base_stat$CONTRA[i]==1){
#    contrat<-c(contrat, 'CDI')
#  } else if (base_stat$CONTRA[i]==2){
#    contrat<-c(contrat, 'CDD')
#  } else if (base_stat$CONTRA[i]==3) {
#    contrat<-c(contrat,'saisonnier')
#  } else if (base_stat$CONTRA[i]==4){
#    contrat<-c(contrat,"intérim")
#  } else if (base_stat$CONTRA[i]==5){
#    contrat <- c(contrat, "Apprentissage")
#  }
#}
#barplot(table(contrat))


#Variable CSER
#CSP de l'individu
table(base_stat$CSER, useNA = "always")
CSP <- c()
for (i in 1:nrow(base_stat)){
  if (base_stat$CSER[i]==1){
    CSP <- c(CSP, 'Agriculteur')
  } else if (base_stat$CSER[i]==2){
    CSP <- c(CSP, 'Artisan')
  } else if (base_stat$CSER[i]==3){
    CSP <- c(CSP,'Cadre')
  } else if (base_stat$CSER[i]==4){
    CSP <- c(CSP,"Intermédiaire")
  } else if (base_stat$CSER[i]==5){
    CSP <- c(CSP, "Employé")
  } else if (base_stat$CSER[i]==6){
    CSP <- c(CSP, "Ouvrier")
  }
}

par()
barplot(sort(table(CSP)), 
        cex.names = 0.6, 
        horiz = TRUE, 
        las = 1,
        xlab = "Effectif")
        #main="Répartition des CSP")
# pie(table(CSP))
dev.off()

save(base_stat, file = "base_stat.Rda")


##########################################################################################


pdf(file = "./sorties/graphiques_stats_des_quanti_quanti.pdf")
load("base_finale2.Rda")
load("base_stat.Rda")
### STATISTIQUES DESCRIPTIVES BIVARIEES
#Quanti quanti

plot(y = base$SALHR_LN, 
     x = base$AGE, 
     ylim = c(1, 4),
     xlab = "Âge",
     ylab = "Logarithme du salaire horaire")

plot(x = base$ANCENTR, 
     y = base$SALHR_LN,
     xlab = "Ancienneté en années",
     ylab = "Logarithme du salaire horaire")

plot(x = base$NBHEUR, 
     y = base$SALHR_LN,
     xlab = "Temps de travail hebdomadaire",
     ylab = "Logarithme du salaire horaire")

dev.off()

#Quali Quali

#SEXE
sexe <- c()
for (i in 1:nrow(base)){
  if (base$SEXE[i] == 0){
    sexe <- c(sexe, "homme")
  } else if (base$SEXE[i] ==1){
    sexe <- c(sexe, "femme")
  }
}
#PAYNEU27
payneu27 <- c()
for (i in 1:nrow(base)){
  if (base$PAYNEU27[i] == 0){
    payneu27 <- c(payneu27, "oui")
  } else if (base$PAYNEU27[i] == 1){
    payneu27 <- c(payneu27, "non")
  }
}

#IDF
idf <- c()
for (i in 1:nrow(base)){
  if (base$IDF[i] == 0){
    idf <- c(idf, "hors IDF")
  } else if (base$IDF[i] == 1){
    idf <- c(idf, "IDF")
  }
}

#LNAIS
lnais <- c()
for (i in 1:nrow(base)){
  if (base$LNAIS[i] == 0){
    lnais <- c(lnais, "France")
  } else if (base$LNAIS[i] == 1){
    lnais <- c(lnais, "étranger")
  }
}

#NFR
nfr <- c()
for (i in 1:nrow(base)){
  if (base$NFR[i] == 0){
    nfr <- c(nfr, "France")
  } else if (base$NFR[i] == 1){
    nfr <- c(nfr, "étranger")
  }
}

#TUU
tuu <- c()
for (i in 1:nrow(base)){
  if (base$TUU[i] == 0){
    tuu <- c(tuu, "Rural")
  } else if (base$TUU[i] == 1){
    tuu <- c(tuu, "Urbain")
  }
}

#PUBLIC
public <- c()
for (i in 1:nrow(base)){
  if (base$PUBLIC[i] == 0){
    public <- c(public, "privé")
  } else if (base$PUBLIC[i] == 1){
    public <- c(public, "public")
  }
}

#MATRI
matri <- c()
for (i in 1:nrow(base_stat)){
  if (base_stat$MATRI[i] == 1){
    matri <- c(matri, "célibataire")
  } else if (base_stat$MATRI[i] == 2){
    matri <- c(matri, "marié")
  } else if (base_stat$MATRI[i]== 3){
    matri <- c(matri, "veuf")
  } else if (base_stat$MATRI[i]== 4){
    matri <- c(matri, "divorcé")
  }
}

#NAFG10N
act <- c()
for (i in 1:nrow(base_stat)){
  if (base_stat$NAFG10N[i] == "00"){
    act <- c(act, "Non renseigné")
  } else if (base_stat$NAFG10N[i] == "AZ"){
    act <- c(act, "Agriculture")
  } else if (base_stat$NAFG10N[i]== "BE"){
    act <- c(act, "Industrie")
  } else if (base_stat$NAFG10N[i]== "FZ"){
    act <- c(act, "Construction")
  } else if (base_stat$NAFG10N[i]== "GI"){
    act <- c(act, "Transport")
  } else if (base_stat$NAFG10N[i]== "JZ"){
    act <- c(act, "Communication")
  } else if (base_stat$NAFG10N[i]== "KZ"){
    act <- c(act, "Finance")
  } else if (base_stat$NAFG10N[i]== "LZ"){
    act <- c(act, "Immobilier")
  } else if (base_stat$NAFG10N[i]== "MN"){
    act <- c(act, "Spécialisée")
  } else if (base_stat$NAFG10N[i]== "OQ"){
    act <- c(act, "Administration")
  } else if (base_stat$NAFG10N[i]== "RU"){
    act <- c(act, "Autres services")
  }
}
a <- data.frame(act = act, sexe = sexe)
b <- subset(a, (a$act != "Non renseigné"))
table(b$act)
b$act = as.factor(as.character(b$act))
rm(a)
for (i in 1:length(contrat)){
    if (contrat[i]=="Autres contrats"){
        contrat[i] <- ""
    }
}
# for (i in 1:length(CSP)){
#  if (CSP[i]=="Ouvrier"){
#    CSP[i] <- ""
#  }
# }

# définition d'une palette de couleurs customisées pour les mosaicplots
couleurs = c("lightblue", "lightgreen", "lightsalmon", "lightyellow", "lightcyan")

pdf(file = "./sorties/graphiques_stats_des_bivar.pdf")
#pdf(file="./graphiques_mosaicplots.pdf")
mosaicplot(table(sexe, contrat),
           main = NA,
           ylab = "Type de contrat", 
           xlab = "", 
           color = couleurs[1:3])
mosaicplot(table(sexe, CSP),
           main = "",
           ylab = "Catégorie Socio-Professionnelle", 
           xlab = "", 
           color = couleurs[1:4])
mosaicplot(table(sexe, matri),
           main = "",
           ylab = "Situation matrimoniale", 
           xlab = "", 
           color = couleurs[1:4])
mosaicplot(table(sexe, public),
           main = "",
           ylab = "Secteur de l'entreprise", 
           xlab = "", 
           color = couleurs[1:3])
mosaicplot(table(b$act, b$sexe),
           main = "",
           xlab = "Activité de l'entreprise", 
           ylab = "", 
           color = couleurs[1:2],
           cex.axis = 0.0001)
mosaicplot(table(sexe, etudes),
           main = "",
           ylab = "Niveau d'études", 
           xlab = "", 
           color = couleurs)
# dev.off()
rm(b)

dev.off()


rm(base2)


save(base_stat, file="base_stat.Rda")

load("base_finale.Rda")
load("base_stat.Rda")
#Quanti Quali

#Recodage du sexe
for (i in 1:nrow(base_stat)){
  if (base$SEXE[i] == 0){
    base_stat$sexe[i] <- "homme"
  } else {
    base_stat$sexe[i] <- "femme"
  }
}
pdf(file="./sorties/graphiques_pyramides_boxplots.pdf")
#Pyramide des âges
#base_stat$AGE <- as.numeric(base_stat$AGE)
#ggplot(data=base_stat,aes(x=as.factor(AGE),fill=sexe)) + 
#  geom_bar(data=subset(base_stat,sexe=="homme")) + 
#  geom_bar(data=subset(base_stat,sexe=="femme"),aes(y=..count..*(-1))) + 
#  scale_y_continuous(breaks=seq(-1000,1000,100),labels=abs(seq(-1000,1000,100))) + 
#  ylab("population") +
#  xlab("age") +
#  coord_flip()

#Pyramide des classes d'âge
classes_a <- c(17, seq(20, 70, 5))
base_stat$class_age <- cut(as.numeric(base_stat$AGE), classes_a)
table(base_stat$class_age, useNA = "always")
ggplot(data = base_stat,
       aes(x = as.factor(class_age),
           fill = sexe)) +
    geom_bar(data = subset(base_stat, sexe=="homme")) + 
    geom_bar(data = subset(base_stat, sexe=="femme"),
             aes(y = ..count..*(-1))) + 
    scale_y_continuous(breaks = seq(-2000, 2000, 500),
                       labels = abs(seq(-2000, 2000, 500))) + 
    scale_x_discrete(labels = c("[18 ; 20]","[21 ; 25]","[26 ; 30]","[31 ; 35]","[36 ; 40]",
                                "[41 ; 45]","[46 ; 50]","[51 ; 55]","[56 ; 60]", "[61 ; 65]", "[66 ; 70]")) +
    ylab("population") +
    xlab("classe d'âge") +
    coord_flip()

#Pyramide du salaire mensuel
classes_s <- c(seq(600, 4000, 200), 4500, 5000, 10000, 17000)
base_stat$class_sal <- cut(as.numeric(base_stat$SALRED), classes_s)

ggplot(data = base_stat,
       aes(x = as.factor(class_sal),
           fill = sexe)) + 
    geom_bar(data = subset(base_stat, sexe=="homme")) + 
    geom_bar(data = subset(base_stat, sexe=="femme"),
             aes(y = ..count..*(-1))) + 
    scale_y_continuous(breaks = seq(-3000,3000,500),
                       labels = abs(seq(-3000,3000,500))) + 
    scale_x_discrete(labels = c("[600 ; 800]", "[800 ; 1000]", "[1000 ; 1200]", "[1200 ; 1400]", "[1400 ; 1600]",
                                "[1600 ; 1800]", "[1800 ; 2000]", "[2000 ; 2200]", "[2200 ; 2400]", "[2400 ; 2600]",
                                "[2600 ; 2800]", "[2800 ; 3000]", "[3000 ; 3200]", "[3200 ; 3400]", "[3400 ; 3600]",
                                "[3600 ; 3800]", "[3800 ; 4000]", "[4000 ; 4500]", "[4500 ; 5000]", "[5000 ; 10000]",
                                "[10000 ; 17000]")) + 
    ylab("population") +
    xlab("classes de salaire") +
    coord_flip()

#Pyramide du nombre d'heures
class_nbh <- seq(0, 260, 10)
base_stat$class_nbh <- cut(as.numeric(base_stat$NBHEUR), class_nbh)
ggplot(data = base_stat,
       aes(x = as.factor(class_nbh),
           fill = sexe)) + 
    geom_bar(data = subset(base_stat,sexe == "homme")) + 
    geom_bar(data = subset(base_stat,sexe == "femme"),
             aes(y = ..count..*(-1))) + 
    scale_y_continuous(breaks = seq(-7500, 7500, 2500),
                       labels = abs(seq(-7500, 7500, 2500))) +
    ylab("population") +
    xlab("classes du nombre d'heures") +
    coord_flip()

#Pyramide de l'ancienneté
class_anc <- seq(-1, 588, 12)
base_stat$class_anc <- cut(as.numeric(base_stat$ANCENTR), class_anc)
ggplot(data = base_stat,
       aes(x = as.numeric(class_anc),
           fill = sexe)) + 
    geom_bar(data = subset(base_stat, sexe == "homme")) + 
    geom_bar(data = subset(base_stat,sexe == "femme"), 
             aes(y = ..count..*(-1))) + 
    scale_y_continuous(breaks = seq(-3000, 3000, 500),
                       labels = abs(seq(-3000, 3000, 500))) + 
    ylab("population") +
    xlab("ancienneté, en années") +
    coord_flip()

## Boxplots

base_stat2 <- subset(base_stat, (base_stat$SALRED < 4001))
contra <- c()
for (i in 1:nrow(base_stat2)){
  if (base_stat2$CONTRA[i] == 1){
    contra <- c(contra, 'CDI')
  } else if (base_stat2$CONTRA[i] == 2){
    contra <- c(contra, 'CDD')
  } else {
    contra <- c(contra, 'Autres contrats')
  }
}
base2 <- subset(base, (base$SALRED < 4001))
etude <- c()
for (i in 1:nrow(base2)){
  if (base2$BAC[i] == 1){
    etude <- c(etude, "bac")
  } else if (base$BAC2[i] == 1){
    etude <- c(etude, "bac+2")
  } else if (base2$BAC3[i] == 1){
    etude <- c(etude, "bac+3")
  } else if (base2$BAC5[i] == 1){
    etude <- c(etude, "bac+5")
  } else {
    etude <- c(etude, " sans bac")
  }
}

idf2 <- c()
for (i in 1:nrow(base2)){
  if (base2$IDF[i] == 0){
    idf2 <- c(idf2, "hors IDF")
  } else if (base2$IDF[i] == 1){
    idf2 <- c(idf2, "IDF")
  }
}

#LNAIS
lnai <- c()
for (i in 1:nrow(base2)){
  if (base2$LNAIS[i] == 0){
    lnai <- c(lnai, "France")
  } else if (base2$LNAIS[i] == 1){
    lnai <- c(lnai, "étranger")
  }
}

#NFR
nfr2 <- c()
for (i in 1:nrow(base2)){
  if (base2$NFR[i] == 0){
    nfr2 <- c(nfr2, "française")
  } else if (base2$NFR[i] == 1){
    nfr2 <- c(nfr2, "étrangère")
  }
}

#TUU
tuu2 <- c()
for (i in 1:nrow(base2)){
  if (base2$TUU[i] == 0){
    tuu2 <- c(tuu2, "Rural")
  } else if (base2$TUU[i] == 1){
    tuu2 <- c(tuu2, "Urbain")
  }
}

#PUBLIC
public2 <- c()
for (i in 1:nrow(base2)){
  if (base2$PUBLIC[i] == 0){
    public2 <- c(public2, "privé")
  } else if (base2$PUBLIC[i] == 1){
    public2 <- c(public2, "public")
  }
}

#MATRI
matri2 <- c()
for (i in 1:nrow(base_stat2)){
  if (base_stat2$MATRI[i] == 1){
    matri2 <- c(matri2, "célibataire")
  } else if (base_stat2$MATRI[i] == 2){
    matri2 <- c(matri2, "marié")
  } else if (base_stat2$MATRI[i]== 3){
    matri2 <- c(matri2, "veuf")
  } else if (base_stat2$MATRI[i]== 4){
    matri2 <- c(matri2, "divorcé")
  }
}

#NAFG10N
act2 <- c()
for (i in 1:nrow(base_stat2)){
  if (base_stat2$NAFG10N[i] == "00"){
    act2 <- c(act2, "Non renseigné")
  } else if (base_stat2$NAFG10N[i] == "AZ"){
    act2 <- c(act2, "Agriculture")
  } else if (base_stat2$NAFG10N[i] == "BE"){
    act2 <- c(act2, "Industrie")
  } else if (base_stat2$NAFG10N[i] == "FZ"){
    act2 <- c(act2, "Construction")
  } else if (base_stat2$NAFG10N[i] == "GI"){
    act2 <- c(act2, "Transport")
  } else if (base_stat2$NAFG10N[i] == "JZ"){
    act2 <- c(act2, "Communication")
  } else if (base_stat2$NAFG10N[i] == "KZ"){
    act2 <- c(act2, "Finance")
  } else if (base_stat2$NAFG10N[i] == "LZ"){
    act2 <- c(act2, "Immobilier")
  } else if (base_stat2$NAFG10N[i] == "MN"){
    act2 <- c(act2, "Spécialisée")
  } else if (base_stat2$NAFG10N[i] == "OQ"){
    act2 <- c(act2, "Administration")
  } else if (base_stat2$NAFG10N[i] == "RU"){
    act2 <- c(act2, "Autres services")
  }
}


boxplot(base_stat2$SALRED ~ contra, 
        xlab = "Contrat", 
        ylab = "Salaire mensuel",
        col = couleurs)
boxplot(base_stat2$SALRED ~ etude, 
        xlab = "Niveau d'étude", 
        ylab = "Salaire mensuel",
        col = couleurs)
boxplot(base2$SALRED ~ idf2, 
        xlab = "Lieu de travail", 
        ylab = "Salaire mensuel",
        col = couleurs)
boxplot(base2$SALRED ~ lnai, 
        xlab = "Lieu de naissance", 
        ylab = "Salaire mensuel",
        col = couleurs)
boxplot(base2$SALRED ~ public2, 
        xlab = "Propriété de l'entreprise", 
        ylab = "Salaire mensuel",
        col = couleurs)
boxplot(base2$SALRED ~ tuu2, 
        xlab = "Lieu de travail", 
        ylab = "Salaire mensuel",
        col = couleurs)
boxplot(base2$SALRED ~ nfr2, 
        xlab = "Nationalité", 
        ylab = "Salaire mensuel",
        col = couleurs)
boxplot(base_stat2$SALRED ~ matri2, 
        xlab = "Situation maritale", 
        ylab = "Salaire mensuel",
        col = couleurs)
base_stat2 <- data.frame(base_stat2, act2)
ggplot(base_stat2, 
       aes(x = act2, 
           y = SALRED,
           fill = act2)) + 
    geom_boxplot() +
    coord_flip() +
    xlab("Activité de l'entreprise") +
    ylab("Salaire mensuel") +
    guides(fill = FALSE)
dev.off()




##########################################################################################

# 11/01 : Calcul de similarités entre vecteurs dichotomiques (= ~ corrélations)

load("base_finale3.Rda") # 


pdf(file="./sorties/graphiques_correlations.pdf")
matrice_cor = cor(subset(base, select = c(SALHR, SALRED, AGE, ANCENTR, NBHEUR)))
corrplot(matrice_cor, 
         method = "number", 
         tl.cex = 0.9, 
         tl.col = c("black", "blue"), 
         tl.srt = 0,
         tl.offset = 1,
         col = colorRampPalette(c("darkblue","gray90","darkred"))(200))
dev.off()

df_dummies = subset(base[, ordre_variables], select = -c(SALHR_LN, SALHR, SALRED, AGE, ANCENTR, NBHEUR, CSP_AGRI, CSP_ARTI,
                                                         AGE_SQUARE, ANCENTR_SQUARE, NBHEUR_SQUARE))

sokal = c()
vect_unitaire = rep(1, nrow(df_dummies))

for (var1 in colnames(df_dummies)){
    for (var2 in colnames(df_dummies)){
        top = df_dummies[[var1]] %*% df_dummies[[var2]] + (vect_unitaire - df_dummies[[var1]]) %*% (vect_unitaire - df_dummies[[var2]])
        resultat = as.numeric(top / (2*length(vect_unitaire) - top))
        sokal = c(sokal, resultat)
    }
}

sokal = matrix(sokal, ncol = length(colnames(df_dummies)), byrow = T)

colnames(sokal) = colnames(df_dummies)
rownames(sokal) = colnames(df_dummies)


corrplot(sokal, 
         method = "color", 
         tl.cex = 0.75, 
         tl.col = c("black", "blue"), 
         tl.srt = 90,
         tl.offset = 1.1,
         col = colorRampPalette(c("white", "white", "white", "white", "white", "white", "white", "red"))(200),
         cl.lim = c(0,1))


sokal = as.data.frame(sokal)
sokal = round(sokal, 2)

write.table(sokal,
            file = "./sorties/sokal.txt",
            quote = F,
            sep = "&",
            na = "--",
            row.names = T,
            fileEncoding = "UTF-8")


##########################################################################################