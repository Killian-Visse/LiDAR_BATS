#      /\                 /\    ╔=======================================================================╗
#     ( \'.__   (\_/)   __.'/ ) ║ 05 - Script d'exploration du JDD DataPip en vu du rapport de stage    ║ 
#      \_.'  '--('.')--'  '._/  ╚=======================================================================╝
#          `\=/ " \=/`
#           (/|\)


library (dplyr)
library(tidyverse)
library(ggplot2)
library(factoextra)
library(FactoMineR)
library(ade4)
library(GGally)

# 0. Regarder la répartition des valeurs de chaque variable ####

DataPip %>% #Boucle summary pour chaque variable
  select(hauteurmoyenne_200m, rugosite_200m, densite_arbre_200m,ouverture_canopee_200m,FHD_200m, hauteurmoyenne_500m, rugosite_500m, densite_arbre_500m,ouverture_canopee_500m,FHD_500m) %>%
  summarise(across(everything(), list(
    min = min,
    max = max,
    mean = mean,
    median = median,
    sd = sd
  ), na.rm = TRUE))

# 1. Regarder allure des variables ####

hist(DataPip$densite_arbre_200m) #Distribution type log
hist(DataPip$rugosite_200m) #Distribution type normal
hist(DataPip$FHD_200m) #Distribution type bimodale
hist(DataPip$hauteurmoyenne_200m) #Distribution type log
hist(DataPip$ouverture_canopee_200m) #Distribution type exp
hist(DataPip$nb_contacts) #Distribution type log
hist(DataPip$rugosite_500m) #Distribution type normal
hist(DataPip$FHD_500m) #Distribution type bimodale
hist(DataPip$hauteurmoyenne_500m) #Distribution type log
hist(DataPip$ouverture_canopee_500m) #Distribution type exp
hist(DataPip$densite_arbre_500m) #Distribution type log

# 2. Regarder lien direct entre nb_contacts et chacune des variables ####

plot(DataPip$nb_contacts~DataPip$hauteurmoyenne_200m)
plot(DataPip$nb_contacts~DataPip$hauteurmoyenne_500m)
plot(DataPip$nb_contacts~DataPip$rugosite_200m)
plot(DataPip$nb_contacts~DataPip$rugosite_500m)
plot(DataPip$nb_contacts~DataPip$ouverture_canopee_200m)
plot(DataPip$nb_contacts~DataPip$ouverture_canopee_500m)
plot(DataPip$nb_contacts~DataPip$densite_arbre_200m)
plot(DataPip$nb_contacts~DataPip$densite_arbre_500m)
plot(DataPip$nb_contacts~DataPip$FHD_200m)
plot(DataPip$nb_contacts~DataPip$FHD_500m)


ggplot(DataPip, aes(x = hauteurmoyenne_500m, y = (nb_contacts/nb_heures_enregistrement))) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", se = FALSE, color = "darkcyan") +
  theme_minimal() +
  labs(x = "Variable testée", y = "Nombre de contacts/h")

# 3. ACP ####

TAB1 <- DataGui_vf %>%
  select(rugosite_200m,ouverture_canopee_200m,FHD_200m, rugosite_500m,ouverture_canopee_500m,FHD_500m,espace_vol_libre_200m,espace_vol_libre_500m,recouvrement_herbacee_200m,recouvrement_herbacee_500m,recouvrement_arbustive_basse_200m,recouvrement_arbustive_basse_500m,trouees_canopee_200m,trouees_canopee_500m,hauteurmoyenne_200m,hauteurmoyenne_500m,densite_arbre_200m,densite_arbre_500m) %>%
  na.omit() # enlève les NA pour éviter des erreurs dans l’ACP

ggpairs(TAB1, diag = list(continuous = "blankDiag")) # sans la diagonale

cov(TAB1) #Matrice de covariance(covariance des variables avec elles-mêmes = variance)
cor(TAB1) #Matrice de corrélation(corrélation des variables avec elles-mêmes) = 1

TAB1norm = scale(TAB1) #NCR car variables pas toutes dans les mêmes unités
boxplot(TAB1norm)

VP = eigen(cor(TAB1)) # Valeurs et vecteurs propres de la matrice de corrélation de TAB1. On utiliser corTAB1 car = covTAB1norm
print (VP)

barplot(VP$values) #Afficher graphique des éboulies
abline(h=mean(VP$values)) #Afficher une droite horizontale à la moyenne des VP

CP = VP$values/sum(VP$values) # proportion de variance expliquée par chaque composante principale (CP)
print(CP)

CP*100 # en % de variance expliquée. 1 CP explique >80% de la variation
cumsum(CP*100) # % de variance cumulé


CP = VP$values/sum(VP$values) # proportion de variance expliquée par chaque composante principale (CP)
print(CP)

CP*100 # en % de variance expliquée. 1 CP explique >80% de la variation
cumsum(CP*100) # % de variance cumulé
ACP1 = dudi.pca(TAB1, scannf = FALSE, nf=2) #nf=2 car 2 composantes
fviz_pca_var(ACP1, repel=TRUE) #Faire graphique ACP

res <- PCA(TAB1, graph=FALSE) #Pour savoir quelles variables sont influencés par quelles dimensions
res$var$coord[, 2] #ici dimension 2

# 4. Faire un corrélogramme ####
library(corrplot)
library(psych)
var_lidar <- DataGui_vf[, c("hauteurmoyenne_200m", "hauteurmoyenne_500m",
                          "rugosite_200m", "rugosite_500m",
                          "ouverture_canopee_200m", "ouverture_canopee_500m",
                          "densite_arbre_200m", "densite_arbre_500m",
                          "FHD_200m", "FHD_500m", "espace_vol_libre_200m","espace_vol_libre_500m","recouvrement_herbacee_200m",
                          "recouvrement_herbacee_500m","recouvrement_arbustive_basse_200m","recouvrement_arbustive_basse_500m",
                        "trouees_canopee_200m","trouees_canopee_500m")]

corr_test <- corr.test(var_lidar, use = "complete.obs", method = "pearson")
M <- corr_test$r       # matrice de corrélation
p.mat <- corr_test$p   # matrice des p-values

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

corrplot(M, method = "color", col = col(200),  
         type = "upper", order = "hclust", 
         addCoef.col = "black",
         tl.col = "black", tl.srt = 45,tl.cex=0.7,number.cex=0.8,
         p.mat = p.mat, sig.level = 0.001, insig = "blank",
         diag = FALSE)