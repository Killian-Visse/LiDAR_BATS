#      /\                 /\    ╔===============================================================================╗
#     ( \'.__   (\_/)   __.'/ ) ║ 09 - Script de l'exploration des données sur le JDD final post-soutenance     ║ 
#      \_.'  '--('.')--'  '._/  ╚===============================================================================╝
#          `\=/ " \=/`
#           (/|\)

library(tidyverse)
library(DHARMa)
library(GGally)
library(corrplot)
library(psych)
library(dplyr)
library(ade4)
library(factoextra)

# Histogrammes simplespour visualiser la distribution de mes variables

hist((DataMRE$distance_veg), breaks=10) #Visualiser une variable en particulier

# 1. Boucle pour visualiser l'ensemble des variables (effectifs en fonction des valeurs des variables)  ####
# 1.1. Variables LiDAR ####

var_lidar <- JDD_VC_LiDAR_OSO_VF_08_2025[, c("hauteurmoyenne_200m_filturb", "hauteurmoyenne_500m_filturb",
                            "rugosite_200m_filturb", "rugosite_500m_filturb",
                            "ouverture_canopee_200m_filturb", "ouverture_canopee_500m_filturb",
                            "densite_arbre_200m_filturb", "densite_arbre_500m_filturb",
                            "FHD_200m_filturb", "FHD_500m_filturb", "espace_vol_libre_200m_filturb","espace_vol_libre_500m_filturb","recouvrement_herbacee_200m_filturb",
                            "recouvrement_herbacee_500m_filturb","recouvrement_arbustive_basse_200m_filturb","recouvrement_arbustive_basse_500m_filturb",
                            "trouees_canopee_200m_filturb","trouees_canopee_500m_filturb","nombre de contacts total")]

var_lidar_long <- var_lidar %>% #long car on passe le format du tableau large (var en colonne) vers long (var en lignes) pour faciliter les choses à ggplot
  pivot_longer(cols = everything(), names_to = "variable", values_to = "valeur")

ggplot(var_lidar_long, aes(x = valeur)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  facet_wrap(~ variable, scales = "free", ncol = 4) +
  theme_minimal() +
  labs(title = "Distribution des variables LiDAR",
       x = "Valeur", y = "Effectif")

# 1.2. Variables OSO ####
 
var_oso <- JDD_VC_LiDAR_OSO_VF_08_2025[, c("prop_eau_buf250","prop_eau_buf500","prop_eau_buf750","prop_eau_buf1000", 
                                           "prop_urbain_buf250","prop_urbain_buf500","prop_urbain_buf750","prop_urbain_buf1000",
                                           "prop_prairie_buf250","prop_prairie_buf500","prop_prairie_buf750","prop_prairie_buf1000",
                                           "prop_agricole_buf250","prop_agricole_buf500","prop_agricole_buf750","prop_agricole_buf1000",
                                           "prop_foret_buf250","prop_foret_buf500","prop_foret_buf750","prop_foret_buf1000",
                                           "length_haie_buf250","length_haie_buf500","length_haie_buf750","length_haie_buf1000",
                                           "distance_eau","distance_veg")]

var_oso_long <- var_oso %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "valeur")

ggplot(var_oso_long, aes(x = valeur)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  facet_wrap(~ variable, scales = "free", ncol = 4) +
  theme_minimal() +
  labs(title = "Distribution des variables OSO",
       x = "Valeur", y = "Effectif")

# 2. Matrice des corrélations ####

TAB2 <- JDD_VC_LiDAR_OSO_VF_08_2025 %>%
  select(hauteurmoyenne_200m_filturb,hauteurmoyenne_500m_filturb, rugosite_200m_filturb, rugosite_500m_filturb,ouverture_canopee_200m_filturb, ouverture_canopee_500m_filturb,
         densite_arbre_200m_filturb, densite_arbre_500m_filturb,FHD_200m_filturb, FHD_500m_filturb, espace_vol_libre_200m_filturb,espace_vol_libre_500m_filturb,
         recouvrement_herbacee_200m_filturb,recouvrement_herbacee_500m_filturb,recouvrement_arbustive_basse_200m_filturb,recouvrement_arbustive_basse_500m_filturb,
         trouees_canopee_200m_filturb,trouees_canopee_500m_filturb,prop_eau_buf250,prop_foret_buf250,length_haie_buf250,
         prop_urbain_buf250,prop_prairie_buf250,prop_agricole_buf250,distance_eau,distance_veg,prop_urbain_buf1000,
         prop_prairie_buf1000,prop_agricole_buf1000,prop_eau_buf1000,prop_foret_buf1000,length_haie_buf1000) %>%
na.omit() # enlève les NA pour éviter des erreurs dans l’ACP

  
# Calcul des corrélations avec test des p-values
corr_test <- corr.test(TAB2, use = "complete.obs", method = "pearson")
M <- corr_test$r       # matrice de corrélation
p.mat <- corr_test$p   # matrice des p-values

# Palette de couleurs
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

# Tracé du corrélogramme
corrplot(M, method = "color", col = col(200),  
         type = "upper", order = "hclust", 
         addCoef.col = "black",  # ajouter les coefficients en noir
         tl.col = "black", tl.srt = 45, tl.cex = 0.7, number.cex = 0.55,
         p.mat = p.mat, sig.level = 0.001, insig = "blank",  # masque les coefficients non-significatifs
         diag = FALSE)

# 3. ACP ####

TAB2norm = scale(TAB2) #NCR car variables pas toutes dans les mêmes unités
VP2 = eigen(cor(TAB2)) # Valeurs et vecteurs propres de la matrice de corrélation de TAB1. On utiliser corTAB1 car = covTAB1norm

barplot(VP2$values) #Afficher graphique des éboulies
abline(h=mean(VP2$values)) #Afficher une droite horizontale à la moyenne des VP

ACP2 = dudi.pca(TAB2, scannf = FALSE, nf=2) #nf=2 car 2 composantes
fviz_pca_var(ACP2, repel=TRUE) #Faire graphique ACP