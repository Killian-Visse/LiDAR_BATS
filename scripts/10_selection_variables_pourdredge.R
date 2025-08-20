#      /\                 /\    ╔===================================================================================╗
#     ( \'.__   (\_/)   __.'/ ) ║ 10 - Script des processus de sélection de variables à retenir en vu du dredge     ║ 
#      \_.'  '--('.')--'  '._/  ╚===================================================================================╝
#          `\=/ " \=/`
#           (/|\)

library (glmmTMB)
library(MuMIn)
library(ggeffects)
library(ggplot2)
library(patchwork)
library(DHARMa)
library(performance)
library(tidyr)
library(dplyr)
library(ape)
library(spaMM)

#Au total, 9 variables LiDAR à 2 échelles spatiales différentes & 6 variables OSO de proportion (4 échelles spatiales différentes) + 2 de distance = 44var 
#On va garder qu'une seule métrique par buffer : on va garder celle qui explique le mieux le nombre de contacts totaux (modèles univariés avec loi négative binomiale car forte surdispersion)

#1. Sélection du buffer le plus pertinent pour chaque variable ####

MOD0<- glmmTMB(`nombre de contacts total` ~ 1 + (1 | id), data = JDD_VC_LiDAR_OSO_VF_08_2025, family = nbinom2)                     
AIC(MOD0) #AIC du modele nul = 39 119

# 1.1. Variables LiDAR ####

MOD1<- glmmTMB(`nombre de contacts total` ~ rugosite_200m_filturb + (1 | id), data = JDD_VC_LiDAR_OSO_VF_08_2025, family = nbinom2)
AIC(MOD1) #Rugosité 200m meilleur modèle 
MOD2<- glmmTMB(`nombre de contacts total` ~ rugosite_500m_filturb + (1 | id), data = JDD_VC_LiDAR_OSO_VF_08_2025, family = nbinom2)                     
AIC(MOD2)

MOD3<- glmmTMB(`nombre de contacts total` ~ hauteurmoyenne_200m_filturb + (1 | id), data = JDD_VC_LiDAR_OSO_VF_08_2025, family = nbinom2)
AIC(MOD3)
MOD4<- glmmTMB(`nombre de contacts total` ~ hauteurmoyenne_500m_filturb + (1 | id), data = JDD_VC_LiDAR_OSO_VF_08_2025, family = nbinom2)                     
AIC(MOD4) #Pas de différence significative avec le modèle nul

MOD5<- glmmTMB(`nombre de contacts total` ~ ouverture_canopee_200m_filturb + (1 | id), data = JDD_VC_LiDAR_OSO_VF_08_2025, family = nbinom2)
AIC(MOD5) 
MOD6<- glmmTMB(`nombre de contacts total` ~ ouverture_canopee_500m_filturb + (1 | id), data = JDD_VC_LiDAR_OSO_VF_08_2025, family = nbinom2)                     
AIC(MOD6) #Pas de différence significative avec le modèle nul

MOD7<- glmmTMB(`nombre de contacts total` ~ FHD_200m_filturb + (1 | id), data = JDD_VC_LiDAR_OSO_VF_08_2025, family = nbinom2)
AIC(MOD7) #FHD 200m meilleur modèle
MOD8<- glmmTMB(`nombre de contacts total` ~ FHD_500m_filturb + (1 | id), data = JDD_VC_LiDAR_OSO_VF_08_2025, family = nbinom2)                     
AIC(MOD8)  

MOD9<- glmmTMB(`nombre de contacts total` ~ espace_vol_libre_200m_filturb + (1 | id), data = JDD_VC_LiDAR_OSO_VF_08_2025, family = nbinom2)
AIC(MOD9) #espace vol libre 200m meilleur modèle mais pas significatif par rapport à 500m
MOD10<- glmmTMB(`nombre de contacts total` ~ espace_vol_libre_500m_filturb + (1 | id), data = JDD_VC_LiDAR_OSO_VF_08_2025, family = nbinom2)                     
AIC(MOD10)  

MOD11<- glmmTMB(`nombre de contacts total` ~ trouees_canopee_200m_filturb + (1 | id), data = JDD_VC_LiDAR_OSO_VF_08_2025, family = nbinom2)
AIC(MOD11) 
MOD12<- glmmTMB(`nombre de contacts total` ~ trouees_canopee_500m_filturb + (1 | id), data = JDD_VC_LiDAR_OSO_VF_08_2025, family = nbinom2)                     
AIC(MOD12) #Pas de différence significative avec le modèle nul

MOD13<- glmmTMB(`nombre de contacts total` ~ recouvrement_arbustive_basse_200m + (1 | id), data = JDD_VC_LiDAR_OSO_VF_08_2025, family = nbinom2)
AIC(MOD13) 
MOD14<- glmmTMB(`nombre de contacts total` ~ recouvrement_arbustive_basse_500m + (1 | id), data = JDD_VC_LiDAR_OSO_VF_08_2025, family = nbinom2)                     
AIC(MOD14) #Pas de différence significative avec le modèle nul

#On ne teste pas recouvrement_herbacee car les histogrammes ont montré une faible variation de cette variable


# 1.2. Variables 2D ####


# 1.2.1. Les proportions ####

MOD1<- glmmTMB(`nombre de contacts total`~ prop_foret_buf250 + (1 | id), data = JDD_VC_LiDAR_OSO_VF_08_2025, family = nbinom2)
AIC(MOD1) 
MOD2<- glmmTMB(`nombre de contacts total`~ prop_foret_buf500 + (1 | id), data = JDD_VC_LiDAR_OSO_VF_08_2025, family = nbinom2)                     
AIC(MOD2)
MOD3<- glmmTMB(`nombre de contacts total`~ prop_foret_buf750 + (1 | id), data = JDD_VC_LiDAR_OSO_VF_08_2025, family = nbinom2)                     
AIC(MOD3)
MOD4<- glmmTMB(`nombre de contacts total`~ prop_foret_buf1000+ (1 | id), data = JDD_VC_LiDAR_OSO_VF_08_2025, family = nbinom2)                     
AIC(MOD4) #Pas de différence significative avec le modèle nul


MOD5<- glmmTMB(`nombre de contacts total`~ prop_agricole_buf250 + (1 | id), data = JDD_VC_LiDAR_OSO_VF_08_2025, family = nbinom2)
AIC(MOD5) 
MOD6<- glmmTMB(`nombre de contacts total`~ prop_agricole_buf500 + (1 | id), data = JDD_VC_LiDAR_OSO_VF_08_2025, family = nbinom2)                     
AIC(MOD6)
MOD7<- glmmTMB(`nombre de contacts total`~ prop_agricole_buf750 + (1 | id), data = JDD_VC_LiDAR_OSO_VF_08_2025, family = nbinom2)                     
AIC(MOD7)
MOD8<- glmmTMB(`nombre de contacts total`~ prop_agricole_buf1000+ (1 | id), data = JDD_VC_LiDAR_OSO_VF_08_2025, family = nbinom2)                     
AIC(MOD8) #Pas de différence significative avec le modèle nul


MOD9<- glmmTMB(`nombre de contacts total`~ prop_prairie_buf250 + (1 | id), data = JDD_VC_LiDAR_OSO_VF_08_2025, family = nbinom2)
AIC(MOD9) 
MOD10<- glmmTMB(`nombre de contacts total`~ prop_prairie_buf500 + (1 | id), data = JDD_VC_LiDAR_OSO_VF_08_2025, family = nbinom2)                     
AIC(MOD10)
MOD11<- glmmTMB(`nombre de contacts total`~ prop_prairie_buf750 + (1 | id), data = JDD_VC_LiDAR_OSO_VF_08_2025, family = nbinom2)                     
AIC(MOD11)
MOD12<- glmmTMB(`nombre de contacts total`~ prop_prairie_buf1000+ (1 | id), data = JDD_VC_LiDAR_OSO_VF_08_2025, family = nbinom2)                     
AIC(MOD12) #Prop prairie 1000m meilleur modèle


MOD13<- glmmTMB(`nombre de contacts total`~ prop_urbain_buf250 + (1 | id), data = JDD_VC_LiDAR_OSO_VF_08_2025, family = nbinom2)
AIC(MOD13) 
MOD14<- glmmTMB(`nombre de contacts total`~ prop_urbain_buf500 + (1 | id), data = JDD_VC_LiDAR_OSO_VF_08_2025, family = nbinom2)                     
AIC(MOD14)
MOD15<- glmmTMB(`nombre de contacts total`~ prop_urbain_buf750 + (1 | id), data = JDD_VC_LiDAR_OSO_VF_08_2025, family = nbinom2)                     
AIC(MOD15)
MOD16<- glmmTMB(`nombre de contacts total`~ prop_urbain_buf1000+ (1 | id), data = JDD_VC_LiDAR_OSO_VF_08_2025, family = nbinom2)                     
AIC(MOD16) #Prop urbain 250m et 500m (NS entre eux) meilleurs modèles /!\ filtrer < 0.3


MOD17<- glmmTMB(`nombre de contacts total`~ prop_eau_buf250 + (1 | id), data = JDD_VC_LiDAR_OSO_VF_08_2025, family = nbinom2)
AIC(MOD17) 
MOD18<- glmmTMB(`nombre de contacts total`~ prop_eau_buf500 + (1 | id), data = JDD_VC_LiDAR_OSO_VF_08_2025, family = nbinom2)                     
AIC(MOD18)
MOD19<- glmmTMB(`nombre de contacts total`~ prop_eau_buf750 + (1 | id), data = JDD_VC_LiDAR_OSO_VF_08_2025, family = nbinom2)                     
AIC(MOD19)
MOD20<- glmmTMB(`nombre de contacts total`~ prop_eau_buf1000+ (1 | id), data = JDD_VC_LiDAR_OSO_VF_08_2025, family = nbinom2)                     
AIC(MOD20) #Prop eau 250m meilleur modèle


MOD21<- glmmTMB(`nombre de contacts total`~ length_haie_buf250 + (1 | id), data = JDD_VC_LiDAR_OSO_VF_08_2025, family = nbinom2)
AIC(MOD21) 
MOD22<- glmmTMB(`nombre de contacts total`~ length_haie_buf500 + (1 | id), data = JDD_VC_LiDAR_OSO_VF_08_2025, family = nbinom2)                     
AIC(MOD22)
MOD23<- glmmTMB(`nombre de contacts total`~ length_haie_buf750 + (1 | id), data = JDD_VC_LiDAR_OSO_VF_08_2025, family = nbinom2)                     
AIC(MOD23)
MOD24<- glmmTMB(`nombre de contacts total`~ length_haie_buf1000+ (1 | id), data = JDD_VC_LiDAR_OSO_VF_08_2025, family = nbinom2)                     
AIC(MOD24) #m linéaires de haie 750m et 1000m (NS entre eux) meilleurs modèles

# 1.2.2. Les distances ####

MOD25<- glmmTMB(`nombre de contacts total`~ distance_eau + (1 | id), data = JDD_VC_LiDAR_OSO_VF_08_2025, family = nbinom2)
AIC(MOD25) # Significatif

MOD26<- glmmTMB(`nombre de contacts total`~ distance_veg + (1 | id), data = JDD_VC_LiDAR_OSO_VF_08_2025, family = nbinom2)                     
AIC(MOD26) # Significatif

# 2. Nouvelle matrice de corrélations avec les variables informatives préselectionnées ####

TAB3 <- JDD_VC_LiDAR_OSO_VF_08_2025 %>%
  select(rugosite_200m_filturb,FHD_200m_filturb,espace_vol_libre_200m_filturb,prop_prairie_buf1000,prop_urbain_buf250,
         prop_eau_buf250,length_haie_buf750, distance_eau,distance_veg) %>%
  na.omit() # enlève les NA pour éviter des erreurs dans l’ACP

# Calcul des corrélations avec test des p-values
corr_test <- corr.test(TAB3, use = "complete.obs", method = "pearson")
M <- corr_test$r       # matrice de corrélation
p.mat <- corr_test$p   # matrice des p-values

# Palette de couleurs
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

# Tracé du corrélogramme
corrplot(M, method = "color", col = col(200),  
         type = "upper", order = "hclust", 
         addCoef.col = "black",  # ajouter les coefficients en noir
         tl.col = "black", tl.srt = 45, tl.cex = 0.7, number.cex = 0.55, diag = FALSE)

#Les seules variables fortement corrélées entre-elles (>0.7) sont la rugosité 200m et FHD 200m (r=0.86) & FHD 200m et distance_veg (r=0.71). Pour supprimer 1 variable au lieu de 2 et par pertinence écologique, j'ai privilégié rugosité 200m à FHD 200m
#On a maintenant sélectionné toutes les variables à inclure dans le dredge en veillant à 3 critères : leur variabilité, leur pertinence écologique et les corrélations entretenues entre-elles