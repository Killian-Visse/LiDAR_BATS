#      /\                 /\    ╔====================================================================╗
#     ( \'.__   (\_/)   __.'/ ) ║ 11 - Script des modèles par guilde présentés dans le rapport final ║ 
#      \_.'  '--('.')--'  '._/  ╚====================================================================╝
#          `\=/ " \=/`
#           (/|\)

library(glmmTMB)
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

# 0. Scale des variables ####

#On centre et réduit nos variables afin de standardiser les variables, éviter la colinéarité et pour une meilleure interprétabilité des coefficients,
vars_to_scale <- c("rugosite_200m_filturb", "espace_vol_libre_200m_filturb","prop_prairie_buf1000", 
                   "prop_urbain_buf250", "prop_eau_buf250","length_haie_buf750", "distance_eau", "distance_veg")

JDD_VC_LiDAR_OSO_VF_08_2025_scaled <- JDD_VC_LiDAR_OSO_VF_08_2025 #copie du JDD 
JDD_VC_LiDAR_OSO_VF_08_2025_scaled[vars_to_scale] <- scale(JDD_VC_LiDAR_OSO_VF_08_2025_scaled[vars_to_scale]) #Remplace les variables initiales par les centrées réduites

# 1. Modélisation de la guilde SRE ####
# 1.1 Dredge : on met toutes les variables sélectionnées et ça calcule automatiquement le(s) meilleur(s) modèles ####

vars_dredge <- c("nombre_de_contacts_SRE", "rugosite_200m_filturb","espace_vol_libre_200m_filturb", "prop_prairie_buf1000", 
                 "prop_urbain_buf250","prop_eau_buf250", "length_haie_buf750", "distance_eau", "distance_veg", "id","longlat", "longitude", "latitude")

Dredge_SRE <- na.omit(JDD_VC_LiDAR_OSO_VF_08_2025_scaled[, vars_dredge]) # On garde uniquement les lignes complètes sur ces colonnes pour pas faire buger le dredge

MOD_global_SRE <- glmmTMB(nombre_de_contacts_SRE ~ rugosite_200m_filturb + espace_vol_libre_200m_filturb + prop_prairie_buf1000 
                          + prop_urbain_buf250 + prop_eau_buf250 + length_haie_buf750 + distance_eau  + distance_veg + (1 | id), 
                          data = Dredge_SRE,  family = nbinom2)

options(na.action = "na.fail")  # important pour que le dredge plante si il reste des NA (fausse les AIC etc)

Dredge_SRE_results <- dredge(MOD_global_SRE)
head(Dredge_SRE_results)  # Affiche les meilleurs modèles par AIC
BestMOD_SRE <- get.models(Dredge_SRE_results, subset = delta < 2)
summary(BestMOD_SRE[[1]])  #Meilleur modèle au sens strict, les autres ont tous un delta AIC < 2
summary(BestMOD_SRE[[2]])  
summary(BestMOD_SRE[[3]])  
summary(BestMOD_SRE[[4]])  
summary(BestMOD_SRE[[5]])  

vars_SRE <- names(fixef(BestMOD_SRE[[1]])$cond) #correction=true pour corriger la non prise en compte de l'effet aléatoire dans les effets marginaux
vars_SRE <- vars_SRE[vars_SRE != "(Intercept)"] #On garde toutes les variables sauf l'intercept

plots_SRE <- lapply(vars_SRE, function(v) { # Générer un plot de l'effet pour chaque variable
  ggpredict(BestMOD_SRE[[1]], terms = v, bias_correction = TRUE) %>% 
    plot() + 
    ggtitle(paste("Effet de", v)) +
    theme_minimal()
})

combined_plot_SRE <- wrap_plots(plots_SRE, ncol = 2)  # Combiner tous les plots sur une seule page
print(combined_plot_SRE)

r2_nakagawa(BestMOD_SRE[[1]]) #Calcul du r² de Nakagawa (marginal sans l'effet aléatoire et conditionnel effet fixe + aléatoire)

residuals_MOD_SRE<-DHARMa::simulateResiduals(BestMOD_SRE[[1]],quantreg=T)  #Visualiser les résidus
plot(residuals_MOD_SRE) 
testSpatialAutocorrelation(residuals_MOD_SRE, x=JDD_VC_LiDAR_OSO_VF_08_2025$longitude, y=JDD_VC_LiDAR_OSO_VF_08_2025$latitude)

# 1.2. Test autocorrélation spatiale : test sur les uniques car pour DHARMa il faut seulement une observation par site hors on a plusieurs nuits par site ####

jdd_unique_pt <- JDD_VC_LiDAR_OSO_VF_08_2025 %>% # unique coords
  group_by(id) %>%
  slice(1)

jdd_unique_pt_wNA <- jdd_unique_pt %>% filter(!is.na(longitude)) # on retire les NA pour les longlat

#créer matrice de distances inverses
jdd_unique_pt_dist <- as.matrix(dist(cbind(jdd_unique_pt_wNA$longitude,jdd_unique_pt_wNA$latitude)))
jdd_unique_pt_dist_inv <- 1/jdd_unique_pt_dist #Matrice de l'inverse
diag(jdd_unique_pt_dist_inv) <- 0
jdd_unique_pt_dist_inv[is.infinite(jdd_unique_pt_dist_inv)] <- 0 #Mise à 0 de la diagonale

ape::Moran.I(jdd_unique_pt_wNA$`nombre de contacts SRE`, jdd_unique_pt_dist_inv) #Test autocorrélation spatiale --> pas d'autocorrélation spatiale 

plot(JDD_VC_LiDAR_OSO_VF_08_2025$distance_veg, JDD_VC_LiDAR_OSO_VF_08_2025$`nombre de contacts SRE`)
summary(JDD_VC_LiDAR_OSO_VF_08_2025$`nombre de contacts SRE`)

plot(predict(BestMOD_SRE[[1]], type = "response")) # on regarde les valeurs des predict
# modèle validé par Mary  

# 2. Modélisation de la guilde MRE ####

vars_dredge <- c("nombre_de_contacts_MRE","Nombre_de_contacts_de_MRE_Pippip_exclu","Nombre_de_contacts_de_Pippip",
                 "rugosite_200m_filturb","espace_vol_libre_200m_filturb", "prop_prairie_buf1000", 
                 "prop_urbain_buf250","prop_eau_buf250", "length_haie_buf750", "distance_eau", "distance_veg", "id","longlat", 
                 "longitude", "latitude","jour_julien","nb_heures_enregistrement","Nuit")

Dredge_MRE <- na.omit(JDD_VC_LiDAR_OSO_VF_08_2025_scaled[, vars_dredge]) # On garde uniquement les lignes complètes sur ces colonnes pour pas faire buger le dredge

MOD_global_MRE <- glmmTMB(nombre_de_contacts_MRE ~ rugosite_200m_filturb + espace_vol_libre_200m_filturb + prop_prairie_buf1000 
                          + prop_urbain_buf250 + prop_eau_buf250 + length_haie_buf750 + distance_eau  + distance_veg + (1 | id), 
                          data = Dredge_MRE,  family = nbinom2)

options(na.action = "na.fail")  # important pour que le dredge plante si il reste des NA (fausse les AIC etc)

Dredge_MRE_results <- dredge(MOD_global_MRE)
head(Dredge_MRE_results)  # Affiche les meilleurs modèles par AIC
BestMOD_MRE <- get.models(Dredge_MRE_results, subset = delta < 2)
summary(BestMOD_MRE[[1]])  #Meilleur modèle au sens strict, les autres ont tous un delta AIC < 2 ; pas de variables LiDAR mais ceux qui ont des variables LiDAR sont NS donc on le conserve quand même
summary(BestMOD_MRE[[2]])  
summary(BestMOD_MRE[[3]])  
summary(BestMOD_MRE[[4]])  
summary(BestMOD_MRE[[5]])  
summary(BestMOD_MRE[[6]])  
summary(BestMOD_MRE[[7]])  

vars_MRE <- names(fixef(BestMOD_MRE[[1]])$cond) #correction=true pour corriger la non prise en compte de l'effet aléatoire dans les effets marginaux
vars_MRE <- vars_MRE[vars_MRE != "(Intercept)"] #On garde toutes les variables sauf l'intercept

plots_MRE <- lapply(vars_MRE, function(v) { # Générer un plot pour chaque variable
  ggpredict(BestMOD_MRE[[1]], terms = v, bias_correction = TRUE) %>% 
    plot() + 
    ggtitle(paste("Effet de", v)) +
    theme_minimal()
})

combined_plot_MRE <- wrap_plots(plots_MRE, ncol = 2)  # Combiner tous les plots sur une seule page
print(combined_plot_MRE)

r2_nakagawa(BestMOD_MRE[[1]])

residuals_MOD_MRE<-DHARMa::simulateResiduals(BestMOD_MRE[[1]])  #Visualiser les résidus
plot(residuals_MOD_MRE) #Les résidus sont bons mise à part les faibles valeurs qui sont mal prédites --> il faut ajuster le modele
testUniformity(residuals_MOD_MRE) #Calcul du goodness of fit (qualité globale de l'ajustement aux données)
testDispersion(residuals_MOD_MRE) #Comme modèle mal ajusté, test de surdispersion
testZeroInflation(residuals_MOD_MRE) #Inflation de 0 significative car énormément de 0 dans nos valeurs
#on va donc faire modele 0 inflation negative binomiale pour prendre en compte cette inflation, si marche pas faire modele tronqué ou modele de Hurdel (a double inference) mais plus compliqué

#Modele 0 inflation NB
MOD_global_MRE_ZI <- glmmTMB(nombre_de_contacts_MRE ~ rugosite_200m_filturb + espace_vol_libre_200m_filturb + prop_prairie_buf1000
                             + prop_urbain_buf250 + prop_eau_buf250 + length_haie_buf750 + distance_eau + distance_veg + (1 | id),
                             ziformula = ~1, data = Dredge_MRE, family = nbinom2) 

options(na.action = "na.fail")  
Dredge_MRE_ZI_results <- dredge(MOD_global_MRE_ZI)
BestMOD_MRE_ZI <- get.models(Dredge_MRE_ZI_results, subset = delta < 2)
summary(BestMOD_MRE_ZI[[1]])   #Meilleur modèle au sens strict, les autres ont tous un delta AIC < 2 ; pas de variables LiDAR mais ceux qui ont des variables LiDAR sont NS donc on le conserve quand même
summary(BestMOD_MRE_ZI[[2]])  
summary(BestMOD_MRE_ZI[[3]])  
summary(BestMOD_MRE_ZI[[4]])  
summary(BestMOD_MRE_ZI[[5]])  
summary(BestMOD_MRE_ZI[[6]])  
summary(BestMOD_MRE_ZI[[7]])  
summary(BestMOD_MRE_ZI[[8]])  
summary(BestMOD_MRE_ZI[[9]])  
summary(BestMOD_MRE_ZI[[10]])

residuals_MOD_MRE_ZI<-DHARMa::simulateResiduals(BestMOD_MRE_ZI[[1]])  #Visualiser les résidus
plot(residuals_MOD_MRE_ZI) 

ape::Moran.I(jdd_unique_pt_wNA$`nombre de contacts MRE`, jdd_unique_pt_dist_inv) #Test autocorrélation spatiale --> autocorrélation spatiale 

#On va donc comparer les AIC du modèle nul avant et après correction de l'autocorrélation spatiale 
#Pour prendre en compte l'autocorrélation spatiale, oninclut longlat en var aléatoire grâce au package spaMM qui permet d'inclure l'effet spatial dans les GLMM

Dredge_MRE$long_cr <- scale(Dredge_MRE$longitude) #On centre et réduit long/lat pour gagner du temps de traitement tout en conservant l'information
Dredge_MRE$lat_cr <- scale(Dredge_MRE$latitude)

Dredge_MRE_subset <- Dredge_MRE[1:500, ] #Pour tester sur une petite partie du JDD 

MOD_spaMM <- fitme(`nombre de contacts MRE` ~ distance_eau + length_haie_buf750 + prop_eau_buf250 + prop_prairie_buf1000 + 
                     prop_urbain_buf250 + (1 | id) +  Matern(1 | long_cr + lat_cr), family = negbin(), data = Dredge_MRE)
#Matern = effet de la structure spatial

summary(MOD_spaMM)
AIC(MOD_spaMM) #AIC +5000 donc la correction de l'autocorrélation spatiale semble ne pas être parcimonieux pour le modèle
AIC(BestMOD_MRE_ZI[[1]])

residuals_MOD_spaMM<-DHARMa::simulateResiduals(MOD_spaMM)  #Résidus moins bon que le 0 inflated de base donc on va garder le 0 inflated
plot(residuals_MOD_spaMM) 

#Dernier essai pour corriger le modele : ajout d'un offset sur le jour julien (jour depuis le début de l'année donc entre 1 et 365)

MOD_global_MRE_ZI_offset_day <- glmmTMB(`nombre de contacts MRE` ~  distance_eau + length_haie_buf750 + prop_eau_buf250 + prop_prairie_buf1000 + 
                                          prop_urbain_buf250 + offset(log(jour_julien)) + (1 | id), ziformula = ~1, data = Dredge_MRE, family = nbinom2)
AIC(MOD_global_MRE_ZI_offset_day) # --> pas mieux que model BESTMOD_MRE_ZI[[1]]

MOD_global_MRE_ZI_offset_heure <- glmmTMB(`nombre de contacts MRE` ~  distance_eau + length_haie_buf750 + prop_eau_buf250 + prop_prairie_buf1000 + 
                                            prop_urbain_buf250 + offset(log(nb_heures_enregistrement)) + (1 | id), ziformula = ~1, data = Dredge_MRE, family = nbinom2)
AIC(MOD_global_MRE_ZI_offset_heure) # --> pas mieux que model BESTMOD_MRE_ZI[[1]]


# 2.1. Modélisation de la guilde MRE sans Pippip ####

# Analyse MRE en profondeur : MRE avec juste Pippip et sans Pippip
# Dans certains papiers, on observe cette division car la Pippip est souvent abondante
# L'idée est ici de voir si ce n'est pas cette surabondance qui cause la deviation de notre modèle

MOD_global_withoutPippip <- glmmTMB(Nombre_de_contacts_de_MRE_Pippip_exclu ~ rugosite_200m_filturb + espace_vol_libre_200m_filturb + prop_prairie_buf1000 
                          + prop_urbain_buf250 + prop_eau_buf250 + length_haie_buf750 + distance_eau  + distance_veg + (1 | id), 
                          data = Dredge_MRE,  family = nbinom2)

options(na.action = "na.fail")  # important pour que le dredge plante si il reste des NA (fausse les AIC etc)

Dredge_MRE_withoutPippip_results <- dredge(MOD_global_withoutPippip)
BestMOD_MRE_withoutPipip <- get.models(Dredge_MRE_withoutPippip_results, subset = delta < 2)

summary(BestMOD_MRE_withoutPipip[[1]])  #Meilleur modèle au sens strict, les autres ont tous un delta AIC < 2 ; pas de variables LiDAR mais ceux qui ont des variables LiDAR sont NS donc on le conserve quand même
summary(BestMOD_MRE_withoutPipip[[2]]) 
summary(BestMOD_MRE_withoutPipip[[3]]) 
summary(BestMOD_MRE_withoutPipip[[4]]) 
summary(BestMOD_MRE_withoutPipip[[5]]) 
summary(BestMOD_MRE_withoutPipip[[6]]) 
summary(BestMOD_MRE_withoutPipip[[7]]) 
summary(BestMOD_MRE_withoutPipip[[8]]) 

residuals_MOD_MRE_withoutPippip<-DHARMa::simulateResiduals(BestMOD_MRE_withoutPipip[[1]]) 
plot(residuals_MOD_MRE_withoutPippip) 
#Résidus beaucoup plus conformes que le modèle 0-inflated avec toutes les MRE, on conserve donc celui-ci (validé par Mary)

# 2.2. Modélisation de la Pipistrelle commune uniquement ####

MOD_global_Pipip <- glmmTMB(Nombre_de_contacts_de_Pippip ~ rugosite_200m_filturb + espace_vol_libre_200m_filturb + prop_prairie_buf1000 
                                    + prop_urbain_buf250 + prop_eau_buf250 + length_haie_buf750 + distance_eau  + distance_veg + (1 | id), 
                                    data = Dredge_MRE,  family = nbinom2)

options(na.action = "na.fail")  # important pour que le dredge plante si il reste des NA (fausse les AIC etc)

Dredge_MRE_Pippip_results <- dredge(MOD_global_Pipip)
BestMOD_MRE_Pipip <- get.models(Dredge_MRE_Pippip_results, subset = delta < 2)

summary(BestMOD_MRE_Pipip[[1]]) 
summary(BestMOD_MRE_Pipip[[2]]) 
summary(BestMOD_MRE_Pipip[[3]]) 
summary(BestMOD_MRE_Pipip[[4]]) #Variable LiDAR significative 
summary(BestMOD_MRE_Pipip[[5]]) 
summary(BestMOD_MRE_Pipip[[6]]) 
summary(BestMOD_MRE_Pipip[[7]]) 
summary(BestMOD_MRE_Pipip[[8]]) 
summary(BestMOD_MRE_Pipip[[9]]) 
summary(BestMOD_MRE_Pipip[[10]]) 
summary(BestMOD_MRE_Pipip[[11]]) 
summary(BestMOD_MRE_Pipip[[12]]) 

residuals_MOD_MRE_Pippip<-DHARMa::simulateResiduals(BestMOD_MRE_Pipip[[4]])
plot(residuals_MOD_MRE_Pippip) 

#Résidus beaucoup plus conformes que le modèle 0-inflated avec toutes les MRE, on conserve donc celui-ci (validé par Mary)

# 3. Modélisation de la guilde LRE ####
# 3.1 Dredge : on met toutes les variables sélectionnées et ça calcule automatiquement le(s) meilleur(s) modèles ####

vars_dredge <- c("nombre_de_contacts_LRE", "rugosite_200m_filturb","espace_vol_libre_200m_filturb", "prop_prairie_buf1000", 
                 "prop_urbain_buf250","prop_eau_buf250", "length_haie_buf750", "distance_eau", "distance_veg", "id")

Dredge_LRE <- na.omit(JDD_VC_LiDAR_OSO_VF_08_2025_scaled[, vars_dredge]) # On garde uniquement les lignes complètes sur ces colonnes pour pas faire buger le dredge

MOD_global_LRE <- glmmTMB(nombre_de_contacts_LRE ~ rugosite_200m_filturb + espace_vol_libre_200m_filturb + prop_prairie_buf1000 
                          + prop_urbain_buf250 + prop_eau_buf250 + length_haie_buf750 + distance_eau  + distance_veg + (1 | id), 
                          data = Dredge_LRE,  family = nbinom2)

options(na.action = "na.fail")  # important pour que le dredge plante si il reste des NA (fausse les AIC etc)

Dredge_LRE_results <- dredge(MOD_global_LRE)
head(Dredge_LRE_results)  # Affiche les meilleurs modèles par AIC
BestMOD_LRE <- get.models(Dredge_LRE_results,subset = delta < 2)
summary(BestMOD_LRE[[1]])  #Meilleur modèle au sens strict, les autres ont tous un delta AIC < 2
summary(BestMOD_LRE[[2]])  
summary(BestMOD_LRE[[3]])  
summary(BestMOD_LRE[[4]])  
summary(BestMOD_LRE[[5]])  
summary(BestMOD_LRE[[6]])  
summary(BestMOD_LRE[[7]])  
summary(BestMOD_LRE[[8]])  

vars_LRE <- names(fixef(BestMOD_LRE[[1]])$cond) #correction=true pour corriger la non prise en compte de l'effet aléatoire dans les effets marginaux
vars_LRE <- vars_LRE[vars_LRE != "(Intercept)"] #On garde toutes les variables sauf l'intercept

plots_LRE <- lapply(vars_LRE, function(v) { # Générer un plot pour chaque variable
  ggpredict(BestMOD_LRE[[1]], terms = v, bias_correction = TRUE) %>% 
    plot() + 
    ggtitle(paste("Effet de", v)) +
    theme_minimal()
})

combined_plot_LRE <- wrap_plots(plots_LRE, ncol = 2)  # Combiner tous les plots sur une seule page
print(combined_plot_LRE)

r2_nakagawa(BestMOD_LRE[[1]])

residuals_MOD_LRE<-DHARMa::simulateResiduals(BestMOD_LRE[[1]])  #Visualiser les résidus
plot(residuals_MOD_LRE) 

# 3.2. Test autocorrélation spatiale : test sur les uniques car pour DHARMa il faut seulement une observation par site hors on a plusieurs nuits par site ####

ape::Moran.I(jdd_unique_pt_wNA$`nombre de contacts LRE`, jdd_unique_pt_dist_inv) #Test autocorrélation spatiale --> pas d'autocorrélation spatiale 
#Modèle validé


# 4. Récapitulation des modèles finaux retenues réécrits et élements associés ####

BESTMODEL_SRE <- glmmTMB(nombre_de_contacts_SRE ~ distance_eau + distance_veg + espace_vol_libre_200m_filturb + prop_eau_buf250 + 
prop_prairie_buf1000 + prop_urbain_buf250 + rugosite_200m_filturb + (1 | id), data = Dredge_SRE, family = nbinom2)
summary(BESTMODEL_SRE)  
residuals_MOD_SRE<-DHARMa::simulateResiduals(BestMOD_SRE[[1]],quantreg=T)  #Visualiser les résidus (ça se fait sur le modele non centré et non réduit)
plot(residuals_MOD_SRE) 
r2_nakagawa(BESTMODEL_SRE)

BESTMODEL_MRE_SAUFPIPPIP <- glmmTMB(Nombre_de_contacts_de_MRE_Pippip_exclu ~ prop_eau_buf250 + prop_prairie_buf1000 + (1 | id), data = Dredge_MRE, family = nbinom2)
summary(BESTMODEL_MRE_SAUFPIPPIP) 
residuals_MOD_MRE_SAUFPIPPIP<-DHARMa::simulateResiduals(BestMOD_MRE_withoutPipip[[1]])  
plot(residuals_MOD_MRE_SAUFPIPPIP)
r2_nakagawa(BESTMODEL_MRE_SAUFPIPPIP) 

BESTMODEL_MRE_JUSTEPIPPIP<-glmmTMB(Nombre_de_contacts_de_Pippip ~ distance_eau + prop_eau_buf250 + prop_prairie_buf1000 + prop_urbain_buf250 + rugosite_200m_filturb + (1|id), family=nbinom2,data=Dredge_MRE)
summary(BESTMODEL_MRE_JUSTEPIPPIP) 
residuals_MOD_MRE_JUSTEPIPPIP<-DHARMa::simulateResiduals(BestMOD_MRE_Pipip[[4]])  
plot(residuals_MOD_MRE_JUSTEPIPPIP)
r2_nakagawa(BESTMODEL_MRE_JUSTEPIPPIP) 

BESTMODEL_LRE <- glmmTMB(nombre_de_contacts_LRE ~  distance_eau + espace_vol_libre_200m_filturb + prop_eau_buf250 +
prop_prairie_buf1000 + rugosite_200m_filturb + (1 | id), data = Dredge_LRE, family = nbinom2)
summary(BESTMODEL_LRE)  
residuals_MOD_LRE<-DHARMa::simulateResiduals(BestMOD_LRE[[5]])  
plot(residuals_MOD_LRE) 
r2_nakagawa(BESTMODEL_LRE)





