#      /\                 /\    ╔================================================================╗
#     ( \'.__   (\_/)   __.'/ ) ║ 06 - Script de la modélisation du JDD DataPip (présoutenance)  ║ 
#      \_.'  '--('.')--'  '._/  ╚================================================================╝
#          `\=/ " \=/`
#           (/|\)

library(glmmTMB)
library(MuMIn)
library(knitr)
library(kableExtra)
library(dplyr)
library(DHARMa)
library(tidyr)

# 1. Sélection de modèles ####

#Deux sélections de modèles univariés pour voir quelle variable explique le mieux le nb contacts dans les 2 buffers 

# Modèle nul
MOD__NUL <- glmmTMB(nb_contacts ~ 1 + (1 | id), data = DataPip, family = nbinom2)

# Modèles 200 m
MOD_200_hauteur   <- glmmTMB(nb_contacts ~ hauteurmoyenne_200m + (1 | id), data = DataPip, family = nbinom2)
MOD_200_rugosite  <- glmmTMB(nb_contacts ~ rugosite_200m + (1 | id), data = DataPip, family = nbinom2)
MOD_200_ouverture <- glmmTMB(nb_contacts ~ ouverture_canopee_200m + (1 | id), data = DataPip, family = nbinom2)
MOD_200_densite   <- glmmTMB(nb_contacts ~ densite_arbre_200m + (1 | id), data = DataPip, family = nbinom2)
MOD_200_FHD       <- glmmTMB(nb_contacts ~ FHD_200m + (1 | id), data = DataPip, family = nbinom2)

# Modèles 500 m
MOD_500_hauteur   <- glmmTMB(nb_contacts ~ hauteurmoyenne_500m + (1 | id), data = DataPip, family = nbinom2)
MOD_500_rugosite  <- glmmTMB(nb_contacts ~ rugosite_500m + (1 | id), data = DataPip, family = nbinom2)
MOD_500_ouverture <- glmmTMB(nb_contacts ~ ouverture_canopee_500m + (1 | id), data = DataPip, family = nbinom2)
MOD_500_densite   <- glmmTMB(nb_contacts ~ densite_arbre_500m + (1 | id), data = DataPip, family = nbinom2)
MOD_500_FHD       <- glmmTMB(nb_contacts ~ FHD_500m + (1 | id), data = DataPip, family = nbinom2)


# 2. Générer une table d'AIC pour visualiser les meilleurs modèles ####

# Fonction utilitaire pour générer les stats AICc
get_aic_table <- function(model_list, model_names, buffer_label) {
  logL <- sapply(model_list, function(m) as.numeric(logLik(m)))
  K <- sapply(model_list, function(m) attr(logLik(m), "df"))
  aicc <- sapply(model_list, AICc)
  delta <- aicc - min(aicc)
  poids <- exp(-0.5 * delta) / sum(exp(-0.5 * delta))
  
  data.frame(
    Buffer = buffer_label,
    Modèle = model_names,
    K = K,
    LogVraisemblance = round(logL, 2),
    AICc = round(aicc, 2),
    Delta_AICc = round(delta, 2),
    Poids_AICc = round(poids, 3)
  )
}

# Mettre en forme les modèles et leurs noms
model_list_200 <- list(MOD__NUL, MOD_200_hauteur, MOD_200_rugosite, MOD_200_ouverture, MOD_200_densite, MOD_200_FHD)
model_names_200 <- c("Modèle nul", "Hauteur moyenne (200 m)", "Rugosité (200 m)", "Ouverture canopée (200 m)", "Densité arbres (200 m)", "FHD (200 m)")

model_list_500 <- list(MOD__NUL, MOD_500_hauteur, MOD_500_rugosite, MOD_500_ouverture, MOD_500_densite, MOD_500_FHD)
model_names_500 <- c("Modèle nul", "Hauteur moyenne (500 m)", "Rugosité (500 m)", "Ouverture canopée (500 m)", "Densité arbres (500 m)", "FHD (500 m)")

# Obtenir la table d'AIC 
aic_table_200 <- get_aic_table(model_list_200, model_names_200, buffer_label = "200 m")
aic_table_500 <- get_aic_table(model_list_500, model_names_500, buffer_label = "500 m")

# Fusion et tri de la table 
aic_table_total <- bind_rows(aic_table_200, aic_table_500) %>%
  arrange(Buffer, AICc)

# Indices des meilleurs modèles
best_200_index <- which(aic_table_total$Buffer == "200 m")[which.min(aic_table_total$AICc[aic_table_total$Buffer == "200 m"])]
best_500_index <- which(aic_table_total$Buffer == "500 m")[which.min(aic_table_total$AICc[aic_table_total$Buffer == "500 m"])]

# Indice du modèles nul
null_model_indices <- which(aic_table_total$Modèle == "Modèle nul")

# Mise en forme table AIC
aic_table_total %>%
  kable("html", align = "lcccccc", caption = "Tableau comparatif des modèles univariés selon les buffers (200 m et 500 m)") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
  row_spec(which(aic_table_total$Buffer == "200 m"), background = "#d5f5e3") %>%
  row_spec(which(aic_table_total$Buffer == "500 m"), background = "#d6eaf8") %>%
  row_spec(null_model_indices, background = "#e0e0e0") %>%  # gris sur le modèle nul
  row_spec(c(best_200_index, best_500_index), bold = TRUE) %>%
  collapse_rows(columns = 1, valign = "middle")

# 3. Etude des résidus et du r² ####

#D'abord je centre et réduis les variables explicatives pour que les effets marginaux deviennent les effets quand les autres variables = leur moyenne et non pas 0 comme quand ce n'est pas cr

DataPip <- DataPip %>%
  mutate(
    rugosite_200m_cr = scale(rugosite_200m)[, 1],
    rugosite_500m_cr = scale(rugosite_500m)[, 1])

# Meilleur modèle 200m
MOD_200_rugosite_cr    <- glmmTMB(nb_contacts ~ rugosite_200m_cr + (1 | id), data = DataPip, family = nbinom2)
summary(MOD_200_rugosite_cr)
residuals200cr<-simulateResiduals(MOD_200_rugosite_cr)  
plot(residuals200cr) #Visualiser les résidus

# Meilleur modèle 500m
MOD_500_rugosite_cr    <- glmmTMB(nb_contacts ~ rugosite_500m_cr + (1 | id), data = DataPip, family = nbinom2)
summary(MOD_500_rugosite_cr)
residuals500cr<-simulateResiduals(MOD_500_rugosite_cr)  
plot(residuals500cr) 

#r² des meilleurs modèles
r2_MOD_200 <- r.squaredGLMM(MOD_200_rugosite)
r2_MOD_500 <- r.squaredGLMM(MOD_500_rugosite)

print(r2_MOD_200)
print(r2_MOD_500)