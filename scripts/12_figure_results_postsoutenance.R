#      /\                 /\    ╔===============================================================╗
#     ( \'.__   (\_/)   __.'/ ) ║ 12 - Script de la production de figures pour le rapport final ║ 
#      \_.'  '--('.')--'  '._/  ╚===============================================================╝
#          `\=/ " \=/`
#           (/|\)

library(ggeffects)
library(ggplot2)
library(tidyr)
library(dplyr)
library(MuMIn)
library(AICcmodavg)
library(kableExtra)
library(performance)
library(stringr)
library(gridExtra)
library(broom.mixed)

# 1. Representation des effets marginaux pour chaque modèle retenu ####

# 1.1. Effets marginaux SRE ####

# Variables à représenter (toutes sauf prop_urbain_buf250)
vars_to_plot <- c("distance_eau", "distance_veg", "espace_vol_libre_200m_filturb", 
                  "prop_eau_buf250", "prop_prairie_buf1000", "rugosite_200m_filturb")

# Labels personnalisés
labels <- c(
  "distance_eau" = "Distance à l'eau",
  "distance_veg" = "Distance à la végétation",
  "espace_vol_libre_200m_filturb" = "Espace de vol libre (200 m)",
  "prop_eau_buf250" = "Proportion d'eau (250 m)",
  "prop_prairie_buf1000" = "Proportion de prairies (1000 m)",
  "rugosite_200m_filturb" = "Rugosité de la canopée (200 m)"
)

colors <- c(
  "distance_eau" = "steelblue4",
  "distance_veg" = "darkolivegreen4",
  "espace_vol_libre_200m_filturb" = "darkmagenta",
  "prop_eau_buf250" = "blue4",
  "prop_prairie_buf1000" = "darkgoldenrod3",
  "rugosite_200m_filturb" = "chocolate4"
)

# Générer chaque plot manuellement
p1 <- {
  pred <- ggpredict(BESTMODEL_SRE, terms = "distance_eau")
  ggplot(pred, aes(x = x, y = predicted)) +
    geom_line(color = colors["distance_eau"], size = 1) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = colors["distance_eau"], alpha = 0.3) +
    labs(x = labels["distance_eau"], y = "Nombre de contacts SRE prédit") +
    theme_minimal()
}

p2 <- {
  pred <- ggpredict(BESTMODEL_SRE, terms = "distance_veg")
  ggplot(pred, aes(x = x, y = predicted)) +
    geom_line(color = colors["distance_veg"], size = 1) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = colors["distance_veg"], alpha = 0.3) +
    labs(x = labels["distance_veg"], y = "Nombre de contacts SRE prédit") +
    theme_minimal()
}

p3 <- {
  pred <- ggpredict(BESTMODEL_SRE, terms = "espace_vol_libre_200m_filturb")
  ggplot(pred, aes(x = x, y = predicted)) +
    geom_line(color = colors["espace_vol_libre_200m_filturb"], size = 1) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = colors["espace_vol_libre_200m_filturb"], alpha = 0.3) +
    labs(x = labels["espace_vol_libre_200m_filturb"], y = "Nombre de contacts SRE prédit") +
    theme_minimal()
}

p4 <- {
  pred <- ggpredict(BESTMODEL_SRE, terms = "prop_eau_buf250")
  ggplot(pred, aes(x = x, y = predicted)) +
    geom_line(color = colors["prop_eau_buf250"], size = 1) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = colors["prop_eau_buf250"], alpha = 0.3) +
    labs(x = labels["prop_eau_buf250"], y = "Nombre de contacts SRE prédit") +
    theme_minimal()
}

p5 <- {
  pred <- ggpredict(BESTMODEL_SRE, terms = "prop_prairie_buf1000")
  ggplot(pred, aes(x = x, y = predicted)) +
    geom_line(color = colors["prop_prairie_buf1000"], size = 1) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = colors["prop_prairie_buf1000"], alpha = 0.3) +
    labs(x = labels["prop_prairie_buf1000"], y = "Nombre de contacts SRE prédit") +
    theme_minimal()
}

p6 <- {
  pred <- ggpredict(BESTMODEL_SRE, terms = "rugosite_200m_filturb")
  ggplot(pred, aes(x = x, y = predicted)) +
    geom_line(color = colors["rugosite_200m_filturb"], size = 1) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = colors["rugosite_200m_filturb"], alpha = 0.3) +
    labs(x = labels["rugosite_200m_filturb"], y = "Nombre de contacts SRE prédit") +
    theme_minimal()
}

# Afficher en grille 2 colonnes
library(gridExtra)
grid.arrange(p1, p2, p3, p4, p5, p6, ncol = 2)









# 1.2. Effets marginaux MRE sauf Pippip ####

# Variables à représenter
vars_to_plot <- c("prop_eau_buf250", "prop_prairie_buf1000")

# Labels personnalisés
labels <- c(
  "prop_eau_buf250" = "Proportion d'eau (250 m)",
  "prop_prairie_buf1000" = "Proportion de prairies (1000 m)"
)

# Couleurs personnalisées
colors <- c(
  "prop_eau_buf250" = "blue4",
  "prop_prairie_buf1000" = "darkgoldenrod3"
)

# Plot pour prop_eau_buf250
p1 <- {
  pred <- ggpredict(BESTMODEL_MRE_SAUFPIPPIP, terms = "prop_eau_buf250")
  ggplot(pred, aes(x = x, y = predicted)) +
    geom_line(color = colors["prop_eau_buf250"], size = 1) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = colors["prop_eau_buf250"], alpha = 0.3) +
    labs(x = labels["prop_eau_buf250"], y = "Nombre de contacts  de MRE (sauf Pipistrellus pipistrellus) prédit") +
    theme_minimal()
}

# Plot pour prop_prairie_buf1000
p2 <- {
  pred <- ggpredict(BESTMODEL_MRE_SAUFPIPPIP, terms = "prop_prairie_buf1000")
  ggplot(pred, aes(x = x, y = predicted)) +
    geom_line(color = colors["prop_prairie_buf1000"], size = 1) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = colors["prop_prairie_buf1000"], alpha = 0.3) +
    labs(x = labels["prop_prairie_buf1000"], y = "Nombre de contacts  de MRE (sauf Pipistrellus pipistrellus) prédit") +
    theme_minimal()
}

# Affichage côte à côte
grid.arrange(p1, p2, ncol = 2)




# 1.3. Effets marginaux juste Pippip ####

# Variables à représenter (les 5 variables du modèle)
vars_to_plot <- c("distance_eau", "prop_eau_buf250", "prop_prairie_buf1000", "prop_urbain_buf250", "rugosite_200m_filturb")

# Labels personnalisés
labels <- c(
  "distance_eau" = "Distance à l'eau",
  "prop_eau_buf250" = "Proportion d'eau (250 m)",
  "prop_prairie_buf1000" = "Proportion de prairies (1000 m)",
  "prop_urbain_buf250" = "Proportion de surfaces urbanisées (250 m)",
  "rugosite_200m_filturb" = "Rugosité de la canopée (200 m)"
)

# Couleurs personnalisées
colors <- c(
  "distance_eau" = "steelblue4",
  "prop_eau_buf250" = "blue4",
  "prop_prairie_buf1000" = "darkgoldenrod3",
  "prop_urbain_buf250" = "cornsilk3",
  "rugosite_200m_filturb" = "chocolate4"
)

# Générer les plots pour chaque variable
plots <- lapply(vars_to_plot, function(var) {
  pred <- ggpredict(BESTMODEL_MRE_JUSTEPIPPIP, terms = var)
  ggplot(pred, aes(x = x, y = predicted)) +
    geom_line(color = colors[var], size = 1) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = colors[var], alpha = 0.3) +
    labs(x = labels[var], y = "Nombre de contacts Pippip prédit") +
    theme_minimal()
})

# Afficher les plots en grille (2 colonnes)
grid.arrange(grobs = plots, ncol = 2)









# 1.4. ffets marginaux LRE ####

# Variables significatives à représenter
vars_to_plot <- c("espace_vol_libre_200m_filturb", "prop_eau_buf250", "prop_prairie_buf1000", "rugosite_200m_filturb")

# Labels personnalisés
labels <- c(
  "espace_vol_libre_200m_filturb" = "Espace de vol libre (200 m)",
  "prop_eau_buf250" = "Proportion d'eau (250 m)",
  "prop_prairie_buf1000" = "Proportion de prairies (1000 m)",
  "rugosite_200m_filturb" = "Rugosité de la canopée (200 m)"
)

# Couleurs personnalisées
colors <- c(
  "espace_vol_libre_200m_filturb" = "darkmagenta",
  "prop_eau_buf250" = "blue4",
  "prop_prairie_buf1000" = "darkgoldenrod3",
  "rugosite_200m_filturb" = "chocolate4"
)

# Générer les plots
plots <- lapply(vars_to_plot, function(var) {
  pred <- ggpredict(BESTMODEL_LRE, terms = var)
  ggplot(pred, aes(x = x, y = predicted)) +
    geom_line(color = colors[var], size = 1) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = colors[var], alpha = 0.3) +
    labs(x = labels[var], y = "Nombre de contacts de LRE prédit") +
    theme_minimal()
})

# Affichage en grille 2 colonnes
grid.arrange(grobs = plots, ncol = 2)








# 2. Génération des tables AIC pour comparer les sélections de modèle avec ΔAIC < 2 ####

# 2.1. Table AIC SRE ####

model_names <- sapply(BestMOD_SRE, function(m) { # Noms = variables incluses dans chaque modèle
  vars <- names(fixef(m)$cond)
  vars <- vars[vars != "(Intercept)"]
  paste(vars, collapse = " + ")
})

# Table AIC
aic_table <- aictab(cand.set = BestMOD_SRE, modnames = model_names) %>%
  as.data.frame()

# Calcul uniquement du R² Nakagawa
r2_cond <- sapply(BestMOD_SRE, function(m) {
  r2_nakagawa(m)$R2_conditional
})
r2_cond <- round(r2_cond, 3) # Arrondir à 3 décimales pour le visuel
aic_table$R2 <- r2_cond # Ajouter à la table

# Nettoyage et changement de nom des colonnes
aic_table$LL <- NULL
aic_table$Cum.Wt <- NULL
rownames(aic_table) <- NULL
colnames(aic_table) <- c(
  "Variables incluses",
  "K",
  "AICc",
  "ΔAICc",
  "Poids AICc",
  "Log-vraisemblance",
  "R²"
)

kable(aic_table, # Mise en forme de la table
      booktabs = TRUE,
      align = c("l", "c", "c", "c", "c", "c", "c"),
      caption = "Comparaison des meilleurs modèles (ΔAICc < 2) pour la guilde SRE") %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE,
    position = "center"
  ) %>%
  row_spec(0, bold = TRUE) %>%  # en-tête en gras
  row_spec(1, bold = TRUE) %>%  # premier modèle en gras
  column_spec(1, width = "7cm") %>%
  column_spec(3:7, width = "3cm") %>%
  group_rows("Nb de contacts par nuit", 1, nrow(aic_table))

kable(aic_table, #Mise en forme avec kableExtra
      booktabs = TRUE,
      align = c("l", "c", "c", "c", "c", "c", "c", "c"),
      caption = "Comparaison des meilleurs modèles (ΔAICc < 2) pour la guilde SRE") %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE,
    position = "center"
  ) %>%
  row_spec(0, bold = TRUE) %>%  # en-tête
  row_spec(1, bold = TRUE) %>%  # meilleur modèle rentenu en gras (1er ici)
  column_spec(1, width = "12cm") %>%
  column_spec(3:7, width = "3cm") %>%
  group_rows("Nb de contacts par nuit", 1, nrow(aic_table))


# 2.2. Table AIC MRE sauf Pippip ####

# Noms = variables incluses dans chaque modèle
model_names_MRE <- sapply(BestMOD_MRE_withoutPipip, function(m) {
  vars <- names(fixef(m)$cond)
  vars <- vars[vars != "(Intercept)"]
  paste(vars, collapse = " + ")
})

# Table AIC
aic_table_MRE <- aictab(cand.set = BestMOD_MRE_withoutPipip, modnames = model_names_MRE) %>%
  as.data.frame()

# Calcul du R² conditionnel Nakagawa (arrondi à 3 décimales)
r2_cond_MRE <- sapply(BestMOD_MRE_withoutPipip, function(m) {
  round(r2_nakagawa(m)$R2_conditional, 3)
})

# Ajouter à la table
aic_table_MRE$R2 <- r2_cond_MRE

# Nettoyage et renommage colonnes
aic_table_MRE$LL <- NULL
aic_table_MRE$Cum.Wt <- NULL
rownames(aic_table_MRE) <- NULL
colnames(aic_table_MRE) <- c(
  "Variables incluses",
  "K",
  "AICc",
  "ΔAICc",
  "Poids AICc",
  "Log-vraisemblance",
  "R²"
)

# Mise en forme de la table
kable(aic_table_MRE,
      booktabs = TRUE,
      align = c("l", "c", "c", "c", "c", "c", "c"),
      caption = "Comparaison des meilleurs modèles (ΔAICc < 2) pour la guilde MRE sans Pippip") %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE,
    position = "center"
  ) %>%
  row_spec(0, bold = TRUE) %>%  # en-tête en gras
  row_spec(1, bold = TRUE) %>%  # meilleur modèle en gras
  column_spec(1, width = "12cm") %>%
  column_spec(3:7, width = "3cm") %>%
  group_rows("Nb de contacts par nuit", 1, nrow(aic_table_MRE))

# 2.3. Table AIC Pippip ####

# Noms = variables incluses dans chaque modèle
model_names_Pipip <- sapply(BestMOD_MRE_Pipip, function(m) {
  vars <- names(fixef(m)$cond)
  vars <- vars[vars != "(Intercept)"]
  paste(vars, collapse = " + ")
})

# Table AIC
aic_table_Pipip <- aictab(cand.set = BestMOD_MRE_Pipip, modnames = model_names_Pipip) %>%
  as.data.frame()

# Calcul du R² conditionnel Nakagawa (arrondi 3 décimales)
r2_cond_Pipip <- sapply(BestMOD_MRE_Pipip, function(m) {
  round(r2_nakagawa(m)$R2_conditional, 3)
})

# Ajouter colonne R²
aic_table_Pipip$R2 <- r2_cond_Pipip

# Nettoyage et renommage colonnes
aic_table_Pipip$LL <- NULL
aic_table_Pipip$Cum.Wt <- NULL
rownames(aic_table_Pipip) <- NULL
colnames(aic_table_Pipip) <- c(
  "Variables incluses",
  "K",
  "AICc",
  "ΔAICc",
  "Poids AICc",
  "Log-vraisemblance",
  "R²"
)

# Mise en forme finale de la table
kable(aic_table_Pipip,
      booktabs = TRUE,
      align = c("l", rep("c", ncol(aic_table_Pipip) - 1)),
      caption = "Comparaison des meilleurs modèles (ΔAICc < 2) pour la guilde MRE Pippip") %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE,
    position = "center"
  ) %>%
  row_spec(0, bold = TRUE) %>%  # En-tête en gras
  row_spec(4, bold = TRUE) %>%  # Meilleur modèle en gras
  column_spec(1, width = "18cm") %>%
  column_spec(3:7, width = "3cm") %>%
  group_rows("Nb de contacts par nuit", 1, nrow(aic_table_Pipip))

# 2.4. Table AIC LRE ####

# Création noms des modèles (variables incluses)
model_names_LRE <- sapply(BestMOD_LRE, function(m) {
  vars <- names(fixef(m)$cond)
  vars <- vars[vars != "(Intercept)"]
  paste(vars, collapse = " + ")
})

# Table AIC avec R² conditionnel
aic_table_LRE <- aictab(cand.set = BestMOD_LRE, modnames = model_names_LRE) %>% as.data.frame()

# Calcul R² conditionnel Nakagawa (arrondi)
r2_cond_LRE <- sapply(BestMOD_LRE, function(m) {
  round(r2_nakagawa(m)$R2_conditional, 3)
})

aic_table_LRE$R2 <- r2_cond_LRE

# Nettoyage colonnes inutiles
aic_table_LRE$LL <- NULL
aic_table_LRE$Cum.Wt <- NULL
rownames(aic_table_LRE) <- NULL

# Renommer colonnes
colnames(aic_table_LRE) <- c(
  "Variables incluses",
  "K",
  "AICc",
  "ΔAICc",
  "Poids AICc",
  "Log-vraisemblance",
  "R²"
)

# Mise en forme avec kableExtra
kable(aic_table_LRE,
      booktabs = TRUE,
      align = c("l", rep("c", ncol(aic_table_LRE) - 1)),
      caption = "Comparaison des meilleurs modèles (ΔAICc < 2) pour la guilde LRE") %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE,
    position = "center"
  ) %>%
  row_spec(0, bold = TRUE) %>%     # En-tête en gras
  row_spec(5, bold = TRUE) %>%     # Meilleur modèle en gras
  column_spec(1, width = "12cm") %>%  # Colonne variables plus large
  column_spec(3:7, width = "3cm") %>%
  group_rows("Nb de contacts par nuit", 1, nrow(aic_table_LRE))









# 3. Mise en forme des summary du modèle sélectionné pour chaque guilde ####

# 3.1. Summary meilleur modèle SRE ####

# Fonction d'extraction modifiée pour garder l'intercept et toutes les variables
extract_all_coefs <- function(model) {
  coefs <- summary(model)$coefficients$cond
  data.frame(
    Variable = rownames(coefs),
    Coefficient = coefs[, "Estimate"],
    Std_Error = coefs[, "Std. Error"],
    z_value = coefs[, "z value"],
    p_value = coefs[, "Pr(>|z|)"],
    row.names = NULL
  )
}

# Appliquer à ton modèle
stats_all <- extract_all_coefs(BESTMODEL_SRE)

# Fonction étoiles
add_stars <- function(p) {
  if (is.na(p)) return("")
  else if (p < 0.001) return("***")
  else if (p < 0.01) return("**")
  else if (p < 0.05) return("*")
  else return("")
}

# Ajouter les étoiles et arrondir p-value
stats_all <- stats_all %>%
  mutate(
    p_value_rounded = round(p_value, 4),
    p_value = paste0(formatC(p_value_rounded, format = "f", digits = 4), " ", sapply(p_value, add_stars)),
    Coefficient = round(Coefficient, 4),
    Std_Error = round(Std_Error, 4),
    z_value = round(z_value, 4)
  ) %>%
  select(Variable, Coefficient, Std_Error, z_value, p_value)

# Table finale avec kableExtra
kable(stats_all, 
      caption = "Résumé des coefficients du meilleur modèle SRE",
      booktabs = TRUE,
      align = c("l", "r", "r", "r", "c")) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE) %>%
  row_spec(0, bold = TRUE) 

# 3.2. Summary meilleur modèle MRE sauf Pippip ####

#Fonction pour extraire les coefficients
extract_all_coefs <- function(model) {
  coefs <- summary(model)$coefficients$cond
  data.frame(
    Variable = rownames(coefs),
    Coefficient = coefs[, "Estimate"],
    Std_Error = coefs[, "Std. Error"],
    z_value = coefs[, "z value"],
    p_value = coefs[, "Pr(>|z|)"],
    row.names = NULL
  )
}

# Extraire les stats pour ton modèle
stats_all <- extract_all_coefs(BESTMODEL_MRE_SAUFPIPPIP)

# Fonction pour ajouter des étoiles de significativité
add_stars <- function(p) {
  if (is.na(p)) return("")
  else if (p < 0.001) return("***")
  else if (p < 0.01) return("**")
  else if (p < 0.05) return("*")
  else return("")
}

# Ajouter les étoiles et arrondir les valeurs
stats_all <- stats_all %>%
  mutate(
    p_value_rounded = round(p_value, 4),
    p_value = paste0(formatC(p_value_rounded, format = "f", digits = 4),
                     " ", sapply(p_value, add_stars)),
    Coefficient = round(Coefficient, 4),
    Std_Error = round(Std_Error, 4),
    z_value = round(z_value, 4)
  ) %>%
  select(Variable, Coefficient, Std_Error, z_value, p_value)

# Afficher avec kableExtra
kable(stats_all, 
      caption = "Résumé des coefficients du meilleur modèle MRE (Pippip exclu)",
      booktabs = TRUE,
      align = c("l", "r", "r", "r", "c")) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = FALSE) %>%
  row_spec(0, bold = TRUE)

# 3.3. Summary meilleur modèle MRE juste Pippip ####

# Fonction pour extraire les coefficients du modèle
extract_all_coefs <- function(model) {
  coefs <- summary(model)$coefficients$cond
  data.frame(
    Variable = rownames(coefs),
    Coefficient = coefs[, "Estimate"],
    Std_Error = coefs[, "Std. Error"],
    z_value = coefs[, "z value"],
    p_value = coefs[, "Pr(>|z|)"],
    row.names = NULL
  )
}

# Appliquer à ton modèle
stats_all <- extract_all_coefs(BESTMODEL_MRE_JUSTEPIPPIP)

# Fonction pour ajouter des étoiles de significativité
add_stars <- function(p) {
  if (is.na(p)) return("")
  else if (p < 0.001) return("***")
  else if (p < 0.01) return("**")
  else if (p < 0.05) return("*")
  else return("")
}

# Ajouter étoiles + arrondis
stats_all <- stats_all %>%
  mutate(
    p_value_rounded = round(p_value, 4),
    p_value = paste0(formatC(p_value_rounded, format = "f", digits = 4), " ", sapply(p_value, add_stars)),
    Coefficient = round(Coefficient, 4),
    Std_Error = round(Std_Error, 4),
    z_value = round(z_value, 4)
  ) %>%
  select(Variable, Coefficient, Std_Error, z_value, p_value)

# Affichage avec kableExtra
kable(stats_all, 
      caption = "Résumé des coefficients du meilleur modèle MRE - Juste Pippip",
      booktabs = TRUE,
      align = c("l", "r", "r", "r", "c")) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE) %>%
  row_spec(0, bold = TRUE)

# 3.4. Summary meilleur modèle LRE ####

# Fonction pour extraire les coefficients du modèle
extract_all_coefs <- function(model) {
  coefs <- summary(model)$coefficients$cond
  data.frame(
    Variable = rownames(coefs),
    Coefficient = coefs[, "Estimate"],
    Std_Error = coefs[, "Std. Error"],
    z_value = coefs[, "z value"],
    p_value = coefs[, "Pr(>|z|)"],
    row.names = NULL
  )
}

# Appliquer à ton modèle LRE
stats_all_LRE <- extract_all_coefs(BESTMODEL_LRE)

# Fonction pour ajouter des étoiles de significativité
add_stars <- function(p) {
  if (is.na(p)) return("")
  else if (p < 0.001) return("***")
  else if (p < 0.01) return("**")
  else if (p < 0.05) return("*")
  else return("")
}

# Ajouter étoiles + arrondis
stats_all_LRE <- stats_all_LRE %>%
  mutate(
    p_value_rounded = round(p_value, 4),
    p_value = paste0(formatC(p_value_rounded, format = "f", digits = 4), " ", sapply(p_value, add_stars)),
    Coefficient = round(Coefficient, 4),
    Std_Error = round(Std_Error, 4),
    z_value = round(z_value, 4)
  ) %>%
  select(Variable, Coefficient, Std_Error, z_value, p_value)

# Affichage avec kableExtra
kable(stats_all_LRE, 
      caption = "Résumé des coefficients du meilleur modèle LRE",
      booktabs = TRUE,
      align = c("l", "r", "r", "r", "c")) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE) %>%
  row_spec(0, bold = TRUE)



#