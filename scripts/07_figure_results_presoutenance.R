#      /\                 /\    ╔===================================================================╗
#     ( \'.__   (\_/)   __.'/ ) ║ 07 - Script de la production de figures pour le rapport de stage  ║ 
#      \_.'  '--('.')--'  '._/  ╚===================================================================╝
#          `\=/ " \=/`
#           (/|\)

library(ggplot2)
library(dplyr)
library(lme4)
library(MuMIn) 
library(flextable)
library(magrittr)

# 1. Graphique ACP ####

graphACP <- fviz_pca_var(ACP1, repel = TRUE, col.var='#27ae60', labelsize=3,arrowsize=1.2,label.offset = 1.5)     

graphACP + 
  theme_minimal(base_size = 14) +  # Police plus grande, thème épuré
  theme(
    legend.position = "bottom",    # Légende en bas
    panel.grid = element_blank(),  # Suppression du quadrillage
    plot.title = element_text(hjust = 0.5), # Centrage du titre
    element_text(color='black'),
    plot.background = element_rect(fill = "transparent", color = NA),   
    panel.background = element_rect(fill = "transparent", color = NA))
    
ggsave(filename = "results/05_analysesdescriptives_presoutenance/ACP", plot = graphACP, bg = "transparent", width = 8, height = 6, dpi = 300)

# 2. Graphique des prédictions ####
# 2.1. Prédictions avec IC 95 % pour modèle 200 m ####

# Générer une séquence de rugosité centrée réduite pour prédire
new_data_200 <- data.frame(rugosite_200m_cr = seq(min(DataPip$rugosite_200m_cr, na.rm = TRUE),
                                                  max(DataPip$rugosite_200m_cr, na.rm = TRUE),
                                                  length.out = 100))

pred_200 <- predict(MOD_200_rugosite_cr, newdata = new_data_200, type = "response", se.fit = TRUE, re.form = NA)

# Ajouter résultats au tableau
new_data_200$fit <- pred_200$fit
new_data_200$se <- pred_200$se.fit
new_data_200$lower <- new_data_200$fit - 1.96 * new_data_200$se
new_data_200$upper <- new_data_200$fit + 1.96 * new_data_200$se
new_data_200$modele <- "200 m"
new_data_200$type <- "prédit"

# Renommer pour uniformité
preds_200 <- new_data_200 %>%
  rename(rugosite = rugosite_200m_cr,
         valeur = fit)

# 2.2. Prédictions avec IC 95 % pour modèle 500 m ####

new_data_500 <- data.frame(rugosite_500m_cr = seq(min(DataPip$rugosite_500m_cr, na.rm = TRUE),
                                                  max(DataPip$rugosite_500m_cr, na.rm = TRUE),
                                                  length.out = 100))

pred_500 <- predict(MOD_500_rugosite_cr, newdata = new_data_500, type = "response", se.fit = TRUE, re.form = NA)

new_data_500$fit <- pred_500$fit
new_data_500$se <- pred_500$se.fit
new_data_500$lower <- new_data_500$fit - 1.96 * new_data_500$se
new_data_500$upper <- new_data_500$fit + 1.96 * new_data_500$se
new_data_500$modele <- "500 m"
new_data_500$type <- "prédit"

preds_500 <- new_data_500 %>%
  rename(rugosite = rugosite_500m_cr,
         valeur = fit)

# 2.3. Données observées avec IC 95 % pour modèle 200 m ####

# Observations 200 m
obs_200 <- DataPip %>%
  select(rugosite_200m_cr, nb_contacts) %>%
  mutate(modele = "200 m", type = "observé") %>%
  rename(rugosite = rugosite_200m_cr, valeur = nb_contacts)

# 2.4. Données observées avec IC 95 % pour modèle 200 m ####

# Observations 500 m
obs_500 <- DataPip %>%
  select(rugosite_500m_cr, nb_contacts) %>%
  mutate(modele = "500 m", type = "observé") %>%
  rename(rugosite = rugosite_500m_cr, valeur = nb_contacts)

# Données d'observations fusionnées
observed_data <- bind_rows(obs_200, obs_500)

# Données prédictives fusionnées
predicted_data <- bind_rows(preds_200, preds_500)

# Graphique prédictions vs obs

ggplot() +
  # Points observés
  geom_point(data = observed_data, aes(x = rugosite, y = valeur, color = modele, shape = type),
             alpha = 0.8, size = 2) +
  
  # Rubans IC
  geom_ribbon(data = preds_200, aes(x = rugosite, ymin = lower, ymax = upper, fill = modele), alpha = 0.2) +
  geom_ribbon(data = preds_500, aes(x = rugosite, ymin = lower, ymax = upper, fill = modele), alpha = 0.2) +
  
  # Lignes prédictives
  geom_line(data = preds_200, aes(x = rugosite, y = valeur, color = modele), size = 1.2) +
  geom_line(data = preds_500, aes(x = rugosite, y = valeur, color = modele), size = 1.2) +
  
  # Esthétique
  scale_color_manual(values = c("200 m" = "#27ae60", "500 m" = "#2980b9")) +
  scale_fill_manual(values = c("200 m" = "#27ae60", "500 m" = "#2980b9")) +
  scale_shape_manual(values = c("prédit" = 16, "observé" = 1)) +
  
  labs(
    x = "Rugosité centrée réduite de la canopée",
    y = "Nombre de contacts de P.pip par nuit",
    color = "Modèle",
    fill = "Modèle",
    shape = "Type de donnée"
  ) +
  theme_minimal() +
  theme(text = element_text(size = 14))

# 3. Table coefficients généraux pour l'annexe 5 du rapport ####

# Extraire les informations du summary
extract_model_stats <- function(model, var_name, buffer) {
  coefs <- summary(model)$coefficients$cond
  data.frame(
    Variable = var_name,
    Buffer = buffer,
    Coefficient = coefs[2, "Estimate"],
    Std_Error = coefs[2, "Std. Error"],
    z_value = coefs[2, "z value"],
    p_value = coefs[2, "Pr(>|z|)"]
  )
}

# Liste des modèles
model_list_ordered <- list(
  MOD_200_rugosite, MOD_200_FHD, MOD_200_ouverture, MOD_200_densite, MOD_200_hauteur,
  MOD_500_rugosite, MOD_500_FHD, MOD_500_hauteur, MOD_500_ouverture, MOD_500_densite
)

# Noms variables + buffers
variable_names_ordered <- c(
  "Rugosité", "FHD", "Ouverture canopée", "Densité arbres", "Hauteur moyenne",
  "Rugosité", "FHD", "Hauteur moyenne", "Ouverture canopée", "Densité arbres"
)

buffers_ordered <- c(rep("200 m", 5), rep("500 m", 5))

# Extraire les stats
stats_all <- do.call(rbind, Map(extract_model_stats, model_list_ordered, variable_names_ordered, buffers_ordered))

# Colonne combinée
stats_all$`Paramètres du modèle` <- paste0(stats_all$Variable, " (", stats_all$Buffer, ")")

# Fonction étoiles pour les p-values
add_stars <- function(p) {
  if (is.na(p)) return("")
  else if (p < 0.001) return("***")
  else if (p < 0.01) return("**")
  else if (p < 0.05) return("*")
  else return("")
}

# Arrondir et créer la colonne p_value_etoiles
stats_all$p_value_rounded <- round(stats_all$p_value, 4)
stats_all$p_value_etoiles <- paste0(formatC(stats_all$p_value_rounded, format = "f", digits = 4), " ", sapply(stats_all$p_value, add_stars))

# Sélection pour le tableau (sans p_value simple)
stats_all_sel <- stats_all[, c("Paramètres du modèle", "Coefficient", "Std_Error", "z_value", "p_value_etoiles")]

# Arrondir les autres colonnes
stats_all_sel$Coefficient <- round(stats_all_sel$Coefficient, 4)
stats_all_sel$Std_Error <- round(stats_all_sel$Std_Error, 4)
stats_all_sel$z_value <- round(stats_all_sel$z_value, 4)

# Couleurs pour la trame
buffer_colors <- c("200 m" = "#d5f5e3", "500 m" = "#d6eaf8")

ft <- flextable(stats_all_sel) %>%
  set_header_labels(
    Coefficient = "Coefficient",
    Std_Error = "Erreur standard",
    z_value = "Valeur de z",
    p_value_etoiles = "Valeur de p"
  ) %>%
  # colorer le fond selon le buffer
  bg(i = which(grepl("\\(200 m\\)", stats_all_sel$`Paramètres du modèle`)), bg = buffer_colors["200 m"]) %>%
  bg(i = which(grepl("\\(500 m\\)", stats_all_sel$`Paramètres du modèle`)), bg = buffer_colors["500 m"]) %>%
  autofit()

print(ft)