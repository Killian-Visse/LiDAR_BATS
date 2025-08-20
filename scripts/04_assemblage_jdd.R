#      /\                 /\    ╔=================================================================╗
#     ( \'.__   (\_/)   __.'/ ) ║ 04 - Script d'assemblage des jeux de données chiros et LiDAR    ║ 
#      \_.'  '--('.')--'  '._/  ╚=================================================================╝
#          `\=/ " \=/`
#           (/|\)


#L'idée ici est de réaliser un code pour créer le jeu de données dont on va se servir pour les analyses stats. Il faut donc lier les variables des buffers aux données chiros des différents points d'écoute et enlever les variables inutiles pour la clarté

library(sf)
library(dplyr)
library(tidyr)


# 1. Jeu de données DataPip (présoutenance) ####

inherits(buffer500m, "sf") #Cette fonction sert à vérifier si nos objets sont bien au format spatial (essentiel pour la jointure)
st_crs(coordLAM93) #Et celle-ci sert à vérifier que tous les objets sont au même CRS

DataGui200 <- st_join(coordLAM93, buffer200m, join = st_within, left = TRUE) #On attache les variables du buffer 200m aux données chiros
save(DataGui200_500, file = 'results/04_assemblage_jdd/DataGui200_500.Rdata')DataGui200_500 <- st_join(DataGui200, buffer500m, join = st_within, left = TRUE, suffix = c("_200m", "_500m")) #On rajoute les variables 500m


DataPip <- Data200_500 %>% 
  st_drop_geometry() %>%   # <-- enlève la colonne geometry
  filter(espece == "Pippip") %>% #Garder uniquement les occurrences de Pippip
  filter(!is.na(hauteurmoyenne_200m)) %>% #Garder uniquement les occurrences dans notre zone d'étude en supprimant les poits hors zone d'étude 
  st_drop_geometry() %>%
  filter(nuit_complete == 'TRUE') %>%
  mutate(id = paste0(numero_carre, "_", point)) %>%
  select(-numero_carre, -point, -confiance_observateur,-confiance_validateur,-protocole,-type,-enregistreur,-type_micro,-hauteur_micro,-gite,-nom_gite,-observateur_rice,-id_obs,-type_suivi_gite,-probleme_micro,-idnum) #enlever var inutiles
DataPip <- subset(DataPip, hauteurmoyenne_200m != 0)
save(DataPip, file = 'results/04_assemblage_jdd/DataPip.Rdata')

# 2. Jeu de données sur les guildes (que variables LiDAR) ####
# JDD transitoire fait autour des dates de soutenance avant la réunion

DataGui_vf <- DataGui200_500 %>% 
  filter(nuit_complete == 'TRUE') %>%
  mutate(id = paste0(numero_carre, "_", point)) %>%
  mutate(guilde_echolocation = case_when(
    espece %in% c("Barbar","Myodau","MyoGT","Pleaus","Rhihip","Myocap","Myoema","Myonat","Rhifer","Plemac","Myomys","Rhieur","Myoalc","Pleaur","Myobec","Myodas","Myosp","Plesp", "Rhisp") ~ "LRE",  
    espece %in% c("Hypsav","Minsch","Pipkuh", "Pipnat", "Pippip","Pippyg","Pipmad","Pipsp") ~ "LRE",         
    espece %in% c("Eptser","Nyclei", "Tadten","Nycnoc","Eptnil","Nyclas","Eptisa","Vesmur") ~ "LRE",                                
    TRUE ~ NA_character_
  )) %>%
  select(-numero_carre,-num_micro,-nb_contacts_nd, -point, -confiance_observateur, -confiance_validateur, -protocole, -type, 
         -type_micro, -hauteur_micro, -gite, -nom_gite, -observateur_rice, -id_obs, 
         -type_suivi_gite, -probleme_micro, -idnum,-proportion_sol_nu_200m,-proportion_sol_nu_500m,-proportion_exposee_200m,-proportion_exposee_500m) 
  
DataGui_vf <- subset(DataGui_vf, hauteurmoyenne_200m_filturb != 0)
save(DataGui_vf, file = 'results/05_assemblage_jddfinal/DataGui_vf.Rdata')


# 3. Jeu de données séparé par guilde avec variables LiDAR (filtre urbain) + variables OSO (partie avancée du stage) ####


DataGui_vf <- DataGui200_500 %>% 
  filter(nuit_complete == 'TRUE') %>%
  mutate(id = paste0(numero_carre, "_", point)) %>%
  mutate(guilde_echolocation = case_when(
    espece %in% c("Barbar","Myodau","MyoGT","Pleaus","Rhihip","Myocap","Myoema","Myonat","Rhifer","Plemac","Myomys","Rhieur","Myoalc","Pleaur","Myobec","Myodas","Myosp","Plesp", "Rhisp") ~ "LRE",  
    espece %in% c("Hypsav","Minsch","Pipkuh", "Pipnat", "Pippip","Pippyg","Pipmad","Pipsp") ~ "LRE",         
    espece %in% c("Eptser","Nyclei", "Tadten","Nycnoc","Eptnil","Nyclas","Eptisa","Vesmur") ~ "LRE",                                
    TRUE ~ NA_character_
  )) %>%
  dplyr::select(-numero_carre,-num_micro,-nb_contacts_nd, -point, -confiance_observateur, -confiance_validateur, -protocole, -type, 
         -type_micro, -hauteur_micro, -gite, -nom_gite, -observateur_rice, -id_obs, 
         -type_suivi_gite, -probleme_micro, -idnum,-proportion_sol_nu_200m_filturb,-proportion_sol_nu_500m_filturb,-proportion_exposee_200m_filturb,-proportion_exposee_500m) 


# 4. Création du JDD final avec par site, SRE MRE LRE et les variables LiDAR/OSO ####

#Guilde LRE #Par exemple pour la guilde LRE 

# Alléger l'objet en lui faisant perdre sa géometrie (6go de données stockées sinon)
DataGui_LiDAR_2D_filt_urb_VF <- DataGui_LiDAR_2D_filt_urb_VF %>% 
  st_drop_geometry() 
                                  
# Créer une table de référence avec tous les couples Nuit-id pour conserver les nuits ou nb_contacts = 0
ref_Nuit_ID <- DataGui_LiDAR_2D_filt_urb_VF %>%
  group_by(Nuit, id) %>%
  summarise(
    across(
      .cols = -c(guilde_echolocation, espece, nb_contacts),
      .fns = ~ first(.x),
      .names = "{.col}"
    ),
    .groups = "drop"
  )

DataLRE_contacts <- DataGui_LiDAR_2D_filt_urb_VF %>%
  select(-"hauteurmoyenne_200m", -"hauteurmoyenne_500m",
         -"rugosite_200m", -"rugosite_500m",
         -"ouverture_canopee_200m", -"ouverture_canopee_500m",
         -"densite_arbre_200m", -"densite_arbre_500m",
         -"FHD_200m", -"FHD_500m", -"espace_vol_libre_200m", -"espace_vol_libre_500m",
         -"recouvrement_herbacee_200m", -"recouvrement_herbacee_500m",
         -"recouvrement_arbustive_basse_200m", -"recouvrement_arbustive_basse_500m",
         -"trouees_canopee_200m", -"trouees_canopee_500m") %>%
  filter(guilde_echolocation == "LRE") %>%
  
  # Regrouper par Nuit, id, espece pour éviter doublons, garder 1 valeur par espece car présence de doublons
  group_by(Nuit, id, espece) %>%
  summarise(
    nb_contacts = first(nb_contacts),
    .groups = "drop"
  ) %>%
  
  # Regrouper par Nuit et id pour sommer les contacts sur toutes les espèces de la guilde
  group_by(Nuit, id) %>%
  summarise(
    nb_contacts = sum(nb_contacts, na.rm = TRUE),
    .groups = "drop"
  )

# Fusionner avec la table de référence pour ajouter les absences (nb_contacts = 0)
DataLRE <- ref_Nuit_ID %>%
  left_join(DataLRE_contacts, by = c("Nuit", "id")) %>%
  mutate(
    nb_contacts = tidyr::replace_na(nb_contacts, 0), # Remplir les absences
    espece = "LRE"                                   # Remplacer la colonne espece par LRE
  ) %>%
  select(Nuit, id, nb_contacts, everything()) %>%
  filter(!enregistreur %in% c("Batcorder", "Batlogger", "Batlogger S2")) %>%
  mutate(enregistreur = ifelse(grepl("SM", enregistreur), "materiel SM", enregistreur))
  
# Sauvegarder le jeu de données final
save(DataLRE, file = 'results/04_assemblage_jdd/DataLRE.Rdata')

# Changement du nom des colonnes
DataSRE <- DataSRE %>%
  rename(`nombre de contacts SRE` = nb_contacts)
DataMRE <- DataMRE %>%
  rename(`nombre de contacts MRE` = nb_contacts)
DataLRE <- DataLRE %>%
  rename(`nombre de contacts LRE` = nb_contacts)

JDD_VC_LiDAR_OSO_VF_08_2025 <- DataSRE %>% #Pour avoir le nb de contact de chacune des 3 guildes sur le même dataframe
  left_join(DataMRE %>% select(Nuit, id, `nombre de contacts MRE`), by = c("Nuit", "id")) %>%
  left_join(DataLRE %>% select(Nuit, id, `nombre de contacts LRE`), by = c("Nuit", "id")) %>%
  relocate(`nombre de contacts MRE`, .before=4) %>% #Changer l'ordre de la colonne vers la colonne 4
  relocate(`nombre de contacts LRE`, .before=5) %>%
  mutate(`nombre de contacts total` = `nombre de contacts SRE` + `nombre de contacts MRE` + `nombre de contacts LRE`) %>%
  relocate(`nombre de contacts total`, .before=6)
save(JDD_VC_LiDAR_OSO_VF_08_2025, file = 'results/04_assemblage_jdd/JDD_VC_LiDAR_OSO_VF_08_2025.Rdata')


# 4'. Analyse MRE en profondeur : MRE avec juste Pippip et sans Pippip ####
# Dans certains papiers, on observe cette division car la Pippip est souvent abondante
# L'idée est ici de voir si ce n'est pas cette surabondance qui cause la deviation de notre modèle

Dredge_MRE <- Dredge_MRE %>%
  left_join(DataPip %>% select(id, Nuit, nb_contacts),# nb contacts de Pipip regrouper en fonction de la nuit et du site
    by = c("id", "Nuit") ) %>%
  mutate(
  nb_contacts = replace_na(nb_contacts, 0), # remplace NA uniquement dans nb_contacts pour les sites où 0 contacts de Pippip 
    Nombre_de_contacts_de_MRE_Pippip_exclu = nombre_de_contacts_MRE - nb_contacts, #MRE sauf pippip
    Nombre_de_contacts_de_Pippip = nb_contacts) %>% #Juste Pippip
  select(-nb_contacts)

Dredge_MRE <- Dredge_MRE %>% #Pour supprimer les doublons qui se sont crés
  distinct(id, Nuit, Nombre_de_contacts_de_MRE_Pippip_exclu, Nombre_de_contacts_de_Pippip, nombre_de_contacts_MRE, .keep_all = TRUE)

save(Dredge_MRE, file = "results/11_modeleguildes_postsoutenance/Dredge_MRE.Rdata") #je l'enregistre exceptionnellement dans le dossier correspondant au script de modélisation car c'est là que le dredge est généré et c'est plus cohérent 

