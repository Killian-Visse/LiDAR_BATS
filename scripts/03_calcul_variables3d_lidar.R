#      /\                 /\    ╔=============================================================╗
#     ( \'.__   (\_/)   __.'/ ) ║ 03 - Script d'extraction et de calcul des variables LiDAR   ║ 
#      \_.'  '--('.')--'  '._/  ╚=============================================================╝
#          `\=/ " \=/`
#           (/|\)

library(sf)           
library(exactextractr)
library(pbapply)
library(ggplot2)
library(terra)
library(dplyr)
library(raster) 

#Maintenant que nous avons les données chiros et les produits dérivés (MNH, MNS, MNT) du LiDAR, nous allons calculer quelques variables

# a. Chargement et préparation des données LiDAR ####

#Je charge dans un premier temps les points d'écoute (.shp) pour lesquels je voudrais obtenir la valeur de hauteur
PointsFixeVC <- st_read('results/02_script_extraction_nettoyage_chiros/CoosVigieChiroLam93.shp') 
#Il faut convertir les points en objet Spatial
PointsFixeVC_sp <- as(PointsFixeVC, "Spatial")

#Chargement du buffer
shapefile_path <- 'results/02_script_extraction_nettoyage_chiros/Buffer500VC_cutLIDAR_ta_bocages.gpkg'     
buffer500m <- st_read(shapefile_path) #Faire comprendre à R que c'est un objet spatial
buffer500m_vect <- vect(buffer500m) #Convertit sf en SpaceVector

tif_folder <- 'results/01_script_extraction_mn_lidar/MNH_mailles_interessantes/'              #Chargement des MNH
tif_files <- list.files(tif_folder, pattern = "\\.tif$", full.names = TRUE) #Lui faire comprendre que le dossier contient une liste de fichiers rater
# Créer un fichier VRT temporaire pour stocker tous les raster (1800 trop lourd à stocker en RAM)
vrt_path <- paste0(tempfile(), ".vrt")
vrt(tif_files, filename = vrt_path, overwrite = TRUE)

# Lire le VRT contenant tous les rasters fusionnés (rien n'est chargé en mémoire ici car bcp trop lourd)
raster_vrt <- rast(vrt_path)
crs(raster_vrt)<- "EPSG:2154"  #Verifier système de coordonnées

#Si on veut les télécharger on peut faire cette commande (attention au volume des données) 
#output_path <- "results/01_script_extraction_mn_lidar/MNH_mailles_interessantes/MNH_fusionné.tif"
#writeRaster(raster_vrt, filename = output_path, overwrite = TRUE) #Telecharger le raster virtuel fusionné

# b. Application d'un filtre urbain ####
#On veut recouper le raster LiDAR par un raster contenant une couche d'urbanisation pour que les hauteurs de bâtiments ne soient pas traitées comme des arbres
urbain_couche <- st_read("data/08_calcul_metriques2d_oso_bd//OSO_BIN_URB_CROP_BUF1000.gpkg") #Convertir les polygones sf en format SpatVector (pour terra)
urbain_vecteur <- vect(urbain_couche)
urbain_vecteur_union <- terra::aggregate(urbain_vecteur, by = "DN") #aggrégé les polygones par valeur
urbain_subset <- urbain_vecteur_union[urbain_vecteur_union$DN == 0, ] #On ne garde que les polygones urbains dans le raster 

# c. Traitement buffer par buffer ####

traiter_buffer <- function(i) {
  buffer_i <- buffer500m_vect[i, ]

#On prend un seul buffer pour le traitement
raster_crop <- try(crop(raster_vrt, buffer_i), silent = TRUE)
if (inherits(raster_crop, "try-error")) return(NA)

#Pour chaque buffer on applique les polygones urbains (les pixels d'urbain deviennent NA)
raster_masque_urb <- try(mask(raster_crop, urbain_subset), silent = TRUE)
if (inherits(raster_masque_urb, "try-error")) return(NA)
return(raster_masque_urb)
}

#On généralise le processus à tous les buffers avec une barre de progression
resultats <- pblapply(1:length(buffer500m_vect), traiter_buffer)
saveRDS(resultats, file = 'results/03_calcul_variables3d_lidar/raster_urbain_na.rds') #Sauvegarder nouveau raster ; obliger de relancer le code si on veut manipuler car issus d'un crop de raster_vrt (en mémoire virtuel donc non stocké)

# d. Calcul des variables LiDAR ####

# Calculer les variables au point d'écoute donné est peu intéressant. Il vaut mieux calculer la moyenne dans des buffers. On a choisi 200 et 500m (cf biblio). Les buffers ont été créés sur QGIS

# Fonction indice FHD de dispersion végétale définie
calc_fhd <- function(values) {
  if (all(is.na(values))) return(NA)
  
  breaks <- c(0, 1, 4, 7, 20, Inf)  # Vecteur contenant les différentes strates (herbacée, arbustive basse et haute, arborescente supérieure et inférieure)
  strata <- cut(values, breaks = breaks, include.lowest = TRUE, right = FALSE) # Calcul la proportion de points dans chaque strate (pi)
  p_i <- prop.table(table(strata))  # proportions par strate
  fhd <- -sum(p_i * log(p_i), na.rm = TRUE)  # indice de Shannon avec pi la proportion de points LiDAR dans chaque classe i, la somme est effectuée sur toutes les classes de hauteur
  return(fhd)
}

n_buffers <- length(resultats)

# Initialisation des vecteurs résultats (avec NA par défaut)
hauteurmoyenne <- rep(NA_real_, n_buffers)
rugosite <- rep(NA_real_, n_buffers)
densite_arbre <- rep(NA_real_, n_buffers)
fhd <- rep(NA_real_, n_buffers)
espace_vol_libre <- rep(NA_real_, n_buffers)
recouvrement_herbacee <- rep(NA_real_, n_buffers)
recouvrement_arbustive_basse <- rep(NA_real_, n_buffers)
proportion_sol_nu <- rep(NA_real_, n_buffers)
proportion_exposee <- rep(NA_real_, n_buffers)
trouees_canopee <- rep(NA_real_, n_buffers)

for(i in seq_len(n_buffers)) {
  r <- resultats[[i]]
  b <- buffer500m[i, ]
  
  if(!inherits(r, "SpatRaster")) next # passer si pas de raster
  
  # Calculer les variables au point d'écoute donné est peu intéressant. Il vaut mieux calculer la moyenne dans des buffers. On a choisi 200 et 200m (cf biblio). Les buffers ont été créés sur QGIS
  
  # Convertir SpatRaster en RasterLayer et fixer explicitement le CRS pour éviter les warnings
  r_raster <- raster(r)
  crs(r_raster) <- crs(r)  # <-- Ligne clé pour fixer le CRS et éviter le warning
  
  # Hauteur de la canopée (=MNH) dans le buffer ####
  # Calcul de la hauteur moyenne de canopée dans chaque buffer
  hauteurmoyenne[i] <- exact_extract(r_raster, b, 'mean')
  
  # Joindre les résultats au tableau comportant les points 
  
  # Mesure de la complexité horizontale (rugosité) dans les buffers à travers le sd 
  # Calcul de la rugosité (écart-type de la hauteur de la canopée) ####
  rugosite[i] <- exact_extract(r_raster, b, 'stdev')
  
  # Densité (proportion de surface occupée) par les arbres (>7m) dans les buffers ####
  densite_arbre[i] <- exact_extract(r_raster, b, function(df) {
    mean(df$value > 7, na.rm = TRUE)
  }, summarize_df = TRUE)
  
  # Ouverture de la canopée (canopée définie comme tout point de végétation au delà de 7m = strate arborescente) ####
  # sera calculée après la boucle, car dépend de densite_arbre
  
  # Complexité verticale dans les buffers à travers l'indice FHD de dispersion végétale ####
  fhd[i] <- exact_extract(r_raster, b, function(df) calc_fhd(df$value), summarize_df = TRUE)
  
  # Un FHD grand (proche de log(i) soit nombre de strates, ici 1,609) signifie que la distribution des strates dans le buffer est diversifiée et équilibrée (pixels homogènes entre toutes les strates)
  # Un FHD proche de 0 indique que la plus part des pixels appartiennent à une seule strate (peu de diversité verticale)
  
  # Variables calculées post-soutenance ####
  
  # Espace de vol libre (Rauchenstein et al., 2022 : https://doi.org/10.1016/j.foreco.2022.120210) ####
  espace_vol_libre[i] <- exact_extract(r_raster, b, function(df) {
    values_mod <- df$value
    values_mod[!is.na(values_mod) & values_mod < 0.5] <- 20 # si végétation <50cm --> vol libre de 20m
    values_mod[!is.na(values_mod) & values_mod >= 0.5] <- values_mod[!is.na(values_mod) & values_mod >= 0.5] - 0.5 # Sinon vol libre = hauteur - 0.5
    mean(values_mod, na.rm = TRUE)
  }, summarize_df = TRUE)
  
  # Recouvrement des strates herbacée et arbustive basse (Rauchenstein et al., 2022) ####
  recouvrement_herbacee[i] <- exact_extract(r_raster, b, function(df) {
    mean(df$value >= 0 & df$value < 1, na.rm = TRUE)
  }, summarize_df = TRUE)
  
  recouvrement_arbustive_basse[i] <- exact_extract(r_raster, b, function(df) {
    mean(df$value >= 1 & df$value < 4, na.rm = TRUE)
  }, summarize_df = TRUE)
  
  # Trouées de canopée Jung et al., 2012 les définissent comme The relative amount of ground (>3m²) with a proportion (>1m²) that est directement reached by light) ####
  # sol défini comme des pixels de MNH avec hauteur < 0.5m et lumière directe quand aucune canopée au dessus (seuil de 4m ici)
  # pas possible de calculer des patchs via raster_vrt car nécessite 1380gb de ram -> on fait une estimation des trouées à partir de la proportion du buffer qui est au sol (hauteur < 0.5 m) et où la canopée est assez ouverte (hauteur < 4 m) pour laisser passer la lumière. 
  
  # Proportion de sol nu : hauteur < 0.5 m (ex. sol, herbacée très basse)
  proportion_sol_nu[i] <- exact_extract(r_raster, b, function(df) {
    mean(df$value < 0.5, na.rm = TRUE)
  }, summarize_df = TRUE)
  
  # Proportion exposée à la lumière : hauteur < 4 m
  proportion_exposee[i] <- exact_extract(r_raster, b, function(df) {
    mean(df$value < 4, na.rm = TRUE)
  }, summarize_df = TRUE)
  
  # Approximation des trouées de canopée : produit des deux proportions
  trouees_canopee[i] <- proportion_sol_nu[i] * proportion_exposee[i]
}

# Mesure de l'ouverture de la canopée ####
buffer500m$ouverture_canopee_500m_filturb <- 1 - densite_arbre

# Joindre les résultats au tableau comportant les buffers
buffer500m <- buffer500m %>%
  mutate(
    hauteurmoyenne_500m_filturb = hauteurmoyenne,
    rugosite_500m_filturb = rugosite,
    densite_arbre_500m_filturb = densite_arbre,
    FHD_500m_filturb = fhd,
    espace_vol_libre_500m_filturb = espace_vol_libre,
    recouvrement_herbacee_500m_filturb = recouvrement_herbacee,
    recouvrement_arbustive_basse_500m_filturb = recouvrement_arbustive_basse,
    proportion_sol_nu_500m_filturb = proportion_sol_nu,
    proportion_exposee_500m_filturb = proportion_exposee,
    trouees_canopee_500m_filturb = trouees_canopee
  )

################################################################################
#La même chose mais pour le second buffer de 200m                           ####
################################################################################

PointsFixeVC <- st_read('results/02_script_extraction_nettoyage_chiros/CoosVigieChiroLam93.shp') 
PointsFixeVC_sp <- as(PointsFixeVC, "Spatial")
shapefile_path <- 'results/02_script_extraction_nettoyage_chiros/Buffer200VC_cutLIDAR_ta_bocages.gpkg'     
buffer200m <- st_read(shapefile_path) 
buffer200m_vect <- vect(buffer200m) 
tif_folder <- 'results/01_script_extraction_mn_lidar/MNH_mailles_interessantes/'            
tif_files <- list.files(tif_folder, pattern = "\\.tif$", full.names = TRUE) 
vrt_path <- paste0(tempfile(), ".vrt")
vrt(tif_files, filename = vrt_path, overwrite = TRUE)
raster_vrt <- rast(vrt_path)
crs(raster_vrt)<- "EPSG:2154"  
urbain_couche <- st_read("data/08_calcul_metriques2d_oso_bd/OSO_BIN_URB_CROP_BUF1000.gpkg") #
urbain_vecteur <- vect(urbain_couche)
urbain_vecteur_union <- terra::aggregate(urbain_vecteur, by = "DN") 
urbain_subset <- urbain_vecteur_union[urbain_vecteur_union$DN == 0, ] 
traiter_buffer <- function(i) {
  buffer_i <- buffer200m_vect[i, ]
    raster_crop <- try(crop(raster_vrt, buffer_i), silent = TRUE)
  if (inherits(raster_crop, "try-error")) return(NA)
    raster_masque_urb <- try(mask(raster_crop, urbain_subset), silent = TRUE)
  if (inherits(raster_masque_urb, "try-error")) return(NA)
  return(raster_masque_urb)
}
resultats <- pblapply(1:length(buffer200m_vect), traiter_buffer)
saveRDS(resultats, file = 'results/03_calcul_variables3d_lidar/raster_urbain_na.rds') 
calc_fhd <- function(values) {
  if (all(is.na(values))) return(NA)
  
  breaks <- c(0, 1, 4, 7, 20, Inf)  
  strata <- cut(values, breaks = breaks, include.lowest = TRUE, right = FALSE) 
  p_i <- prop.table(table(strata)) 
  fhd <- -sum(p_i * log(p_i), na.rm = TRUE) 
  return(fhd)
}
n_buffers <- length(resultats)
hauteurmoyenne <- rep(NA_real_, n_buffers)
rugosite <- rep(NA_real_, n_buffers)
densite_arbre <- rep(NA_real_, n_buffers)
fhd <- rep(NA_real_, n_buffers)
espace_vol_libre <- rep(NA_real_, n_buffers)
recouvrement_herbacee <- rep(NA_real_, n_buffers)
recouvrement_arbustive_basse <- rep(NA_real_, n_buffers)
proportion_sol_nu <- rep(NA_real_, n_buffers)
proportion_exposee <- rep(NA_real_, n_buffers)
trouees_canopee <- rep(NA_real_, n_buffers)
for(i in seq_len(n_buffers)) {
  r <- resultats[[i]]
  b <- buffer200m[i, ]
  if(!inherits(r, "SpatRaster")) next 
  r_raster <- raster(r)
  crs(r_raster) <- crs(r) 
  hauteurmoyenne[i] <- exact_extract(r_raster, b, 'mean')
  rugosite[i] <- exact_extract(r_raster, b, 'stdev')
    densite_arbre[i] <- exact_extract(r_raster, b, function(df) {
    mean(df$value > 7, na.rm = TRUE)
  }, summarize_df = TRUE)
  fhd[i] <- exact_extract(r_raster, b, function(df) calc_fhd(df$value), summarize_df = TRUE)
  buffer200m$ouverture_canopee_200m_filturb <- 1 - densite_arbre
  espace_vol_libre[i] <- exact_extract(r_raster, b, function(df) {
    values_mod <- df$value
    values_mod[!is.na(values_mod) & values_mod < 0.5] <- 20 
    values_mod[!is.na(values_mod) & values_mod >= 0.5] <- values_mod[!is.na(values_mod) & values_mod >= 0.5] - 0.5 
    mean(values_mod, na.rm = TRUE)
  }, summarize_df = TRUE)
  recouvrement_herbacee[i] <- exact_extract(r_raster, b, function(df) {
    mean(df$value >= 0 & df$value < 1, na.rm = TRUE)
  }, summarize_df = TRUE)
  recouvrement_arbustive_basse[i] <- exact_extract(r_raster, b, function(df) {
    mean(df$value >= 1 & df$value < 4, na.rm = TRUE)
  }, summarize_df = TRUE)
    proportion_sol_nu[i] <- exact_extract(r_raster, b, function(df) {
    mean(df$value < 0.5, na.rm = TRUE)
  }, summarize_df = TRUE)
    proportion_exposee[i] <- exact_extract(r_raster, b, function(df) {
    mean(df$value < 4, na.rm = TRUE)
  }, summarize_df = TRUE)
    trouees_canopee[i] <- proportion_sol_nu[i] * proportion_exposee[i]
}
buffer200m <- buffer200m %>%
  mutate(
    hauteurmoyenne_200m_filturb = hauteurmoyenne,
    rugosite_200m_filturb = rugosite,
    densite_arbre_200m_filturb = densite_arbre,
    FHD_200m_filturb = fhd,
    espace_vol_libre_200m_filturb = espace_vol_libre,
    recouvrement_herbacee_200m_filturb = recouvrement_herbacee,
    recouvrement_arbustive_basse_200m_filturb = recouvrement_arbustive_basse,
    proportion_sol_nu_200m_filturb = proportion_sol_nu,
    proportion_exposee_200m_filturb = proportion_exposee,
    trouees_canopee_200m_filturb = trouees_canopee
  )