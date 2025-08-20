#      /\                 /\    ╔============================================================================╗
#     ( \'.__   (\_/)   __.'/ ) ║ 08 - Script de l'extraction de variables 2D OSO, BD forêts et BD haies     ║ 
#      \_.'  '--('.')--'  '._/  ╚============================================================================╝
#          `\=/ " \=/`
#           (/|\)

#Après la soutenance, le principal objectif était de voir si les variables LiDAR apportaient une plus-value par rapport aux variables 2D classiques qu'on retrouve habituellement 

library(sf)
library(terra)
library(data.table)
library(stringr)
library(dplyr)
library(units) 
library(tidyr)


# 1. Calcul des variables OSO ####
# Proportion d'eau, urbain, prairie et surface agricole dans les buffers


extract_env_metrics <- function(DataGui_vf, #Fonction d'extraction
                                raster_oso_path = 'data/09_métriques_2d_oso/OSO_POINTS.tif',
                                buffers = c(250, 500, 750, 1000), 
                                batch_size = 50) {
  
  raster_oso <- rast(raster_oso_path)
  
  # On définit les classes OSO d'intérêt selon la nomenclature : https://www.theia-land.fr/wp-content/uploads/2021/12/nomenclature_oso.png
  oso_classes <- list(
    eau = 23,
    urbain = c(1, 2, 3, 4),
    prairie = 13,
    agricole = c(12, 5, 6, 7, 11, 8, 9, 10)
  )
  
  results <- DataGui_vf # Pour que les résultats des nouvelles variables soient ajoutées au tableau des guildes
  
  pb <- txtProgressBar(min = 0, max = length(buffers), style = 3) # Pour visualiser barre de progression
  
  # On crée une boucle sur chaque taille de buffer
  for (i in seq_along(buffers)) {
    buf <- buffers[i]
    setTxtProgressBar(pb, i)
    
    indices <- seq_len(nrow(DataGui_vf))
    batches <- split(indices, ceiling(seq_along(indices) / batch_size))
    
    batch_all <- list() # Pour stocker tous les résultats de batch de ce buffer
    
    for (batch_idx in seq_along(batches)) {
      # Affichage du message avec numéro de buffer et batch
      message(sprintf("Buffer %d m - Batch %d/%d", buf, batch_idx, length(batches)))
      
      batch_ids <- batches[[batch_idx]] # Découpage des buffers en liste
      data_batch <- DataGui_vf[batch_ids, ]
      data_batch$id_geom <- seq_len(nrow(data_batch)) # Création d'un identifiant temporaire
      
      # Création des buffers autour des points
      buffers_sf <- st_buffer(data_batch, dist = buf)
      
      # Extraction raster OSO
      vals_raw <- terra::extract(raster_oso, vect(buffers_sf))
      vals_split <- split(vals_raw[[2]], vals_raw$ID)
      
      # Pour chaque liste de pixels dans un buffer, calcul de la proportion de pixels appartenant à chaque classe OSO
      prop_df <- lapply(vals_split, function(vals) {
        sapply(oso_classes, function(codes) mean(vals %in% codes, na.rm = TRUE))
      }) %>% do.call(rbind, .) %>% as.data.frame()
      
      prop_df$id_geom <- data_batch$id_geom[as.integer(names(vals_split))] # On relie aux géométries
      colnames(prop_df)[1:length(oso_classes)] <- paste0("prop_", names(oso_classes), "_buf", buf)
}}
  
  return(results)}

# Extraction du tableau final 
DataGui_LiDAR_2D <- extract_env_metrics(DataGui_vf, batch_size = 50) # Tableau final avec variables 2D et 3D

save(DataGui_LiDAR_2D, file = 'results/05_assemblage_jddfinal/DataGui_LiDAR_2D.Rdata')


# 2. Calcul des variables des couches BD (forêts et haies) ####
# La proportion de forêts et haies dans les buffers 

# Chargement des couches
bd_foret <- st_read('data/09_métriques_2d_oso/BDFORET_POINTS.gpkg', quiet = TRUE)
bd_haie  <- st_read('data/09_métriques_2d_oso/BDHAIE_POINTS.gpkg',  quiet = TRUE)

buffers <- c(250, 500, 750, 1000)
batch_size <- 50

# Conversion en sf si besoin
DataGui_LiDAR_2D <- st_as_sf(DataGui_LiDAR_2D)

# Fusion des polygones pour éviter le double comptage des surfaces qui se chevauchent
bd_foret_union <- st_union(bd_foret)
bd_haie_union  <- st_union(bd_haie)

# Boucle pour chaque buffer
for (buf in buffers) {
  message("Calcul proportions forêt et haie buffer ", buf, " m")
  
  indices <- seq_len(nrow(DataGui_LiDAR_2D))
  batches <- split(indices, ceiling(seq_along(indices) / batch_size))
  
  prop_foret_all <- numeric(nrow(DataGui_LiDAR_2D))
  length_haie_all  <- numeric(nrow(DataGui_LiDAR_2D))  
  
  for (batch_idx in seq_along(batches)) {
    message(sprintf("Buffer %d m - Batch %d/%d", buf, batch_idx, length(batches)))
    
    batch_ids <- batches[[batch_idx]]
    data_batch <- DataGui_LiDAR_2D[batch_ids, ]
    data_batch$id_geom <- seq_len(nrow(data_batch))
    
    # Création des buffers autour des points
    buffers_sf <- st_buffer(data_batch, dist = buf)
    buffers_sf$id_geom <- seq_len(nrow(buffers_sf))
    
    # Proportion de forêts
    foret_in_buffers <- st_intersection(buffers_sf %>% select(id_geom), bd_foret_union)
    if (nrow(foret_in_buffers) > 0) {
      foret_in_buffers$area <- st_area(foret_in_buffers)
      area_foret <- foret_in_buffers %>%
        group_by(id_geom) %>%
        summarise(area_foret = sum(area), .groups = "drop")
    } else {
      area_foret <- tibble(id_geom = integer(), area_foret = set_units(numeric(0), m^2))
    }
    
    # Longueur totale de haies (mètres linéaires car multilignes et pas polygones comme forêt) 
    haie_in_buffers  <- st_intersection(buffers_sf %>% select(id_geom), bd_haie_union)
    if (nrow(haie_in_buffers) > 0) {
      haie_in_buffers$length <- st_length(haie_in_buffers)
      length_haie <- haie_in_buffers %>%
        group_by(id_geom) %>%
        summarise(length_haie = sum(length), .groups = "drop")
    } else {
      length_haie <- tibble(id_geom = integer(), length_haie = set_units(numeric(0), m))
    }
    
    # Surface totale du buffer
    buffer_area <- pi * (buf^2)
    
    # Créer un tableau avec tous les id_geom pour merge complet
    all_ids <- tibble(id_geom = seq_len(nrow(data_batch)))
    
    # Remplacer les NA par 0
    area_foret <- all_ids %>% left_join(area_foret, by = "id_geom") %>% mutate(area_foret = replace_na(area_foret, set_units(0, m^2)))
    length_haie <- all_ids %>% left_join(length_haie,  by = "id_geom") %>% mutate(length_haie = replace_na(length_haie,  set_units(0, m)))
    
    # Calcul des proportions
    prop_foret_batch <- as.numeric(area_foret$area_foret) / buffer_area
    length_haie_batch  <- as.numeric(length_haie$length_haie)
    
    # Stockage dans vecteurs finaux
    prop_foret_all[batch_ids] <- prop_foret_batch
    length_haie_all[batch_ids] <- length_haie_batch
  }
  
  # Ajout dans DataGui_LiDAR_2D
  DataGui_LiDAR_2D[[paste0("prop_foret_buf", buf)]] <- prop_foret_all
  DataGui_LiDAR_2D[[paste0("length_haie_buf",  buf)]] <- length_haie_all
}

save(DataGui_LiDAR_2D, file = 'results/05_assemblage_jddfinal/DataGui_LiDAR_2D.Rdata')


# 3. Calcul des variables de distance à l'eau et à la végétation ####

DataGui_LiDAR_2D <- st_as_sf(DataGui_LiDAR_2D)
pts_vect <- vect(DataGui_LiDAR_2D)

raster_bin_eau <- rast('data/08_calcul_metriques2d_oso_bd/OSO_BIN_EAU_CROP_Buf500_NA.tif') #Raster binaire avec eau (1) et le reste (0) --> plus léger pour les calculs sur R
raster_bin_veg <- rast('data/08_calcul_metriques2d_oso_bd/OSO_BIN_VEG_CROP_Buf500_NA.tif')

# Calcul des distances les plus proches pour chaque pixel
dist_eau_pixels <- distance(raster_bin_eau)
dist_veg_pixels <- distance(raster_bin_veg)

# Extraction de la valeur correspondant à chaque point du df DataGui_LiDAR_2D
distance_eau <- terra::extract(dist_eau_pixels, pts_vect)[,2]
distance_veg <- terra::extract(dist_veg_pixels, pts_vect)[,2]

# Ajout dans l'objet d'origine
DataGui_LiDAR_2D$distance_eau   <- distance_eau
DataGui_LiDAR_2D$distance_veg <- distance_veg

save(DataGui_LiDAR_2D, file = 'results/08_calcul_metriques2d_oso_bd/DataGui_LiDAR_2D.Rdata')

#Application d'un seuil urbain pour ne conserver que les points dans des zones non urbanisées (< 30% selon Wenliang, 2020)

DataGui_LiDAR_2D_filt_urb <- DataGui_LiDAR_2D %>%
  filter(prop_urbain_buf250 < 0.3)
save(DataGui_LiDAR_2D_filt_urb, file = 'results/08_calcul_metriques2d_oso_bd/DataGui_LiDAR_2D_filt_urb.Rdata')
