#      /\                 /\    ╔===============================================================================================╗
#     ( \'.__   (\_/)   __.'/ ) ║ 13 - Script pour l'application et le calcul automatisé des variables LiDAR à une aire d'étude ║ 
#      \_.'  '--('.')--'  '._/  ╚===============================================================================================╝
#          `\=/ " \=/`
#           (/|\)


library(sf)
library(dplyr)

#-----------------------------------------------------------#
# A. Partie téléchargement automatique des mailles LiDAR ####
#-----------------------------------------------------------#

zip_zone <- st_read('data/13_application_zip/zip_gimouille.shp') # La seule chose à faire ici est de mettre le chemin d'accès du fichier .shp de la zip
zip_zone_transf <- st_transform(zip_zone, 2154) #Transformer coordonnées en lam93
buffer_1000_zip <- st_buffer(zip_zone_transf, dist = 1000) #Buffer de 1000m autour pour le calcul des variables

## 0. Server WFS pour télécharger les mailles pour lesquels les MN (ici MNH) sont disponibles : https://data.geopf.fr/private/wfs/wfs?apikey=interface_catalogue&SERVICE=WFS&REQUEST=GetFeature&VERSION=2.0.0&TYPENAMES=IGNF_LIDAR-HD_TA:derive #### 
#Le but est ici de créer un script qui télécharge uniquement les mailles intéressantes (dans la zone de buffer)

## 1. Charger les couches ####
buffertest <- buffer_1000_zip #Points ou buffer dans lesquels vous voulez extraire les variables LiDAR              
mailles <- st_read('data/01_script_extraction_mn_lidar/lidarhd_ta_mnh.gpkg') #Ici les mailles téléchargées à l'aide du serveur WFS (vous pouvez directement le faire via qgis et sauvegarder la couche)                

#Il faut fusionner les buffer sinon R ne va traiter qu'un seul buffer ou point (un seul point d'écoute) car de base la fonction st ne traite qu'un seul polygone
buffertest_union <- st_union(buffertest)

## 2. Identifier les mailles qui intersectent les buffers ou vos points ####
mailles_interessantes <- mailles[st_intersects(mailles, buffertest_union, sparse = FALSE), ]

## 3. Telecharger les mailles qui intersectent la zone tampon ####

# Definir le dossier où les mailles seront téléchargées
dossier_telechargement <- 'results/01_script_extraction_mn_lidar/MNH_mailles_interessantes/' #attention aux quantités de données, 1 maille de 1x1km = 15,2 mo

# Automatiser le téléchargement : voici une fonction pour extraire le nom de fichier à partir de l'URL
get_filename_from_url <- function(url) {
  # Extraire le nom du fichier après 'FILENAME=' dans l'URL
  match <- regmatches(url, regexpr("FILENAME=[^&]+", url))
  if (length(match) > 0) {
    return(sub("FILENAME=", "", match))
  } else {
    stop("Le paramètre 'FILENAME' est absent dans l'URL")
  }
}

#R est maintenant  capable de lire les URL de téléchargement contenus dans la table d'attributs des mailles

# Télécharger les fichiers directement 
start = Sys.time() #Début du chronomètre
for (i in 1:nrow(mailles_interessantes)) {
  url <- mailles_interessantes$url[i]  # URL du fichier
  nom_fichier <- get_filename_from_url(url)  # Extraire le nom du fichier
  chemin_fichier <- file.path(dossier_telechargement, nom_fichier)  # Chemin complet
  
  # Vérifier si la dalle a déjà été télécharger, sinon la télécharger (éviter les doublons)
  if (!file.exists(chemin_fichier)) {
    cat("Téléchargement de :", nom_fichier, "\n")
    download.file(url, destfile = chemin_fichier, mode = "wb")
  } else {
    cat("Le fichier", nom_fichier, "existe déjà.\n")
  }
}
Sys.sleep(0)
print( Sys.time() - start) #Afficher le temps de téléchargement des dalles 

#Maintenant vous avez téléchargé les dalles LiDAR dont vous aviez besoin

#Verification pour voir si les mailles correspondent bien au buffer de la zip
plot(st_geometry(buffer_1000_zip), col = rgb(1,0,0,0.3), border = "red3", main = "ZIP + Buffer + Mailles")
plot(st_geometry(zip_zone_transf), col = "darkolivegreen4", border = "darkolivegreen4", add = TRUE)
plot(st_geometry(mailles_interessantes), col = NA, border = "steelblue", lwd = 2, add = TRUE)
