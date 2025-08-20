#      /\                 /\    ╔=============================================================╗
#     ( \'.__   (\_/)   __.'/ ) ║ 02 - Script d'extraction/nettoyage des données Vigie-Chiro  ║ 
#      \_.'  '--('.')--'  '._/  ╚=============================================================╝
#          `\=/ " \=/`
#           (/|\)

library(dplyr)
library(sf)

# 1. Nettoyage des données ####
# 1.1. Filtrer le JDD #####

DataTot <- read.csv('data/02_script_extraction_chiros/DataTot.csv', sep=";") 
head(DataTot)
str(DataTot)
summary(DataTot)

#Je crée un JDD filtré en ne gardant que les chiroptères avec degré de certitude >50%
DataTot_filtered <- DataTot %>% 
  filter(groupe == "Chauve-souris", #J'élimine les orthoptères/oiseaux/bruits parasites
         score_max > 0.5) %>%       #On évite les faux positifs en prennant les données où TADARIDA est sûr à minimum 50% 
mutate(longlat = paste(longitude, latitude)) #Création colonne unique avec les coordonnées (x y)
coord <- unique(DataTot_filtered$longlat)    #Pour chaque coordonnée, on veut un seul point GPS 
length(coord)                                #Il existe 13 846 points fixe d'écoute 

# 1.2. Enlever les valeurs manquantes #####

sum(is.na(DataTot_filtered$latitude)) #Pour voir si il y a des données manquantes 
sum(is.na(DataTot_filtered$longitude)) #Il y a 661 lignes (sur 909 331) où les coord GPS manquent 

# On supprime ces lignes manquantes
DataTot_clean <- subset(DataTot_filtered, !is.na(latitude) & !is.na(longitude))

# 2. Conversion des données en format spatial ####
# 2.1. Transformation des coordonnées en LAM93 ####

coordWGS84 <- st_as_sf(DataTot_clean, coords = c("longitude", "latitude"), crs = 4326) # Convertir coord en WGS84 
print(coordWGS84)
coordLAM93 <- st_transform(coordWGS84, crs = 2154) # Convertir les coordonnées en LAM93 pour qGIS (plus pratique et SCR commun)
print(coordLAM93) # Afficher les résultats LAM93
#coordLAM93_clean <- coordLAM93[, "geometry"] Si on veut garder uniquement la colonne contenant les coord pour qGIS

#Supprimer possibles doublons
coordLAM93 <- st_as_sf(coordLAM93)
coordLAM93_unique <- coordLAM93[!duplicated(st_as_text(st_geometry(coordLAM93))), ] #Stock le résultat dans un nouvel objet (pour ne pas écraser l'ancien)

# 2.2. Exportation en shapefile ####

#On exporte les coordonnées en shapefile pour les visualiser dans qGIS
st_write(coordLAM93_unique, 'results/02_script_extraction_nettoyage_chiros/CoosVigieChiroLam93.shp')
