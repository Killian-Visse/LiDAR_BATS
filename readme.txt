01 : Ce script permet le téléchargement automatique des modèles numériques LiDAR du projet LiDAR HD de l'IGN ("results > 01...") à partir du tableau d'assemblage des mailles LiDAR. Ce tableau est téléchargeable directement sur qGIS via le server WFS et disponible dans "data > 01".  
/!\ Attention à la mise à jour du tableau car le tableau actuel ne couvre pas encore toute la France métropolitaine. 

02 : Ce script permet l'agencement et le nettoyage des données brutes Vigie-Chiro ("data > 02...") pour ne conserver que les données qui nous intéressaient dans le cadre de notre projet. Il permet également l'exportation en .shp des points des sites à partir des coordonnées x,y. 

03 : Script d'automatisation du calcul des 9 métriques LiDAR (hauteur de la canopée, sa rugosité, son ouverture, densité d'arbres, FHD, recouvrement des strates arbustive basse et herbacée, espace de vol libre,  trouées de canopée) dans des buffers de 200 et 500 m autour des points d'écoute.  /!\ Le calcul se fait à partir des MNH uniquement, et nécessite la couche d'urbain ("data>08...") pour filtrer les données  si nécessaire. 

04 : Ce script a servi à fusionner les JDD chiroptères & LiDAR aux divers moments du stage (d'abord les pipistrelles pour le rapport de stage, puis par guilde, avec les métriques OSO, ...).  
/!\ S'applique uniquement si le JDD chiroptères a la même structure que celui Vigie-Chiro.

05 : Ce script présente les pistes d'exploration préliminaire des données que j'ai mené pour le JDD sur les pipistrelles 
en vu de ma soutenance : histogrammes, plots, ACP, matrice de corrélations, ...

06 : Ce script présente le processus de modélisation (modèles univariés, GLMM distribution négative binomiale) effectué pour ce même jeu de données

07 : Script des figures utilisés pour le rapport de stage

08 : Ce script permet le calcul automatique des variables 2D OSO (proportion d'urbain, d'eau, de prairies, de surfaces agricoles ; distance à l'eau et à la végétation), BD foret (proportion de forêts), BD haies (quantité de haies) dans des buffers de 250, 500, 750 & 1000 m autour de sites. Nécessite au préalable les couches brutes (trop lourdes pour être transmises sur le drive)
/!\ Faire attention à la mise à jour des couches

09 : Ce script présente les pistes d'exploration de données que j'ai mené pour le JDD sur les guildes avec les variables LiDAR 3D et 2D : histogrammes, plots, ACP, matrice de corrélations, ...

10 : Ce script présente la sélection des variables retenues en vu du dredge. Il y a à la fois la sélection du buffer le plus pertinent pour chaque variable en comparaison avec le modèle nul ainsi que le choix des variables en fonction des corrélations entretenues entre-elles. Requiert le JDD final. 

11 : Ce script présente le processus de modélisation (modèles multivariés, GLMM distribution négative binomiale) effectué pour le jeu de données final sur les guildes. Pour chaque guilde (SRE, MRE, LRE), exploration du meilleur modèle : visualisation des résidus, test d'autocorrélation spatiale, comparaison modèles delta AIC <2, r², ...

12 : Script des figures utilisées pour le rapport final de Auddicé (summary, table AIC et effets marginaux)

13 : Script des prémices de l'utilisation de la méthodologie LiDAR en structure professionnelle. Permet, à partir d'une aire d'étude, le téléchargement automatique des dalles se situant dans un buffer de 1000 m autour de la ZIP.
