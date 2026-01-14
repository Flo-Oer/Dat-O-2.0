# Dat-O-2.0
## Présentation du projet

Dat-O-2.0 est un projet de semestre dont l’objectif est d’intégrer une webapp R Shiny
dans le système existant de Dato, tout en améliorant son UI/UX et en y intégrant
un script de Machine Learning pour la prédiction des sulfates.

Le projet vise à fournir une interface interactive permettant :
- le chargement de données environnementales,
- la visualisation des données,
- l’exécution de modèles de prédiction (sulfates).

---

## Objectifs du projet

- Intégrer une nouvelle webapp R Shiny dans l’écosystème existant de Dato
- Repenser la structure UI/UX de la webapp existante
- Intégrer un modèle de Machine Learning fonctionnel dans la webapp

---

![webapp screenshot](./webapp_rshiny/webapp_screenshot.png)

## Structure du projet

webapp_rshiny/
│
├── data/
│   ├── raw/                    # Données brutes (non modifiées)
│   ├── clean/                  # Données nettoyées et prêtes pour les modèles
│   ├── prediction_sulfate/     # Données spécifiques à la prédiction des sulfates
│
├── docs/                       # Documentation et éléments de référence
│
├── models/                     # Modèles ML entraînés sauvegardés (.rds)
│
├── scripts/
│   ├── Carto_2.R               # Scripts de cartographie
│   ├── COT_Analysis.R          # Analyses statistiques
│   ├── Graphes.R               # Génération de graphiques
│   ├── Kapta-PSV.R             # Analyses PSV
│   ├── Meteo.R                 # Prétraitement des données météorologiques
│   ├── Prediction_Sulfate.R    # Script principal de prédiction des sulfates
│   ├── Seuil_chlore.R          # Calcul des seuils chlore
│   ├── Seuil_chlore2.R         # Variante du calcul des seuils
│   └── Sulfate_*.R             # Scripts liés aux sulfates
│
├── www/
│   ├── HTML/                   # Pages HTML intégrées à la webapp
│   └── Image/                  # Images utilisées dans l’interface
│
├── App.R                      # Application Shiny (UI + Server)
├── P4A_DATO.Rproj              # Projet RStudio
├── webapp_screenshot.png
└── README.md


## prediction_sulfate

Script ML a integrer dans `webapp_rshiny`

### Status

Pas encore integrer

### Old readme du stagiaire 

Chère Agathe ou pauvre stagiaire qui va devoir me relire, tu vas trouver ici les informations essentielles pour retrouver les résultats présentés dans mon rapport.

Dans chacun des dossiers, il y a l'intégralité des données et scripts utilisés.

Pour le sulfate, "Script Sulfate rapport.qmd" correspond au script que j'ai utilisé pour mon rapport. "Script pluvio St Martin" permet de préparer correctement les données de Saint-Martin-Vésubie.

**Enfin, "Script réseau neurones" contient le modèle final de prédiction des sulfates. Attention, l'algorithme de sélection du lag optimal a des résultats aléatoires, il faut donc les calculer une fois, modifier vec\_variables puis ne plus y toucher. S'il y a une erreur lorsque le code tourne, elle vient toujours du nom des variables de cumul de pluie.** Ce problème n'existe pas sur le Script Sulfate rapport, ce qui permet de retrouver exactement les mêmes chiffres que moi.

Pour les non-conformités, le script le plus important s'appelle "Script non conformité.qmd". Il permet de sortir tous les graphiques du rapport et d'importer les données. "Script causalité" contient les modèles logits et matchings. Il faut importer les données depuis "Script non conformité.qmd".

Bon courage,

Clément Barcaroli

clementbarcaroli@hotmail.com

## `webapp_rshiny`

Webapp a integrer dans le systeme global de Dato

### Statut

Travail en cours

### Execution

```
# start R!
R

# Set webapp directory as working directory
setwd("~/Dat-O-2.0/webapp_rshiny")

# Launch Rshiny
shiny::runApp()
# L’application est accessible via le navigateur à l’adresse indiquée dans la console R
```
