library("ggplot2", character.only = TRUE) 
library("openxlsx", character.only = TRUE, options("openxlsx.dateFormat" = "mm/dd/yyyy")) 
library("rmarkdown")
library(MASS)
library("tidyr", character.only = TRUE) 
library("dplyr", character.only = TRUE) 
library(purrr)
library(randomForest)
library(maptree)
library(caret)
library("StatMatch")
library(lubridate)
library(VIM)
library(simputation)
library(hms)
library(ordinal)
library(ranger)
library(ROSE)
library("nnet")
library(Factoshiny)

chemin = "S:/DPH/07 - DATASCIENCE/6-Process_qualite/Scripts + rapport Clément Barcaroli/Concentration Sulfate/"

#Au départ, il faut deux dataframes : 
#un premier qui contient la colonne Conductivité.mg.L sans décimale, la colonne Concentration.mg.L sans décimale, la colonne temperature avec deux décimales et la colonne jour qui contient la date au format aaaa-mm-jj
#un second qui contient la colonne jour qui contient la date au format aaaa-mm-jj et autant de colonne que voulue, chaque colonne contenant le cumul de plue quotidien pour une station météo donnée.


#Pour l'exemple, j'utilise ici data_SO4_Ves_meteo pour ma première base et meteo pour ma seconde. Les colonnes contenant la météo ne sont pas utiles.


importer_donnees_sulfates = function(chemin_fichier){
  data = read.xlsx(chemin_fichier)
  
  # Renommage des colonnes avec la première ligne
  colnames(data) = as.character(data[1, ])
  data = data[-1, ]
  
  # Conversion de la date
  data$`Date de prélèvement` = as.Date(as.numeric(data$`Date de prélèvement`), origin = "1899-12-30")
  
  
  return(data)
}

data_S04_Ves = importer_donnees_sulfates(paste0(chemin,"Données sulfates et conductivités Vésubie.xlsx"))

temperature_ves = read.xlsx("d:/users/rea0956/Desktop/Données de travail/Sulfates/Données températures in situ Vesubie et Raybaud.xlsx",sheet = 1,colNames=TRUE,detectDates = TRUE,startRow = 2)
temperature_ves = temperature_ves %>% dplyr::select(Date.de.prélèvement,Résultat) %>% group_by(Date.de.prélèvement) %>%  summarise(temperature = round(mean(Résultat, na.rm = TRUE),digits=2), .groups = "drop")


data_S04_Ves = data_S04_Ves %>% rename('jour'='Date de prélèvement')
temperature_ves = temperature_ves %>% rename('jour'= Date.de.prélèvement)
data_S04_Ves = merge(data_S04_Ves,temperature_ves, by="jour")

load(paste0(chemin,'Météo/meteofr_HorQuot.RData'))
meteo_ante_2023 = meteo_horquot
load(paste0(chemin,'Météo/meteoFrAPI_06075007.RData'))
meteo_Levens = meteo_horquot #Levens
load(paste0(chemin,'Météo/meteoFrAPI_06088001.RData'))
meteo_Nice = meteo_horquot #Nice
load(paste0(chemin,'Météo/meteoFrAPI_06103002.RData'))
meteo_Berthemont = meteo_horquot #BERTHEMONT-LES-BAINS_SAPC
load(paste0(chemin,'Météo/meteoFrAPI_06120004.RData'))
meteo_St_Et = meteo_horquot #Saint-Et

load(paste0(chemin,'Météo/Tests Agathe/meteoFrAPI_06005001.RData'))
meteo_Ascros = meteo_horquot

load(paste0(chemin,'Météo/Tests Agathe/meteoFrAPI_06033002.RData'))
meteo_Carros = meteo_horquot        

load(paste0(chemin,'Météo/Tests Agathe/meteoFrAPI_06050002.RData'))
meteo_Coursegoules = meteo_horquot 

load(paste0(chemin,'Météo/Tests Agathe/meteoFrAPI_06074005.RData'))
meteo_Lantosque = meteo_horquot 

load(paste0(chemin,'Météo/Tests Agathe/meteoFrAPI_06077006.RData'))
meteo_Luceram = meteo_horquot 

load(paste0(chemin,'Météo/Tests Agathe/meteoFrAPI_06081001.RData'))
meteo_Le_mas = meteo_horquot

load(paste0(chemin,'Météo/Tests Agathe/meteoFrAPI_06094002.RData'))
meteo_Peone = meteo_horquot

load(paste0(chemin,'Météo/Tests Agathe/meteoFrAPI_06099004.RData'))
meteo_Puget = meteo_horquot

load(paste0(chemin,'Météo/Tests Agathe/meteoFrAPI_06102001.RData'))
meteo_Rimplas = meteo_horquot


load(paste0(chemin,'Météo/Tests Agathe/meteoFrAPI_06125001.RData'))
meteo_Saint_martin_dentraunes = meteo_horquot
meteo_Saint_martin_dentraunes= meteo_Saint_martin_dentraunes %>% filter((date != "2022-12-31" | rr1 != 0.0) & (date != "2020-12-31" | rr1 != 0.0)) #doublons pour 2022 12 31 et 2020 12 31

meteo_St_martin = read.csv(paste0(chemin,'Météo/CSV St Martin/meteo_St_martin.csv')) #J'ai créé ce csv à partir du script St Martin présent dans le dossier
meteo_St_martin$jour=as.Date(meteo_St_martin$jour)
meteo_St_martin$St_martin=round(meteo_St_martin$St_martin,2)
rm(meteo_horquot)



df=meteo_ante_2023[meteo_ante_2023$jour>=as.Date('2000-01-07'),] #on n'a pas besoin des observations d'avant ce jour
df_wide = spread(df, key = nom_usuel, value = rr) #tidyr mon amour, on éclate la colonne des sites de relevés météos en 3 variables 
df_meteo_avant_2023 = df_wide %>% group_by(jour) %>% summarise(across(everything(), ~ first(na.omit(.x))), .groups = "drop") #on fusionne les lignes qui correspondent à la même date
colnames(df_meteo_avant_2023)=c('jour','num_poste','Levens','Nice','St_Et')
rm(df,df_wide)

#On obtient un df avec en première colonne la date et les autres colonnes contiennent les relevés pluvio pour chaque station


get_nom_ville <- function(df) {
  # Récupère le nom de l'objet passé en argument
  nom_objet <- deparse(substitute(df))
  
  # Supprime le préfixe "meteo_" pour garder uniquement le nom de la ville
  nom_ville <- sub("^meteo_", "", nom_objet)
  
  return(nom_ville)
}


liste_meteo <- list(
  meteo_Nice = meteo_Nice,
  meteo_Levens = meteo_Levens,
  meteo_St_Et = meteo_St_Et,
  meteo_Carros = meteo_Carros,
  meteo_Coursegoules = meteo_Coursegoules,
  meteo_Lantosque = meteo_Lantosque,
  meteo_Le_mas = meteo_Le_mas,
  meteo_Luceram = meteo_Luceram,
  meteo_Peone = meteo_Peone,
  meteo_Rimplas = meteo_Rimplas,
  meteo_Saint_martin_dentraunes = meteo_Saint_martin_dentraunes,
  meteo_Puget=meteo_Puget,
  meteo_Ascros = meteo_Ascros
)


vec_meteo = c(meteo_Nice,
              meteo_Levens,
              meteo_St_Et,
              meteo_Carros,
              meteo_Coursegoules,
              meteo_Lantosque,
              meteo_Le_mas,
              meteo_Luceram,
              meteo_Peone,
              meteo_Rimplas,
              meteo_Saint_martin_dentraunes,
              meteo_Puget,
              meteo_Ascros)


for (nom_objet in names(liste_meteo)) {
  df <- liste_meteo[[nom_objet]]
  nom_ville <- sub("^meteo_", "", nom_objet)
  colnames(df)[3] <- nom_ville
  liste_meteo[[nom_objet]] <- df  # Met à jour la liste
}

for (nom in names(liste_meteo)) {
  assign(nom, liste_meteo[[nom]])
}

liste_meteo_sans_poste <- lapply(liste_meteo, function(df) df %>% dplyr::select(-'poste'))

#On fusionne tous les df sur la colonne "date"
meteo <- reduce(liste_meteo_sans_poste, full_join, by = "date")

#On met jour en permier pouis les stations météo
meteo <- meteo %>%
  dplyr::select(date, sort(names(.)[names(.) != "date"]))
colnames(meteo)[1] = "jour"
meteo = bind_rows(meteo,df_meteo_avant_2023)

#Ces lignes servent à corriger un problème étrange causé par la fusion de l'ancien et du nouvel API
meteo =  meteo %>%
  group_by(jour) %>%
  summarise(across(everything(), ~ {
    # Supprimer les NA et prendre la première valeur non manquante
    vals <- .x[!is.na(.x)]
    if (length(vals) > 0) vals[1] else NA
  }), .groups = "drop")
meteo = meteo %>% dplyr::select(-num_poste)
meteo = left_join(meteo,meteo_St_martin)

rm(meteo_ante_2023,meteo_Ascros,meteo_Berthemont,meteo_Carros,meteo_Coursegoules,meteo_Lantosque,meteo_Le_mas,meteo_Levens,meteo_Luceram,meteo_Nice,meteo_Peone,meteo_Puget,meteo_Rimplas,meteo_Saint_martin_dentraunes,meteo_St_Et,meteo_St_Martin,vec_meteo,liste_meteo,liste_meteo_sans_poste,df_meteo_avant_2023,df,meteo_St_martin)

data_S04_Ves_meteo = merge(data_S04_Ves,meteo,by="jour")
data_S04_Ves_meteo = data_S04_Ves_meteo %>% filter(!is.na(jour))

bdd1 = data_S04_Ves_meteo %>% filter(grepl("mg",Unité)) #On sépare la base en 2, l'une qui contient la conductivité dans la colonne Résultat, l'autre la concentration
bdd1 = bdd1 %>% rename('Concentration.mg.L'='Résultat')
bdd2 = data_S04_Ves_meteo %>% filter(!grepl("mg",Unité))
bdd2 = bdd2 %>% rename('Conductivité.µS.cm'='Résultat')

#On recolle les bases
data_S04_Ves_meteo = merge(bdd2,bdd1,by=c('jour','Point de surveillance','temperature','Levens','Nice','St_Et','Ascros','Carros','Coursegoules','Lantosque','Le_mas','Luceram','Peone','Puget','Rimplas','Saint_martin_dentraunes','St_martin'))

#On garde uniquement les colonnes intéressantes
data_S04_Ves_meteo = data_S04_Ves_meteo %>% dplyr::select(c('jour','Point de surveillance','temperature','Levens','Nice','St_Et','Ascros','Carros','Coursegoules','Lantosque','Le_mas','Luceram','Peone','Puget','Rimplas','Saint_martin_dentraunes','St_martin','Conductivité.µS.cm','Concentration.mg.L'))


#on remplace la valeur du 2020-09-09 car on a une valeur abbérante de 0 de conductivité
data_S04_Ves_meteo$Conductivité.µS.cm=as.numeric(data_S04_Ves_meteo$Conductivité.µS.cm)
interv= data_S04_Ves_meteo %>% filter(`Concentration.mg.L` >= 167 & `Concentration.mg.L`<=169 & `Conductivité.µS.cm`!=0)

data_S04_Ves_meteo[1967,'Conductivité.µS.cm']=mean(interv$Conductivité.µS.cm)
rm(data_S04_Ves,interv,bdd1,bdd2,chemin,nom,nom_objet,nom_ville)

#Les données d'avant mars 2017 ne pourront pas être délayées
data_S04_Ves_meteo = data_S04_Ves_meteo %>% filter(jour>=as.Date("2017-03-01"))%>% mutate(Conductivité.µS.cm=as.numeric(Conductivité.µS.cm),Concentration.mg.L=as.numeric(Concentration.mg.L))

data_S04_Ves_meteo = data_S04_Ves_meteo %>% dplyr::select(-`Point de surveillance`) #eaux brutes et potables confondues


#Fonction pour ajouter le cumul de pluie optimal
###############################################################################
fonction_cumul_glissant <- function(df, col, lag, inclure_jour_courant) {
  nom_colonne_sortie <- paste0("cumul_glissant_", col, "_", lag)
  
  df <- df %>%
    arrange(jour) %>%
    mutate(
      !!nom_colonne_sortie := map_dbl(row_number(), function(i) {
        if (inclure_jour_courant) {
          end <- i
          start <- max(1, i - lag + 1)
        } else {
          end <- i - 1
          start <- max(1, i - lag)
        }
        if (start > end) return(NA_real_)  # Si pas assez de données
        sum(df[[col]][start:end], na.rm = TRUE)
      })
    )
  
  return(df)
}

analyser_correlation_precipitation_cumul <- function(df_mesures, meteo_df, max_lag = 90) {
  
  plot_correlation_vs_lag <- function(vec, titre) {
    lag_values <- seq_along(vec)
    plot(lag_values, vec, type = "b", pch = 19, col = "steelblue",
         xlab = "Lag (jours)", ylab = "Corrélation", main = titre,
         ylim = c(-1, 1))
    grid()
  }
  
  #Le vecteur ci-dessous est à adapter si on veut utiliser d'autres stations météos
  stations <- c('Levens','Nice','St_Et','Ascros','Carros','Coursegoules',
                'Lantosque','Le_mas','Luceram','Peone','Puget','Rimplas',
                'Saint_martin_dentraunes','St_martin')
  
  resultats <- data.frame(
    station = character(),
    lag_max = integer(),
    correlation = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (station in stations) {
    vec <- numeric(max_lag)
    
    for (i in 1:max_lag) {
      meteo_cum <- fonction_cumul_glissant(meteo_df, station, lag = i, inclure_jour_courant = FALSE)
      nom_colonne <- paste0("cumul_glissant_", station, "_", i)
      
      # Fusion avec df_mesures
      df_test_cor <- merge(df_mesures, meteo_cum, by = c("jour", 'Levens','Nice','St_Et','Ascros','Carros',
                                                         'Coursegoules','Lantosque','Le_mas','Luceram','Peone',
                                                         'Puget','Rimplas','Saint_martin_dentraunes','St_martin'))
      
      # Calcul de la corrélation
      if (all(c('Concentration.mg.L', nom_colonne) %in% names(df_test_cor))) {
        vec[i] <- cor(df_test_cor[['Concentration.mg.L']], df_test_cor[[nom_colonne]], use = "complete.obs")
      } else {
        vec[i] <- NA
      }
    }
    
    #plot_correlation_vs_lag(vec, paste("Corrélation entre cumul précipitations à", station, "et concentration à Raybaud"))
    
    # Trouver le lag max (valeur absolue)
    if (all(is.na(vec))) {
      lag_max <- NA
      corr_max <- NA
    } else {
      lag_max <- which.max(abs(vec))
      corr_max <- vec[lag_max]
    }
    
    resultats <- rbind(resultats, data.frame(
      station = station,
      lag_max = lag_max,
      correlation = corr_max,
      stringsAsFactors = FALSE
    ))
  }
  
  return(resultats)
}

#On obtient d'une part un graphique par station pour voir l'évolution de la 
#corrélation et d'autre part un tableau qui donne le lag optimal par station
resultats_ves = analyser_correlation_precipitation_cumul(data_S04_Ves_meteo,meteo,90)


by_vector = c("jour", 'Levens','Nice','St_Et','Ascros','Carros','Coursegoules','Lantosque','Le_mas','Luceram','Peone','Puget','Rimplas','Saint_martin_dentraunes','St_martin')


#Ajout manuel des cumuls pour la Vésubie
#villes_groupes <- list(
#  `86` = c("Lantosque", "Carros", "Levens"),
#  `43` = c("Peone", "Saint_martin_dentraunes"),
#  `45` = c("St_Et"),  
#  `54` = c("Le_mas", "Puget", "Ascros"),
#  `55` = c("Luceram", "Rimplas"),
#  `87` = c("Nice", "Coursegoules","St_martin")
#)

#Ajout automatique (peut différer des colonnes au-dessus pour une raison inconnue)
villes_groupes <- resultats_ves %>%
  group_by(lag_max) %>%
  summarise(stations = list(station), .groups = "drop") %>%
  { setNames(.$stations, as.character(.$lag_max)) }


#On ajoute les colonnes de lag 
for (index in names(villes_groupes)) {
  villes <- villes_groupes[[index]]
  for (ville in villes) {
    df_temp <- fonction_cumul_glissant(meteo, ville, as.numeric(index),TRUE)
    df_temp <- fonction_cumul_glissant(meteo, ville, as.numeric(index), TRUE)
    cols_a_garder <- setdiff(names(df_temp), names(data_S04_Ves_meteo))
    df_temp <- df_temp[, c(by_vector, cols_a_garder), drop = FALSE]
    
    data_S04_Ves_meteo <- merge(data_S04_Ves_meteo, df_temp, by = by_vector, all.x = TRUE)
  }
}

rm(df_temp,resultats,villes_groupes,index,ville,villes)

#A la fin de cette étape, on a notre base de donnée de travail complète, il faut
#à présent choisir les variables
################################################################################


################################################################################
#L'ACP permet de voir quelles sont les variables à garder. On repère des groupes
#de variables et on garde la variable la plus contributrice aux dimensions d'intérêt par groupe

base_acp_Ves = scale(data_S04_Ves_meteo %>% dplyr::select(-c(jour,
                                                             Levens,
                                                             Nice,
                                                             St_Et,
                                                             Ascros,
                                                             Carros,
                                                             Coursegoules,
                                                             Lantosque,Le_mas,Luceram,Peone,Puget,Rimplas,Saint_martin_dentraunes,St_martin)))


acp_Ves= PCA(base_acp_Ves, quanti.sup="Concentration.mg.L")
#resshiny = PCAshiny(acp_Ves)
plot(acp_Ves, choix = "var", axes = c(1,2))
plot(acp_Ves, choix = "var", axes = c(2,3))
#La concentration est correlée aux dimensions 1 et 3.

acp_Ves$var  
cor.test(data_S04_Ves_meteo$Concentration.mg.L,data_S04_Ves_meteo$temperature) 

acp_Ves$quanti.sup$cor

#Cette partie ne peut pas être automatisée :(  En revanche il suffit de la faire une fois puis de modifier le vecteur de variables utilisées
#Pour la suite, la sélection des variables est déja implémentée
################################################################################



################################################################################
#On commence par la partie clustering. Cette étape permet d'éviter des estimations
#de concentration inutiles mais n'est pas indispensable ==> le modèle NNet est
#très précis même sans cette étape

#Il faut modifier la liste de variables dans le select manuellement si on change de base
set.seed(124)
vec_variables = c(temperature,Concentration.mg.L,Conductivité.µS.cm)###### ici, il faut ajouter les variables retenues par l'ACP
rf_dataframe_ves <- data_S04_Ves_meteo %>% filter(year(jour) >= 2017 | month(jour) >= 03) %>% dplyr::select(vec_variables) %>% mutate(alerte = as.factor(ifelse(Concentration.mg.L> 200, 1 , 0)))

base_cah_Ves <- rf_dataframe_ves %>% dplyr::select(-alerte)






set.seed(12) #On fixe la graine pour avoir les mêmes clusters à chaque fois

index_ech = sample(seq_len(nrow(rf_dataframe_ves)), size = 0.8 * nrow(rf_dataframe_ves)) #Split aléatoire de l'échantillon
df_train <- base_cah_Ves[index_ech, ] #df d'entraînement
df_train_clust = scale(df_train) #df d'entraînement standarisé pour pouvoir être utilisé dans le clustering

df_test = base_cah_Ves[-index_ech, ] #df de test
#Calcul du dendogramme
mat_distance_Ves=as.dist(gower.dist(df_train_clust)) #on calcule la distance entre nos points d'entraînement
dendo=hclust(mat_distance_Ves,method="ward.D2")
kgs(dendo, mat_distance_Ves, alpha=1, maxclust=10) #nombre opti de clusters = 4

#Visualisation du dendogramme
plot(dendo,labels=FALSE,main="Dendogramme")
rect.hclust(dendo,4,border="blue") 

#on ajoute une colonne avec le groupe de chaque observation du df d'entraînement
clusters = cutree(dendo, k = 4)
df_train = df_train %>% mutate(groupe=as.factor(clusters))


vecteur_dates_groupe_1 = df_test[df_test$groupe==1,1]


df_train = df_train %>% mutate(alerte = as.factor(ifelse(Concentration.mg.L>200,1,0)))
df_test = df_test %>% mutate(alerte = as.factor(ifelse(Concentration.mg.L>200,2,ifelse(Concentration.mg.L>=180,1,0))))
  
#Pour regarder la répartition d'alerte dans chaque groupe

table(df_train$groupe, df_train$alerte)

#A présent, on construit un modèle qui associe à chaque observation un groupe
################################################################################

################################################################################
set.seed(12)
#On crée un premier modèle qui permet d'attribuer à un cluster chaque observation. Ce faisant, on retrouve une partie de l'information contenue dans nos clusters qui provient de la concentration. 

rf.fit_clust = randomForest(`groupe` ~. - Concentration.mg.L - alerte
                            , data=df_train, ntree=1000, importance=TRUE)

df_test = df_test %>% mutate( groupe = predict(rf.fit_clust,df_test)) #on rajoute les colonnes de groupe (ici ce sont des prédictions) et d'alerte

#Lorsqu'on vérifie manuellement, aucune observation des groupes 2, 3 et 4 ne dépasse les 200 mg/L, la méthode fonctionne bien ici
################################################################################


################################################################################
#Estimation du modèle final



set.seed(23)



#choix des var explicatives (à modifier si changement de base de données)
cols=c("temperature","Conductivité.µS.cm","cumul_glissant_Peone_46","cumul_glissant_Puget_53","cumul_glissant_Lantosque_87","cumul_glissant_Luceram_54","cumul_glissant_Coursegoules_86")

#pour standariser/déstandariser 
means <- sapply(df_train[, cols], mean)
sds <- sapply(df_train[, cols], sd)

#on est obligé de standariser les vars explicatives pour que les neurones fonctionnent correctement
X_train <- scale(df_train[, cols], center = means, scale = sds)
X_test  <- scale(df_test[, cols], center = means, scale = sds)
y_train <- df_train$Concentration.mg.L #pas besoin pour l'expliquée (pas d'effet d'échelle)

#entraînement du modèle. size = nbr de neurones, maxit = nombre max d'itérations (On pourrait utiliser 250 plutôt que 300, la convergence est rapide), linout = si on veut une régression, decay = coefficient de perte de mémoire à chaque étape (éviter l'overfitting)
nn <- nnet(X_train, y_train, size = 7, maxit = 400, linout = TRUE, decay=0.65)



df_test$pred_concentration_nnet <- predict(nn, newdata = X_test)  #On prédit toutes les concentrations mais seules celles du groupe 1 servent vraiement
df_test$erreur_nnet = df_test$pred_concentration_nnet - df_test$Concentration.mg.L
df_test %>% ggplot(aes(x = Concentration.mg.L, y = erreur_nnet)) +geom_point(alpha = 0.4, color = "steelblue")+geom_smooth(method = "loess", se = FALSE, color = "black") + geom_hline(yintercept = 0, linetype = "dashed", color = "red") 



summary(nn) #permet de regarder la convergence de l'algo et la tête qu'à chaque neurone


#classification sur les valeurs estimées
df_test$alerte= as.factor(ifelse(df_test$groupe==1,ifelse(df_test$Concentration.mg.L>200,2,  ifelse(df_test$Concentration.mg.L>=180,1,0)  ),0 ))
df_test$predict_alerte_nnet = as.factor( ifelse( df_test$groupe == 1,ifelse( df_test$pred_concentration_nnet>200,2,  ifelse( df_test$pred_concentration_nnet>=180,1,0 )  ),0))            
df_test$erreur = df_test$pred_concentration_nnet-df_test$Concentration.mg.L
df_erreur = df_test %>% filter(predict_alerte_nnet!=alerte)


confusionMatrix(df_test$predict_alerte_nnet,df_test$alerte)


conf <- as.matrix(confusionMatrix(df_test$predict_alerte_nnet, df_test$alerte)$table)

#Matrice de pondération des erreurs (ligne = prédict, colonne = factuel)
# 1 = erreur légère, 0 = erreur très grave
W <- matrix(c(
  1,   0.9, 0.2,   # prédiction = 0
  0.7, 1,   0.4,   # prédiction = 1
  0.2, 0.5, 1      # prédiction = 2
), nrow = 3, byrow = TRUE)



# F1 Gravité Inversée 
TP_w <- sum(conf * W) #somme des prédictions * leur poids
Total_possible <- sum(conf) #somme des prédictions
F1_GI <- TP_w / Total_possible #ratio des deux qui se lit comme un F1 classique

cat("\n","F1-GI avec pondération :", round(F1_GI, 4))
################################################################################


#Le modèle final est stocké sous le nom nn
# ATTENTION alerte predite == 0 =/= concentration <180 mg/L. Les observations des groupes 2 3 et 4 ont toutes alerte predite == 0
#Pour éviter la confusion, possibilité de supprimer la partie clustering pour seulement garder les valeurs prédites.
