
# oceanic_theme <- fs::path_temp("Oceanic-Eighties", ext = "rstheme")
# download.file("https://git.io/rstudio-theme-oceanic-eighties", oceanic_theme)
# rstudioapi::addTheme(oceanic_theme, apply = TRUE)


# on fait d'abord la somme des entrées avec des poids, et on ajoute unune constante
# (on l'appelle biais mais ce n'est pas un biais, c'est une constante')
# fonction d'activtation
# le perceptron multicouche
# La Heaviside renvoie 0 ou 1
# linéairement séparable ?
# Indice de Gini (arbre), entropie (classification)
# le gradient, c'est la pente
# ReLU : Rectified Linear Unit
# l'échantillon de test est sacralisé ! on n'y touche pas (on ne fait même pas
# de normalisation des données) --> dans des conditions où de vraies nouvelles
# données arrivent, comme si je l'avais le lendemain

# ridge, lasso, elasticnet : obligatoire de faire del a normalisation
# arbre/forêts/gradient boosting : pas besoin de normaliser
# SVM SVR : requièrent la normalisation
# spline pas besoin
# GAM modèle additif généralisé : morceaux/splines, pas besoin
# réseaux de neurones : oui il faut normaliser c'est obligatoire
# normaliser ne change pas les  résultats en termes de prévision ! le seul point,
# c'est que l'interprétation n'est plus la même.

# le problème des réseaux de neurones, c'est que la structure de calibration est compliquée
# autre souci, c'est vraiment une boîte noire


