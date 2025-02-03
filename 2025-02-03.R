

  
  # (apprentissage) supervisé : on sait ce qu'on cible (régression, classification)
  
  # (apprentissage) non supervisé : regroupement
  
  # méthode d'ensemble : on a un algorithme simple (un classificateur simple), et 
  # on l'agrège. on pourrait agréger des régressions linéaires
  
  # le neurone. C'est une modèle mathématiue
  
  # neurone : fonction sigma
  
  # sigmoïde et fonction logistique. Notre sigma peut être une sigmoïde.
  
  # De base, le neurone ne permet pas de voir les interactions
  
  # on a la même fonction pas couche (cas des couches cachées)
  
  # on n'a pas toujours 0 ou 1 en input, et pas toujours 0 ou 1 en sortie
  # (c'était le cas uniquement pour le premier exemple)
  # en particulier pour la couche cachée, ce n'est pas 0 ou 1 en sortie
  
  # on fait une descente de gradient
  
  # localement, je prends la direction qui minimise ma fonction
  
  # convexe : droite qui relie les deux points est au-dessus de la courbe
  
  # (réseau de) neurones : fonction avec 1 million de paramètres (poids et biais), avec un minimum
  
  # on optimise l'erreur du réseau de neurones
  
  # ce n'est pas t = 0 mais k = 0
  
  # la perte
  
  # batch : sous-ensemble des données
  
  # l'intérêt de la descente de gradient stochastique est computationnel (et aussi statistique)
  
  # on doit faire une boucle avec 1 million d'étapes si on a 1 million de paramètres
  
  # on utilise la composition de fonctions pour remonter les couches (backpropagation, rétropropagation)
  
  # profondeur et largeur des réseaux de neurones

# la sigmoïde est une sorte de généralisation de l'indicatrice

# ReLU : fonction partie positive

