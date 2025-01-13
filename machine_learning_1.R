


   rm(list=ls()) ; gc()
  
  # Comparaison de régressions
  
  library(glmnet)
  library(rpart)
  library(randomForest)
  library(tidyverse)
  library(glue)
  library(ggplot2)
  library(caret)


  
  ML1 <- "C:/Users/aurel/Mon Drive/Certification/Machine learning 1"
  
  don <- read.table(glue("{ML1}/eucalyptus.txt"), 
                         header=T,
                         sep=";",
                         stringsAsFactors = T)
  
  dim(don)
  
  don$bloc <- as.factor(don$bloc)
  
  # circ : circonf?rence ; ht : hauteur
  ggplot(don) +
    aes(x=circ, y=ht, col=bloc) +  # on met des couleurs selon la variable bloc
    geom_point()
  
  mod <- lm(ht ~ circ, data=don)
  
  # ajout de la droite de régression
  ggplot(don) + 
    aes(x=circ, y=ht) +
    geom_point() + 
    geom_smooth(method=lm)
  
  don <- read.table(glue("{ML1}/ozone.txt"),
                    header=T,
                    sep=";",
                    row.names = 1,  # la 1e colonne contient les num?ros de ligne
                    stringsAsFactors = T)
  
  
  don$N12 = factor(don$N12)
  don$S12 = factor(don$S12)
  don$E12 = factor(don$E12)
  don$W12 = factor(don$W12)
  
  # O3 est la concentration en ozone
  # Vx est la projection du vent sur l'axe est-ouest
  # T12 est la temp?rature
  # Ne est la n?bulosit?
  
  modcours <- lm(O3 ~ T12 + Vx + Ne12, data=don)
  summary(modcours)

  mod2 <- lm(O3 ~ T12 + T15 + Ne12 + Vx + O3v, data=don)
  summary(mod2)
  
  mod3 <- lm(O3 ~ T15 + Ne12 + Vx + O3v, data=don)
  summary(mod3)
  
  # sélection itérative. Par défaut step fait une sélection AIC.
  
  step(mod2)
  res3 <- rstudent(mod3)
  plot(res3)
  plot(mod3$fitted.values,res3)
  abline(h=c(-2,2))
  levier = hatvalues(mod3)
  plot(levier,type="h")
  plot(sort(levier),type="h")
  
  nb=10
  blocs <- sample(rep(1:nb, length=nrow(don)))  # tirage sans remise
  RES <- data.frame(Y=don$O3)
  
  for(i in 1:nb){
    
    donA = don[blocs!=i, ]  # Échantillon d'apprentissage
    donT = don[blocs==i, ]  # Échantillon de test
    
    # Mod?le global (avec toutes les variables)
    mod1 <- lm(O3 ~ T12 + T15 + Ne12 + Vx + O3v, data=donA)
    RES[blocs==i, "Global"] <- predict(mod1, donT)
    
    # Mod?le sans la temp?rature ? 12h00
    mod2 <- lm(O3 ~ T15 + Ne12 + Vx + O3v, data=donA)
    RES[blocs==i, "sans T12"] <- predict(mod2, donT)
    
    # Mod?le sans la temp?rature ? 15h00
    mod3 <- lm(O3 ~ T12 + Ne12 + Vx + O3v,data=donA)
    RES[blocs==i,"sans T15"] <- predict(mod3, donT)
  }
  
  apply(RES, 2, function(x,y) {mean((x-RES[,1])^2)})
  

  


 # XX <- model.matrix(O3 ~ ., data=don)[,-1]
  
  dmy <- dummyVars(O3 ~ ., data = don)
  
  XX <- predict(dmy,don)
  
  YY <- don$O3
  
  for(i in 1:nb){
    
    donA = don[blocs!=i,]
    donT = don[blocs==i,]
    
    XXA = XX[blocs!=i,]
    XXT = XX[blocs==i,]
    YYA = YY[blocs!=i]
    
    mod1 <- glm(O3 ~ ., data=donA, family="binomial")
    RES[blocs==i, "Global"] <- predict(mod1, donT, type="response")
    
    # mod2 <- step(mod1, trace=0)
    # RES[blocs==i, "AIC"] <- predict(mod2, donT, type="response")
    
    # mod3 <- step(mod1, trace=0, k=log(nrow(donA)))
    #  RES[blocs==i, "BIC"] <- predict(mod3, donT, type="response")
    
    ###arbre
    #  arbre <- rpart(factor(O3)~.,data=donA)
    #  RES[blocs==i, "arbre"] <- predict(arbre,donT,type="prob")[, 2]
    
    ###arbre
    #  foret <- randomForest(factor(O3) ~ ., data=donA)
    #  RES[blocs==i, "foret"] <- predict(foret, donT, type="prob")[, 2]
    
    ###ridge
    #  tmp <- cv.glmnet(XXA, YYA, alpha=0, family="binomial")
    #   RES[blocs==i, "ridgemin"] <- predict(tmp,XXT,s="lambda.min",type="response")
    #   RES[blocs==i, "ridge1se"] <- predict(tmp,XXT,s="lambda.1se",type="response")
    
    ###lasso
    #   tmp <- cv.glmnet(XXA, YYA, alpha=1, family="binomial")
    #   RES[blocs==i, "lassomin"] <- predict(tmp, XXT, s="lambda.min", type="response")
    #   RES[blocs==i, "lasso1se"] <- predict(tmp, XXT, s="lambda.1se", type="response")
    
    ###elastic
    #  tmp <- cv.glmnet(XXA,YYA,alpha=.5,family="binomial")
    #  RES[blocs==i, "elasmin"] <- predict(tmp, XXT, s="lambda.min", type="response")
    #   RES[blocs==i, "elas1se"] <- predict(tmp, XXT, s="lambda.1se", type="response")
    
  }
  
  RES[1:4,]
  round(apply(RES,2,erreur,Y=RES[,1]),2)
  
  
  
  
  
  
  
  
  ####classification supervisée
  artere <- read.table(glue("{ML1}/artere.txt",header=T))
  
  # chd : coronary heart disease. Indique la présence ou l'absence de la maladie.
  
  plot(chd ~ age, data=artere, col=artere$chd + 1, pch=16)
  
  table(artere$chd / nrow(artere) * 100)
  
  artere %>% group_by(agrp) %>% summarise(mean(chd))
  
  # glm et glmnet
  # binomial permet de faire une régression logistique
  
  modl <- glm(chd ~ age, data=artere, family="binomial")
  
  summary(modl)
  
  predict(modl)
  predict(modl,type="response")
  predict(modl,type="terms")
  
  df <- data.frame(age=seq(1:100))
  
  prev=predict(modl,df,type="response")
  lines(df$age,prev,col=2,lwd=2)
  
 
  # Qualité du vin
  
  rouge <- read.table(glue("{ML1}/winequality-red.csv"), header=T, sep=";")
  blanc <- read.table(glue("{ML1}/winequality-white.csv"), header=T, sep=";")
  
  don <- rbind(rouge, blanc)
  
  # Y : indicatrice d'un vin blanc (sinon rouge)
  don$Y <- c(rep(0,nrow(rouge)), rep(1,nrow(blanc)))

  
  
  don <- read.table(glue("{ML1}/eucalyptus.txt"),
                    header=T,
                    sep=";",
                    stringsAsFactors = T)
  
  don$bloc <- as.factor(don$bloc)
  
  ggplot(don) + 
    aes(x=circ, y=ht, col=bloc) + 
    geom_point()
  
  mod <- lm(ht ~ circ, data=don)

  summary(mod)
  
  ggplot(don) + 
    aes(x=circ, y=ht) +
    geom_point() + 
    geom_smooth(method=lm)
  
  
  
  # Par défaut le tirage avec sample() est sans remise
  
  nb = 100
  beta = rep(NA, nb)
  
  for(i in 1:nb){
    ind <- sample(1:nrow(don), 1000)
    beta[i] <- lm(ht ~ circ, data=don[ind,])$coefficients[2]
  }
  
  sd(beta)
  plot(density(beta))

  

  don <- read.table(glue("{ML1}/ozone.txt"),
                    header=T,
                    sep=";",
                    row.names = 1,
                    stringsAsFactors = T)
  
  don$N12 = factor(don$N12)
  don$S12 = factor(don$S12)
  don$E12 = factor(don$E12)
  don$W12 = factor(don$W12)
  
  modcours <- lm(O3 ~ T12 + Vx + Ne12, data=don)
  
  summary(modcours)
  
  modcours$coefficients
  
  typevar <- sapply(don, class)
  
  # Le point signifie : toutes les variables qui ne sont pas explicitées dans la formule
  modvarquanti <- lm(O3 ~ ., data=don[, typevar != "factor"])
  
  summary(modvarquanti)
  
  mod2 <- lm(O3 ~ T12 + T15 + Ne12 + Vx + O3v, data=don)
  
  summary(mod2)
  
  mod3 <- lm(O3 ~ T15 + Ne12 + Vx + O3v, data=don)
  summary(mod3)
  
  step(mod2)
  
  # Obtention des résidus studentisés
  res3 <- rstudent(mod3)
  
  plot(res3)
  
  plot(mod3$fitted.values, res3)
  
  # ajout d'une ligne horizontale (ici de deux lignes)
  abline(h=c(-2,2))
  
  # hatvalues() retourne les éléments diagonaux de la matrice chapeau, appelées valeurs leviers
  # Une valeur levier élevée indique que l'observation est influente
  
  levier = hatvalues(mod3)

  # Histogrammes
  plot(levier, type="h")
  plot(sort(levier),type="h")
  
  
  nb=10 ; set.seed(1246)
  blocs <- sample(rep(1:nb,length=nrow(don)))
  
  RES <- data.frame(Y=don$O3)

  for(i in 1:nb) {
    donA = don[blocs!=i, ]
    donT = don[blocs==i, ]
    
    mod1 <- lm(O3 ~ T12 + T15 + Ne12 + Vx + O3v, data=donA)
    RES[blocs==i, "Global"] <- predict(mod1, donT)
    
    mod2 <- lm(O3 ~ T15 + Ne12 + Vx + O3v, data=donA)
    RES[blocs==i,"Sans T12"] <- predict(mod2,donT)
    
    mod3 <- lm(O3 ~ T12 + Ne12 + Vx + O3v, data=donA)
    RES[blocs==i,"Sans T15"] <- predict(mod3,donT)
  }
  
  # Calcul des erreurs de prévision
  erreur <- function(X1, X2) {mean((X1-X2)^2)}
  
  apply(RES, 2, erreur, X2=RES$Y)
  

  
  iter=100
  err <- matrix(0,nrow=iter,ncol=4)
  for(jj in 1:iter){
    set.seed(1233+jj)
    blocs <- sample(rep(1:nb,length=nrow(don)))
    RES <- data.frame(Y=don$O3)
    for(ii in 1:nb){
      donA = don[blocs!=ii,]
      donT = don[blocs==ii,]
      mod1 <- lm(O3~T12+T15+Ne12+Vx+O3v,data=donA)
      RES[blocs==ii,"glob"] <- predict(mod1,donT)
      mod2 <- lm(O3~T15+Ne12+Vx+O3v,data=donA)
      RES[blocs==ii,"sansT12"] <- predict(mod2,donT)
      mod3 <- lm(O3~T12+Ne12+Vx+O3v,data=donA)
      RES[blocs==ii,"sansT15"] <- predict(mod3,donT)
    }
    err[jj,]=apply(RES,2,erreur,Y=RES[,1])
  }
  
  boxplot(err[,-1])
  sum(err[,3]<err[,4])
  
  
  
  ####variables quali
  mod4 <- lm(O3~T12+nebulosite+vent,data=don)
  summary(mod4)
  don2 = don
  don2$vent = relevel(don2$vent,"OUEST")
  mod5 <- lm(O3~T12+nebulosite+vent,data=don2)
  summary(mod5)
  
  tmp <- model.matrix(O3~T12+nebulosite+vent,data=don2)[,-1]
  tmp
  df <-data.frame(Y=don2$O3,tmp)
  titi <- lm(Y~.,data=df)
  summary(titi)
  step(titi)
  
  Xmat = model.matrix(Y~.,data=don)[,-1]
  Xmat
  Y = don$Y
  library(glmnet)
  toto = glmnet(Xmat,Y,alpha=0)
  plot(toto,xvar="lambda")
  toto = glmnet(Xmat,Y,alpha=0,
                lambda=c(0,1,10,100,1000))
  plot(toto,xvar="lambda")
  toto = glmnet(Xmat,Y,alpha=0)
  plot(toto,xvar="lambda")
  toto = glmnet(Xmat,Y,alpha=1)
  plot(toto,xvar="lambda")
  toto = glmnet(Xmat,Y,alpha=0.5)
  plot(toto,xvar="lambda")
  
 
  
  # Comparaison de régressions



nb=10
set.seed(1234)
blocs <- sample(rep(1:nb,length=nrow(don)))
RES <- data.frame(Y=don$Y)
### en matrices pour glmnet
#library(caret)
#dmy <- dummyVars(Y~ ., data = don)
#XX <- predict(dmy,don)
XX <- model.matrix(Y~.,data=don)[,-1]
YY <- don$Y
for(ii in 1:nb){
  print(ii)
  donA = don[blocs!=ii,]
  donT = don[blocs==ii,]
  XXA = XX[blocs!=ii,]
  XXT = XX[blocs==ii,]
  YYA = YY[blocs!=ii]
  ###############
  mod1 <- glm(Y~.,data=donA,family="binomial")
  RES[blocs==ii,"glob"] <- predict(mod1,donT,type="response")
  mod2 <- step(mod1,trace=0)
  RES[blocs==ii,"AIC"] <- predict(mod2,donT,type="response")
  mod3 <- step(mod1,trace=0,k=log(nrow(donA)))
  RES[blocs==ii,"BIC"] <- predict(mod3,donT,type="response")
  ###arbre
  arbre <- rpart(factor(Y)~.,data=donA)
  RES[blocs==ii,"arbre"] <- predict(arbre,donT,type="prob")[,2]
  ###arbre
  foret <- randomForest(factor(Y)~.,data=donA)
  RES[blocs==ii,"foret"] <- predict(foret,donT,type="prob")[,2]
  ###ridge
  tmp <- cv.glmnet(XXA,YYA,alpha=0,family="binomial")
  RES[blocs==ii,"ridgemin"] <- predict(tmp,XXT,s="lambda.min",type="response")
  RES[blocs==ii,"ridge1se"] <- predict(tmp,XXT,s="lambda.1se",type="response")
  ###lasso
  tmp <- cv.glmnet(XXA,YYA,alpha=1,family="binomial")
  RES[blocs==ii,"lassomin"] <- predict(tmp,XXT,s="lambda.min",type="response")
  RES[blocs==ii,"lasso1se"] <- predict(tmp,XXT,s="lambda.1se",type="response")
  ###elastic
  tmp <- cv.glmnet(XXA,YYA,alpha=.5,family="binomial")
  RES[blocs==ii,"elasmin"] <- predict(tmp,XXT,s="lambda.min",type="response")
  RES[blocs==ii,"elas1se"] <- predict(tmp,XXT,s="lambda.1se",type="response")
}
RES[1:4,]
round(apply(RES,2,erreur,Y=RES[,1]),2)

