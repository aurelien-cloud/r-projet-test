


  # problème, bases de données et objectifs

  library(glmnet)
  library(rpart)
  library(randomForest)
  library(caret)
  library(tidyverse)
  library(gbm)
  library(xgboost)
  library(rpart.plot)
  library(bestglm)
  

  don <- read.table("C:/Users/aurel/Mon Drive/Certification/Bloc 3/data/ozone.txt",
                    header = T,
                    sep = ";",
                    stringsAsFactors = T)

  don <- don |> select(-Date) |> rename(Y=O3)
  
  set.seed(12345)
  
  nb <- 10
  
  blocs <- sample(rep(1:nb, length=nrow(don)))
  
  RES <- data.frame(Y = don$Y)

  
  # Comparaison de régression

  XX <- model.matrix(Y~.,data=don)[,-1]
  
  YY <- don$Y
  
  for(i in 1:nb){
    print(i)
    donA = don[blocs!=i,]
    donT = don[blocs==i,]
    XXA = XX[blocs!=i,]
    XXT = XX[blocs==i,]
    YYA = YY[blocs!=i]

    # Modèle global
    mod1 <- lm(Y~.,data=donA)
    RES[blocs==i,"glob"] <- predict(mod1,donT)
    
    # Modèles AIC et BIC
    mod2 <- step(mod1,trace=0)
    RES[blocs==i,"AIC"] <- predict(mod2,donT)
    mod3 <- step(mod1,trace=0,k=log(nrow(donA)))
    RES[blocs==i,"BIC"] <- predict(mod3,donT)
    
    # Arbre
    arbre <- rpart(Y~.,data=donA)
    RES[blocs==i,"arbre"] <- predict(arbre,donT)
    
    # Forêt
    foret <- randomForest(Y~.,data=donA)
    RES[blocs==i,"foret"] <- predict(foret,donT)
    
    # Ridge 
    tmp <- cv.glmnet(XXA,YYA,alpha=0)
    RES[blocs==i,"ridgemin"] <- predict(tmp,XXT,s="lambda.min")
    RES[blocs==i,"ridge1se"] <- predict(tmp,XXT,s="lambda.1se")
    
    # Lasso
    tmp <- cv.glmnet(XXA,YYA,alpha=1)
    RES[blocs==i,"lassomin"] <- predict(tmp,XXT,s="lambda.min")
    RES[blocs==i,"lasso1se"] <- predict(tmp,XXT,s="lambda.1se")
    
    # Elastic-net
    tmp <- cv.glmnet(XXA,YYA,alpha=.5)
    RES[blocs==i,"elasmin"] <- predict(tmp,XXT,s="lambda.min")
    RES[blocs==i,"elas1se"] <- predict(tmp,XXT,s="lambda.1se")
    
    # Boosting
    modele_gbm <- gbm(
      formula = Y ~ .,
      data = donA,
      distribution = "gaussian",  # erreur quadratique (pour binaire, mettre bernoulli)
      n.trees = 100, # ce sont des itérations
      interaction.depth = 3, # profondeur maximum de chaque arbre (nombre de feuilles)
      shrinkage = 0.1, # lambda : paramètre de frein pour la descente de gradient
      cv.folds = 10, # nombre de folds de validation croisée
      bag.fraction = 0.5, 
      n.minobsinnode = 5 # nombre minimal d'observations dans les feuilles (normalement on n'utilise pas ça)
    )
    RES[blocs==i, "gbm"] <- predict(modele_gbm, donT)
  }

  erreur <- function(X,Y){mean((X-Y)^2)}
  
  round(apply(RES,2,erreur,Y=RES[,1]),2)


  
  
  
  
  # Comparaison de classifications supervisées
  
  don = read_csv("C:/Users/aurel/Mon Drive/Certification/Bloc 3/data/SAheart.csv")
  
  don <- don |> rename(Y = chd)
  
  nb <- 10
  
  set.seed(1234)
  
  blocs <- sample(rep(1:nb,length=nrow(don)))
  
  RES <- data.frame(Y=don$Y)

  dmy <- dummyVars(Y~ ., data = don)
  
  XX <- predict(dmy,don)
  

  YY <- don$Y
  
  for(ii in 1:nb){
    print(ii)
    donA = don[blocs!=ii,]
    donT = don[blocs==ii,]
    XXA = XX[blocs!=ii,]
    XXT = XX[blocs==ii,]
    YYA = YY[blocs!=ii]

    mod1 <- glm(Y~.,data=donA,family="binomial")
    RES[blocs==ii,"glob"] <- predict(mod1,donT,type="response")
    
    mod2 <- step(mod1,trace=0)
    RES[blocs==ii,"AIC"] <- predict(mod2,donT,type="response")
    
    mod3 <- step(mod1,trace=0,k=log(nrow(donA)))
    RES[blocs==ii,"BIC"] <- predict(mod3,donT,type="response")
    
    # Arbre
    arbre <- rpart(factor(Y)~.,data=donA)
    RES[blocs==ii,"arbre"] <- predict(arbre,donT,type="prob")[,2]
    
    # Arbre
    foret <- randomForest(factor(Y)~.,data=donA)
    RES[blocs==ii,"foret"] <- predict(foret,donT,type="prob")[,2]
    
    # Ridge
    tmp <- cv.glmnet(XXA,YYA,alpha=0,family="binomial")
    RES[blocs==ii,"ridgemin"] <- predict(tmp,XXT,s="lambda.min",type="response")
    RES[blocs==ii,"ridge1se"] <- predict(tmp,XXT,s="lambda.1se",type="response")
    
    # Lasso
    tmp <- cv.glmnet(XXA,YYA,alpha=1,family="binomial")
    RES[blocs==ii,"lassomin"] <- predict(tmp,XXT,s="lambda.min",type="response")
    RES[blocs==ii,"lasso1se"] <- predict(tmp,XXT,s="lambda.1se",type="response")
    
    # Elastic net
    tmp <- cv.glmnet(XXA,YYA,alpha=.5,family="binomial")
    RES[blocs==ii,"elasmin"] <- predict(tmp,XXT,s="lambda.min",type="response")
    RES[blocs==ii,"elas1se"] <- predict(tmp,XXT,s="lambda.1se",type="response")
  }

  erreur <- function(X,Y){mean((X-Y)^2)}
  
  round(apply(RES,2,erreur,Y=RES[,1]),3)
  
