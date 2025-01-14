


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
  

  
  don <- read.table("C:/Users/cepe-s4-09/Desktop/ravary_2025-01-13/ozone.txt",
                    header=T,sep=";",stringsAsFactors = T)

  
  don <- don[,c(2:13)]
  don <- rename(don, Y=O3)

  set.seed(12345)
  nb=10
  
  blocs <- sample(rep(1:nb, length=nrow(don)))
  
  ma_foret <- randomForest(Y~., data=don, importance=T)
  summary(ma_foret)
  importance(ma_foret)
  varImpPlot(ma_foret)
  
  plot(ma_foret$mse)
  


  for(ii in 1:nb){
    donA = don[blocs!=ii,]
    donT = don[blocs==ii,]
    foret <- randomForest(Y ~ ., data=donA, ntree=2000, mtry=4)
    RES[blocs==ii, "foret"] <- predict(foret, donT)
  }
  
  erreur <- function(X, Y) {mean((X - Y)^2)}
  round(apply(RES, 2, erreur, Y=RES[,1]), 2)
  
  
  
  
  
  # ozone avec le package gbm
  
  don <- read.table("C:/Users/cepe-s4-09/Desktop/ravary_2025-01-13/ozone.txt",
                    header=T,sep=";",stringsAsFactors = T)
  
  don <- don[,c(2:13)]
  don <- rename(don, Y=O3)
  
  set.seed(12345)
  nb=10
  
  blocs <- sample(rep(1:nb, length=nrow(don)))
  
  RES <- data.frame(Y=don$Y)
  
  for(i in 1:nb){
    donA = don[blocs!=i,]
    donT = don[blocs==i,]
    modele_gbm <- gbm(
      formula = Y ~ .,
      data = donA,
      distribution = "gaussian",  # erreur quadratique
      n.trees = 100,
      interaction.depth = 3, # profondeur maximum de chaque arbre
      shrinkage = 0.1, # lambda
      cv.folds = 20, # nombre de folds de validation croisée
      bag.fraction = 0.5, 
      n.minobsinnode = 5 # nombre minimal d'observations dans les feuilles
    )
    RES[blocs==i, "gbm"] <- predict(modele_gbm, donT)
  }
  
  erreur <- function(X, Y) {mean((X - Y)^2)}
  round(apply(RES, 2, erreur, Y=RES[,1]), 2)
  
  # Afficher un résumé du modèle
  #summary(modele_gbm)
  
  # Faire des prédictions
  # predictions <- predict(modele_gbm, newdata = data_test, n.trees = 100, type = "response")
  
  
  
  
  
  
  
  # Comparaison de régression

  don <- read.table("C:/Users/cepe-s4-09/Desktop/ravary_2025-01-13/ozone.txt",
                    header=T,sep=";",stringsAsFactors = T)
  
  don <- don[,c(2:13)]
  don <- rename(don, Y=O3)
  
  set.seed(12345)
  nb=10
  
  blocs <- sample(rep(1:nb, length=nrow(don)))
  
  RES <- data.frame(Y=don$Y)
  
  
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

  # tracer le graphique avec gbm.perf
  
  
  # xgboost
  
  tmp <- xgboost()
  
  
  XX <- model.matrix(Y~., data=don)
  tmp <- xgboost(XX, label=don$Y,
                 param=list(eta=0.1, max_depht=1))
  
  
  # Créer les objets DMatrix pour XGBoost
  dtrain <- xgb.DMatrix(data = X_train)
  dtest <- xgb.DMatrix(data = X_test)
  
  # Définir les paramètres du modèle
  params <- list(
    objective = "reg:squarederror",
    max_depth = 3,
    eta = 0.1
  )
  
  # Entraîner le modèle XGBoost
  model <- xgboost(
    params = params,
    data = dtrain,
    nrounds = 100,
    verbose = 0
  )
  
  # Faire des prédictions sur l'ensemble de test
  predictions <- predict(model, dtest)
  
  
  
  
  
  
  
  ####COMPARAISON de régression : ici pour la classification supervisée
  don=readRDS("donI.RDS")
  dim(don)


  dim(don)
  nb=10
  set.seed(1234)
  blocs <- sample(rep(1:nb,length=nrow(don)))
  RES <- data.frame(Y=don$Y)
  ### en matrices pour glmnet

  dmy <- dummyVars(Y~ ., data = don)
  XX <- predict(dmy,don)
  #XX <- model.matrix(Y~.,data=don)[,-1]
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
  saveRDS(RES,"RESI.RDS")
  

  
  don <- read.table("ozone.txt",header=T,sep=";",
                    row.names = 1,stringsAsFactors = T)
  dim(don)

  don=rename(don,Y=O3)
  #####

  arbre <- rpart(Y~.,data=don)
  
  rpart.plot(arbre)

  arbre2 <- rpart(Y~.,data=don[-c(1:5),])
  rpart.plot(arbre2)


  set.seed(1234)
  foret500 <- randomForest(Y~.,data=don,importance=T)
  summary(foret500)
  importance(foret500)
  varImpPlot(foret500)
  set.seed(1234)
  foret100_d <- randomForest(Y~.,data=don,ntree=100)
  set.seed(1234)
  foret100_1 <- randomForest(Y~.,data=don,ntree=100,mtry=1)
  plot(foret500$mse)
  points(1:100,foret100_d$mse,col=2,pch=16)
  points(1:100,foret100_1$mse,col=4,pch=16)
  set.seed(1234)
  foret100_11 <- randomForest(Y~.,data=don,ntree=100,mtry=11)
  points(1:100,foret100_11$mse,col=3,pch=16)
  
  
 
  testgbm_d <- gbm(Y~.,data=don,
                   distribution ="gaussian",
                   n.trees=500,
                   interaction.depth=1,
                   shrinkage=0.1,
                   cv.folds=10)
  gbm.perf(testgbm_d)
  nbopt <- gbm.perf(testgbm_d)
  prev1 <- predict(testgbm_d,n.trees=nbopt)
  testgbmopt <- gbm(Y~.,data=don,
                    distribution ="gaussian",
                    n.trees=nbopt,
                    interaction.depth=1,
                    shrinkage=0.1)
  prev2 <- predict(testgbmopt,n.trees=nbopt)
  prev3 <- predict(testgbm_d)
  
  # xgboost
  
  don <- read.csv("C:/Users/cepe-s4-09/Desktop/ravary_2025-01-13/SAheart.csv", sep=",", header=TRUE, row.names=1, stringsAsFactors=TRUE)
  
  don <- rename(don, Y=chd)
  
  XX=model.matrix(Y~., data=don)
  
  dtrain <- xgb.DMatrix(data = XX, label = don$Y)
  
  params <- list(
    objective = "binary:logistic",  # Regression (reg:squarederror)
    max_depth = 1,                   # Maximum depth of trees
    eta = 0.1,                       # Learning rate
    eval_metric = "auc"             # Evaluation metric
  )
  
  cv_results <- xgb.cv(
    params = params,
    data = dtrain,
    nfold = 10,          # 10-fold cross-validation
    nrounds = 100,      # Maximum number of rounds to test
    early_stopping_rounds = 10,  # Stop early if no improvement
    verbose = FALSE
  )
  
  titi = cv_results$evaluation_log
  plot(titi$iter,titi$train_auc_mean)
  points(titi$iter,titi$test_auc_mean,col=2)
  best_nrounds <- cv_results$best_iteration
  
  final_model <- xgb.train(
    params = params,
    data = dtrain,
    nrounds = best_nrounds
  )
  
  
  
  summary(final_model)
  
  ####COMPARAISON de régression
  don=readRDS("donI.RDS")
  
  nb=10
  set.seed(1234)
  blocs <- sample(rep(1:nb,length=nrow(don)))
  RES <- data.frame(Y=don$Y)
  ### en matrices pour glmnet
  
  dmy <- dummyVars(Y~ ., data = don)
  XX <- predict(dmy,don)
  #XX <- model.matrix(Y~.,data=don)[,-1]
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
    
    ###arbre
    arbre <- rpart(factor(Y)~.,data=donA)
    RES[blocs==ii,"arbre"] <- predict(arbre,donT,type="prob")[,2]
    
    ###arbre
    foret <- randomForest(factor(Y)~.,data=donA)
    RES[blocs==ii,"foret"] <- predict(foret,donT,type="prob")[,2]
    
    ###gbm
    gb <- gbm(Y~.,data=donA,distribution="bernoulli",cv.folds=10,n.minobsinnode=5)
    RES[blocs==ii,"gbmvc"] <- predict(gb,donT,type="response")
    nop <- gbm.perf(gb)
    gbopt <- gbm(Y ~ ., data=donA, distribution="bernoulli",n.trees=nop,n.minobsinnode=5)
    RES[blocs==ii,"gbm2e"] <- predict(gbopt,donT,type="response")
    
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
  saveRDS(RES,"RESI.RDS")
  
  
  ###nettoyage
  don <- read.table("DONNEES/ozone.txt",header=T,
                    sep=";",row.names = 1,stringsAsFactors = T)
  summary(don)
  don$Y = don$O3
  don$O3 = NULL
  dim(don)
  saveRDS(don,"don.RDS")
  ###########
  typevar = sapply(don,class)
  XX <- don[,typevar!="factor"] %>% select(-Y)
  Xca = XX^2
  Xcu = XX^3
  colnames(Xca) <- paste(colnames(XX),"car",sep="")
  colnames(Xcu) <- paste(colnames(XX),"cub",sep="")
  donP <- cbind(don,Xca,Xcu)
  dim(donP)
  saveRDS(donP,"donP.RDS")
  
  
  ###nettoyage
  don <- read.table("DONNEES/donapp.csv",header=T,sep=",")
  summary(don)
  don$Nacelle_angle = NULL
  saveRDS(don,"don.RDS")
  typevar = sapply(don,class)

  XX <- don[,typevar!="factor"] %>% select(-Y)
  Xca = XX^2
  Xcu = XX^3
  colnames(Xca) <- paste(colnames(XX),"car",sep="")
  colnames(Xcu) <- paste(colnames(XX),"cub",sep="")
  donP <- cbind(don,Xca,Xcu)
  dim(donP)
  saveRDS(donP,"donP.RDS")
  
  tmp <- model.matrix(Y~.^2,data=don)[,-1]
  dim(tmp)
  donI <- data.frame(Y=don$Y,tmp)
  saveRDS(donI,"donI.RDS")
  
  
  
  # spline
  
  n <- 100
  set.seed(546)  
  x <- sort(runif(n))
  f <- sin(2 * pi * x)
  sigma2 <- 0.1
  erreur <- rnorm(n, 0, sqrt(sigma2))
  y <- f + erreur
  plot(x, y)
  lines(x, f)
  
  df <-data.frame(x=x, xcar=x^2, xcub=x^3, Y=y)
  
  PREV <- data.frame(Y=df$Y) 
  EST <- data.frame(Y=df$Y)
  
  nb=10
  set.seed(1234)
  bloc <- sample(rep(1:nb,length=nrow(df)))
  for(ii in 1:nb){
    print(i)
    # Création des données
    donA <- df[bloc!=i,]
    donT <- df[bloc==i,]
   
    modL <- lm(Y~x,data=donA)
    PREV[bloc==i,"lin"] <- predict(modL,donT)
   
    modQ <- lm(Y~x+xcar,data=donA)
    PREV[bloc==i,"qua"] <- predict(modQ,donT)
   
    modC <- lm(Y~.,data=donA)
    PREV[bloc==i,"cub"] <- predict(modC,donT)
  }

  
  modL <- lm(Y~x, data=df)
  EST[, "lin"] <- predict(modL, df)
 
  modQ <- lm(Y ~ x + xcar, data=df)
  EST[, "qua"] <- predict(modQ, df)

  modC <- lm(Y ~ ., data=df)
  EST[, "cub"] <- predict(modC, df)

  erreur <- function(X, Y){mean((X - Y)^2)}
  apply(PREV, 2, erreur, Y=PREV$Y)  

  apply(EST,2,erreur,Y=EST$Y)


  xnew = seq(0, 1, length=100)
  df2 <- data.frame(x=xnew, xcar=xnew^2, xcub=xnew^3, Y=y)
  prevL <- predict(modL, newdata=df2)
  prevQ <- predict(modQ, newdata=df2)
  prevC <- predict(modC, newdata=df2)
  plot(x, y)
  lines(x, f)
  lines(xnew, prevL, col=2)
  lines(xnew, prevQ, col=3)
  lines(xnew, prevC, col=4)

  
  
  library(splines)
  xx = seq(0,1,length=100)
  Bpoly <- matrix(c(xx,xx^2,xx^3),byrow=F,nrow=100)
  matplot(Bpoly,type="l")  
  
  