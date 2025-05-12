library(caret) # necessaire pour la validation croisee stratifiee et le cv
library(tidyverse)
library(pROC)
library(gbm)
library(ranger)
library(glmnet)
library(xgboost)
library(tidymodels)
library(recipes)

# Lecture des données -----

# Répertoire de travail
setwd("L:/REVBDF")

# lecture
bank <- read.csv("bank.csv",sep=";") 

# visualisation rapide
bank %>% 
  summary()

bank %>% str()

# on n'a pas de valeurs manquantes

# Premières transformations ----

# la variable cible est "y" qui est indiquée comme étant "yes" ou "no"
# on recode toutes les variables caractères en facteurs


bank <- bank %>% 
  mutate(y=ifelse(y=="no",0,1))

my_recipe = recipe(y ~ ., data = bank) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_normalize(all_predictors())

don <- my_recipe %>% 
  prep() %>% 
  bake(new_data = bank)

don <- don %>% 
  rename(Y=y)

# Comparaison d'algo ----
# Division en Apprentissage et Test
split=0.9

trainSize=round(0.9*nrow(don))

A = sample(nrow(don), trainSize)

donApp  = don[A,]

donTest = don[-A,]

# Division en Apprentissage et Validation

nbloc=5

folds = createFolds(donApp$Y,
                    k=nbloc,
                    list=F) # a voir si createFolds garde la proportion automatiquement

# Pre-processing
donAppX = model.matrix(Y~., data=donApp)
donY = donApp$Y

# ==============================================================================
# Grilles
# ==============================================================================

# Foret
gr.foret=expand.grid(num.trees=c(100,600),
                     max.depth=c(1,3,5),
                     min.buckets=c(1,5),
                     mtry=c(1,2,5))
gr.foret.params=data.frame(gr.foret,'auc'=NA)

# SVM
gr.poly=expand.grid(C=c(0.1,10,100),
                    degree=c(1,2,3),
                    scale=1)
gr.radial=expand.grid(C=c(0.1,1,10),
                      sigma = c(0.0001,0.001,0.01,0.1,1))
ctrl = trainControl(method='cv', number=3)


# Validation croisee
SCORE=data.frame('Y'=donApp$Y,'logistic'=NA,'aic'=NA,'bic'=NA,'ridge'=NA,'lasso'=NA,'elnet'=NA,'foret'=NA)
SCORE %>% head()
SCORE %>% dim()

jj=2

for(jj in 1:nbloc){
  
  cat('Fold: ', jj, '\n')
  
  donA=donApp[folds!=jj,]
  donV=donApp[folds==jj,]
  
  donXA=donAppX[folds!=jj,]
  donXV=donAppX[folds==jj,]  
  donYA=donY[folds!=jj]
  
  
  # Logistique =================================================================
  logistic=glm(Y~., data=donA, family='binomial')
  SCORE[folds==jj,'logistic'] = predict(logistic,newdata=donV,type='response')
  
  # AIC ========================================================================
  aic=stats::step(logistic,trace=0)
  
  SCORE[folds==jj,'aic']=predict(aic,newdata=donV,type='response')
  
  # BIC ========================================================================
  bic=stats::step(logistic,trace=0,k=log(nrow(donA)))
  
  SCORE[folds==jj,'bic']=predict(bic,newdata=donV,type='response')
  
  # Penalisation =================================================================
  ridge=cv.glmnet(donXA,donYA,alpha=0  ,family='binomial',nfolds=5,type.measure='auc')
  lasso=cv.glmnet(donXA,donYA,alpha=1  ,family='binomial',nfolds=5,type.measure='auc')
  elnet=cv.glmnet(donXA,donYA,alpha=0.5,family='binomial',nfolds=5,type.measure='auc')
  SCORE[folds==jj,'ridge']=predict(ridge,newx=donXV,type='response',s='lambda.min')
  SCORE[folds==jj,'lasso']=predict(lasso,newx=donXV,type='response',s='lambda.min')
  SCORE[folds==jj,'elnet']=predict(elnet,newx=donXV,type='response',s='lambda.min')
  
  # Foret ======================================================================
  
  ### Hyper-parametrage
  for(xx in 1:nrow(gr.foret)){
    foret = ranger(factor(Y)~., data=donA,
                   classification = T,
                   probability = T,
                   num.trees = gr.foret$num.trees[xx],
                   max.depth = gr.foret$max.depth[xx])
    gr.foret.results = predict(foret,donV,type='response')
    gr.foret.params[xx,'auc'] = roc(donV$Y,gr.foret.results$predictions[,'1'],quiet=T)$auc
  }
  
  
  
  ### Selection du meilleur parametrage
  best.foret.params = gr.foret.params[which.max(gr.foret.params$auc),]
  best.foret = ranger(factor(Y)~., data=donA,
                      classification=T, probability = T,
                      num.trees=best.foret.params$num.trees,
                      max.depth = best.foret.params$max.depth,
                      min.bucket = best.foret.params$min.buckets,
                      mtry = best.foret.params$mtry)
  SCORE[folds==jj, 'foret']=predict(best.foret, donV, type='response')$predictions[,'1']
  

  # # XGBOOST =====================================================================
  
  indY = which(names(donA)=='Y')
  
  indY
  
  X_train = as.matrix(donA[,-indY])
  
  Y_train = donA[[indY]]
  
  X_test = as.matrix(donV[,-indY])
  
  Y_test = donV[[indY]]
  
  xgb_data_train=xgb.DMatrix(data = X_train, label = Y_train)
  
  xgb_data_test=xgb.DMatrix(data = X_test, label = Y_test)
  
  iterations = 300
  
  xgrid = expand.grid(
    max_depth = c(1,3),
    eta = c(0.1,0.05)
  )
  
  results = data.frame()
  
  for(pp in 1:nrow(xgrid)){
    
    print(pp/nrow(xgrid))
    
    params = list(
      objective = "binary:logistic",
      eval_metric = "logloss",
      max_depth = xgrid$max_depth[pp],
      eta = xgrid$eta[pp]
    )

      tmp = xgb.cv(
      params = params,
      nrounds = iterations,
      nfold = 5,
      verbose = F,
      early_stopping_rounds = 10,
      data=xgb_data_train
    )
      
    bestIter = tmp$best_iteration
    bestLogloss = tmp$evaluation_log$test_logloss_mean[bestIter]
    
    x = data.frame(cbind(xgrid[pp,],bestIter,bestLogloss))
    
    results[pp,1:nrow(xgrid)] = x
    
  }
  
  results
  
 
  
  best_params_index <- results[which.max(results$bestLogloss),] 
  
  best_params = list(
    objective = "binary:logistic",
    eval_metric = "logloss",
    max_depth = best_params_index$max_depth,
    eta = best_params_index$eta
  )
  
  xgb = xgb.train(params = best_params,data = xgb_data_train,nrounds = best_params_index$bestIter)
  
  predict(xgb,newdata = xgb_data_test)
  
  SCORE[folds==jj, 'xgb']=  predict(xgb,newdata = xgb_data_test,type='response')
  
}


rocCV = roc(factor(Y)~., data=SCORE, quiet=T)

aucmodele = sort(round(unlist(lapply(rocCV,auc)),5),dec=TRUE)

tmp = lapply(rocCV, FUN=coords, x = 'best', 
             ret = c('threshold','tp','fp','fn','tn','sensitivity','specificity','ac'),
             transpose=T)

mat=do.call(rbind,tmp)

aucmodele
mat

# faire le feature engenering
