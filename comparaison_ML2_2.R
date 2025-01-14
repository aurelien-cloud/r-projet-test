

# grande dimension : p grand
# données massives : n grand


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
  library(fastcluster)
  library(dbscan)
  library(fpc) # ça fait aussi du dbscan
  library(mclust)
  library(FactoMineR)
  
  
  
  D <- data.frame(X1 = c(0, 10, 11, 0, 10, 11),
                  X2 = c(0, 0, 0, 1, 1, 1))

  plot(D)
  
  a1 <- kmeans(D, centers=D[c(1,4),])
  a2 <- kmeans(D, centers=D[c(1,4),], algorithm="Lloyd")
  a3 <- kmeans(D, centers=D[c(1,4),], algorithm="MacQueen")
  a1$cluster

  a1$tot.withinss

  a2$cluster

  a2$tot.withinss

  a3$cluster

  a3$tot.withinss

  a2bis <- kmeans(D,centers=2,nstart=20,algorithm="Lloyd")
  a2bis$cluster

  don <- read.table("C:/Users/cepe-s4-09/Desktop/ravary_2025-01-13/donclassif.txt",
                    header=T,sep=";",stringsAsFactors = T)

  plot(don)
  
  a1 <- kmeans(don, centers=don[c(1,4),])
  a2 <- kmeans(don, centers=don[c(1,4),], algorithm="Lloyd")
  a3 <- kmeans(don, centers=don[c(1,4),], algorithm="MacQueen")

  a1$cluster
  a1$tot.withinss
  
  a2$cluster
  a2$tot.withinss
  
  a3$cluster
  a3$tot.withinss
  
  a2bis <- kmeans(don, centers=4)
  a2bis$cluster

  plot(don, col=a2bis$cluster)
  
  points(a2bis$centers, cex=3, pch=16, col=1:4)
  
  legend("topright", legend=paste("Classe", 1:4), col=1:4,pch=16)  

  
  
  
  # CAH
  
  
  donnees <- data.frame(x=c(0,0,0.25,4,5,4.2,4.8,5),
                        y=c(1,1.5,1,4,3.9,2,2.1,1))
  
  rownames(donnees) <- LETTERS[1:8]
  round(dist(donnees),2)
  cah <- hclust(dist(donnees), method="single")
  plot(as.dendrogram(cah))
  plot(sort(cah$height,dec=T),type="h")
  gpcah <- cutree(cah,h=3)
  plot(donnees,col=gpcah)

  cah <- hclust(dist(don, "manhattan"), method="single")
  plot(rev(sort(cah$height))[1:50], type="h")
  
  plot(as.dendrogram(cah))
  plot(sort(cah$height,dec=T), type="h")
  gpcah <- cutree(cah,h=5)
  plot(don, col=gpcah)
  
  cah <- hclust(dist(don), method="ward.D2")
  plot(as.dendrogram(cah))
  plot(sort(cah$height,dec=T), type="h")
  gpcah <- cutree(cah,h=3)
  plot(don,col=gpcah)
  
  cah <- hclust(dist(don), method="centroid")
  plot(as.dendrogram(cah))
  plot(sort(cah$height, dec=T), type="h")
  gpcah <- cutree(cah,h=3)
  plot(don,col=gpcah)
  
  hc <- hclust(dist(don), method = "complete")

  cah <- hclust(dist(don), method="mcquitty")
  plot(as.dendrogram(cah))
  plot(sort(cah$height,dec=T), type="h")
  gpcah <- cutree(cah,h=3)
  plot(don,col=gpcah)
  
  plot(hc)  
    
  str(cah)
  
  # dbscan
  
  
  don <- read.table("C:/Users/cepe-s4-09/Desktop/ravary_2025-01-13/donclassif.txt",
                    header=T,sep=";",stringsAsFactors = T)
  
  don_data <- as.matrix(don)
  
  kNNdistplot(don_data, k = 10) # k : nombre de plus proches voisins
  
  abline(h = 0.4, lty = 2)

  resultat <- dbscan(don_data, eps = 0.5, minPts = 0.5)
  
  plot(resultat, don_data, main = "Clusters DBSCAN")
    
  args(hclust)
  
  
  
  # Mclust
  

   melange <- Mclust(don,1:12)
   summary(melange)
   names(summary(melange))
   melange$bic
   plot(don, col=summary(melange)$classification, cex=.3,main="K=10")
  

  melange$BIC
  
  # ACP
  
  library(factoextra)
  
  # Importation des données
  decathlon <- read.table("http://www.agrocampus-ouest.fr/math/livreR/decathlon.csv", 
                          header=TRUE, sep=";", dec=".", row.names=1, 
                          check.names=FALSE, fileEncoding="latin1")
  
  data(decathlon)
  
  res.pca <- PCA(decathlon[,1:10], graph = FALSE)
  
  summary(res.pca)
  
  fviz_eig(res.pca, addlabels = TRUE)
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ##########Classif
  don <- read.table("DONNEES/donclassif.txt",header=T,sep=";")
  dim(don)
  library(ggplot2)
  ggplot(don,aes(V1,V2))+geom_point()
  ###test de kmeans
  gp5 <- kmeans(don,5)
  names(gp5)
  ggplot(don,aes(V1,V2))+geom_point(col=gp5$cluster)       
  gp5
  
  inerI <- 1:30
  for(ii in 1:30){
    tmp <- kmeans(don,centers=ii,nstart=10,iter=20)
    inerI[ii] <- tmp$betweenss
  }
  plot(1:30,inerI/tmp$totss*100,type="h")
  names(tmp)
  gp5 <- kmeans(don,5,nstart=100)
  names(gp5)
  ggplot(don,aes(V1,V2))+geom_point(col=gp5$cluster)       
  gp5
  par(mfrow=c(1,2))
  plot(don,col=gp5$cluster,pch=16,cex=.5)
  points(gp5$centers,pch=16,col=1:5)
  gp4 <- kmeans(don,4,nstart=100)
  plot(don,col=gp4$cluster,pch=16,cex=.5)
  points(gp4$centers,pch=16,col=1:4)
  
  
  don <- read.table("DONNEES/donclassif2.txt",header=T,sep=";")
  dim(don)
  inerI <- 1:10
  for(ii in 1:10){
    tmp <- kmeans(don,centers=ii,nstart=10,iter=20)
    inerI[ii] <- tmp$betweenss
  }
  par(mfrow=c(1,1))
  plot(1:10,inerI/tmp$totss*100,type="h")
  gp4 <- kmeans(don,4,nstart=100)
  plot(don,col=gp4$cluster,pch=16,cex=.5)
  points(gp4$centers,pch=16,col=1:4)
  
  
  don <- read.table("DONNEES/donclassif.txt",header=T,sep=";")
  donM2 <- dist(don,"euclidean")
  donM1 <- dist(don,"manhattan")
  #######
  cahs2 <- hclust(donM2,"single")
  plot(rev(sort(cahs2$height))[1:50],type="h")
  labelcahs2 <- cutree(cahs2,k=35)
  table(labelcahs2)
  plot(don,col=labelcahs2,pch=16)
  ggplot(don,aes(V1,V2))+geom_point(col=labelcahs2,pch=labelcahs2)
  
  
  
  
  cahs1 <- hclust(donM1,"single")
  cahw2 <- hclust(donM2,"ward.D")
  plot(rev(sort(cahw2$height))[1:30],type="h")
  abline(h=0.43)
  labelcahw2 <- cutree(cahw2,k=4)
  
  cahw1 <- hclust(donM1,"ward.D")
  plot(rev(sort(cahw1$height))[1:30],type="h")
  labelcahw1 <- cutree(cahw2,k=5)
  
  
  
  cahm2 <- hclust(donM2,"complete")
  plot(rev(sort(cahw2$height))[1:30],type="h")
  abline(h=1000)
  labelcahm2 <- cutree(cahm2,k=4)
  table(labelcahm2)
  plot(don,col=labelcahm2,pch=16)
  
  
  cahm1 <- hclust(donM1,"complete")
  caha2 <- hclust(donM2,"average")
  caha1 <- hclust(donM1,"average")
  
  plot(sort(cahs2$height)[1:30])
  
  
  library(dbscan)
  library(fpc)
  
  testdb <- dbscan::dbscan(don,0.2)
  testfp <- fpc::dbscan(don,0.2)
  names(testdb)
  names(testfp)
  table(testdb$cluster,testfp$cluster)
  ###############################
  library(microbenchmark)
  tmp <- microbenchmark(dbscan::dbscan(don,0.2),
                        fpc::dbscan(don,0.2),times=10)
  plot(tmp)
  tmp
  #donc on choisit dbscan de dbscan(dbscan)
  remove.packages("fpc")
  db02 <- dbscan(don,0.2)
  table(db02$cluster)
  db021 <- dbscan(don,0.21)
  table(db021$cluster)
  db019 <- dbscan(don,0.19)
  table(db019$cluster)
  kNNdistplot(don,3)
  
  #################################
  par(mfrow=c(1,2))
  plot(don,col=db021$cluster,pch=16)
  points(don[db021$cluster==0,])
  table(db021$cluster)
  
  table(labelcahs2)
  ##tous les groupes inf à xx mettre dans un groupe label 0
  compt <- table(labelcahs2)
  which(compt<5)
  etif <- labelcahs2
  gp0 <- which(compt<5)
  for(ii in gp0){
    etif[etif==ii] <- 0
  }
  table(etif)
  plot(don,col=etif,pch=16)
  points(don[etif==0,])
  
  
  don1 <- don[etif==1,]
  donC <- split(don,etif)
  length(donC)
  par(mfrow=c(1,1))
  plot(don,col=gp5$cluster)
  points(x=2,y=2,pch=16)
  
  madist <- function(X,centre){
    tmp <- apply((X-centre)^2,1,sum)
    return(which.min(tmp))
  }
  
  madist(c(2,2),gp5$centers)
  
  don <- read.table("DONNEES/donclassif2.txt",header=T,sep=";")
  library(dbscan)
  db02 <- dbscan::dbscan(don,0.3)
  table(db02$cluster)
  
  
  
  donM2 <- dist(don,"euclidean")
  
  
  
