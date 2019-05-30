
# TP3 POUJOL TEDDY COQUILLARD REMI
# I GENERATION DE POINTS

# création de la matrice contenant les 300 points à classer
mligne <- 300
matrice <- matrix(NA,nrow = mligne, ncol = 2)
rownames(matrice)<-c(1:300)

# a) génération x et y uniformes sur [0,1]
unifX <- runif(100,0,1)
unifY <- runif(100,0,1)

# b) génération x et y gaussiennes de variance 1 avec x de moyenne 4 et y centrée
norm1X <- rnorm(100,4,1)
norm1Y <- rnorm(100,0,1)

# c) génération x et y gaussiennes de variance 2, avec x de moyenne 0.5 et y de moyenne 6
norm2X <- rnorm(100,0.5,sqrt(2))
norm2Y <- rnorm(100,6,sqrt(2))

# Remplissage de la matrice
for (i in 1:100){
  matrice[i,1] <- unifX[i]
  matrice[i,2] <- unifY[i]
  
  matrice[100+i,1] <- norm1X[i]
  matrice[100+i,2] <- norm1Y[i]
  
  matrice[200+i,1] <- norm2X[i]
  matrice[200+i,2] <- norm2Y[i]
}


# fonction distance Euclidienne permettant d'avoir la distance entre deux points
distance_euclidienne<-function(x1,y1,x2,y2){
  X = cbind(x1,x2)
  Y = cbind(y1,y2)
  dist <- sqrt((X[,2]- Y[,2])^2 + (X[,1] - Y[,1])^2)
  return(dist)
}

# II Classification non supervisée

# fonction de classification rendant les points centraux des classes
classiAscendantehierarchique<- function(K,m,matrice){
  
  # matrice distance contenant les distance entre tout les points de la matrice
  dist <- dist(matrice,method="euclidean")
  
  # conversion de dist en matrice
  dist <- as.matrix(dist)
  
  # transformation de dist en matrice triangulaire inférieur
  dist[upper.tri(dist)] <- NA 
  diag(dist) <- NA
  
  # recherce de la distance minimum dans la matrice distance
  elim <- which(dist==min(dist,na.rm=T),arr.ind = T)
  # recupération des deux éléments contenant les lignes et colonnes à supprimer
  elimX <- elim[,1]
  elimY <- elim[,2]
  # calcul moyenne
  moyenneX <- (matrice[elimX,1]+matrice[elimY,1])/2
  moyenneY <- (matrice[elimX,2]+matrice[elimY,2])/2
  
  
  dist <- dist(matrice,method="euclidean")
  dist <- as.matrix(dist)
  
  dist[upper.tri(dist)] <- NA
  diag(dist) <- NA
  
  # tant que le nombre de ligne de dist est supérieur au nombre de classes
  while(nrow(dist) > K){
    # on recherche la distance minimum
    elim <- which(dist==min(dist,na.rm=T),arr.ind = T)
    elimX <- elim[1,1]
    elimY <- elim[1,2]
    
    # calcul moyenne
    moyenneX <- (matrice[elimX,1]+matrice[elimY,1])/2
    moyenneY <- (matrice[elimX,2]+matrice[elimY,2])/2
    
    # debut fusion en remplace la ligne par la moyenne des deux
    matrice[elimX,1] <- moyenneX
    matrice[elimX,2] <- moyenneY
    
    # renommage des lignes dans la matrice principale
    rownames(matrice)[elimX]<-paste(rownames(matrice)[elimX],rownames(matrice)[elimY])
    
    # suppression seconde ligne
    matrice <- matrice[-elimY,]
    
    # suppression la seconde ligne avec la colonne associée
    dist <- dist[-elimY,]
    dist <- dist[,-elimY] 
    
    # redimensionnement de la matrice dist
    dist <- dist(matrice,method="euclidean")
    dist <- as.matrix(dist)
    
    dist[upper.tri(dist)] <- NA
    diag(dist) <- NA
    
  }
  
  # retourne la matrice avec les centres des classes 
  return (matrice)
}

# Récupération du centre des classes
centreClasse <- classiAscendantehierarchique(3,300,matrice)

# Récupération des points dans chaque classes 
tab <- rownames(centreClasse)
t1 <- strsplit(tab[1]," ")
t2 <- strsplit(tab[2]," ")
t3 <- strsplit(tab[3]," ")

# conversion des noms des lignes en int
classe1 <- as.numeric(unlist(t1))
classe2 <- as.numeric(unlist(t2))
classe3 <- as.numeric(unlist(t3))

# Regroupement des points avec les classes associées 
classe <- cbind(matrice, c(0))

for(i in 1:length(classe1)){
  classe[classe1[i],3] <- 1
}
for(i in 1:length(classe2)){
  classe[classe2[i],3] <- 2
}
for(i in 1:length(classe3)){
  classe[classe3[i],3] <- 3
}
centreClasse <- cbind(centreClasse,c(1,2,3))

classe

# affichage nuages de points avec les centres de classes 
plot(classe,pch=16, col=factor(classe[,3]))
points(centreClasse,pch=21,col="cyan",lwd=5 ,bg=factor(centreClasse[,3]),cex=3)


# Vérification avec le programme donné
# matrice distances
D <- dist(matrice, method = "euclidean")

AscHierarchique <- hclust(D, method= "complete")

# affichage du dendrogram
plot(AscHierarchique, cex = 0.6, hang = -1)

# cluster
cluster = cutree(AscHierarchique,3)

# affichage du cluster
plot(cluster)

# diagramme inertie en fonction du nombre de classes pour 
# justifier le choix du nombre de classes
inertie <- sort(AscHierarchique$height, decreasing = TRUE)
plot(inertie[1:20], type = "s", xlab = "Nombre de classes", ylab = "Inertie")
points(c(2, 4, 5), inertieD[c(2, 4, 5)], col = c("green3", "red3", 
                                                 "blue3"), cex = 2, lwd = 3)

# coloriage du dendrogram avec les choix de classes pertinent pour l'étude
plot(AscHierarchique, labels = FALSE, main = "Partition en 2, 4 ou 5 classes", 
     xlab = "", ylab = "", sub = "", axes = FALSE, hang = -1)
rect.hclust(AscHierarchique, 2, border = "green3")
rect.hclust(AscHierarchique, 4, border = "red3")
rect.hclust(AscHierarchique, 5, border = "blue3")

# autre fonction de calcul des barycentres
barycentre2 <- function(dfxy, fac, wt = rep(1, length(fac))){
  f1 <- function(cl) {
    n <- length(cl)
    cl <- as.factor(cl)
    x <- matrix(0, n, length(levels(cl)))
    x[(1:n) + n * (unclass(cl) - 1)] <- 1
    dimnames(x) <- list(names(cl), levels(cl))
    data.frame(x)
  }
  dfxy <- data.frame(dfxy)
  dfdistri <- f1(fac) * wt
  w1 <- unlist(lapply(dfdistri, sum))
  dfdistri <- t(t(dfdistri)/w1)
  coo2 <- t(dfdistri) %*% as.matrix(dfxy)
  rownames(coo2) <- levels(fac)
  coo2
}

ba1 <- barycentre2(matrice, cluster)

# la distance entre le barycentre du nuage de points et les barycentres des classes :
db1 <- (t(ba1)-colMeans(matrice))^2
db1 <- colSums(db1)

# multiplier par le nombre d'individus par groupe :
db1 <- db1*table(cluster)
db1
# l'inertie inter (%de l'inertie totale) :
iner.inter <- sum(db1)/sum((t(matrice)-colMeans(matrice))^2)
iner.inter

# inertie intra :
iner.intra <- 1-iner.inter
iner.intra

