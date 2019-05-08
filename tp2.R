library(scatterplot3d)
library(plot3D)
library(rgl)
individus <- read.delim("/amuhome/p17027265/Bureau/DataAnalysis-master/data1TP2.txt")
individus <- na.omit(individus)
#gens <- subset(individus, select = c(Stature, Poids, Taille))
A <- individus[,1:3]
scatterplot3d(A)

B <- scale(A, center = TRUE, scale = FALSE)
B
V <- cov(A)
V

valeurP <- eigen( V)$values
vecteurP <- eigen( V)$vectors

#https://bioinfo-fr.net/lanalyse-en-composantes-principales-avec-r#exemple
pca <- prcomp(A)
pca$rotation
ecartType <- pca$sdev
proporVariance <- 100 * pca$sdev^2 / sum(pca$sdev^2)  
varianceTotale <- sum(100 * (pca$sdev^2)[1:2] / sum(pca$sdev^2)) # variance totale entre 2 premières colonnes 

ecartType
proporVariance
varianceTotale
plot(pca)

GenererTableauC <- function(B,vp) {
  # C <- matrix(nrow = 10,ncol=3);
  C <- B  %*%  vp 
  return(C) 
}

GenererTableauC(B,vecteurP)
princomp(A)$scores

#6 

pc1 <- pca$rotation[,1]

pc1 <- as.vector(pc1)
x <- c(pca[2]$rotation[1,1],0)
y <- c(pca[2]$rotation[2,1],0)
z <- c(pca[2]$rotation[3,1],0)

plot3d(x,y,z, type="l")
scatter(pc2,pc3)

traceAxe <- function(A,pc1) {
  C <- matrix(nrow = 10,ncol=1);
  for( i in 1:10){
    C[i,1] <- pc1[1] * A[i,1] + pc1[2] * A[i,2] + pc1[3] * A[i,3]
  }
  return(C)
}

traceAxe(A,pc1)

plt<-scatterplot3d(pca$rotation)
plt$points3d(pc1, type="l", col="blue", lwd=2)


