

# 1.Tracez en dimension 3 le nuage de 10 points.
library(scatterplot3d)
library(plot3D)
library(rgl)
#individus <- read.delim("/amuhome/p17027265/Bureau/DataAnalysis-master/data1TP2.txt")
individus <- read.delim("/home/raccoon/Desktop/data1TP2.txt")
individus <- na.omit(individus)
#gens <- subset(individus, select = c(Stature, Poids, Taille))
A <- individus[,1:3]
scatterplot3d(A)

#2. Écrivez le tableau centré B, et la matrice de covariance V.
B <- scale(A, center = TRUE, scale = FALSE)
B
V <- cov(A)
V

#3. Déterminez la représentation spectrale (valeurs propres et vecteurs propres de V).
valeurP <- eigen( V)$values
vecteurP <- eigen( V)$vectors

#4. Indiquez les axes principaux (dans l’ordre).
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

# 5. Générez le tableau C en multipliant B par les vecteurs propres de V.
# *vérifiez vos nouvelles données générées avec la fonction princomp(A)$scores
GenererTableauC <- function(B,vp) {
  # C <- matrix(nrow = 10,ncol=3);
  C <- B  %*%  vp 
  return(C) 
}

C <- GenererTableauC(B,vecteurP)
C
princomp(A)$scores

#6. Observez en dimension 3 le nuage de points avec tracé du premier axe principal.

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

# 7. Représentez le nuage de points en dimension 2, projetés des points de départ sur le plan formé des
# deux premiers axes principaux.

P = matrix(c(C[,1],C[,2]), ncol=2)
plot(P[,1], P[,2], main="Nuage de Point", xlab="Stature", ylab="Poids", pch=19)
  
plot(C[,1],C[,2])

# 8. Interprétez les résultats obtenus.

# La stature est l'élément le plus important, compte tenu des vlp.


