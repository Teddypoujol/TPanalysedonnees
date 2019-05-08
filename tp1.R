# Poujol Teddy
# Info 4
# TP 1
d <- read.delim("/amuhome/p17027265/Bureau/DataAnalysis-master/data1TP1.txt", header=TRUE, sep="\t")

attach(d)
par(mfrow=c(2,3))


# Le calcul des coefficients de corrélation:
# 1. tracez le nuage de 15 points, que pouvez vous observer ?
# 
# On observe différents types de courbes, A, B, D sont monotone.
# C, D, E sont non linéaire, et A,B sont linéaire.


# Code 1
  plot(d$A,d$Y,col="red", pch=16)
  plot(d$B,d$Y, pch=16,col="blue")
  plot(d$C,d$Y, pch=16,col="magenta")
  plot(d$D,d$Y, pch=16, col="green")
  plot(d$E,d$Y, pch=16)
  

# 2. Fonction de pearson: pour calculer le coefficient r de pearson
# Pearson permet d'analyser les relations linéaires.

  pearson <- function(X,Y) {
    sX <- sqrt(var(X))
    sY <- sqrt(var(Y)) 
    p <- cov(X,Y)/(sX*sY)
    return(p)
  }
  
  pearson(d$A,d$Y) #résultat pour A : -0.9722452
  pearson(d$B,d$Y) #résultat pour B : 0.9815886
  pearson(d$C,d$Y) #résultat pour C : 0.4119462
  pearson(d$D,d$Y) #résultat pour D : 0.7513686
  pearson(d$E,d$Y) #résultat pour E : 0.210302

# Quelle variable à la plus petite corrélation ? pourquoi ?
# réponse : c'est donc E qui à la plus petit corrélation car c'est la moins linéaire.

# verifier votre programme avec la fonction cor()
# réponse: on obtient le même résultat.
  
  cor(d$A,d$Y, method="pearson") #résulat : -0.9722452
  

# 3. Fonction de spearman permet d'analyser les relations non linéaires monotones
# créer la fonction du coefficient de spearman
 
  spearman <- function(X,Y,N) {
    Xi <- rank(X)
    Yi <- rank(Y)
    somme <- 0
    for (i in 1:N) {
      somme <- somme + (Xi[i]-Yi[i])^2
    }
    coeff <- 1- ((6*somme)/(N^3 - N))
    return(coeff)
  }
  
  spearman(d$A,d$Y,15) #-0.9973214
  spearman(d$B,d$Y,15) # 0.9982143
  spearman(d$C,d$Y,15) # 0.4169643
  spearman(d$D,d$Y,15) # 1
  spearman(d$E,d$Y,15) # 0.3419643
  
  # Comparer le score obtenu au résulat de la question 2, quelle est la différence?
  
  #                 pearson:          spearman:
  # résultat pour A : -0.9722452      -0.9973214 
  # résultat pour B : 0.9815886       0.9982143
  # résultat pour C : 0.4119462       0.4169643
  # résultat pour D : 0.7513686       1
  # résultat pour E : 0.210302        0.3419643
  # 
  # pearson s'applique pour analyser les relations linéaires 
  # spearman pour analyser les relations non linéaires et monotones
  # Ainsi on remarque que pour D le score passé a 1 car D est non linéaire et monotone, c'est donc parfait pour appliquer spearman
  # de même, le score de E augmente car spearman détecte la non linéarité.
  
  cor(d$A,d$Y, method="spearman") # résultat: -0.9991067
  # Notre fonction est plus approximative que la fonction cor
  
  
# 4. Comment calculer la relation non linéaire et monotone entre E et Y ?
  
  #réponse : On peut appliquer d'abord pearson pour évaluer la linéarité, puis spearman pour la monotonie
  
# TEST PARAMETRIQUE  
# Test d'indépendance pour une variable quantitative.
  # On cherche à savoir si l'inflation 2010-2019 à affecté le cout de la vie à Marseille.
# 5. Fonction de score t

  etudiant <- read.delim("/amuhome/p17027265/Bureau/DataAnalysis-master/data2TP1.txt", header=TRUE, sep="\t")
  
  testinde <- function(X,moyth,n) {
    m <- mean(X)
    sigma <- sqrt(var(X))
    testinde <- abs(m - moyth)/(sigma/sqrt(n))
    return(testinde)
  }
  
# Soit H0: l'inflation n'a pas affecté le cout de la vie à Marseille. 
  testinde(etudiant$Marseille,19,15) # résultat: 2.177369
  # En regardant la table pour alpha = 5% et degré de liberté = n-1 = 14
  # on trouve comme valeur critique Kddl,5% = 2.145
  
  #Ainsi Qc > Kddl,5% (2.177369 > 2.145) donc on rejette H0
  
  # Donc l'inflation a peut être affecté le cout de la vie à Marseille

  
# Test d'indépendance pour 2 variables quantitatives.
  # On cherche à savoir si il existe une dépendance significative entre Marseille et Aix.
# 6. Fonction de score t pour deux variables quantitatives 
  
  testinde2 <- function(X,Y) {
    m1 <- mean(X)
    m2 <- mean(Y)
    testinde <- abs(m1 - m2)/sqrt(((var(X)/length(X))+(var(Y)/length(Y))))
    return(testinde)
  }
  
  # Soit H0: il n'existe pas de dépendance significative entre Marseille et Aix. 
  testinde2(etudiant$Marseille,etudiant$Aix) # résultat: 2.321494
  
  # En regardant la table pour alpha = 5% et degré de liberté = n-1 = 14
  # on trouve comme valeur critique Kddl,5% = 2.048
  
  #Ainsi Qc > Kddl,5% (2.321494 > 2.048) donc on rejette H0
  # Pour alpha = 5%, il existe peut être une dépendance significative entre Marseille et Aix. 
  
  #Pour Qc <= Kddl,2% (2.321494 > 2.468) on ne peut pas conclure
  # Pour alpha = 2%, on ne peut pas conclure
  
  
# TEST NON PARAMETRIQUE  
  # Test d'indépendance pour une variable qualitative.
  # On etudie l'hérédité de pois de senteur.
  #ci dessous la création du tableau phetotype
  
  ratio <- c(9, 3, 3, 1)
  observe <- c(1528, 106, 117, 381)
  phenotype <- matrix(c(ratio, observe),nrow=2,ncol=4,byrow=T)
  rownames(phenotype) <- c("ratio","observe")
  colnames(phenotype) <- c("violet, long","violet, rond", "rouge, long","rouge, rond")
  phenotype
  
# 7.a calculez la valeur théorique de chaque catégorie de phénotype.
  valthq7a <- function(X) {
    valth <- 0;
    listval <- list();
    sommeRatio <- 0;
    n <-0
    for(i in 1:4){
      sommeRatio = sommeRatio + X[1,i];
      n <- n + X[2,i]
    }
    for(i in 1:4){
      valth <- (X[1,i]/sommeRatio)*n;
      listval <- append(listval, valth);
      valth <- 0;
    }
    
      
    return(listval);
  }
  
  valth <- valthq7a(phenotype)
  valth
  # valeur théorique des catégories: 
  # 1199.25
  # 399.75
  # 399.75
  # 133.25
  
# 7.b créez fonction khi deux
  
  khideux <- function(X,vt,n) {
    X2 <-0
    for(i in 1:n){
      X2 = X2 + ((X[2,i] - vt[[i]])^2/vt[[i]])
    }
    return(X2);
  }
  
  # résultat : 966,61 
  khideux(phenotype,valth,4)
  
# 7.c En regardant la table, avec alpha= 5% et degré de liberté = 3, et comme H0 le vrai ratio est 9:3:3:1.
  # la valeur critique est donc 7.81 
  # et notre valeur est en dehors de la table du khi 2, donc le ratio est complètement faux
  
#Test d'indépendance pour les variables qualitatives.
  
  commonf <- c(29, 5, 46);
  atypicalf <- c(40,32,8);
  melanomaf <- c(18, 22, 0);
  
  commonc <- c(20, 60);
  atypicalc <- c(29, 51);
  melanomac <- c(12, 28);
  
  form <- matrix(c(commonf, atypicalf, melanomaf),nrow=3,ncol=3,byrow=T);
  color <- matrix(c(commonc, atypicalc, melanomac),nrow=3,ncol=2,byrow=T);
 
  rownames(form) <- c("Common nevus","Atypical nevus", "Melanoma")
  colnames(form) <- c("Absent","Atypical","Typical")
  
  rownames(color) <- c("Common nevus","Atypical nevus", "Melanoma")
  colnames(color) <- c("Absent","Present")
  form
  color
  
  # 8. alpha = 5% et H0 = deux variables sont independantes
  
  # Voici la fonction qui permet de calculer les valeurs théoriques pour les variables qualitatives.
  valthq8 <- function(X,l,c) {
    mat <- matrix(nrow = l,ncol=c);
    n <- sum(X);
    for(i in 1:l){
      for(j in 1:c){
        
        mat[i,j] = (sum(X[,j])* sum(X[i,]))/n;
        
      }
    }
    return(mat) 
  }
 valthForm <- valthq8(form,3,3);
 valthColor <-valthq8(color,3,2)
 
 # application du khi deux à une matrice
 khideux8 <- function(X,matvalth,l,c) {
   X2 <-0
   for(i in 1:l){
     for(j in 1:c){
       X2 = X2 + ((X[i,j] - matvalth[i,j])^2/matvalth[i,j])
     }
     
   }
   return(X2);
 }
 
 khideux8(form,valthForm,3,3)
 khideux8(color,valthColor,3,2)
 
 # >  khideux8(form,valthForm,3,3)
 # [1] 75.1564
 # >  khideux8(color,valthColor,3,2)
 # [1] 2.39415
 
 # d'apres la table on obtient comme valeurs critiques à 5%: 3.84 pour la couleur et 5.99 pour la forme
 # ainsi pour la couleur on ne peut pas conclure
 # et pour la forme on rejette H0, ainsi la forme peut être importante pour détecter un mélanome.
 
# 9 
# Les données quantitatives comprennent les dénombrements et les mesures. 
# Les tests paramétriques se basent sur des données quantitatives. Par conséquent, 
# certaines conditions de validité doivent être vérifiées pour que le résultat d’un test paramétrique soit fiable.
# Par exemple, le test t de Student pour échantillons indépendants n’est fiable que si les données associées à chaque échantillon suivent une distribution normale et
# si les variances des échantillons sont homogènes. Ainsi on ne peut appliquer student à des données qualitatives car le résultat ne sera pas fiable
# 
#
# Les données qualitatives peuvent être assimilées au cas des variables discontinues
# Les tests non-paramétriques ne se basent pas sur des distributions statistiques. 
# Ils peuvent donc être utilisés même si les conditions de validité des tests paramétriques ne sont pas vérifiées.
# 
# Les tests paramétriques ont souvent des tests non-paramétriques équivalents. 
# 
# 
# Les tests non-paramétriques sont plus robustes que les tests paramétriques.
# En d'autres termes, peuvent être utilisés dans un plus grand nombre de situations.
# 
# 
# Les tests paramétriques sont, eux, plus puissants en général que leurs équivalents non-paramétriques.
# En d’autres termes, un test paramétrique sera plus apte à aboutir à un rejet de H0, si ce rejet est justifié.

