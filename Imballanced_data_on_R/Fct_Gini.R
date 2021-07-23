#Implémentation de CART
#---------------------------------#
#Comparaison graphique entre entropy et Gini
a<-seq(0,1,100)
etpy<-function(u){
  -u*log(u,2)-(1-u)*log(1-u,2)
}
#--------------------------------#
#Critère de Gini(Cas de 2 classes)
Gini1<-function(u){
  1-u^2-(1-u)^2
}
curve(Gini1, from=0, to=1, n=200, xlab="P", ylab="G(S)", col="black",lwd=2, 
     main="Plot de la mesure Gini ")
#------------------------------------------------#
#Crière de Gini Split
Gini<-function(u){
  1-u^2-(1-u)^2
}
Gini_split<-function(S,A){
  y<-c()
  #S vecteur contenant les éléments du noeud initial
  #où A est une matrice qui représente nos sous-noeuds
  for (i in seq(1,dim(A)[1])){
    y[i]<-sum(A[i,])/sum(S)*Gini(A[i,1]/sum(A[i,]))
  }
sum(y)
}
#Exemple
#Attribut Pif
#Représente nos différentes distribution au sous-noeud
#A<-matrix(c(4,0,3,2,2,3),nrow = 3,byrow=TRUE)
#S<-c(9,5)
#------------------------------------------------#
#1ère Version
a<-curve(Gini1, from=0, to=1, n=200, xlab="P", ylab="G(S)", col="black",lwd=2, 
      main="Plot de la mesure Gini ")
b<-curve(etpy, from=0, to=1, n=200, xlab="P", ylab="I(S)", col="blue",lwd=2, 
         main="Plot de la mesure d'entropie ")
#Représentation et comparaison
#Gini<-function(u){
 # 2*u*(1-u)
#}
#curve(etpy, from=0, to=1, n=200, xlab="P", ylab="I(S)", col="blue",lwd=2, 
 #     main="Plot de la mesure d'entropie ")
#par(new=FALSE)
#curve(Gini1, from=0, to=1, n=200, xlab="P", ylab="G(S)", col="black",lwd=2, 
#     main="Plot de la mesure Gini ")
#-------------------------------------------------#
#Meilleure Verison
library(ggplot2)
library(reshape2)
Gini1<-function(u){
  1-u^2-(1-u)^2
}
etpy<-function(u){
  -u*log(u,2)-(1-u)*log(1-u,2)
}
x  <- seq(0, 1, 0.001)
Gini <- Gini1(x)
Entropie <- etpy(x)
df <- data.frame(x,Gini,Entropie)
df2 <- melt(data = df, id.vars = "x")
ggplot(data = df2, aes(x = x, y = value, colour = variable)) + geom_line()

