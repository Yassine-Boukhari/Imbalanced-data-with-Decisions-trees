library(zeallot)
#Implémantation pour le calcul du Gain (Cas binaire)
#Faudra donner l'entropie (Cas de ID3)
#Faudra pouvoir utiliser une boucle pour le calcul de l'entropie au sein
#des noeuds
#Donc la première et l'étape la plus cruciale est de donner la fct d'entropie
etpy<-function(u){
  #u est représenté par la distribution des classes
  y=c()
  for (i in seq(1,2)){
    v<-u[i]/sum(u)
    if (v==0){
      y[i]<-0
    }else{
  y[i]<-(-v)*log(v,2)
    }
  }
  sum(y)
}
Gain_info<-function(S,A){
  y<-c()
#où A est une matrice qui représente nos sous-noeuds
for (i in seq(1,dim(A)[1])){
y[i]<-sum(A[i,])/sum(S)*etpy(A[i,])
}
b<-etpy(S)-sum(y)
print(sum(y))
print(b)
}
#Exemple
#Attribut Pif
#Représente nos différentes diqtribution au sous-noeud
#A<-matrix(c(4,0,3,2,2,3),nrow = 3,byrow=TRUE)
#S<-c(9,5)

  
