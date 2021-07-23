etpy<-function(u){
  #u est représenté par la distribution des classes (ex a<-c(3,10), etpy(a))
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
#------------------------------------------------------#
  #Schéma de la fonction d'entropie
a<-seq(0,1,100)
etpy1<-function(u){
  -u*log(u,2)-(1-u)*log(1-u,2)
}
curve(etpy1, from=0, to=1, n=200, xlab="P", ylab="I(S)", col="blue",lwd=2, 
      main="Plot de la mesure d'entropie ")
abline(v=0.5, col="black")
arrows(0.4,0.5,0.5,0.5, length = 0.15, angle = 30, lty=1, lwd=2,col="black")